library(ggplot2)
library(tidyverse)
library(doParallel)
library(parallel)
library(mlr3measures)
library(mlr3verse) # package for optimization of ML models # https://mlr-org.com/
library(mlr3learners) # package for optimization of ML models,
library(lgr) # to control logging all output to a file
library(GA) # genetic algorithm
library(snow)
library(mlr3hyperband)

# OPTIONS
options(lgr.default_threshold = 0) # set logger of mlr3 to minimum
# FUNCTIONS ----
#tuneRF_mlr. Function to tune random forest based on dat, binary vector and response variable as a string
tuneRF_mlr <- function(dat, 
                       x, # vector with selected variables
                       y) # string with name of response variable
{
  # Select data used for testing - the first three variables stay                   
  if(y == "mean.auct") {
    dat <- dat %>% select(!ph) # eliminate response variable  
  } else {
    dat <- dat %>% select(!mean.auct) # eliminate response variable  
  }
  
  dat <- dat[,c(TRUE, # This correspond to response variable
                ifelse(x == 1, TRUE, FALSE))]   
  
  # define task and learner - range of sequence is the same for all models
  task <- as_task_regr(dat, target = y) # Note this fill fail if there are accents or spaces in column names
  learner_train = lrn("regr.ranger",
                      mtry  = to_tune(p_int(1, # min
                                            sum(x == 1) # all variables selected
                      )), # number of parameters to include in each tree - forcing intergers
                      num.trees = to_tune(p_int(50,500, tag = "budget")), # will be 500
                      sample.fraction = to_tune(0.1, 1),
                      min.node.size = to_tune(p_int(1, # min
                                                    30))) # need to check maximum.
  
  # tune algorithm using grid search
  instance = tune(
    tuner = tnr("hyperband", 
                eta = 2,
                repetitions = 1), # 
    task = task,
    learner = learner_train,
    resampling = rsmp("cv", folds = 5), # 5 CV fold for validation of tunning
    measure = msr("regr.rmse") # using mse as measure of tunning
  )
  
  # return set parameters
  return(instance$result_learner_param_vals)
}

# Fitness function. 
fitnessFunctionRF <- function(x, # vector for code of selected dataset 
                              dat) # dataset
{
  
  # has has twice the number of variabbles to test
  xAUC <- x[1:62]
  xpH <- x[63:124]
  
  # Tune RF for AUC and pH separately
  y <- "mean.auct"; learner_tuned_params_AUC <- tuneRF_mlr(dat, xAUC, y)
  y <- "ph"; learner_tuned_params_pH <- tuneRF_mlr(dat, xpH, y)
  
  # Fit model using tuned parameters
  learner_tuned_AUC = lrn("regr.ranger")
  learner_tuned_AUC$param_set$values = learner_tuned_params_AUC
  learner_tuned_pH = lrn("regr.ranger")
  learner_tuned_pH$param_set$values = learner_tuned_params_pH
  
  # Run prediction on 10 fold dataset
  dat10CV <- dat %>%
    slice_sample(n = nrow(dat), replace = FALSE) %>%
    mutate(kFold = rep(1:10, n())[1:n()]) # Create 10-fold cross validation
  
  eucDist <- sapply(1:10, function(kFoldIndex) {
    # Create sets.
    test <- dat10CV[with(dat10CV, kFold == kFoldIndex),-ncol(dat10CV)]
    train <- dat10CV[with(dat10CV, kFold != kFoldIndex),-ncol(dat10CV)]
    
    # Note. The original code was using dplyr here but for some reason I was getting a bug regarding the use of kFold as not found
    # when using parallel processing
    
    # Instantiate tasks
    taskAUC <- as_task_regr(train, target = "mean.auct") # Note this fill fail if there are accents or spaces in column names
    taskpH <- as_task_regr(train, target = "ph") # Note this fill fail if there are accents or spaces in column names
    
    # Train tasks
    learner_tuned_AUC$train(taskAUC)
    learner_tuned_pH$train(taskpH)
    
    # Get predictions
    predAUC <- learner_tuned_AUC$predict_newdata(newdata = test)
    predpH <- learner_tuned_pH$predict_newdata(newdata = test)
    
    # Calculate euclidean distance for this prediction
    mean(sqrt((test$ph - predpH$response)^2 + (test$mean.auct - predAUC$response)^2))
  })

  # Return inverted of average euclidean distance
  return(1/mean(eucDist))
  
}

# DATA
# Load data
ds <- read.csv("final_dataset_filtered.csv") # dataset with choices and AUC
covar_dat <- read.csv("covariates_per_subject.csv") #1069 total subjects - # Load covariate dataset

# Create classification data set
dsReg <- ds %>% 
  mutate(AUCt = (AUC)^(1/3)) %>% 
  filter(context == "typical") %>% 
  group_by(ID) %>% 
  summarise(mean.AUCt = mean(AUCt[sub_healthy_choice == "healthy"]), # get mean averages
            pH = sum(sub_healthy_choice == "healthy") / n(), 
            mean.dHealth = mean(dHealth), 
            mean.dTaste = mean(dTaste)) %>% 
  left_join(., 
            covar_dat, 
            by = "ID") %>% 
  dplyr::select(!X)  %>% 
  mutate(weekend_day = ifelse(weekend_day == "weekday", 0,1 )) %>% # make sure all columns are numeric
  dplyr::select(!context_order) %>% 
  mutate(across(sexo:otras_drogas, as.numeric))
names(dsReg) <- tolower(names(dsReg))

# Eliminate redundant covariates
eliminated_covariates <- c("vegetariano", "vegano", "pescetariano", # consideradas dentro de variable compuesta food_restriction_yes_no
                           "intolerancia_lactosa",  "intolerancia_gluten",  # consideradas dentro de variable compuestas food_intolerance_any
                           "consume_cocaina", "consume_alucinogenos", "consume_otras_drogas", # consideradas dentro de variable otras_drogas
                           "food_approach", # consideradas en variable food_approach
                           "food_avoidance", # consideradas en variable food_avoidance
                           "intolerancia_fructosa", # eliminado porque es constante
                           "anorexia", "bulimia", "trastorno_atracon", # considerados en variable compuesta tca
                           "comuna", # not of interest
                           "anticonceptivo_hormonal", # se elimina de analisis y se deja para sub analisis en mujeres
                           "dias_ultima_menstruacion",# se elimina de analisis y se deja para sub analisis en mujeres
                           "bmi_cat") # se deja numerico
dsReg <- dsReg %>% 
  dplyr::select(!all_of(eliminated_covariates)) %>% 
  dplyr::select(!id)

# Run genetic algorithm 10 times
for (i in 3:9) {
  print(paste("RUN", i, sep = ": "))
  fit <- ga(type = "binary", 
            fitness = fitnessFunctionRF, dsReg, 
            nBits = 124,
            popSize = 200,
            maxiter = 150, 
            run = 10,
            monitor = TRUE,
            parallel = TRUE,
            keepBest = TRUE) 
  save(fit, file = paste("ga_rf_fit_200chr_150iter_10max", i, sep = "_"))
}

