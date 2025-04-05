# Libs ----
pacman::p_load(
    ggplot2,
    tidyverse,
    doParallel,
    parallel,
    mlr3measures,
    mlr3verse,
    mlr3learners,
    lgr,
    GA,
    snow,
    mlr3hyperband,
    furrr,
    ggsci,
    patchwork
)

# https://stackoverflow.com/questions/47044068/get-the-path-of-current-script
# get path of source file
getCurrentFileLocation <-  function()
{
    this_file <- commandArgs() %>% 
        tibble::enframe(name = NULL) %>%
        tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
        dplyr::filter(key == "--file") %>%
        dplyr::pull(value)
    if (length(this_file)==0)
    {
        this_file <- rstudioapi::getSourceEditorContext()$path
    }
    return(dirname(this_file))
}

# sets path on source file location
script_path <- getCurrentFileLocation()
setwd(script_path)

# Load data ----

## raw data with all possible features

raw_data <- read_csv("ran_for1.csv") %>% 
    select(-class, -dist)
raw_data

## dat contains all used features
## channel_1 features were removed
## Integrated intensity was computed from initial features

dat <-
    read_csv("ran_for1.csv") %>% 
    select(
        filename,
        neuron_class,
        ID,
        dist_norm,
        Volume:ellipticity_prolate,
        contains("Intensity"),
        pos_z,
        pos_x,
        pos_y
        )  %>% 
    group_by(filename) %>%
    mutate(
        integrated_intensity_ch2 = Intensity_Mean_channel_2 * Volume,
        ch2_residual = integrated_intensity_ch2 - mean(integrated_intensity_ch2[neuron_class == "neuron"])
                ) %>%
    ungroup() %>% 
    select(
        !contains(c("channel_1"))
    )

# Variable correlation ----

feature_vector_corr <-
    dat %>% 
    filter(neuron_class != "unknown") %>% 
    select(!c(filename, neuron_class, ID)) %>% 
    cor() %>% 
    as_tibble(., rownames = "rowname")
feature_vector_corr


# this is for prediction purposes
## is a subset of data containing all label
train_data <-
    dat %>% 
    filter(neuron_class != "unknown")

## this function creates raw or z-score data, was created just in case
## we needed to add multiple data transformations
create_datasets <- function(dataset, type){
    if (type == "z-score"){
        d <-
            dataset %>% 
            group_by(filename) %>% 
            mutate_at(
                which(!(colnames(.) %in% c("filename", "neuron_class", "ID"))), ~scale(.) %>% as_vector
            )
    }
    else if(type == "raw"){
        d <-
            dataset
    }
    return(d %>% ungroup())
}

dat1 <- create_datasets(train_data, "raw") %>% 
    select(-ID)
dat2 <- create_datasets(train_data, "z-score") %>% 
    select(-ID)

datasets <- list(dat1, dat2)


# Pre-plots

## just to check for obvious distribution changes
variable_dist <-
    datasets %>% 
    map(., function(X){
        X %>% 
        pivot_longer(
            cols = which(!(colnames(.) %in% c("filename","neuron_class"))),
            values_to = "val",
            names_to = "var"
        ) %>% 
        ggplot(aes(
            var, val, color = neuron_class
        )) +
        geom_point(position = position_dodge(0.75),
                   shape = 21,
                   size = 1) +
        geom_boxplot(outlier.shape = NA) +
        facet_wrap(~var, scale = "free")
    })

variable_dist[[2]] +
    ggpubr::theme_classic2() +
    xlab("") +
    ylab("")

datasets[[2]] %>% 
    ggplot(aes(
        neuron_class, dist_norm
    )) +
    geom_point() +
    geom_boxplot(width = 0.5,
                 outlier.shape = NA) +
    ggpubr::theme_classic2() +
    theme(
        text = element_text(size = 30)
    ) +
    xlab("")



# Variable vectors

## this is out dependent variable
y <- "neuron_class"
#x <- rep(1, 11)
    


# OPTIONS
options(lgr.default_threshold = 0) # set logger of mlr3 to minimum
# Optimization function ----
#tuneRF_mlr. Function to tune random forest based on dat, binary vector and response variable as a string
tuneRF_mlr <- function(X, y)
{
  # define number of predictors
  x <- length(names(X)) - 3
  # define task and learner - range of sequence is the same for all models
  task = as_task_classif(X, target = y) # Note this fill fail if there are accents or spaces in column names
  task$set_col_roles("filename", roles ="stratum", add_to = NULL, remove_from = NULL)
  learner_train = lrn("classif.ranger",
                      importance = "impurity",
                      mtry  = to_tune(p_int(1, x)), # number of parameters to include in each tree - forcing intergers
                      num.trees = to_tune(p_int(50,500, tag = "budget")), # will be 500
                      sample.fraction = to_tune(0.1, 1),
                      min.node.size = to_tune(p_int(1, # min
                                                    10))) # need to check maximum.
  
  # tune algorithm using grid search
  instance = tune(
    tuner = tnr("hyperband", 
                eta = 2,
                repetitions = 2), # 
    task = task,
    learner = learner_train,
    resampling = rsmp("repeated_cv", folds = 20, repeats = 3), # 5 CV fold for validation of tunning
    measure = msr("classif.precision") # using classification precision as measure of tunning
  )
  # return set parameters
  return(instance)
}


## model fit is made using "raw" dataset without any modifications
RF <-
    datasets[1] %>% 
    map(., function(dat){
    # true model
    rf_optimization <- tuneRF_mlr(
        X = dat, 
        y = y
    )
    
    # shuffled model contains shuffled labels
    rf_optimization_shuffled <- tuneRF_mlr(
        X = dat %>% mutate(neuron_class = sample(neuron_class, replace = FALSE)),
        y = y
    )
    
    ## Optimization results ----
    
    opt_res <-
        as.data.table(rf_optimization$archive)
    opt_res_shuffled <-
        as.data.table(rf_optimization_shuffled$archive)
    
    ce_res <-
        tibble(
            mdl_ce = opt_res$classif.precision,
            shuffled_ce = opt_res_shuffled$classif.precision,
        ) %>% 
        pivot_longer(
            cols = everything(),
            names_to = "var",
            values_to = "val"
        )
    
    
    # Optimal model performance ----
    
    optim_params <- rf_optimization$result_learner_param_vals
    optim_params_shuffled <- rf_optimization_shuffled$result_learner_param_vals
    
    get_cv_performance <- function(dat, target, metric, params){
        perf_task <- as_task_classif(dat, target = target)
        perf_task$set_col_roles("filename", roles = "stratum", add_to = NULL, remove_from = NULL)
        perf_lrn = lrn("classif.ranger")
        perf_lrn$param_set$values = params
        rcv55 <- rsmp("repeated_cv", repeats = 1, folds = 5)
        perf_dat <- rcv55$instantiate(perf_task)
        perf_rsmp <- resample(perf_task, perf_lrn, rcv55, store_models = TRUE)
        perf_res <- perf_rsmp$score(msr(metric), predict_sets = "test") %>% 
            as_tibble()
        return(perf_res)
    }
    
    perf_res <-
        get_cv_performance(dat, "neuron_class", "classif.precision", optim_params)
    perf_res_shuffled <-
        get_cv_performance(dat %>% 
                               mutate(neuron_class=sample(neuron_class, replace=FALSE)), "neuron_class", "classif.precision", optim_params_shuffled)
    
    cv_results <-
        tibble(
            iteration = perf_res$iteration,
            mdl = perf_res$classif.precision,
            mdl_shuffled = perf_res_shuffled$classif.precision
        ) %>% 
        pivot_longer(
            cols = c("mdl", "mdl_shuffled"),
            values_to = "val",
            names_to = "var"
        )
    
    # Variable importance ----
    
    get_importance <- function(learner){
        return(
            enframe(learner$learner[[1]]$importance()) %>% 
                mutate(iteration = learner$iteration)
        )
    }
    
    var_importance <-
        perf_res %>% 
        group_by(iteration) %>% 
        group_split() %>% 
        map_dfr(., function(x){
            get_importance(x)
        }) %>% 
        mutate(mdl = "mdl")
    
    var_importance_shuffled <-
        perf_res_shuffled %>% 
        group_by(iteration) %>% 
        group_split() %>% 
        map_dfr(., function(x){
            get_importance(x)
        }) %>% 
        mutate(mdl = "shuffled")
    
    return(
        list(
            opt_res = opt_res,
            opt_res_shuffled = opt_res_shuffled,
            optim_params = optim_params,
            optim_params_shuffled = optim_params_shuffled,
            goodness_of_fit = perf_res,
            goodness_of_fit_shuffled = perf_res_shuffled,
            var_importance = var_importance,
            var_importance_shuffled = var_importance_shuffled
        )
    )
    })

# save fit results
#saveRDS(RF, "random_forest_fit4.rds")
RF <- readRDS("random_forest_fit4.rds")

# OOB error ----

# load model

RF_mdl <- readRDS("random_forest_fit4.rds")

# compute out of bag error as crude measure of performance
oob_error <-
    RF_mdl[[1]]$goodness_of_fit$learner %>% 
    map(., function(X){
        X$oob_error()
    })
oob_error_shuffled <-
    RF_mdl[[1]]$goodness_of_fit_shuffled$learner %>% 
    map(., function(X){
        X$oob_error()
    })

oob_error_all <-
    tibble(
        mdl = unlist(oob_error),
        shuffled = unlist(oob_error_shuffled)
    )
oob_error_all

oob_error_all %>% 
    pivot_longer(
        cols = everything()
    ) %>% 
    ggplot(aes(
        name, value
    )) +
    geom_boxplot(outlier.shape = NA, width = 0.5) +
    geom_jitter(size = 5) +
    ggpubr::theme_classic2() +
    theme(
        text = element_text(size = 30)
    ) +
    ylab("5 fold cv oob error") +
    xlab("")

# Prediction ----


# graph data contains graph mean distances for the complete datasets
add_data <-
    read_csv("graph_data.csv") %>%
    group_by(filename) 

# filter out z axis position, to add into other datasets
add_pos <-
    read_csv("ran_for1.csv") %>% 
    select(filename, ID, pos_z)

# create dataset with desired transformation to predict
to_pred <- create_datasets(dat, "raw")

pred_dat <-
    to_pred %>% 
    select(-dist_norm, -neuron_class) %>% 
    left_join(., add_data, by = c("ID", "filename")) %>% 
    ungroup()

# re fit model with optimal parameters
rf_lrn <- lrn("classif.ranger")
rf_task <- as_task_classif(dat1,
                           target = "neuron_class")
rf_lrn$param_set$values <- RF_mdl[[1]]$optim_params
rf_lrn$train(rf_task)

# generate predictions
# and add back into original dataset
preds_vec <- predict(rf_lrn, pred_dat)
preds <- 
    to_pred %>% 
    ungroup() %>% 
    mutate(predictions = preds_vec)

# write predictions
#write_csv(x = preds, file = "predictions4.csv")
preds <- read_csv("predictions.csv")

preds %>% 
    group_by(neuron_class)


# just to re-check model performance in this particular run
confusion_mat <- preds %>% 
    filter(neuron_class != "unknown") %>% 
    mutate(
        predictions = as.factor(predictions),
        neuron_class = as.factor(neuron_class)
    )
caret::confusionMatrix(confusion_mat$predictions,
                       confusion_mat$neuron_class)



# add animal metadata
ids <- readRDS("../ID_group.RDS") %>% 
    rename(
        animal_id = ID
    )

pred_merge <-
    preds %>% 
    mutate(animal_id = str_extract(filename, "[0-9]{3}")) %>% 
    left_join(., ids, by = c("animal_id"))

pred_merge %>% 
    filter(neuron_class == "neuron") %>% 
    mutate(
        z_pos_scaled = scale(pos_z),
        t = if_else(z_pos_scaled < 0, "Anterior", "Posterior")
    ) %>% 
    group_by(animal_id, t, group) %>% 
    summarise(integrated_intensity_ch2 = mean(integrated_intensity_ch2)) %>% 
    ggplot(aes(
        t, (integrated_intensity_ch2)
    )) +
    geom_boxplot(
        width = 0.5,
        outlier.shape = NA,
        fill = NA,
        aes(color = group)
    ) +
    geom_line(aes(group = animal_id, color = group)) +
    geom_point(
        size = 3,
        shape = 21,
        aes(fill = group, color = group)
    ) +
    see::geom_violinhalf(scale = c("area"),
                         flip = c(1, 3),
                         alpha = 0.5,
                         color = "black",
                         trim = FALSE,
                         aes(fill = group)
                         ) +
    ggsignif::geom_signif(
        comparisons = list(c("Anterior", "Posterior")),
        map_signif_level = TRUE,
        size = 1.5,
        color = "black",
        annotations = c("")
    ) +
    theme_classic2() +
    theme(
        text = element_text(size = 30),
        legend.position = "none"
    ) +
    xlab("") +
    ylab("Orexin neurons intensity (a.u.)") +
    scale_fill_manual(values = c("gray", "orange")) +
    scale_color_manual(values = c("gray", "orange")) +
    facet_wrap(~group)

mdl <- lmerTest::lmer(
    data = pred_merge %>% 
        filter(neuron_class == "neuron") %>% 
        mutate(
            z_pos_scaled = scale(pos_z),
            t = if_else(z_pos_scaled < 0, "anterior", "posterior")
        ),
    log(integrated_intensity_ch2) ~ group * t + (1 | animal_id),
    control = lme4::lmerControl(optimizer = "bobyqa")
)
summary(mdl)

ttt <- pred_merge %>% 
        filter(neuron_class == "neuron") %>% 
        mutate(
            z_pos_scaled = scale(pos_z),
            t = if_else(z_pos_scaled < 0, "anterior", "posterior")
        )
ttt

mdl_emm <- emmeans::emmeans(
    mdl,
    pairwise ~ group * t,
    type = "response"
)
mdl_emm


# mdl data contains all relevant variables including intensity normalization
# use this for models, ii_rel should be dependent variable
mdl_data <-
    pred_merge %>% 
    ungroup() %>% 
    group_by(ID) %>% 
    mutate(pos_z_scale=scale(pos_z),
           ii=scale(integrated_intensity_ch2)) %>% 
    filter(predictions=="neuron") %>% 
    mutate(
        ii = integrated_intensity_ch2,
        ii_rel = integrated_intensity_ch2 / max(integrated_intensity_ch2)
    )

out_data <-
    mdl_data %>% 
    ungroup() %>% 
    select(
        animal_id, group, pos_z_scale, ii_rel, filename, predictions
    ) %>% 
    rename(
        relative_intensity = ii_rel,
        z_position_scaled = pos_z_scale
    ) %>% 
    mutate(
        z_position_scaled = as.vector(z_position_scaled)
    )

write_csv(out_data, "relative_intesity_data.csv")

counts <- read_csv("relative_intesity_data.csv") %>%
    mutate(
        axis = if_else(z_position_scaled < 0, "Anterior", "Posterior")
    ) %>% 
    group_by(group, axis, animal_id, filename) %>% 
    summarise(
        cnt = n(),
        intensity = mean(relative_intensity)
    ) %>% 
    ungroup() %>% 
    group_by(group, axis, animal_id) %>% 
    summarise(
        cnt = mean(cnt)
    ) %>% 
    drop_na()
counts


sd(counts$cnt)

saveRDS(counts, "../../../thesis_figures/counts.rds")


mdl <- lme4::glmer.nb(
    data = counts %>% mutate(animal_id = as.factor(animal_id)),
    (cnt) ~ group * axis + (1 | animal_id/filename),
    control = lme4::glmerControl(optimizer = "bobyqa")
)
summary(mdl)

emmeans::emmeans(
    mdl,
    pairwise~group |axis
)

counts %>% 
    ggplot(aes(
        group, log(cnt)
    )) +
    geom_point() +
    facet_wrap(~axis)


add_logticks  <- function (base = 10, sides = "bl", scaled = TRUE, 
                           short = unit(0.1, "cm"), mid = unit(0.2, "cm"),  long = unit(0.3, "cm"), 
                           colour = "black",  size = 0.5, linetype = 1, alpha = 1, color = NULL, 
                           data =data.frame(x = NA),... )   {
    if (!is.null(color)) 
        colour <- color
    layer(geom = "logticks", params = list(base = base, 
                                           sides = sides, scaled = scaled, short = short, 
                                           mid = mid, long = long, colour = colour, size = size, 
                                           linetype = linetype, alpha = alpha, ...), 
          stat = "identity", data = data , mapping = NULL, inherit.aes = FALSE, position = "identity",
          show.legend = FALSE)
}

annotation_df <- data.frame(
    axis = c("Posterior"),
    start = c("Control"),
    end = c("Uncertainty"),
    y = c(log(900)),
    label = c("")
)
p1 <- counts %>% 
    mutate(
        group = if_else(group == "Uncertainty", group, "Control")
    ) %>% 
    ggplot(aes(
        group, (cnt), color = group, fill = group
    )) +
    geom_boxplot(
        width = 0.5,
        outlier.shape = NA,
        fill = NA,
        lwd = 0.75
    ) +
    geom_point(
        size = 5,
        shape = 21,
        color = "black",
        alpha = 0.5
    ) +
    ggsignif::geom_signif(
        data = annotation_df,
        tip_length = c(0.02, 0.02),
        inherit.aes = FALSE,
        map_signif_level = FALSE,
        manual = TRUE,
        aes(xmin = start, xmax = end, annotations = label, y_position = y),
        size = 1,
        textsize = 6,
        vjust = 0.5,
        color = "black"
    ) +
    ggpubr::theme_pubr() +
    theme(
        text = element_text(size = 30),
        axis.text=element_text(size=24),
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside"
    ) +
    xlab("") +
    ylab("# OXA+") +
    scale_fill_manual(values = c("black", "orange", "orange")) +
    scale_color_manual(values = c("black", "orange", "orange")) +
    scale_y_continuous(trans = "log", labels = scales::label_number(accuracy = 1)) +
    add_logticks(side = 'l', data = data.frame(x= NA, axis = 'Anterior')) +
    facet_wrap(~axis) +
    scale_x_discrete(labels = c("Ctrl", "Unc", "Ctrl", "Unc"))
p1



intensity_mdl_1 <-
    lmerTest::lmer(
        data = mdl_data,
        ii_rel ~ group * pos_z_scale + (1|animal_id/filename)
    )
summary(intensity_mdl_1)

emmeans::emm_options(lmerTest.limit=13696)
emmeans::emm_options(pbkrtest.limit=13696)
emmeans::emtrends(
    intensity_mdl_1,
    var = "pos_z_scale"
) %>% pairs()

