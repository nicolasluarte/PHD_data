# Optimization ----

# true model
rf_optimization <- tuneRF_mlr(
    dat = dat, 
    y = y
)

# shuffled model
rf_optimization_shuffled <- tuneRF_mlr(
    dat = dat %>% mutate(neuron_class = sample(neuron_class, replace = FALSE)),
    y = y
)

## Optimization results ----

opt_res <-
    as.data.table(rf_optimization$archive)
opt_res_shuffled <-
    as.data.table(rf_optimization_shuffled$archive)

precision_res <-
    tibble(
        mdl_precision = opt_res$classif.precision,
        shuffled_precision = opt_res_shuffled$classif.precision,
    ) %>% 
    pivot_longer(
        cols = everything(),
        names_to = "var",
        values_to = "val"
    )

or1 <-
    precision_res %>% 
    ggplot(aes(
        val, fill = var
    )) +
    geom_histogram(bins = 60)
or1

or2 <-
    opt_res %>% 
    ggplot(aes(
        mtry, min.node.size, fill = classif.precision
    )) +
    geom_tile(
              color = "black",
              lwd = 0.1,
              linetype = 1) +
    coord_fixed() +
    scale_fill_gradientn(colours = terrain.colors(10)) +
    facet_wrap(~num.trees)
or2

# Optimal model performance ----

optim_params <- rf_optimization$result_learner_param_vals
optim_params_shuffled <- rf_optimization_shuffled$result_learner_param_vals

get_cv_performance <- function(dat, target, metric, params){
    perf_task <- as_task_classif(dat, target = target)
    perf_lrn = lrn("classif.ranger")
    perf_lrn$param_set$values = params
    rcv55 <- rsmp("repeated_cv", repeats = 5, folds = 5)
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


omp1 <-
    cv_results %>% 
    ggplot(aes(
        iteration, val, color = var
    )) +
    geom_line() +
    geom_point()
omp1

omp2 <-
    cv_results %>% 
    ggplot(aes(
        val, fill = var
    )) +
    geom_density()
omp2

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

vi1 <-
    bind_rows(var_importance, var_importance_shuffled) %>% 
    ggplot(aes(
        reorder(name, -value), value, color = mdl
    )) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(position = position_dodge(0.9)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
vi1

# Predictions ----
pred_dat <-
    read_csv("ran_for.csv") %>% 
    select(
        neuron_class,
        Volume:sum_intensity,
        dist,
        dist_norm
    ) %>% 
    filter(
        neuron_class == "unknown"
    ) %>% 
    mutate(
        dist = replace_na(dist, 0),
        dist_norm = replace_na(dist_norm, 0),
    ) %>% 
    select(!neuron_class)

rf_lrn <- lrn("classif.ranger")
rf_task <- as_task_classif(dat, target = "neuron_class")
rf_lrn$param_set$values <- optim_params
rf_lrn$train(rf_task)
preds <- predict(rf_lrn, pred_dat)
preds <- 
    pred_dat %>% 
    mutate(predictions = preds)

preds %>% 
    group_by(predictions) %>% 
    summarise(
        cnt = n()
    )