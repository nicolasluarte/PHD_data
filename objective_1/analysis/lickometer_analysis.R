pacman::p_load(
    ggplot2,
    tidyverse
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

# import lickometer data ----
devtools::source_url("https://github.com/lab-cpl/lickometer-library/blob/main/src/lickometer_functions_compilate.R?raw=TRUE")

# load data  ----

raw_data <- load_experiment(
    "../data/metadata/lickometer_metadata.csv",
    "../data/raw/lickometer"
)

pool4 <- raw_data %>% 
    filter(pool %in% c("unc_pool_3", "unc_pool_4")) %>% 
    mutate(n_sesion=if_else(pool =="unc_pool_3" & n_sesion == 8, 10, n_sesion))


pool4$n_sesion %>% unique
pool4$tipo_recompensa %>% unique

dc_data <-
    data_for_demand_curve(
        pool4,
        "sacarosa_crv",
        "events",
        c(10)
    ) %>% drop_na()

dc_fit <-
    parse_demand_curve_fit(dc_data)

groups <- groups %>% 
    mutate(ID = as.character(ID))

dc_params1 <-
    dc_fit$fits %>% 
    left_join(
        ., groups, by = c("ID")
    ) %>% 
    select(
        ID, group, term, estimate, std.error, p.value
    )
dc_params1

saveRDS(dc_params1, "../../thesis_figures/dc_params.rds")

dc_params0 <-
    read_csv("~/scripts/r-scripts/demand_curve_q0_alpha.csv")

bind_rows(
    dc_params0 %>% mutate(across(.cols=everything(), .fns = as.character)),
    dc_params1 %>% mutate(across(.cols=everything(), .fns = as.character))
) %>% 
    write_csv(
        ., "~/repos_sync/experimental_data/demand_curve_q0_alpha.csv"
    )

dc_plot <-
    dc_fit$complete$newdats

dc_names <- names(dc_plot)

groups <- read_csv("~/repos_sync/phd_objective_1/datasets/weights/objective_1_weights.csv") %>% 
    select(ID, group) %>% 
    unique() %>% 
    mutate(ID = as.character(ID))

plot_data <-
    dc_plot %>% 
    imap_dfr(
        ., function(X, idx){
            return(
                X %>%
                    as_tibble() %>% 
                    mutate(
                        id = str_extract(idx, "[0-9]{3}")
                    ) %>% 
                    rename(ID = id)
            )
        }
    ) %>% 
    left_join(
        ., groups, by = c("ID")
    ) %>% 
    select(
        ID, group, x, y, k
    ) %>% 
    mutate(
        ID = as.numeric(ID)
    )

write_csv(
    plot_data,
    "~/repos_sync/experimental_data/ll_mf_494_501/demand_curve_fits.csv"
)

d0 <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/demand_curve_fits.csv")


bind_rows(
    d0,
    plot_data
) %>% 
    ggplot(aes(
        log(x), log(y), color = group, group = ID
    )) +
    geom_line()

pref <-
    pool4 %>% 
    filter(n_sesion %in% c(1, 2, 3)) %>% 
    group_by(
        ID, n_sesion, tipo_recompensa
    ) %>% 
    summarise(
        licks = n(),
        events = length(unique(evento))-1
    ) %>% 
    ungroup() %>% 
    mutate(
    tipo_recompensa = recode(
        tipo_recompensa,
        agua = "water",
        sacarosa= "sucrose"
    )) %>% 
    rename(
        session_number = n_sesion,
        sensor = tipo_recompensa
    ) %>% 
    select(ID, sensor, licks, events, session_number)
pref

pref %>% 
    filter(sensor == "sucrose") %>% 
    summary()
pref0 %>% 
    filter(sensor == "sucrose") %>% 
    summary()

pref0 <-
    read_csv("~/scripts/r-scripts/demand_curve_preference_data.csv")

bind_rows(
    pref0 %>% mutate(across(.cols=everything(), .fns = as.character)),
    pref %>% mutate(across(.cols=everything(), .fns = as.character))
) %>% 
    write_csv("~/repos_sync/experimental_data/demand_curve_preference_data.csv")



