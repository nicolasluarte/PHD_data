pacman::p_load(
    tidyverse,
    ggplot2,
    ggpubr,
    lme4,
    lmerTest,
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

#################################################################
##                    Long term uncertainty                    ##
#################################################################

# font size
fs = 52
t = 0
b = 0
r = 0
l = 0
unc <- "#56B4E9"
ctrl <- "black"

# FIGURE A
# Weight increase as %

ltu_figA_data_pool1 <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/weights.csv")
ltu_figA_data_pool2 <- read_csv("~/repos_sync/experimental_data/ll_mf_494_501/weights.csv")
ltu_figA_data <- bind_rows(ltu_figA_data_pool1, ltu_figA_data_pool2) %>% 
    group_by(ID) %>% 
    mutate(
        init_weight = head(weight, 1),
        week = as.numeric(as.factor(lubridate::week(date)))-1,
        date = as.numeric(as.factor(date))
        ) %>%
    ungroup() %>% 
    filter(date <= 30) %>% 
    mutate(weight_increase_perc = ((weight / init_weight) - 1) * 100)

ltu_figA <- ltu_figA_data %>% 
    group_by(ID, group) %>% 
    summarise(
        weight_increase_perc =((tail(weight, 1) / weight[1]) - 1) * 100
    ) %>% 
    ggplot(aes(
        group, weight_increase_perc, color = group
    )) +
    geom_boxplot(outlier.shape = NA, linewidth = 2) +
    geom_point(shape = 0, size = 8) +
    theme_pubr() +
    xlab("") +
    ylab("% wt increase") +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_discrete(labels = c("Cer", "Unc")) +
    scale_y_continuous(breaks = seq(-5, 30, 5), 
                       expand = c(0,0)) +
    coord_cartesian(ylim=c(-5, 30)) +
    theme(
        text = element_text(size = fs),
        legend.position = "none",
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
    ) +
    coord_cartesian(ylim = c(-5, 30))
ltu_figA
    

# FIGURE A'
# Weight increase as %, time course

ltu_figA2 <- ltu_figA_data %>% 
    ggplot(aes(
        week, weight_increase_perc, group = ID, color = group
    )) +
    stat_summary(
        aes(group = interaction(week , ID)),
        fun = "median",
        geom = "point",
        size = 8,
        shape = 0
    ) +
    stat_summary(
        aes(group = group),
        fun.data = "mean_cl_boot",
        geom = "point",
        size = 8,
        shape = 15
    ) +
    stat_summary(
        aes(group = group),
        fun.data = "mean_cl_boot",
        geom = "errorbar",
        width = 0.2,
        linewidth = 1
    ) +
    stat_summary(
        aes(group = group),
        fun.data = "mean_cl_boot",
        geom = "line",
        linewidth = 1
    ) +
    xlab("") +
    ylab("% wt increase") +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7 , 8, 9, 10),
                       expand = c(0, 0.6)) +
    theme_pubr() +
    theme(
        text = element_text(size = fs),
        legend.position = "none",
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
    ) +
    scale_y_continuous(breaks = seq(-30, 30, 10), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(-10, 30))
ltu_figA2
    



# FIGURE B
# Mean number of pellets

ltu_figB_data_pool1 <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/daily_intake.csv")
ltu_figB_data_pool2 <- read_csv("~/repos_sync/experimental_data/ll_mf_494_501/daily_intake.csv")
ltu_figB_data <- bind_rows(ltu_figB_data_pool2, ltu_figB_data_pool1)
ltu_figB <- ltu_figB_data %>% 
    group_by(ID, group) %>% 
    summarise(intake_ = median(intake)) %>% 
    rename(intake = intake_) %>% 
    ggplot(aes(
        group, intake, color = group
    )) +
    geom_boxplot(outlier.shape = NA, linewidth = 2) +
    geom_point(
        shape = 0,
        size = 8
    ) +
    theme_pubr() +
    xlab("") +
    ylab("Mean # pellets") +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_discrete(labels = c("Cer", "Unc")) +
    theme(
        text = element_text(size = fs),
        legend.position = "none",
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
    ) +
    scale_y_continuous(breaks = seq(40, 80, 10), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(40, 80))
ltu_figB

# FIGURE C
# Energy efficiency
exp_weight <- ltu_figA_data %>% 
    group_by(ID, group) %>% 
    summarise(weight_ = median(weight)) %>% 
    rename(weight = weight_)
exp_intake <- ltu_figB_data %>% 
    group_by(ID) %>% 
    summarise(intake_ = median(intake)) %>% 
    rename(intake = intake_)
merge_intake_weight <- left_join(exp_weight, exp_intake, by = c("ID")) %>% 
    mutate(eff = weight / intake)

mdl_figC <- lm(
    data = merge_intake_weight,
    eff ~ group
)
summary(mdl_figC)

ltu_figC <- merge_intake_weight %>% 
    ggplot(aes(group, eff, color = group)) +
    geom_boxplot(
        outlier.shape = NA,
        linewidth = 2
    ) +
    geom_point(
        shape = 0,
        size = 8
    ) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_discrete(labels = c("Cer", "Unc")) +
    xlab("") +
    ylab("Wt (gr) / # pellets") +
    theme_pubr() +
    theme(
        text = element_text(size = fs),
        legend.position = "none",
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
    ) +
    scale_y_continuous(breaks = seq(0.3, 0.6, 0.1), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(0.3, 0.6))
ltu_figC

# FIGURE D
# Retrieval times

retrieval_times <- readRDS("intake_data.rds")
ret <- retrieval_times %>% 
  filter(week > 1, group == "uncertainty") %>% 
  mutate(hour = lubridate::hour(time)) %>% 
  group_by(ID, hour) %>% 
  dplyr::slice(-1) %>% # remove first removal, corresponds to previous block
  dplyr::mutate(
    ret = (time - lag(time)) - delay
  ) %>% 
  filter(ret < 3600, delay %in% c(15, 60, 120, 180, 240, 300)) %>%  # this are sensor probes without pellet actually being there, max ret is 1 hour by definition
  mutate(ret = as.numeric(ret)) %>% 
  ungroup() %>% 
  group_by(ID) %>% 
  mutate(z_ret = scale(ret))
# categorize retrieval times
ret_cat <- ret %>% 
  group_by(ID) %>% 
  mutate(
    qntls = as.numeric(as.factor(cut(
      ret, 
      breaks = quantile(ret, probs=seq(0,1, by=0.25), na.rm=TRUE),
      include.lowest=TRUE
    )))
  )
# proportions of retrievals
ltu_figD_data <- ret_cat %>% 
    filter(week <= 8) %>% 
  mutate(slow_fast = case_when(
      qntls %in% c(1, 2) ~ "Fast",
      qntls %in% c(3, 4) ~ "Slow"
      ),
         init_end = if_else(week <= 4, "3-6", "7-10")) %>% 
  group_by(ID, init_end) %>% 
  mutate(cnt = n()) %>% 
  ungroup() %>% 
  group_by(ID, init_end, cnt, slow_fast) %>% 
  summarise(
    cc = n()
  ) %>% 
  ungroup() %>% 
  group_by(ID, init_end, slow_fast) %>% 
  mutate(prop = cc / cnt)
ltu_figD <- ltu_figD_data %>% 
    mutate(prop = prop * 100) %>% 
  ggplot(aes(
    as.factor(init_end), prop, color = as.factor(slow_fast)
  )) +
    geom_boxplot(
                 position = position_dodge(0),
                 outlier.shape = NA,
                 linewidth = 2) +
    geom_line(aes(group = interaction(ID, slow_fast)), color = "gray70") +
    geom_point(shape = 0,
               size = 8
               ) +
  theme_pubr() +
    theme(
        text = element_text(size = fs),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
        ) +
    scale_color_manual(values = c("#E3A97E", "#415B71")) +
  ylab("% of total") +
  xlab("") +
  theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(40, 60, 5), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(40, 60))
ltu_figD

# Long term uncertainty row 1

A <- ltu_figA2 + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
B <- ltu_figA + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
C <- ltu_figB + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
D <- ltu_figC + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
E <- ltu_figD + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))

aa <- wrap_plots(A, B, ncol = 2, nrow =1, widths = c(2, 1))
bb <- wrap_plots(C, D, E, ncol = 3, nrow = 1)

ltu_row <- wrap_plots(aa, bb, ncol = 1, nrow = 2) +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 32, face = 'bold'))
svg("panel1.svg", width = 15.11, height = 6.59)
ltu_row
dev.off()


# FIGURE F
# Licks and events from demand curve

# load lickometer data

# load lickometer library
devtools::source_url("https://github.com/lab-cpl/lickometer-library/blob/main/src/lickometer_functions_compilate.R?raw=TRUE")

ltu_figF_data <- load_experiment(
  '../data/metadata/lickometer_metadata.csv',
  '../data/raw/lickometer'
)

# filter demand curve sessions

last_session_date <- ltu_figF_data %>% 
    select(ID, fecha) %>% 
    group_by(ID) %>% 
    filter(fecha == max(fecha)) %>% 
    distinct(fecha) %>% 
    pull(fecha) %>% 
    unique()
    
demand_curve_sessions <- ltu_figF_data %>% 
    filter(fecha %in% last_session_date)

#saveRDS(demand_curve_sessions, "demand_curve.RDS")

# get groups
ID_group <- ltu_figA_data %>% 
    select(ID, group) %>% 
    distinct(ID, group) %>% 
    mutate(
        ID = as.factor(ID),
        group = as.factor(group)
    )

licks_events <- demand_curve_sessions %>% 
    group_by(ID, tipo_recompensa) %>% 
    summarise(
        e = max(evento),
        l = n()
    ) %>% 
    pivot_longer(
        cols = c("e", "l")
    ) %>% 
    left_join(., ID_group, by = c("ID"))


ltu_figF <- licks_events %>% 
    filter(name == "e") %>% 
    ggplot(aes(
        tipo_recompensa, value, color = group
    )) +
    geom_line(
        aes(group = interaction(ID, name)),
        color = "gray"
    ) +
    geom_boxplot(
        outlier.shape = NA,
        linewidth = 2,
        fill = NA
    ) +
    geom_point(
        size = 8,
        shape = 0
    ) +
    facet_wrap(~group) +
    theme_pubr() +
    theme(
        legend.position = "none",
        text = element_text(size = fs),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
    ) +
    scale_y_continuous(breaks = seq(0, 80, 20), 
                       expand = c(0,0)) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_discrete(labels = c("W", "S")) +
    coord_cartesian(ylim = c(0, 80)) +
    xlab("") +
    ylab("# rewards")
ltu_figF

# FIGURE G
# Licks and events from demand curve

ltu_figG <- licks_events %>% 
    filter(name == "l") %>% 
    ggplot(aes(
        tipo_recompensa, value, color = group
    )) +
    geom_line(
        aes(group = interaction(ID, name)),
        color = "gray"
    ) +
    geom_boxplot(
        outlier.shape = NA,
        linewidth = 2,
        fill = NA
    ) +
    geom_point(
        size = 8,
        shape = 0
    ) +
    facet_wrap(~group) +
    theme_pubr() +
    theme(
        legend.position = "none",
        text = element_text(size = fs),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
    ) +
    scale_y_continuous(breaks = seq(0, 6000, 2000), 
                       expand = c(0,0)) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_discrete(labels = c("W", "S")) +
    coord_cartesian(ylim = c(0, 6000)) +
    xlab("") +
    ylab("# licks")
ltu_figG

# Figure H
# demand curve: individual curves

ltu_figH_data <- readRDS("demand_curve_data.RDS") %>%
    left_join(., ID_group, by = "ID")

ltu_figH <- ltu_figH_data %>% 
    mutate(
        x = log(x),
        y = log(y)
    ) %>% 
    ggplot(aes(
        x, y, color = group, group = ID
    )) +
    geom_line(
        linewidth = 2
    ) +
    scale_y_continuous(
                       breaks = seq(-1.0, 3.5, 1.5),
                       expand = c(0, 0)
                       ) +
    scale_x_continuous(breaks = seq(1, 4.5, 0.5)) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    theme_pubr() +
    theme(
        legend.position = "none",
        text = element_text(size = fs),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
    ) +
    coord_cartesian(ylim = c(-1.0, 3.5), xlim = c(1.5, 4.5)) +
    ylab("Log rewards") +
    xlab("Log cost")
ltu_figH

# FIGURE I
# models results for q0 and alpha

ltu_figI_data <- ltu_figH_data %>% 
    pivot_longer(
        cols = c("estimate_q0", "estimate_alpha")
    ) %>% 
    distinct(ID, name, value, group) %>% 
    group_by(name) %>% 
    group_split()

ltu_figI_mdl <- ltu_figI_data %>% 
    map(.,
        function(parameter){
            p <- parameter$name %>% unique()
            mdl <- lm(
                data = parameter,
                value ~ group
            )
            emm <- mdl %>% 
                emmeans::emmeans(~group) %>% 
                confint()
            return(broom::tidy(emm) %>% mutate(param = p))
        })

ltu_figI <- ltu_figI_data[[1]] %>% 
    ggplot(aes(
        group, value*10000, color = group
    )) +
    geom_boxplot(outlier.shape = NA, linewidth = 2) +
    geom_point(
        shape = 0,
        size = 8
    ) +
    ylab("α est x 10^4") +
    xlab("") +
    theme_pubr() +
      theme(legend.position = "none",
        text = element_text(size = fs),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
            plot.margin = margin(
                t = t,
                b = b,
                r = r,
                l = l
            )
            ) +
      scale_y_continuous(expand = c(0, 0),
                        breaks = seq(0, 15, 5),
      ) +
        coord_cartesian(ylim = c(0, 15)) +
      scale_color_manual(values = c('No-Uncertainty'="black", 'Uncertainty'= unc)) +
      scale_x_discrete(labels = c("Cer", "Unc")) 
ltu_figI

ltu_figJ <- ltu_figI_data[[2]] %>% 
    ggplot(aes(
        group, value, color = group
    )) +
    geom_boxplot(outlier.shape = NA, linewidth = 2) +
    geom_point(
        shape = 0,
        size = 8
    ) +
    ylab("Q0 est") +
    xlab("") +
    theme_pubr() +
      theme(legend.position = "none",
        text = element_text(size = fs),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
            plot.margin = margin(
                t = t,
                b = b,
                r = r,
                l = l
            )
            ) +
      scale_y_continuous(expand = c(0, 0),
                        breaks = seq(0, 30, 7.5),
      ) +
        coord_cartesian(ylim = c(0, 30)) +
      scale_color_manual(values = c('No-Uncertainty'="black", 'Uncertainty'= unc)) +
      scale_x_discrete(labels = c("Cer", "Unc")) 
ltu_figJ



A <- ltu_figF + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt")) 
B <- ltu_figG + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
C <- ltu_figH + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
D <- ltu_figI + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
E <- ltu_figJ + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))

aa <- (A | B) / C
bb <- D / E
ltu_row2 <- wrap_plots(aa, bb, ncol = 2, nrow = 1) +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 32, face = 'bold'))
svg("panel3.svg", width = 15.11, height = 6.59)
print(ltu_row2)
dev.off()




    
crv_example <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/demand_curve_fits.csv")

ltu_figD <- crv_example %>% 
    ggplot(aes(
        x, y, group = ID, color = group
    )) +
    geom_line(linewidth = 2) +
    theme_pubr() +
    theme(
        legend.position = "none",
        text = element_text(size = fs)
    ) +
    ylab("Demand") +
    xlab("Cost") +
    scale_y_continuous(
        breaks = seq(0, 15, 5),
        limits = c(0, 15),
        expand = c(0, 0)
    ) +
    labs(tag = "D") +
    scale_color_manual(values = c("black", "#54ABDA")) +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(breaks = seq(0, 15, 5), 
                       limits = c(0,15), 
                       expand = c(0,1))

# FIGURE E
# Q0 and alpha estimates
demand_curve_plot <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/demand_curve_q0_alpha.csv")
ltu_figE <- demand_curve_plot %>% 
    mutate(term = if_else(term == "alpha", "Alpha", "Q0")) %>% 
    ggplot(aes(
        group, estimate, color = group
    )) +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "point",
        size = 4,
        shape = 15
    ) +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "errorbar",
        width = 0.2
    ) +
    geom_point(shape = 0, size = 4) +
    scale_color_manual(values = c("black", "#54ABDA")) +
    facet_wrap(~term, scales = "free") +
    theme_pubr() +
    theme(
        legend.position = "none",
        text = element_text(size = fs),
        strip.background = element_blank(),
        strip.text.x = element_blank()
    ) +
    scale_x_discrete(breaks = c()) +
    ylab("Estimates") +
    xlab("") +
    labs(tag = "E") +
    theme(legend.position = "none")

cowplot::plot_grid(
    ltu_figD,
    ltu_figE,
    col = 2
)




# figure food availability and obesity

# fed intake data
intake_files <- list.files(path = "~/repos_sync/nbolab_FED/raw_data",
                          pattern = "(494|495|496|497|498|499|500|501).CSV",
                          recursive = TRUE,
                          full.names = TRUE)
raw_files <- intake_files %>% 
    map_dfr(., function(x){
        read_csv(x) %>% 
            mutate(animal = as.numeric(if_else(animal == "00", "498", as.character(animal))))
    })

intake_files <- raw_files %>% 
    filter(delay != "01") %>%
    mutate(
        group = if_else(animal %in% c(495, 496, 497, 500), "Uncertainty", "No-Uncertainty"),
        experimental_phase = case_when(
            protocol == "basal" ~ "Baseline",
            TRUE ~ "Experimental"
        ),
        time = lubridate::as_datetime(time),
        week = as.numeric(as.factor(week(time))),
        week = week - min(week)
    ) %>% 
    filter(lubridate::year(time) == 2023) %>% 
    select(
        -c("protocol", "motorTurns", "battery")
    ) %>% 
    arrange(sort(desc(time))) %>% 
    mutate(date = lubridate::date(time)) %>% 
    group_by(animal, group, experimental_phase, date) %>% 
    summarise(intake = n()) %>% 
    rename(ID = animal) %>% 
    select(ID, experimental_phase, group, date, intake)


# figure retrieval times

retrieval_times <- readRDS("intake_data.rds")

ret <- retrieval_times %>% 
  filter(week > 1, group == "uncertainty") %>% 
  mutate(hour = lubridate::hour(time)) %>% 
  group_by(ID, hour) %>% 
  dplyr::slice(-1) %>% # remove first removal, corresponds to previous block
  dplyr::mutate(
    ret = (time - lag(time)) - delay
  ) %>% 
  filter(ret < 3600, delay %in% c(15, 60, 120, 180, 240, 300)) %>%  # this are sensor probes without pellet actually being there, max ret is 1 hour by definition
  mutate(ret = as.numeric(ret)) %>% 
  ungroup() %>% 
  group_by(ID) %>% 
  mutate(z_ret = scale(ret))
# categorize retrieval times
ret_cat <- ret %>% 
  group_by(ID) %>% 
  mutate(
    qntls = as.numeric(as.factor(cut(
      ret, 
      breaks = quantile(ret, probs=seq(0,1, by=0.25), na.rm=TRUE),
      include.lowest=TRUE
    )))
  )
# proportions of retrievals
proportions <- ret_cat %>% 
    filter(week <= 8) %>% 
  mutate(slow_fast = case_when(
      qntls %in% c(1, 2) ~ "Fast",
      qntls %in% c(3, 4) ~ "Slow"
      ),
         init_end = if_else(week <= 4, "First weeks", "Last weeks")) %>% 
  group_by(ID, init_end) %>% 
  mutate(cnt = n()) %>% 
  ungroup() %>% 
  group_by(ID, init_end, cnt, slow_fast) %>% 
  summarise(
    cc = n()
  ) %>% 
  ungroup() %>% 
  group_by(ID, init_end, slow_fast) %>% 
  mutate(prop = cc / cnt)

proportions %>% 
  ggplot(aes(
    as.factor(init_end), prop * 100, color = as.factor(slow_fast)
  )) +
    geom_line(aes(group = interaction(ID, slow_fast)), color = "gray70", alpha = 0.5) +
  stat_summary(aes(group=slow_fast),
               fun.data="mean_cl_boot",
               geom="point",
               size = 3) +
  stat_summary(aes(group=slow_fast),
               fun.data="mean_cl_boot",
               geom="errorbar",
               width = 0.2,
               linewidth = 1) +
    geom_point(shape = 0,
               size = 4) +
  theme_pubr() +
    theme(text = element_text(size = 20)) +
    scale_color_manual(values = c("red", "blue")) +
  ylab("% of total") +
  xlab("") +
  labs(color = 'Retrieval speed')



# figure intake

intake_data <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/daily_intake.csv")

intake_data %>% 
    ggplot(aes(
        group, intake, color = group
    )) +
    stat_summary(
        aes(group = group),
        fun.data = "mean_cl_boot",
        geom = "point",
        size = 4
    ) +
    stat_summary(
        aes(group = group),
        fun.data = "mean_cl_boot",
        geom = "errorbar",
        width = 0.2,
        linewidth = 1
    ) +
    stat_summary(
        aes(group = ID),
        fun.data = "mean_cl_boot",
        geom = "point",
        shape = 0,
        size = 4
    ) +
    theme_pubr() +
    xlab("") +
    ylab("Mean # pellets") +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    theme(
        text = element_text(size = 20),
        legend.position = "none"
    )

# figure weight

weight_data <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/weights.csv")

weight_data %>% 
    group_by(ID, group) %>% 
    summarise(
        weight_increase_perc =((tail(weight, 1) / weight[1]) - 1) * 100
    ) %>% 
    ggplot(aes(
        group, weight_increase_perc, color = group
    )) +
    stat_summary(
        aes(group = group),
        fun.data = "mean_cl_boot",
        geom = "point",
        size = 4
    ) +
    stat_summary(
        aes(group = group),
        fun.data = "mean_cl_boot",
        geom = "errorbar",
        width = 0.2,
        linewidth = 1
    ) +
    geom_point(shape = 0, size = 4) +
    theme_pubr() +
    xlab("") +
    ylab("% wt increase") +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_y_continuous(breaks = seq(0, 30, 5), 
                       limits = c(0, 30), 
                       expand = c(0,0)) +
    theme(
        text = element_text(size = 20),
        legend.position = "none"
    )

# CI for association between food insecurity and BMI
fi_bmi <- read_csv("fi_ci.csv")

fi_bmi %>% 
    filter(BMI == "Obese") %>% 
    ggplot(aes(
        interaction(Sex, BMI, sep = ' '), OR, ymax = high, ymin = low
    )) +
    geom_hline(yintercept = 1, color = "gray70") +
    geom_point(shape = 0, size = 5) +
    geom_errorbar(width = 0.2) +
    scale_y_continuous(breaks = seq(0.75, 1.5, 0.25), 
                       limits = c(0.75, 1.5), 
                       expand = c(0,0)) +
    xlab("") +
    theme_pubr() +
    theme(
        text = element_text(size = 20)
    )

# figure: association between FI level and BMI
fi_level_bmi <- read_csv("fi_level_bmi.csv")

fi_level_bmi %>% 
    filter(FI_level == "Mild") %>% 
    ggplot(aes(
        BMI, OR, ymin = low, ymax = high
    )) +
    geom_hline(yintercept = 1, color = "gray70") +
    geom_point(shape = 0, size = 5) +
    geom_line(aes(group = 1)) +
    geom_errorbar(width = 0.2) +
    scale_y_continuous(breaks = seq(0, 1.75, 0.25), 
                       limits = c(0, 1.75), 
                       expand = c(0,0)) +
    xlab("") +
    theme_pubr() +
    theme(
        text = element_text(size = 20)
    )
    


plot_data <- readRDS(file = "plot_3_data.rds")


# figure 1: pr licks over sessions

plot_data %>% 
    mutate(pre_post = if_else(session <=4, "First sessions", "Last sessions")) %>% 
    filter(
        task_type == "pr",
        tipo_recompensa == "sacarosa",
        experiment_id == "high_uncertainty_fed"
    ) %>% 
    ggplot(aes(
        pre_post, n_licks, fill = group, color = group)) +
    stat_summary(
        fun.data = "mean_cl_boot",
        aes(group = group),
        geom = "col",
        position = position_dodge(0.9)
    ) +
    stat_summary(
        fun.data = "mean_cl_boot",
        aes(group = group),
        geom = "errorbar",
        position = position_dodge(0.9)
    ) +
    theme_pubr() +
    scale_y_continuous(breaks = seq(0, 2000, 500), 
                       limits = c(0,2000), 
                       expand = c(0,0)) +
    scale_fill_manual(values = c("black", "#54ABDA")) +
    scale_color_manual(values = c("black", "#54ABDA")) +
    ylab("# licks") +
    xlab("") + 
    theme(
        legend.position = "none",
        text = element_text(size = 20)
    )

# figure 2: slopes
model_data <- plot_data %>% 
    filter(
        task_type == "pr",
        tipo_recompensa == "sacarosa",
        experiment_id == "high_uncertainty_fed"
    )
model <- glmer.nb(
    data = model_data,
    n_licks ~ group * session + (1 | ID)
)
summary(model)

slopes <- emmeans::emtrends(model, ~ group, var = "session", trans = "response") %>% 
    broom::tidy()
slopes

slopes %>% 
    ggplot(aes(
        group, session.trend, fill = group, color = group,
        ymin = session.trend - std.error,
        ymax = session.trend + std.error
    )) +
    geom_errorbar(width = 0.1, linewidth = 2) +
    geom_col() +
    theme_pubr() +
    theme(
        legend.position = "none",
        text = element_text(size = 20)
    ) +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values = c("black", "#54ABDA")) +
    scale_color_manual(values = c("black", "#54ABDA")) +
    ylab("Slope estimates") +
    xlab("") +
    scale_x_discrete(breaks = c()) +
    scale_y_continuous(breaks = seq(-100, 100, 50), 
                       limits = c(-100,100), 
                       expand = c(0,0))

# figure 3: demand curve

demand_curve_plot <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/demand_curve_q0_alpha.csv")

demand_curve_plot %>% 
    mutate(term = if_else(term == "alpha", "Alpha", "Q0")) %>% 
    ggplot(aes(
        group, estimate, color = group
    )) +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "point",
        size = 3
    ) +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "errorbar",
        width = 0.2
    ) +
    geom_point(shape = 0, size = 4) +
    scale_color_manual(values = c("black", "#54ABDA")) +
    facet_wrap(~term, scales = "free") +
    theme_pubr() +
    scale_x_discrete(breaks = c()) +
    ylab("Estimates") +
    xlab("") +
    theme(legend.position = "none")
    
# figure 4: demand curve examples
    
crv_example <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/demand_curve_fits.csv")

crv_example %>% 
    ggplot(aes(
        x, y, group = ID, color = group
    )) +
    geom_line() +
    theme_pubr() +
    theme(
        legend.position = "none",
        text = element_text(size = 20)
    ) +
    ylab("Demand") +
    xlab("Cost") +
    scale_y_continuous(
        breaks = seq(0, 15, 5),
        limits = c(0, 15),
        expand = c(0, 0)
    ) +
    scale_color_manual(values = c("black", "#54ABDA")) +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(breaks = seq(0, 15, 5), 
                       limits = c(0,15), 
                       expand = c(0,0))

# figure 5: length of clusters

cluster_length <- readRDS(file = "plot_4_data.rds")

cluster_length %>% 
    filter(group != "experimental_low_unc") %>% 
    ggplot(aes(
        group, session.trend,
        ymin = session.trend - SE,
        ymax = session.trend + SE,
        color = group, fill = group
    )) +
    geom_errorbar(width = 0.1, linewidth = 2) +
    geom_col() +
    theme_pubr() +
    theme(
        legend.position = "none",
        text = element_text(size = 20)
    ) +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values = c("black", "#54ABDA")) +
    scale_color_manual(values = c("black", "#54ABDA")) +
    ylab("Meal size slope") +
    xlab("") +
    scale_x_discrete(breaks = c()) +
    scale_y_continuous(breaks = seq(-50, 100, 50), 
                       limits = c(-50,100), 
                       expand = c(0,0))

#################################################################
##                    Short term uncertainty                   ##
#################################################################

# FIGURE A
# licks pre post

stu_figA_data <- readRDS('../../objective_2/analysis/plot_2_data.rds') %>% 
  ungroup() %>% 
  group_by(ID, period, exp_group) %>% 
  summarise(
    l = mean(l),
    e = mean(e)
  ) %>% 
  mutate(
    period = recode(period, basal = "Base", experimental = "Exp")
  )
stu_figA <- stu_figA_data %>% 
  ggplot(aes(period, l,
             group = period,
             color = exp_group,
             #color= interaction(exp_group, period)
             )) +
  facet_wrap(~exp_group, strip.position = "bottom") +
  geom_line(aes(group = ID), color = "gray") +
  geom_boxplot(outlier.shape = NA, linewidth = 2, fill = NA) +
  geom_point(size = 8, shape = 0) +
  theme_pubr() +
  theme(
        text = element_text(size = fs),
        legend.position = "none",
        strip.text.x = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank()
        ) +
  scale_color_manual(values = c("black", "#52AAD9"),
                     ) +
  ylab('Total # licks') +
  xlab("") +
    scale_y_continuous(breaks = seq(0, 2000, 1000), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(0, 2000))
stu_figA

# FIGURE B
# licks over time bins

stu_figB_data <- readRDS('../../objective_2/analysis/plot_3_data') %>% 
  filter(exp_phase == 'experimental') %>% 
  ungroup() %>% 
  group_by(ID, exp_group, bins, n_sesion) %>% 
  summarise(
    l = median(l)
  ) %>% 
  ungroup() %>% 
  group_by(ID, exp_group, bins) %>% 
  summarise(
    l = median(l)
  ) %>% 
  mutate(
    exp_group = recode(exp_group, control = "Control", experimental = "Uncertainty")
  )
stu_figB <- stu_figB_data %>% 
  ggplot(aes(
    as.numeric(bins), l, color = exp_group
  )) +
  geom_point(size = 8,
             shape = 0) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = 'line',
               aes(group = exp_group),
               linewidth = 1) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = 'point',
               aes(group = exp_group),
               size = 8,
               shape = 15) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", aes(group = exp_group), width = .2) +
  theme_pubr() +
  ylab('# licks') +
  xlab('10 min intervals') +
  scale_color_manual(values = c(ctrl, unc)) +
  theme(
    text = element_text(size = fs),
    legend.position = 'none',
    legend.title = element_blank(),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
  ) +
    scale_y_continuous(breaks = seq(0, 500, 250), 
                       expand = c(0,0)) +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6),
                       expand = c(0, 0.6)) +
    coord_cartesian(ylim = c(0, 500))
stu_figB


# FIGURE C
# Expected number of licks

stu_figC_data <- readRDS('../../objective_2/analysis/plot_1_data.rds') %>% 
  ungroup() %>% 
  group_by(
    ID, exp_group, period, group
  ) %>% 
  summarise(
    l = mean(l),
    e = mean(e)
  ) %>% 
  mutate(unc = if_else(group == "Expected", "No-uncertainty", "Uncertainty"))
stu_figC <- stu_figC_data %>% 
  ggplot(aes(group, l, color = unc)) +
  geom_line(aes(group = ID), color = "gray") +
  geom_boxplot(outlier.shape = NA, linewidth = 2, fill = NA,
               position = position_dodge(width = 0)) +
  geom_point(shape = 0, size = 8) +
  theme_pubr() +
  theme(legend.position = "none",
    text = element_text(size = fs),
    legend.title = element_blank(),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
        ) +
  scale_color_manual(values = c("gray70", unc, unc)) +
  ylab('Average # licks') +
  xlab("") +
  scale_x_discrete(labels = c("Base", "100%", "50%")) +
    scale_y_continuous(breaks = seq(0, 1000, 500), 
                   expand = c(0,0)) +
    coord_cartesian(ylim = c(0, 1000))
stu_figC

# FIGURE D
# Number of clusters

stu_figD_data <- readRDS('../../objective_2/analysis/plot_4_data.rds') %>% 
  ungroup() %>% 
  group_by(group, exp_phase, ID) %>% 
  summarise(
    c = mean(n_clusters)
  ) %>% 
  mutate(
    exp_phase = recode(exp_phase, basal = "Base", experimental = "Exp"),
    group = recode(group, control = "Control", experimental = "Uncertainty"),
  )
stu_figD <- stu_figD_data %>% 
  ggplot(aes(
    exp_phase, c, color = group
  )) +
  facet_wrap(~group, strip.position = 'bottom') +
  geom_line(aes(group = ID), color = "gray") +
  geom_boxplot(outlier.shape = NA, linewidth = 2, fill = NA) +
  geom_point(size = 8, position = position_dodge(0.9), shape = 0) +
  theme_pubr() +
  theme(legend.position = "none",
    text = element_text(size = fs),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.title = element_blank(),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
        ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 200, 100),
  ) +
    coord_cartesian(ylim = c(0, 200)) +
  scale_color_manual(values = c('Control'="black", 'Uncertainty'= unc)) +
  ylab('# of clusters') +
  xlab("")
stu_figD

# FIGURE E
# Size of clusters

stu_figE_data <- readRDS('../../objective_2/analysis/plot_5_data.rds') %>% 
  ungroup() %>% 
  group_by(group, exp_phase, ID, n_sesion) %>% 
  summarise(
    cnt = mean(cluster_length)
  ) %>% 
    ungroup() %>% 
    group_by(group, exp_phase, ID) %>% 
    summarise(
        c = median(cnt)
    ) %>% 
  mutate(
    exp_phase = recode(exp_phase, basal = "Base", experimental = "Exp"),
    group = recode(group, control = "Control", experimental = "Uncertainty"),
  )
stu_figE <- stu_figE_data %>% 
  ggplot(aes(
    exp_phase, c, color = group
  )) +
  facet_wrap(~group, strip.position = 'bottom') +
  geom_line(aes(group = ID), color = "gray") +
  geom_boxplot(outlier.shape = NA, linewidth = 2, fill = NA) +
  geom_point(size = 8, position = position_dodge(0.9), shape = 0) +
  theme_pubr() +
  theme(legend.position = "none",
    text = element_text(size = fs),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    legend.title = element_blank(),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
        ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 20, 10),
  ) +
    coord_cartesian(ylim = c(0, 20)) +
  scale_color_manual(values = c('Control'="black", 'Uncertainty'= unc)) +
  ylab('Cluster size') +
  xlab("")
stu_figE

mdl_figE <- lmer(
    data = stu_figE_data,
    c ~ group * exp_phase + (1 | ID)
)
summary(mdl_figE)

# FIGURE F
# weights

stu_figF_data <- read_csv("~/repos_sync/PHD_data/objective_2/data/weights_objective_2.csv") %>% 
    mutate(init_end = if_else(`date-ymd` == "2022-08-08", "Base", "Exp")) %>% 
    group_by(ID, group, init_end) %>% 
    summarise(
        w = median(weight)
    )
stu_figF <- stu_figF_data %>% 
    ggplot(aes(
        init_end, w, group = ID, color = group
    )) +
    geom_line(
        color = "gray"
    ) +
    geom_boxplot(outlier.shape = NA, linewidth = 2, aes(group = interaction(init_end, group)), fill = NA) +
    geom_point(
        shape = 0,
        size = 8
    ) +
  facet_wrap(~group, strip.position = 'bottom') +
    theme_pubr() +
  theme(legend.position = "none",
    text = element_text(size = fs),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    legend.title = element_blank(),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
        ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(20, 30, 5),
  ) +
    coord_cartesian(ylim = c(20, 30)) +
  scale_color_manual(values = c('Control'="black", 'Uncertainty'= unc)) +
  ylab('Wt (gr)') +
  xlab("")
stu_figF



fs_f <- 30
A <- stu_figA + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
B <- stu_figB + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
C <- stu_figC + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
D <- stu_figD + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
E <- stu_figE + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
F <- stu_figF + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))


stu_row2 <- wrap_plots(B, A, C, D, E, F, ncol = 2, nrow = 3) +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 32, face = 'bold'),
          plot.margin=unit(c(0,0,0,0), "pt")
          )
svg("panel0.svg", width = 20, height = 11.83)
print(stu_row2)
dev.off()


#################################################################
##                           RNA-Seq                           ##
#################################################################

rs_figA_data <- readRDS("pdp_data.rds")
rs_figB_data <- readRDS("pdp_data.rds")

rs_figB_data_r <- rs_figA_data %>% 
    group_by(symbol) %>% 
    group_split()

rs_figB_data <- rs_figB_data_r %>% 
    map_dfr(
        ., function(sym){
            gene <- sym$symbol %>% unique()
            min_p <- min(sym$y)
            max_p <- max(sym$y)
            sign <- head(sym$y, n = 1) - tail(sym$y, n = 1)
            t <- tibble(
                symbol = gene,
                delta = (max_p - min_p) * sign
            )
            mdl <- lm(y ~ x, data = sym)
            out <- broom::tidy(mdl)
            slope <- out %>%
                pull(estimate) %>% 
                {.[2]}
            err <- out %>% 
                pull(std.error) %>% 
                {.[2]}
            r <- tibble(
                slope = slope,
                err = err,
                gene = sym$symbol %>% unique()
            )
            return(r)
        }
    )

rs_figB <- rs_figB_data %>% 
    mutate(
        dir = if_else(slope > 0, "Up", "Down"),
        slope = slope * 100,
        err = err * 100,
        gene = fct_reorder(gene, desc(slope))
    ) %>% 
    ggplot(aes(
        gene, slope,
        ymin = slope - err,
        ymax = slope + err,
        color = dir, fill = dir
    )) +
    geom_errorbar(width = 0.2, linewidth = 1) +
    geom_col() +
    ylab("P(Group = Unc)") +
    xlab("") +
    theme_pubr() +
    scale_y_continuous(breaks = c(-14, 0.6), 
                       expand = c(0,0)) +
    coord_cartesian(ylim=c(-14, 0.6)) +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        text = element_text(size = 30),
        legend.position = "none",
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
    )
rs_figB

rs_figA <- rs_figA_data %>% 
    ggplot(aes(
        x, y
    )) +
    geom_line(linewidth = 3) +
    facet_wrap(~symbol, scales = "free") +
    theme_pubr() +
    theme(legend.position = "none",
      text = element_text(size = 30),
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
          ) +
    ylab("P(Group = Unc)") +
    xlab("Gene expression")
rs_figA


A <- rs_figA + theme(text = element_text(size = 28), plot.margin = unit(c(0, 0, 0, 0), "pt"))
B <- rs_figB + theme(text = element_text(size = 28), plot.margin = unit(c(0, 0, 0, 0), "pt"))
B <- B / plot_spacer() + plot_layout(heights = c(50, 0.1))
rs_row <- wrap_plots(A, B, ncol = 2, nrow = 1, widths = c(2, 1)) +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 32, face = 'bold'),
          plot.margin=unit(c(0,0,0,0), "pt")
          )
rs_row
svg("panel4.svg", width = 15.11, height = 6.59)
print(rs_row)
dev.off()

