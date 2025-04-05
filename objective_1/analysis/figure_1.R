##################################################################
##                             LIBS                             ##
##################################################################

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

# this is weight data
ltu_figA_data_pool1 <- read_csv("~/repos_sync/experimental_data/ll_uncertainty_413_422/weights.csv")
ltu_figA_data_pool2 <- read_csv("~/repos_sync/experimental_data/ll_mf_494_501/weights.csv")

ltu_figA_data <- bind_rows(ltu_figA_data_pool1, ltu_figA_data_pool2)


# ltu_figA_stats

# compare both groups with repeated measures
weight_model <- lmer(
    data = ltu_figA_data %>%
        filter(experimental_phase == "Experimental") %>% 
        group_by(ID) %>% 
        mutate(date = as.numeric(as.factor(date))),
    weight ~ group * date + (date | ID)
        )
weight_model

# estimated marginal means for weight

weight_model_tidy <- weight_model %>% 
    broom.mixed::tidy(conf.int = TRUE, conf.level = 0.95) %>% 
    mutate(
        p_sig = if_else(p.value < 0.05, "Y", "N")
    )
weight_model_tidy
weight_model_slopes <- emmeans::emtrends(
    weight_model, "group", var = "date", type = "response"
) %>% 
    broom::tidy(conf.int = TRUE, conf.level = 0.95)
weight_model_slopes

ltu_figA2_slopes <- weight_model_slopes %>% 
    ggplot(aes(
        group, date.trend, ymin = conf.low, ymax = conf.high, color = group
    )) +
    geom_point(
        shape = 15,
        size = 8
    ) +
    geom_errorbar(
        width = 0.2,
        linewidth = 2
    ) +
    ggsignif::geom_signif(
        comparisons = list(c("No-Uncertainty", "Uncertainty")),
        map_signif_level = TRUE,
        annotations = c("*"),
        size = 2,
        textsize = 25
    ) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
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
    scale_y_continuous(breaks = seq(0, 0.25, 0.05), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(0, 0.25)) +
    xlab("") +
    ylab("Wt inc. (gr) est.")
ltu_figA2_slopes


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
    geom_point(shape = 0, size = 8) +
    ggsignif::geom_signif(
        comparisons = list(c("No-Uncertainty", "Uncertainty")),
        map_signif_level = TRUE,
        annotations = c(""),
        size = 2,
        tip_length = 0,
        textsize = 25) +
    geom_errorbar(
        inherit.aes = FALSE,
        data = weight_model_slopes,
        width = 0.2,
        linewidth = 2,
        aes(group, color = group,
            ymin = conf.low*100, ymax = conf.high*100
        )
    ) +
    geom_point(
        inherit.aes = FALSE,
        data = weight_model_slopes,
        shape = 15, 
        size = 8,
        aes(group, date.trend*100, color = group)
    ) +
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
    ggsignif::geom_signif(
        comparisons = list(c(2, 10)),
        map_signif_level = TRUE,
        annotations = c("Experimental"),
        size = 1,
        textsize = 12,
        y_position = -10,
        tip_length = 0,
        alpha = 0.25
    ) +
    coord_cartesian(ylim = c(-10, 30))
ltu_figA2

# thesis fig 1: weight

A <- ltu_figA2 + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
B <- ltu_figA + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))

weight_figure <- wrap_plots(A, B, ncol = 2, nrow = 1) +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 32, face = 'bold'))
tiff("weight_figure.tiff", width = 1080, height = 720, units = "px")
weight_figure
dev.off()




# FIGURE B

# Intake per hour
h_remap <- tibble(
    h = c(13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 0),
    nh = c(1, 2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12)
)
hourly_intake <- readRDS("intake_data.rds") %>% 
    mutate(
        h = lubridate::hour(time)
    ) %>% 
    right_join(h_remap, by = c("h")) %>% 
    group_by(
        ID, group, experimental_phase, date, nh
    ) %>% 
    summarise(
        intake = n()
    ) %>% 
    ungroup() %>% 
    group_by(ID, experimental_phase) %>% 
    mutate(
        date = as.numeric(as.factor(date))
    )
hourly_intake

hourly_intake_mdl <- lmer(
    data = hourly_intake %>% filter(experimental_phase == "Experimental"),
    intake ~ group * date * nh + (1 | ID)
)
summary(hourly_intake_mdl)

hourly_intake_emm <- emmeans::emmeans(
    hourly_intake_mdl,
    ~"group * date * nh",
    at = list(nh = c(1:12))
) %>% 
    confint() %>% 
    broom::tidy(conf.int = TRUE)
hourly_intake_emm

hourly_intake_emm_date_test <- emmeans::emtrends(
    hourly_intake_mdl,
    revpairwise~"group * date",
    var = "nh"
)$contrasts %>% 
    broom::tidy(conf.int = TRUE)
hourly_intake_emm_date_test


hourly_intake_emm_date <- emmeans::emtrends(
    hourly_intake_mdl,
    ~"group * date",
    var = "nh",
    at = list(date = c(1, 28, 56)),
    pbkrtest.limit = 11286
) %>% 
    broom::tidy(conf.int = TRUE)
hourly_intake_emm_date


hourly_intake_emm_group <- emmeans::emtrends(
    hourly_intake_mdl,
    revpairwise~"date | group",
    var = "nh",
    at = list(date = c(1, 28, 56)),
    pbkrtest.limit = 11286
)$contrasts %>% 
    broom::tidy(conf.int = TRUE)
hourly_intake_emm_group

hourly_intake_emm %>% 
    ggplot(aes(
        nh, estimate,
        ymin = conf.low, ymax = conf.high, color = group
    )) +
    geom_point(
        inherit.aes = FALSE,
        data = hourly_intake %>% filter(experimental_phase == "Experimental") %>% 
            group_by(ID, group, nh, date) %>% 
            summarise(intake = mean(intake)) %>% 
            ungroup() %>% group_by(ID, group, nh) %>% 
            summarise(intake = mean(intake)),
        size = 8,
        shape = 0,
        aes(nh, intake, color = group)
    ) +
    geom_point(
        size = 8,
        shape = 15
    ) +
    geom_line(
        linewidth = 2
    ) +
    geom_errorbar(
        width = 0.2,
        linewidth = 2
    ) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    theme_pubr() +
    ylab("Estimated intake") +
    xlab("Hour") +
    scale_x_continuous(breaks = 1:12) +
    theme(
        text = element_text(size = 30),
        legend.position = "none"
    )

intake_slopes_plot <- hourly_intake_emm_date %>% 
    ggplot(aes(
        date, nh.trend,
        ymin = conf.low, ymax = conf.high, color = group, group = group
    )) +
    geom_line(
        color = "gray"
    ) +
    geom_point(
        shape = 15,
        size = 8
    ) +
    geom_errorbar(
        linewidth = 2,
        width = 2
    ) +
    ylab("\u03B2 (Unc - Ctrl)") +
    xlab("Days") +
    scale_x_continuous(breaks = c(1, 28, 56),
                       expand = c(0, 0.6)) +
    ggsignif::geom_signif(
        y_position = -0.05,
        comparisons = list(c(1, 56)),
        annotations = c(""),
        tip_length = 0,
        size = 2,
        textsize = 25) +
    ylab("\u03B2 (Unc - Ctrl)") +
    xlab("Days") +
    annotate(
        "text", label = c("*"), x = 28, y = -0.22, size = 24
    ) +
    annotate(
        "text", label = c("*"), x = 56, y = -0.07, size = 24
    ) +
    scale_x_continuous(breaks = c(1, 28, 56),
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
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_y_continuous(breaks = seq(-0.6, 0, 0.1), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(-0.6, 0))
intake_slopes_plot


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
    ggsignif::geom_signif(
        comparisons = list(c("No-Uncertainty", "Uncertainty")),
        map_signif_level = TRUE,
        annotations = c(""),
        size = 2,
        textsize = 25,
        tip_length = 0
    ) +
    theme_pubr() +
    xlab("") +
    ylab("Median # pellets") +
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
    scale_y_continuous(breaks = seq(40, 85, 5), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(40, 85))
ltu_figB

# compare both groups with repeated measures for intake
ltu_figB_model_data <- ltu_figB_data %>% 
    filter(experimental_phase == "Experimental") %>% 
    group_by(ID, experimental_phase) %>% 
    mutate(days = row_number())

intake_model <- glmer.nb(
    data = ltu_figB_model_data,
    intake ~ group * days + (1 | ID)
)
intake_model_tidy <- intake_model %>% 
    broom.mixed::tidy(conf.int = TRUE, conf.level = 0.95) %>% 
    mutate(
        p_sig = if_else(p.value < 0.05, "Y", "N")
    )
intake_model_tidy

emmeans::emmeans(intake_model,pairwise ~ "group * days", type = "response", regrid = "response")$contrasts %>% 
    broom::tidy(conf.int = TRUE)

# marginal means for intake
intake_model_emm <- emmeans::emmeans(intake_model, ~ "group * days", type = "response", at = list(days = c(1:56)))
ltu_figB_2 <- intake_model_emm %>% broom::tidy(conf.int = TRUE) %>% 
    ggplot(aes(
        days, response, ymin = conf.low, ymax = conf.high, group = group
    )) +
    geom_ribbon(fill = NA, aes(color = group), linewidth = 1) +
    geom_line(aes(color = group), linewidth = 2) +
    geom_point(
        inherit.aes = FALSE,
        data = ltu_figB_model_data %>% 
            filter(days <= 56) %>% 
            group_by(days, group) %>% 
            summarise(median_intake = median(intake)),
        aes(days, median_intake, color = group),
        shape = 0, size = 8
    ) +
    theme_pubr() +
    xlab("Days") +
    ylab("Median # pellets") +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_continuous(breaks = c(0, 28, 56)) +
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
ltu_figB_2


# FIGURE C
# Energy efficiency
exp_weight <- ltu_figA_data %>%
    drop_na() %>% 
    group_by(ID, group, date) %>% 
    summarise(weight_ = median(weight)) %>% 
    rename(weight = weight_) %>% 
    mutate(date = as.numeric(as.factor(date)))
exp_intake <- ltu_figB_data %>% 
    group_by(ID, group, date) %>% 
    summarise(intake_ = median(intake)) %>% 
    rename(intake = intake_) %>% 
    group_by(ID) %>% 
    mutate(date = as.numeric(as.factor(date)))
merge_intake_weight <- left_join(exp_weight, exp_intake, by = c("ID", "date", "group")) %>% 
    mutate(eff = weight / intake) %>% 
    drop_na()

# model efficiency

eff_mdl_control <-
    lm(
        data = merge_intake_weight %>% filter(group == "No-Uncertainty"),
        intake ~ weight + date
    )
summary(eff_mdl_control)

preds <-
    predict(eff_mdl_control, newdata = merge_intake_weight) %>% 
    bind_cols(merge_intake_weight,preds=.)

preds %>% 
    filter(date <= 30) %>% 
    ggplot(aes(
        date, preds - intake, group = ID, color = group
    )) +
    geom_smooth(method = "gam", aes(group=group)) +
    stat_summary(
        fun.data = "mean_se",
        geom = "line",
        aes(group = group)
    ) +
    stat_summary(
        fun.data = "mean_se",
        geom = "point",
        shape = 15,
        size = 3,
        aes(group = group)
    ) +
    ylab("Expected intake by weight - Actual intake") +
    xlab("Days") +
    scale_color_manual(values = c("black", "#52ABDD")) +
    ggpubr::theme_classic2() +
    theme(
        text = element_text(size=21),
        legend.title = element_blank()
    )


merge_intake_weight %>% 
    ggplot(aes(date, eff, color = group)) +
    geom_point(
        shape = 0,
        size = 8
    ) +
    geom_smooth(method = "lm", se = FALSE, size = 3) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    xlab("Measurment #") +
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
    scale_y_continuous(breaks = seq(0.2, 0.7, 0.1), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(0.2, 0.7))

ltu_figC <- merge_intake_weight %>% 
    filter(date == max(date)) %>% 
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
    ggsignif::geom_signif(
        comparisons = list(c("No-Uncertainty", "Uncertainty")),
        map_signif_level = TRUE,
        annotations = c(""),
        size = 2,
        textsize = 25,
        tip_length = 0
    ) +
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
    scale_y_continuous(breaks = seq(0.1, 0.7, 0.1), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(0.1, 0.7))
ltu_figC

# eff linear model for control group

mdl_weight_galgani <-
    merge_intake_weight %>% 
    group_by(group) %>% 
    group_split() %>% 
    map(
        ., function(grp){
            print(grp$group %>% unique)
            mdl <- lm(
                data = grp,
                intake ~ weight * date
            )
            # mdl_tidy <- mdl %>% 
            #     broom::tidy(conf.int = TRUE) %>% 
            #     mutate(
            #         group = grp$group %>% unique
            #     )
            return(mdl)
        }
    )
mdl_weight_galgani

residual_eff <-
    predict(
        mdl_weight_galgani[[1]],
        newdata = merge_intake_weight %>% 
            select(date, weight)
    ) %>% 
    as_tibble() %>% 
    rename(pred = value) %>% 
    bind_cols(merge_intake_weight,.) %>% 
    filter(group == "Uncertainty") %>% 
    mutate(residuals = intake - pred)
residual_eff

mdl_lmer <- residual_eff %>% 
    lmer(
        data = .,
        residuals ~ date + (1 + date | ID)
    )
summary(mdl_lmer)

slopes_eff <-
    emmeans::emmeans(mdl_lmer,
                     ~ date,
                     type = "response",
                     at = list(date = 1:30)
    ) %>% 
    broom::tidy(conf.int = TRUE) %>% 
    rename(residuals = estimate) %>% 
    mutate(group = "Uncertainty")
slopes_eff

residual_eff_plot <-
    residual_eff %>% 
    ggplot(aes(
        date, residuals, color = group
    )) +
    geom_point(shape = 0, size = 5) +
    geom_hline(yintercept = 0, size = 1.5) +
    geom_ribbon(
        data = slopes_eff,
        aes(ymin = conf.low, ymax = conf.high),
        fill = NA,
        size = 1.5
    ) +
    geom_line(
        data = slopes_eff
    ) +
    scale_color_manual(
        values = c("No-Uncertainty" = "black",
                   "Uncertainty" = "#52AAD9")) +
    ylab("Intake ~ weight residuals") +
    xlab("Measurement #") +
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
    scale_y_continuous(breaks = seq(-50, 30, 10), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(-50, 30))
residual_eff_plot



mdl_weight_galgani %>% 
    ggplot(aes(
        interaction(group, term), estimate,
        ymin = conf.low, ymax = conf.high, color = group
    )) +
    geom_point(
        size = 8,
        shape = 15
    ) +
    geom_errorbar(
        width = 0.2,
        linewidth = 2
    ) +
    facet_wrap(~term, scales = "free")

# energy efficency stats
eff_model <- lm(
    data = merge_intake_weight,
    eff ~ group
)
eff_model_emm <- emmeans::emmeans(
    eff_model, pairwise ~ "group"
)$contrasts %>% broom::tidy(conf.int = TRUE, conf.level = 0.95)
eff_model_emm

A <- ltu_figB_2 + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
B <- ltu_figB + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
C <- ltu_figC + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))

intake_figure <- wrap_plots(A, B, C, ncol = 3, nrow = 1) +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 32, face = 'bold'))
tiff("intake_figure.tiff", width = 1080, height = 720, units = "px")
intake_figure
dev.off()



# FIGURE D
# Retrieval times

# failed retrievals
retrieval_times <- readRDS("intake_data.rds")

failed_ret <- retrieval_times %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    group_by(ID, hour) %>% 
    dplyr::slice(-1) %>% # remove first removal, corresponds to previous block
    dplyr::mutate(
        ret = as.numeric(time - lag(time)) - as.numeric(delay)
    ) %>% 
    mutate(ret = as.numeric(ret)) %>% 
    ungroup() %>% 
    ungroup() %>% 
    group_by(ID) %>% 
    mutate(week = as.numeric(as.factor(week))-1, t = interaction(week, experimental_phase)) %>% 
    filter(ret < 0) %>%
    group_by(
        ID, group, week, experimental_phase, date
    ) %>% 
    summarise(
        failed_ret = n()
    )
failed_ret

failed_ret_mdl <- glmer.nb(
    data = failed_ret,
    failed_ret ~ group * experimental_phase + (1 | ID)
)
summary(failed_ret_mdl)

failed_ret_cmp <- emmeans::emmeans(
    failed_ret_mdl,
    revpairwise~"group * experimental_phase",
    by = "experimental_phase",
    type = "response"
)$contrasts %>% 
    broom::tidy(conf.int = TRUE)
failed_ret_cmp

failed_ret_emm <- emmeans::emmeans(
    failed_ret_mdl,
    ~"group * experimental_phase",
    by = "experimental_phase",
    type = "response"
) %>% 
    broom::tidy(conf.int = TRUE)
failed_ret_emm

failed_ret_emm_p <- emmeans::emmeans(
    failed_ret_mdl,
    pairwise ~ "group * week * experimental_phase",
    type = "response",
    
)$contrasts %>% 
    broom::tidy(conf.int = TRUE)
failed_ret_emm_p

fail_ret_raw <- failed_ret %>% 
    group_by(group, ID, experimental_phase) %>% 
    summarise(response = median(failed_ret))
fail_ret_raw

failed_ret_plot <- failed_ret_emm %>% 
    ggplot(aes(
        experimental_phase, response, color = group,
        ymin = conf.low, ymax = conf.high
    )) +
    geom_point(
        data = fail_ret_raw,
        inherit.aes = FALSE,
        aes(experimental_phase, response, color = group, group = ID),
        shape = 0,
        size = 8
    ) +
    geom_line(
        data = fail_ret_raw,
        inherit.aes = FALSE,
        aes(experimental_phase, response, color = group,
            group = ID),
        linewidth = 0.5
    ) +
    geom_errorbar(
        width = 0.2,
        linewidth = 2
    ) +
    geom_line(aes(group = group), linewidth = 2) +
    geom_point(
        shape = 15, 
        size = 8
    ) +
    theme_pubr() +
    xlab("") +
    ylab("# daily failed ret.") +
    scale_x_discrete(labels = c("Base", "Exp")) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_y_continuous(breaks = seq(0,20, 5), 
                       expand = c(0,0)) +
    coord_cartesian(ylim=c(0, 20)) +
    annotate(
        "text", label = "*", x = "Experimental", y = 13, size = 24
    ) +
    theme(
        text = element_text(size = fs),
        legend.position = "none",
        plot.margin = margin(
            t = t,
            b = b,
            r = r,
            l = l
        )
    )
failed_ret_plot


failed_ret %>% 
    ggplot(aes(
        week, failed_ret, color = group, group = ID
    )) +
    geom_point() +
    geom_line()

retrieval_times <- readRDS("intake_data.rds")
ret <- retrieval_times %>% 
    #  filter(group == "Uncertainty") %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    group_by(ID, hour) %>% 
    dplyr::slice(-1) %>% # remove first removal, corresponds to previous block
    dplyr::mutate(
        ret = as.numeric(time - lag(time)) - as.numeric(delay)
    ) %>% 
    filter(ret > 0, ret < 3600, delay %in% c(15, 60, 120, 180, 240, 300)) %>%  # this are sensor probes without pellet actually being there, max ret is 1 hour by definition
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
    mutate(slow_fast = case_when(
        qntls %in% c(1, 2) ~ "Fast",
        qntls %in% c(3, 4) ~ "Slow"
    )) %>% 
    group_by(ID, week, group, experimental_phase) %>% 
    mutate(cnt = n()) %>% 
    ungroup() %>% 
    group_by(ID, cnt, slow_fast, week, group, experimental_phase) %>% 
    summarise(
        cc = n()
    ) %>% 
    ungroup() %>% 
    group_by(ID, slow_fast, week, experimental_phase) %>% 
    mutate(prop = cc / cnt) %>% 
    ungroup() %>% 
    group_by(ID) %>% 
    mutate(week = as.numeric(as.factor(week))-1)

ret_mdl <- lmer(
    data = ret_cat, 
    ret ~ group * experimental_phase + (1 | ID)
)
summary(ret_mdl)
ret_mdl %>% 
    broom::tidy(conf.int=TRUE)

ret_mdl_emm_s <- emmeans::emmeans(
    ret_mdl,
    ~"group*experimental_phase",
    type = "response"
) %>% 
    broom::tidy(conf.int = TRUE)
ret_mdl_emm_s

ret_times_plot_raw <- ret_cat %>% 
    group_by(group, experimental_phase, ID) %>% 
    summarise(
        estimate = mean(ret)
    )
ret_times_plot_raw

retrieval_times_plot <- ret_mdl_emm_s %>% 
    ggplot(aes(
        experimental_phase, estimate,
        ymin = conf.low, ymax = conf.high, color = group
    )) +
    geom_point(
        size = 8,
        shape = 0,
        inherit.aes = FALSE,
        data = ret_times_plot_raw,
        aes(experimental_phase, estimate, color = group, group = ID)
    ) +
    geom_line(
        linewidth = 0.5,
        inherit.aes = FALSE,
        data = ret_times_plot_raw,
        aes(experimental_phase, estimate, color = group, group = ID)
    ) +
    geom_point(
        size = 8,
        shape = 15
    ) +
    geom_errorbar(
        linewidth = 2,
        width = 0.2
    ) +
    geom_line(
        linewidth = 2,
        aes(group = group)
    ) +
    theme_pubr() +
    xlab("") +
    ylab("Ret. length (sec)") +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_discrete(labels = c("Base", "Exp")) +
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
    scale_x_discrete(labels = c("Base", "Exp")) +
    scale_y_continuous(breaks = seq(100, 700, 100), 
                       expand = c(0,0)) +
    annotate(
        "text", label = "*", x = "Experimental", y = 300, size = 24
    ) +
    coord_cartesian(ylim = c(100, 700))
retrieval_times_plot

A <- retrieval_times_plot
B <- failed_ret_plot
C <- intake_slopes_plot

behavioral_figure <- wrap_plots(A, B, C, ncol = 3, nrow = 1) +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 32, face = 'bold'))
png("behavioral_figure.png", width = 1080, height = 720)
behavioral_figure
dev.off()

fast_ret_slope <- ret_mdl_emm_s %>% 
    ggplot(aes(
        group, week.trend*100,
        ymin = conf.low*100, ymax = conf.high*100, color = group
    )) +
    geom_point(
        size = 8,
        shape = 15
    ) +
    geom_errorbar(
        width = 0.2,
        linewidth = 2
    ) +
    ggsignif::geom_signif(
        comparisons = list(c("No-Uncertainty", "Uncertainty")),
        map_signif_level = TRUE,
        annotations = c(""),
        size = 2,
        tip_length = 0,
        textsize = 25) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_discrete(labels = c("Cer", "Unc")) +
    scale_y_continuous(breaks = seq(-1, 4, 1), 
                       expand = c(0,0)) +
    coord_cartesian(ylim=c(-1, 4)) +
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
    xlab("") +
    ylab("Fast retrievals % inc.")
fast_ret_slope


A <- retrieval_times_plot + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
B <- fast_ret_slope + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))
C <- failed_ret_plot + theme(text = element_text(size = 30), plot.margin = unit(c(0, 0, 0, 0), "pt"))

behavioral_figure <- wrap_plots(A, B, C, ncol = 3, nrow = 1, widths = c(3, 1, 1) ) +
    plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 32, face = 'bold'))
tiff("behavioral_figure.tiff", width = 1080, height = 720, units = "px")
behavioral_figure
dev.off()

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

# licks stat
licks_mdl <- lm(
    data = licks_events %>% filter(
        tipo_recompensa == "sacarosa_crv",
        name == "l"
    ),
    value ~ group
) %>% 
    broom::tidy(conf.int = TRUE)
licks_mdl

# events stat
events_mdl <- lm(
    data = licks_events %>% filter(
        tipo_recompensa == "sacarosa_crv",
        name == "e"
    ),
    value ~ group
) %>% 
    broom::tidy(conf.int = TRUE)
events_mdl


ltu_figF <- licks_events %>% 
    filter(name == "e") %>% 
    ggplot(aes(
        tipo_recompensa, value, color = group
    )) +
    geom_line(
        aes(group = interaction(ID, name), color = group),
        linewidth = 0.5,
        alpha = 0.5
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
    ggsignif::geom_signif(
        comparisons = list(c("agua", "sacarosa_crv")),
        map_signif_level = TRUE,
        annotations = c(""),
        size = 2,
        tip_length = 0,
        textsize = 25) +
    scale_y_continuous(breaks = seq(0, 100, 20), 
                       expand = c(0,0)) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_discrete(labels = c("W", "S")) +
    annotate(
        "text",
        label = "*",
        size = 24,
        x = "sacarosa_crv",
        hjust = -1,
        y = 82.5
    ) +
    coord_cartesian(ylim = c(0, 100)) +
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
        aes(group = interaction(ID, name), color = group),
        linewidth = 0.5,
        alpha = 0.5
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
    ggsignif::geom_signif(
        comparisons = list(c("agua", "sacarosa_crv")),
        map_signif_level = TRUE,
        annotations = c(""),
        size = 2,
        tip_length = 0,
        textsize = 25) +
    annotate(
        "text",
        label = "*",
        size = 24,
        hjust = -1,
        x = "sacarosa_crv",
        y = 6000
    ) +
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
    scale_y_continuous(breaks = seq(0, 7000, 1000), 
                       expand = c(0,0)) +
    scale_color_manual(values = c("No-Uncertainty" = "black", "Uncertainty" = "#52AAD9")) +
    scale_x_discrete(labels = c("W", "S")) +
    coord_cartesian(ylim = c(0, 7000)) +
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

q0_mdl <- ltu_figI_data[[2]] %>% 
    lm(
        data = .,
        value ~ group
    ) %>% 
    broom::tidy(conf.int = TRUE)
q0_mdl

alpha_mdl <- ltu_figI_data[[1]] %>% 
    lm(
        data = .,
        value ~ group
    ) %>% 
    broom::tidy(conf.int = TRUE)
alpha_mdl

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
        group, value*10000, color = group, fill = group
    )) +
    geom_boxplot(outlier.shape = NA, linewidth = 2, fill = NA) +
    geom_point(
        shape = 21,
        size = 5,
        color = "black",
        alpha = 0.5
    ) +
    ggsignif::geom_signif(
        comparisons = list(c("No-Uncertainty", "Uncertainty")),
        map_signif_level = TRUE,
        annotations = c(""),
        tip_length = c(0, 0.35),
        size = 2,
        textsize = 25) +
    ylab("α") +
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
    scale_fill_manual(values = c("black", "orange")) +
    scale_color_manual(values = c("black", "orange")) +
    scale_x_discrete(labels = c("Cer", "Unc")) 
ltu_figI


ltu_figJ <- ltu_figI_data[[2]] %>% 
    ggplot(aes(
        group, value, color = group, fill = group
    )) +
    geom_boxplot(outlier.shape = NA, linewidth = 2, fill = NA) +
    geom_point(
        shape = 21,
        size = 5,
        color = "black",
        alpha = 0.5
    ) +
    ggsignif::geom_signif(
        comparisons = list(c("No-Uncertainty", "Uncertainty")),
        map_signif_level = TRUE,
        annotations = c(""),
        tip_length = c(0.05, 0),
        size = 2,
        textsize = 25) +
    ylab("Q0") +
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
    scale_fill_manual(values = c("black", "orange")) +
    scale_color_manual(values = c("black", "orange")) +
    scale_x_discrete(labels = c("Cer", "Unc")) 
ltu_figJ

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
    ggsignif::geom_signif(
        comparisons = list(c("No-Uncertainty", "Uncertainty")),
        map_signif_level = TRUE,
        annotations = c(""),
        size = 2,
        tip_length = 0,
        textsize = 25) +
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



A <- ltu_figF
B <- ltu_figG
C <- ltu_figH
D <- ltu_figI
E <- ltu_figJ

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
                           pattern = "(413|414|415|416|417|418|419|420|421|422|494|495|496|497|498|499|500|501).CSV",
                           recursive = TRUE,
                           full.names = TRUE)

raw_files <- intake_files %>% 
    map(., function(x){
        out <- read_csv(x)
        v <- out %>% pull(protocol)
        r <- sum(v == "locomotor") %>% unlist
        if(r > 0){
            return(invisible(NULL))
        }
        else{
            out <- out %>% 
                mutate(animal = as.numeric(if_else(animal == "00", "498", as.character(animal))))
            return(out)
        }
    }) %>% 
    # this removes null
    compact() %>% 
    bind_rows()

intake_files <- raw_files %>% 
    filter(delay != "01") %>%
    group_by(animal) %>% 
    mutate(
        group = if_else(animal %in% c(413, 416, 417, 418, 419, 495, 496, 497, 500), "Uncertainty", "No-Uncertainty"),
        experimental_phase = case_when(
            protocol == "basal" ~ "Baseline",
            protocol == "baseline" ~ "Baseline",
            TRUE ~ "Experimental"
        ),
        time = lubridate::as_datetime(time),
        week = as.numeric(as.factor(week(time))),
        week = week - min(week)
    ) %>% 
    mutate(experimental_phase = if_else(week <= 1, "Baseline", "Experimental")) %>% 
    filter(lubridate::year(time) > 2020) %>% 
    select(
        -c("protocol", "motorTurns", "battery")
    ) %>% 
    arrange(sort(desc(time))) %>% 
    mutate(date = lubridate::date(time)) %>% 
    rename(ID = animal)
saveRDS(intake_files, "intake_data.rds")



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
               group = interaction(exp_group, period),
               color = exp_group,
               #color= interaction(exp_group, period)
    )) +
    geom_line(aes(group = ID, color = exp_group),
              linewidth = 0.5,
              alpha = 0.5) +
    geom_boxplot(outlier.shape = NA, linewidth = 2, fill = NA) +
    geom_point(size = 8, shape = 0) +
    annotate(
        "text",
        label = "*",
        size = 24,
        x = "Exp",
        y = 1700
    ) +
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

licks_mdl_stu <- lm(
    data = stu_figA_data,
    l ~ exp_group * period
) %>% 
    broom::tidy(conf.int = TRUE)
licks_mdl_stu

events_mdl_stu <- lm(
    data = stu_figA_data,
    e ~ exp_group * period
) %>% 
    broom::tidy(conf.int = TRUE)
events_mdl_stu


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

bins_mdl <- lmer(
    data = stu_figB_data,
    l ~ exp_group * bins + (1 | ID)
)
summary(bins_mdl)

emmeans::emmeans(
    bins_mdl,
    pairwise ~ "exp_group * bins",
    by = "bins",
    type = "response",
    adjust = "tukey",
    at = list(bins = c(1:6))
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
    ggsignif::geom_signif(
        comparisons = list(c(1, 6)),
        annotations = c(""),
        tip_length = 0,
        size = 2,
        textsize = 25
    )+
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

stu_figC_data_raw <- readRDS('../../objective_2/analysis/plot_1_data.rds')
stu_figC_data_raw
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
    ylab('# licks') +
    xlab("") +
    ggsignif::geom_signif(
        comparisons = list(
            c("Expected", "Non-random")
        ),
        annotations = c(""),
        size = 2,
        y_position = 1100,
        color = "black"
    ) +
    ggsignif::geom_signif(
        comparisons = list(
            c("Non-random", "Random")
        ),
        annotations = c(""),
        size = 2,
        color = "black"
    ) +
    scale_x_discrete(labels = c("Base", "100%", "50%")) +
    scale_y_continuous(breaks = seq(0, 1200, 200), 
                       expand = c(0,0)) +
    coord_cartesian(ylim = c(0, 1200))
stu_figC

spout_mdl <- lmer(
    data = stu_figC_data_raw,
    l ~ group  + (1|ID)
)
summary(spout_mdl)

spout_mdl_emm <- emmeans::emmeans(
    spout_mdl,
    revpairwise ~ "group",
    type = "response"
)$contrasts %>% 
    broom::tidy(conf.int = TRUE)



# FIGURE D
# Number of clusters

stu_figD_data_raw <- readRDS('../../objective_2/analysis/plot_4_data.rds') %>% 
    mutate(c = n_clusters,
           exp_phase = recode(exp_phase, basal = "Base", experimental = "Exp"),
           group = recode(group, control = "Control", experimental = "Uncertainty"),
           int = interaction(group, exp_phase)
    )

stu_figD_data <- readRDS('../../objective_2/analysis/plot_4_data.rds') %>% 
    ungroup() %>% 
    group_by(group, exp_phase, ID) %>% 
    summarise(
        c = mean(n_clusters)
    ) %>% 
    mutate(
        exp_phase = recode(exp_phase, basal = "Base", experimental = "Exp"),
        group = recode(group, control = "Control", experimental = "Uncertainty"),
        int = interaction(group, exp_phase)
    )
stu_figD <- stu_figD_data %>% 
    ggplot(aes(
        int, c, color = group
    )) +
    geom_line(aes(group = ID, color = group), alpha = 0.5, linewidth = 0.5) +
    geom_boxplot(outlier.shape = NA, linewidth = 2, fill = NA) +
    geom_point(size = 8, shape = 0) +
    ggsignif::geom_signif(
        comparisons = list(
            c("Uncertainty.Base", "Uncertainty.Exp"),
            c("Control.Exp", "Uncertainty.Exp")),
        annotations = c("", ""),
        y_position = c(150, 160),
        size = 2,
        tip_length = 0,
        color = "black"
    ) +
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
    scale_x_discrete(labels = c("Base", "Exp", "Base", "Exp")) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(40, 180, 20),
    ) +
    coord_cartesian(ylim = c(40, 180)) +
    scale_color_manual(values = c('Control'="black", 'Uncertainty'= unc)) +
    ylab('# of clusters') +
    xlab("")
stu_figD

n_clusters_mdl <- lmer(
    data = stu_figD_data_raw,
    c ~ group * exp_phase + (1 | ID)
) %>%
    broom.mixed::tidy(conf.int = TRUE)
n_clusters_mdl

# FIGURE E
# Size of clusters

stu_figE_data_raw <- readRDS('../../objective_2/analysis/plot_5_data.rds') %>% 
    mutate(
        exp_phase = recode(exp_phase, basal = "Base", experimental = "Exp"),
        group = recode(group, control = "Control", experimental = "Uncertainty"),
        int = interaction(group, exp_phase)
    )
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
        int = interaction(group, exp_phase)
    )
stu_figE <- stu_figE_data %>% 
    ggplot(aes(
        int, c, color = group
    )) +
    geom_line(aes(group = ID), color = "gray") +
    geom_boxplot(outlier.shape = NA, linewidth = 2, fill = NA) +
    geom_point(size = 8, position = position_dodge(0.9), shape = 0) +
    ggsignif::geom_signif(
        comparisons = list(
            c("Uncertainty.Base", "Uncertainty.Exp"),
            c("Control.Exp", "Uncertainty.Exp")),
        annotations = c("", ""),
        y_position = c(15, 16),
        size = 2,
        tip_length = 0,
        color = "black"
    ) +
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
    scale_x_discrete(labels = c("Base", "Exp", "Base", "Exp")) +
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

size_clusters_mdl <- lmer(
    data = stu_figE_data_raw,
    cluster_length ~ group * exp_phase + (1 | ID)
) %>%
    broom.mixed::tidy(conf.int = TRUE)
size_clusters_mdl

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

weight_mdl_stu <- lm(
    data = stu_figF_data,
    w ~ group
) %>% 
    broom::tidy(conf.int = TRUE)
weight_mdl_stu



fs_f <- 30
A <- stu_figA + theme(text = element_text(size = 30))
B <- stu_figB + theme(text = element_text(size = 30))
C <- stu_figC + theme(text = element_text(size = 30))
D <- stu_figD + theme(text = element_text(size = 30))
E <- stu_figE + theme(text = element_text(size = 30))
F <- stu_figF + theme(text = element_text(size = 30))


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

rs_figA_data <- readRDS("~/scripts/r-scripts/pdp_data.rds")
rs_figB_data <- readRDS("~/scripts/r-scripts/pdp_data.rds")

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

