##################################################################
##                             LIBS                             ##
##################################################################

pacman::p_load(
    tidyverse,
    ggplot2,
    ggpubr,
    lme4,
    lmerTest,
    patchwork,
    parameters
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
##                          Data prep                          ##
#################################################################

retrieval_data <- readRDS("intake_data.rds")

ret_analysis_data <- retrieval_data %>% 
    mutate(
        hour = lubridate::hour(time)
    ) %>% 
    group_by(ID) %>% 
    arrange(time, .by_group = TRUE) %>% 
    mutate(
        ret_raw = lubridate::as.duration(time - lag(time)),
        delay_sec = lubridate::as.duration(as.numeric(delay)),
        ret = ret_raw - delay_sec
    ) %>% 
    ungroup() %>% 
    group_by(ID) %>% 
    mutate(
        week = as.numeric(as.factor(week))-1
    ) %>% 
    filter(week < 11) %>% 
    drop_na()
ret_analysis_data

ret_mdl_data <- ret_analysis_data %>% 
    filter(ret > 0, ret < 3600)

mdl_ret <- lmerTest::lmer(
    data = ret_mdl_data,
    ret ~ group * week * experimental_phase + (1|ID/week)
)
summary(mdl_ret)

ranef(mdl_ret)

mdl_ret_emm <- emmeans::emmeans(
    mdl_ret,
    pairwise ~ group | week * experimental_phase,
    at = list(week = c(1,2,3,4))
)
mdl_ret_emm


mdl_ret_emm <- emmeans::emmeans(
    mdl_ret,
    "source"
)
mdl_ret_emm

emmeans::eff_size(mdl_ret_emm, sigma = sigma(mdl_ret), edf=160)


#################################################################
##                      Failed retrievals                      ##
#################################################################

failed_retrieval_data <- retrieval_data %>% 
    filter(ret < 0) %>% 
    group_by(
        ID, group, experimental_phase, week, date
    ) %>% 
    summarise(
        failed_retrievals = n()
    ) %>% 
    ungroup() %>% 
    group_by(ID, experimental_phase) %>% 
    mutate(
        date = scale(as.numeric(as.factor(date))),
        week = as.numeric(as.factor(week))
    ) %>% 
    filter(week < 11)
failed_retrieval_data

#################################################################
##                   Failed retrievals stats                   ##
#################################################################

# model semi-raw data
failed_retrievals_raw <- failed_retrieval_data %>% 
    filter(experimental_phase == "Experimental") %>% 
    group_by(ID, group, week, experimental_phase) %>% 
    summarise(
        failed_retrievals = median(failed_retrievals)
    ) %>% 
    filter(week < 9)

# linear model
failed_retrievals_mdl <- glmer.nb(
    data = failed_retrieval_data %>%
        filter(experimental_phase  == "Experimental"),
    failed_retrievals ~ group * week + (week | ID)
)
summary(failed_retrievals_mdl)

# marginal means
failed_retrievals_emm <- emmeans::emmeans(
    failed_retrievals_mdl,
    ~"group * week",
    type = "response",
    at = list(week = seq(1,8,1))
) %>% 
    broom::tidy(conf.int = TRUE)
failed_retrievals_emm

failed_retrievals_emm %>% 
    ggplot(aes(
        week, response, color = group, ymin = conf.low, ymax = conf.high, fill = group
    )) +
    geom_ribbon(alpha = 0.2) +
    geom_line(linewidth = 2) +
    geom_point(
        shape = 15,
        size = 8
    ) +
    geom_point(
        inherit.aes = FALSE,
        data = failed_retrievals_raw,
        shape = 0,
        size = 8,
        aes(week, failed_retrievals, color = group)
    ) +
    ylab("# daily failed retrievals") +
    xlab("Weeks") +
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
    scale_x_continuous(breaks = seq(1,8,1)) +
    scale_y_continuous(breaks = seq(0, 25, 5), 
                       expand = c(0,0)) +
    coord_cartesian(ylim=c(0, 25))
    

