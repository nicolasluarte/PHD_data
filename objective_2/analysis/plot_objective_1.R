pacman::p_load(
  tidyverse,
  rstudioapi,
  ggplot2,
  ggpubr,
  gghighlight,
  ggsignif, 
  shades,
  cowplot
)
setwd(dirname(getActiveDocumentContext()$path)) 

# colors
color_p = c("black", "#56B4E9", "#56B4E9")
unc <- "#56B4E9"
ctrl <- "black"

p1_data <- readRDS('plot_1_data.rds') %>% 
  ungroup() %>% 
  group_by(
    ID, exp_group, period, group
  ) %>% 
  summarise(
    l = mean(l),
    e = mean(e)
  ) %>% 
  mutate(unc = if_else(group == "Expected", "No-uncertainty", "Uncertainty"))

# plot 1: comparison between random, non-random and expected number of licks

# main boxplot
p1 <- p1_data %>% 
  ggplot(aes(group, l, color = unc)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, position = position_dodge(width = 0)) +
  geom_line(aes(group = ID), alpha = 0.25, color = "black") +
  geom_jitter(width = 0.01, alpha = 0.5, size = 2.5) +
  geom_signif(y_position = c(1150, 1350, 1150), xmin = c(1, 1, 2.1), 
              xmax = c(1.9, 3, 3), annotation = c("*", "p=0.051", "*"),
              tip_length = 0.01, color = "black") +
  theme_pubr() +
  theme(legend.position = "none") +
  scale_color_manual(values = color_p) +
  ylab('# licks per spout') +
  xlab("") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 1500, 500),
                     limits = c(0, 1500)
                     )
p1

# plot 2: pre-post licks control vs uncertainty group

p2_data <- readRDS('plot_2_data.rds') %>% 
  ungroup() %>% 
  group_by(ID, period, exp_group) %>% 
  summarise(
    l = mean(l),
    e = mean(e)
  ) %>% 
  mutate(
    period = recode(period, basal = "Baseline", experimental = "Experimental")
  )

p2 <- p2_data %>% 
  ggplot(aes(period, l,
             group = period,
             color = period,
             #color= interaction(exp_group, period)
             )) +
  facet_wrap(~exp_group, strip.position = "bottom") +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(alpha = 0.25, size = 2.5) +
  geom_line(aes(group = ID), alpha = 0.25) +
  theme_pubr() +
  theme(#legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank()
        ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 2000, 500),
                     limits = c(0, 2000)
  ) +
  scale_color_manual(values = c('Baseline'="gray30", 'Experimental'="black")) +
  ylab('# licks per session') +
  xlab("") +
  geom_signif(
    comparisons = list(c('Baseline', 'Experimental')),
    map_signif_level = TRUE
  )
p2

# plot 3: licks per time bins

p3_data <- readRDS('plot_3_data') %>% 
  filter(exp_phase == 'experimental') %>% 
  ungroup() %>% 
  group_by(ID, exp_group, bins, n_sesion) %>% 
  summarise(
    l = mean(l)
  ) %>% 
  ungroup() %>% 
  group_by(ID, exp_group, bins) %>% 
  summarise(
    l = mean(l)
  ) %>% 
  mutate(
    exp_group = recode(exp_group, control = "Control", experimental = "Uncertainty")
  )

p3 <- p3_data %>% 
  ggplot(aes(
    as.factor(bins), l, color = exp_group
  )) +
  geom_point(alpha = 0.25, size = 2.5, position = position_jitter(width = 0.05)) +
  stat_summary(fun = mean, geom = 'line', aes(group = exp_group), size = 0.5) +
  stat_summary(fun = mean, geom = 'point', aes(group = exp_group), size = 2) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", aes(group = exp_group), width = .2) +
  theme_pubr() +
  ylab('# licks') +
  xlab('10 min intervals') +
  geom_signif(y_position = c(450), xmin = c(1), 
              xmax = c(6), annotation = c("*"),
              tip_length = 0, color = 'black') +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 500, 100),
                     limits = c(0, 500)) +
  scale_color_manual(values = c(ctrl, unc)) +
  theme(
    #legend.position = 'none',
    legend.title = element_blank()
  )
p3

# plot 4: number of clusters

p4_data <- readRDS('plot_4_data.rds') %>% 
  ungroup() %>% 
  group_by(group, exp_phase, ID) %>% 
  summarise(
    c = mean(n_clusters)
  ) %>% 
  mutate(
    exp_phase = recode(exp_phase, basal = "Baseline", experimental = "Experimental"),
    group = recode(group, control = "Control", experimental = "Uncertainty"),
  )
  

p4 <- p4_data %>% 
  ggplot(aes(
    exp_phase, c, color = exp_phase
  )) +
  facet_wrap(~group, strip.position = 'bottom') +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(alpha = 0.25, size = 2.5, position = position_dodge(0.9)) +
  geom_line(aes(group = ID), alpha = 0.25) +
  theme_pubr() +
  theme(#legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 200, 50),
                     limits = c(0, 200)
  ) +
  scale_color_manual(values = c('Baseline'="gray30", 'Experimental'="black")) +
  ylab('# of clusters') +
  xlab("") +
  geom_signif(data = data.frame(group = c("Control","Uncertainty")),
              aes(y_position = c(150), xmin = c(1), 
              xmax = c(2), annotations = c('NS.', '*')),
              tip_length = 0.01, color = "black", manual = TRUE)
p4

# plot 5: length of clusters

p5_data <- readRDS('plot_5_data.rds') %>% 
  ungroup() %>% 
  group_by(group, exp_phase, ID) %>% 
  summarise(
    c = mean(cluster_length)
  ) %>% 
  mutate(
    exp_phase = recode(exp_phase, basal = "Baseline", experimental = "Experimental"),
    group = recode(group, control = "Control", experimental = "Uncertainty"),
  )


p5 <- p5_data %>% 
  ggplot(aes(
    exp_phase, c, color = exp_phase
  )) +
  facet_wrap(~group, strip.position = 'bottom') +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(alpha = 0.25, size = 2.5, position = position_dodge(0.9)) +
  geom_line(aes(group = ID), alpha = 0.25) +
  theme_pubr() +
  theme(#legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 20, 5),
                     limits = c(0, 20)
  ) +
  scale_color_manual(values = c('Baseline'="gray30", 'Experimental'="black")) +
  ylab('Size of clusters') +
  xlab("") +
  geom_signif(data = data.frame(group = c("Control","Uncertainty")),
              aes(y_position = c(18), xmin = c(1), 
                  xmax = c(2), annotations = c('NS.', '*')),
              tip_length = 0.01, color = "black", manual = TRUE)
p5


# plot 6: length of clusters

# panel

panel_2 <- ggdraw() +
  draw_plot(p2+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=0)), x = 0,       y = .5, width = 1/3, height = .5) +
  draw_plot(p4+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=0)), x = 1/3,     y = .5, width = 1/3, height = .5) +
  draw_plot(p5+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=0)), x = 1/3 * 2, y = .5, width = 1/3, height = .5) +
  draw_plot(p1+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=0)), x = 0,       y = 0, width = 1/2, height = .5) +
  draw_plot(p3+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=0)), x = 1/2,     y = 0, width = 1/2, height = .5) +
  draw_plot_label(label = c("A", "B", "C", "D", "E"), size = 15, x = c(0,1/3,1/3*2,0,1/2), y = c(1, 1, 1, 0.5, 0.5))

png("panel_2.png", width = 6, height = 4, units = "in", res = 300)
print(panel_2)
dev.off()

