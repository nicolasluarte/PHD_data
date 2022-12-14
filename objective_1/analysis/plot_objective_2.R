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

# plot 1: weight

p1_data <- readRDS('plot_1_data.rds')

p1 <- p1_data %>% 
  mutate(group = factor(group, levels = c("control", "experimental_low_unc", "experimental_high_unc")),
         group = recode(group,
                        control = "C",
                        experimental_low_unc = "L",
                        experimental_high_unc = "H")) %>% 
  ggplot(aes(
    group, m, color = group
  )) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = .2, 
                                             dodge.width = .7), 
             alpha = .5) +
  scale_color_brewer(palette = "Set2") +
  theme_pubr() +
  xlab("Group uncertainty") +
  ylab("Mean weight (gr)") +
  theme(
    legend.position = "none",
    legend.title = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 35, 5),
                     limits = c(15, 35)
  ) +
  geom_signif(comparisons = list(c("Control", "High")),
              map_signif_level = TRUE,
              y_position = c(33),
              tip_length = 0,
              color = "black")
p1
png("weight.png", width = 6, height = 4, units = "in", res = 300)
print(p1)
dev.off()

# plot 2: intake

p2_data <- readRDS('plot_2_data.rds')

p2 <- p2_data %>% 
  filter(protocol %in% c("control", "experimental")) %>% 
  mutate(group = factor(group, levels = c("control", "experimental_low_unc", "experimental_high_unc")),
         group = recode(group,
                        control = "C",
                        experimental_low_unc = "L",
                        experimental_high_unc = "H")) %>% 
  ggplot(aes(
    group, m, color = group
  )) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = .2, 
                                             dodge.width = .7), 
             alpha = .5) +
  scale_color_brewer(palette = "Set2") +
  theme_pubr() +
  xlab("Group uncertainty") +
  ylab("# pellets") +
  theme(
    legend.position = "none",
    legend.title = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 80, 5),
                     limits = c(50, 80)
  ) +
  geom_signif(comparisons = list(c("Control", "High")),
              map_signif_level = TRUE,
              y_position = c(76),
              tip_length = 0,
              color = "black")
p2
png("intake.png", width = 6, height = 4, units = "in", res = 300)
print(p2)
dev.off()


# plot 3: licks over sessions

p3_data <- readRDS('plot_3_data.rds') %>% 
  filter(task_type == "pr", tipo_recompensa == "sacarosa") %>% 
  mutate(group = factor(group, levels = c("control", "experimental_low_unc", "experimental_high_unc")),
         group = recode(group,
                        control = "C",
                        experimental_low_unc = "L",
                        experimental_high_unc = "H"))

p3 <- p3_data %>% ggplot(aes(
  session, n_licks, color = group, group = group
)) +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "col", aes(fill = group), 
               size = 0,
               position = position_dodge(width = .9)) +
  stat_summary(aes(group = group, color = group), fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width = .2, size = 0.5,
               position = position_dodge(width = .9)) +
  geom_point(inherit.aes = FALSE, aes(session, n_licks, group = group, fill = group),
             position = position_jitterdodge(jitter.width = .1, 
                                            dodge.width = .9), 
             alpha = .1) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_pubr() +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 3000, 500),
                     limits = c(0, 3000)
  ) +
  xlab("Sessions") +
  ylab("# licks")
p3


# plot 4: slope estimate of licks over sessions

p4_data <- readRDS('plot_4_data.rds')

p4 <- p4_data %>% 
  mutate(
    group = factor(group, levels = c("control", "experimental_low_unc", "experimental_high_unc")),
    group = recode(group,
                   control = "C",
                   experimental_high_unc = "H",
                   experimental_low_unc = "L"
    )
  ) %>% 
  ggplot(aes(
    group, session.trend, fill = group, color = group,
    ymin = if_else(session.trend>0,((session.trend - SE) > 0) * (session.trend - SE), session.trend - SE),
    ymax = session.trend + SE
  )) +
  geom_col() +
  geom_errorbar(width = .2, size = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_pubr() +
  theme(legend.title = element_blank()) +
  ylab("Slope estimates") +
  xlab("") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(-50, 100, 25),
                     limits = c(-50, 100)
  )
p4


# plot 5: number of licks during fr5

p5_data <- readRDS('plot_5_data.rds') %>% 
  mutate(
    group = factor(group, levels = c("control", "experimental_low_unc", "experimental_high_unc")),
    group = recode(group,
                   control = "C",
                   experimental_high_unc = "H",
                   experimental_low_unc = "L"
    ))


p5 <- p5_data %>% 
  ggplot(aes(
    group, l_, color = group
  )) +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "col", aes(group = group, fill = group), 
               size = 0,
               position = position_dodge(width = .9)) +
  stat_summary(aes(group = group, color = group), fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width = .2, size = 0.5,
               position = position_dodge(width = .9)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_pubr() +
  theme(legend.title = element_blank()) +
  ylab("# of licks") +
  xlab("") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 4500, 500),
                     limits = c(0, 4500)
  ) +
  geom_signif(y_position = c(3600, 4100), xmin = c(1, 2), 
              xmax = c(3, 3), annotation = c(""),
              tip_length = 0.01, color = "black")
p5


# plot 6: number of clusters

p6_data <- readRDS('plot_6_data.rds') %>% 
  mutate(group = factor(group, levels = c("control", "experimental_low_unc", "experimental_high_unc")),
         group = recode(group,
                        control = "C",
                        experimental_low_unc = "L",
                        experimental_high_unc = "H"))

p6 <- p6_data %>% 
  ggplot(aes(
  n_sesion, n_cluster
)) +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "col", aes(fill = group), 
               size = 0,
               position = position_dodge(width = .9)) +
  stat_summary(aes(group = group, color = group), fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width = .2, size = 0.5,
               position = position_dodge(width = .9)) +
  geom_point(inherit.aes = FALSE, aes(n_sesion, n_cluster, group = group, fill = group),
             position = position_jitterdodge(jitter.width = .1, 
                                             dodge.width = .9), 
             alpha = .1) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_pubr() +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 600, 100),
                     limits = c(0, 600)
  ) +
  ylab("# clusters") +
  xlab("Sessions")

# plot 7: slope of  # clusters

p7_data <- readRDS('plot_7_data.rds') %>% 
  mutate(
        group = factor(group, levels = c("control", "experimental_low_unc", "experimental_high_unc")),
        group = recode(group,
                       control = "C",
                       experimental_high_unc = "H",
                       experimental_low_unc = "L"
        )
      )

p7 <- p7_data %>% 
      ggplot(aes(
        group, n_sesion.trend, fill = group, color = group,
        ymax = n_sesion.trend + SE,
        ymin = if_else(n_sesion.trend>0,((n_sesion.trend - SE) > 0) * (n_sesion.trend - SE), n_sesion.trend - SE)
      )) +
      geom_col() +
      geom_errorbar(width = .2, size = 0.5) +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Set2") +
      theme_pubr() +
      theme(legend.title = element_blank()) +
      ylab("Slope estimates") +
      xlab("") +
      scale_y_continuous(expand = c(0, 0),
                     breaks = seq(-10, 40, 10),
                     limits = c(-10, 40)
      )


# Plot 8: length of clusters
c_l <- readRDS('plot_8_data.rds')$emtrends %>% 
  as_tibble() %>% 
  mutate(
    group = factor(group, levels = c("control", "experimental_low_unc", "experimental_high_unc")),
    group = recode(group,
                   control = "C",
                   experimental_high_unc = "H",
                   experimental_low_unc = "L"
    )
  )

p8 <- c_l %>% 
  ggplot(aes(
    group, n_sesion.trend, fill = group, color = group,
    ymax = n_sesion.trend + SE,
    ymin = if_else(n_sesion.trend>0,((n_sesion.trend - SE) > 0) * (n_sesion.trend - SE), n_sesion.trend - SE)
  )) +
  geom_col() +
  geom_errorbar(width = .2, size = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_pubr() +
  theme(legend.title = element_blank()) +
  ylab("Slope estimates") +
  xlab("") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(-50, 150, 50),
                     limits = c(-50, 150)
  )


# panel

ylab_hjust <- 0.5

panel_1 <- ggdraw() +
  draw_plot(p3+xlab('')+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=ylab_hjust)), x = 0,       y = .5, width = 3/6, height = .5) +
  draw_plot(p4+xlab('')+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=ylab_hjust)), x = 3/6,     y = .5, width = 1.5/6, height = .5) +
  draw_plot(p5+xlab('')+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=ylab_hjust)), x = 4.5/6,   y = .5, width = 1.5/6, height = .5) +
  draw_plot(p6+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=ylab_hjust)), x = 0,       y = 0,  width = 3/6, height = .5) +
  draw_plot(p7+xlab('')+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=ylab_hjust)), x = 3/6,     y = 0,  width = 1.5/6, height = .5) +
  draw_plot(p8+xlab('')+theme(legend.position = 'none', text = element_text(size = 10), axis.title.y = element_text(hjust=ylab_hjust)), x = 4.5/6,   y = 0,  width = 1.5/6, height = .5) +
  draw_plot_label(label = c("A", "B", "C", "D", "E", "F"), size = 12, x = c(0, 3/6, 4.5/6, 0, 3/6, 4.5/6), y = c(1, 1, 1, 0.5, 0.5, 0.5))

png("panel_1.png", width = 6, height = 4, units = "in", res = 300)
print(panel_1)
dev.off()
