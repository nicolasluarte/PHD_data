#The purpose of this script is to summarise important chunks of code for the analysis of the demand curve.

### [1] Tratamiento inicial de datos

#Los datos corresponden a los obtenidos del paquete de funciones de licómetro después del merge de metadata.
#De ser necesario revisar formato, los datos de test_data están guardados en el archivo: 
#[exp03 CURVA DEMANDA SACAROSA/doc03 R curva_demanda/src/20221220_test_data]

#Tambien se guardó metadata, esta tiene columnas adicionales al formato regular:
#[exp03 CURVA DEMANDA SACAROSA/doc03 R curva_demanda/src/20221220_metadata_caf]

pacman::p_load(
    beezdemand
)

test_data <- readRDS("demand_curve.RDS") %>% 
    mutate(
        n_sesion = as.numeric(as.character(n_sesion))
    )

d <-
    data_for_demand_curve(test_data, "sacarosa_crv", "events", c(1:100))
d

fits <-
    parse_demand_curve_fit(d)
fits

retrieval_data %>% 
    group_by(ID) %>% 
    slice(1) %>% 
    select(ID, group) %>% 
    filter(group == "Uncertainty")

stat_data <-
    fits$parsed %>% 
    mutate(
        exp_group = if_else(ID %in% c(413,416,417,418,419,495,496,497,500), "uncertainty", "control") %>% 
            as.factor(),
        ID = as.factor(ID)
    )

mdl <-
    lm(
        data = stat_data,
        log(Q0d) ~ exp_group
    )

mdl_emm <-
    emmeans::emmeans(mdl, pairwise ~ exp_group, type = "response")
mdl_emm

# create time bins
unique_time_bins <- test_data %>%
  group_by(ID,n_sesion) %>%
  filter(tipo_recompensa=="sacarosa_crv") %>%
  ungroup() %>%
  mutate(n_bin= as.integer(timestamp%/%600000)) %>% #modulo of timestamps (ms) by 10 minutes (600000ms) to get n_bin
  group_by(ID,pool,n_sesion,n_bin) %>%
  summarise(max_licks=max(actividad),
            max_eventos=max(evento)) %>%
  ungroup()


# Transform data to non-cumulative licks and events and calculate means
unique_bins <- unique_time_bins %>%
  group_by(ID, n_sesion) %>%
  arrange(n_bin, .by_group = TRUE) %>% # to make sure bins are arranged
  mutate(non_cum_max_licks= ifelse(max_licks==0,0,max_licks - c(0,max_licks[1:(n() - 1)])),
         non_cum_max_eventos= ifelse(max_eventos==0,0,max_eventos - c(0,max_eventos[1:(n() - 1)]))) %>% 
  select(!max_licks:max_eventos) %>% 
  pivot_longer(non_cum_max_licks:non_cum_max_eventos, names_to = "endpoint", values_to = "val") %>% 
  ungroup() %>%
  complete(nesting(ID,pool, n_sesion,endpoint), n_bin, fill=list(val=0)) %>%
  mutate(price = (case_when( #transform bins to licks values
    n_bin == 0 ~ 5,
    n_bin == 1 ~ 10,
    n_bin == 2 ~ 20,
    n_bin == 3 ~ 40,
    n_bin == 4 ~ 80,
    n_bin == 5 ~ 120))) %>% # average per mice over bin across sessions
  ungroup() 

log_unique_bins <- unique_bins %>%
  mutate(val = log10(val+1), # transform endpoint into log scale
         price = log10(price)) # average per mice over bin across sessions



readRDS("ID_group.RDS")
unique_bins <-  unique_bins %>%
  filter(endpoint=="non_cum_max_eventos") %>% 
  left_join(., ID_group, by = c("ID")) %>% 
  rename(tratamiento = group) %>% 
  group_by(ID,endpoint,n_bin,price,tratamiento,n_sesion)


### [3.3.4] EDA plot
p <- unique_bins %>% 
  group_by(ID,endpoint,n_bin,price,tratamiento) %>%
  summarise(m_val=mean(val),sd_val=sd(val)/sqrt(n())) %>%
  mutate(Q = log(m_val + exp(0.0001))) %>% 
  ggplot(aes(x = log(price), y = Q, group = tratamiento, color = tratamiento)) +
  facet_wrap(~ ID) + 
  geom_line() 
p



### [3.3.5] Toy example for nls fitting for demand curve
# ln(Q) = ln(Q0) + k * (e^(-alpha * Q0 * C) - 1)
#* Q is demand
#* Q0 is demand at cost 0, 
#* k is range of dosage in the same logarithmic scale as the cost

fitDemandCurve <- function(df) {
  # Calculate constants
  # K is distribution of work for maximum and minimum price
  k <- log((max(df$Q) - min(df$Q)))
  
  # Convert C and Q to log natural base
  df2 <- df %>% 
    mutate(C = log(C),
           Q = log(Q + exp(1^-4))) # To account for zero
  
  # Fit function
  m <- nls(Q ~ I(Q0 + k * (exp(1)^(-1 * alpha * Q0 * C) - 1)), 
           data = df2, 
           start = list(alpha = 0.001,
                        Q0 = 2), 
           control = list(maxiter = 1000),
           trace = FALSE)
  
  df3 <- data.frame(C = seq(0, 5, by = 0.01))
  plot(df3$C, predict(m, newdata = df3), type = "l", ylim = c(0, 4), xlim = c(0,5)); points(df2$C, df2$Q, pch = 16)
  return(coef(m))
}



### [3.3.6] Fit all data
df <- unique_bins %>%
  rename(C = price,
         Q = val) %>%
  mutate(ID = paste(ID, tratamiento,n_sesion, sep = ".")) %>%
  select(ID, C, Q) %>%
  group_by(ID,C) %>%
  summarise(Q=mean(Q))

p <- df %>% 
  mutate(Q = log(Q + exp(0.0001))) %>%
  ggplot(aes(x = log(C), y = Q)) +
  facet_wrap(~ ID) + 
  geom_line() 
p


### [3.3.7] Fit pooled data
pooled_baseline <- data.frame(df %>%
                                filter(grepl(ID,pattern="baseline")) %>%
                                fitDemandCurve())



### [3.3.8] Fit equation of Koffarnus, Franck, Stein, and Bickel

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6701494/
#Koffarnus
#This approach uses an exponentiated version of the economic demand curve. This works for us because we are not sure how cafeteria diet will modify our parameters. We epect it to reduce sucrose demand flexibility and increase Q0.


m3 <- FitCurves(as.data.frame(df), equation = "koff", xcol = "C", ycol = "Q", idcol = "ID", detailed = TRUE)

D_koff <- do.call(rbind, lapply(1:length(m3$newdats), function(i) { 
  X1 <- m3$newdats[[i]] # get data
  X1$ID.DROGA <- names(m3$newdats)[[i]] # assign ID
  return(X1)
})) %>%
  mutate(
    ID = gsub("[[:punct:]].*", "", ID.DROGA),
    n_sesion = gsub(".*.[[:punct:]]", "", ID.DROGA),
    TRAT = ifelse(grepl("caf", ID.DROGA), "caf", "baseline")) %>%
  select(c("ID","TRAT","n_sesion","x","y","k"))

C_koff <- do.call(rbind, lapply(1:length(m3$fits), function(i) { 
  X1 <- broom::tidy(m3$fits[[i]]); # get coefficients
  X1$ID.DROGA <- names(m3$fits)[[i]] # assign ID
  return(X1)
})) %>% 
  mutate(
    ID = gsub("[[:punct:]].*", "", ID.DROGA),
    n_sesion = gsub(".*.[[:punct:]]", "", ID.DROGA),
    TRAT = ifelse(grepl("caf", ID.DROGA), "caf", "baseline")) %>%
  select(c("ID","TRAT","n_sesion","term","estimate","std.error","p.value"))

data_curva_caf <- left_join(D_koff,C_koff, by=c("ID","TRAT","n_sesion")) %>%
  pivot_wider(names_from = "term",values_from = c("estimate","std.error","p.value"))

saveRDS(data_curva_caf, "demand_curve_data.RDS")

#save(data=data_curva_caf, file="20221129_data_curva_caf")


### [3.3.9] Create plot for each demand curve

p <- D_koff %>%
  group_by(ID,TRAT,x) %>%
  summarise(y=mean(y)) %>%
  ggplot(aes(x = x, y = log(y))) +
  scale_x_log10() +
  facet_wrap(~ ID) + 
  geom_line() +
  geom_line(data=D_koff,aes(x=x, y=log(y)),alpha=0.5)
p



### [3.3.10] Create plot for alpha and Q0

p <- C_koff %>% 
  #filter(ID != 406) %>% 
  ggplot(aes(x = TRAT, y = estimate)) +
  facet_wrap(~ term, scale = "free_y") +
  geom_boxplot() +
  geom_line(aes(group = ID)) +
  geom_text(aes(label = ID))
p

p <- C_koff %>%
  select(ID:estimate) %>%
  pivot_wider(names_from=term,values_from=estimate) %>%
  ggplot(aes(x= q0,y=alpha)) +
  geom_point() +
  geom_smooth(method=lm)
p

p <- C_koff %>% 
  ggplot(aes(x = ID, y = estimate)) +
  facet_wrap(~ term, scale = "free_y") +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2))
p


### [3.3.11] merge both dataframes and compare values

# merged_parameters <- combine(C_hs,C_koff) 
# p <- merged_parameters %>% 
#   #  filter(ID != 406) %>% 
#   ggplot(aes(x = source, y = estimate)) +
#   facet_wrap(~ term + DROGA, scale = "free_y") +
#   geom_boxplot() +
#   geom_line(aes(group = ID)) +
#   geom_text(aes(label = ID))
# p
# 
# aov_merge <- merged_parameters %>%
#   mutate(grp = ifelse(ID %in% c(405, 407, 403), 0, 1)) %>% 
#   #filter(grp == 1) %>% 
#   group_by(term,DROGA) %>% 
#   group_split() %>% 
#   map(., ~aov(estimate ~ source + Error(ID), data = .x))
# lapply(aov_merge, summary)

# ANOVA test to compare each individual approach
# fit <- C_hs  %>% 
#   mutate(grp = ifelse(ID %in% c(405, 407, 403), 0, 1)) %>% 
#   #filter(grp == 1) %>% 
#   group_by(term) %>% 
#   group_split() %>% 
#   map(., ~aov(estimate ~ DROGA + Error(ID), data = .x))
# lapply(fit, summary)



fit <- C_koff  %>% 
  mutate(grp = ifelse(ID %in% c(405, 407, 403), 0, 1)) %>% 
  #filter(grp == 1) %>% 
  group_by(term) %>% 
  group_split() %>% 
  map(., ~aov(estimate ~ DROGA + Error(ID), data = .x))
lapply(fit, summary)



### [3.3.12] MEAN DATA

#Summarise before demand curve

mean_data <- as.data.frame(df) %>%
  mutate(
    id = gsub("[[:punct:]].*", "", ID),
    DROGA = ifelse(grepl("acsf", ID), "acsf", "dyn")
  ) %>%
  mutate(grp = ifelse(ID %in% c(405, 407, 403), 0, 1)) %>% 
  #filter(grp == 1) %>% 
  select(DROGA, C,Q) %>%
  group_by(DROGA,C) %>%
  summarise(Q=mean(Q))

m5 <- FitCurves(mean_data, equation = "koff", xcol = "C", ycol = "Q", idcol = "DROGA", detailed = TRUE)

C_merge <- do.call(rbind, lapply(1:length(m5$fits), function(i) { 
  X1 <- tidy(m5$fits[[i]]); # get coefficients
  X1$DROGA <- names(m5$fits)[[i]] # assign ID
  return(X1)
}))

p <- C_merge %>% 
  ggplot(aes(x = DROGA, y = estimate)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=estimate-std.error,ymax=estimate+std.error)) +
  #geom_boxplot(data=C_koff, aes(x = DROGA, y = estimate),alpha=0.5) +
  facet_wrap(~ term, scale = "free_y")
p

