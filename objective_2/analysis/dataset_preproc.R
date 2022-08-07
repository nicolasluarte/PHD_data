# load all libs
pacman::p_load(
  tidyverse,
  stringi,
  rprojroot
)

# load metadata
here::here()
metadata <- read_csv("~/PHD_data/objective_2_metadata_pilot.csv") %>% 
  mutate_all(as.character) %>% 
  mutate(
    start = hms::as_hms(start),
    end = hms::as_hms(end),
    date = lubridate::ymd(date)
  )

# load data

# list all lickometer csv files
list_of_files <- list.files(
  path = "~/PHD_data",
  recursive = TRUE,
  pattern = "\\.csv$",
  full.names = TRUE
) %>% 
  as_tibble() %>% 
  filter(grepl('experimental_log', value)) %>% 
  pull(value)

# import
df_raw <- list_of_files %>% 
  map_df(., function(file){
    r <- read_csv(file, id = "file_name") %>% 
      mutate_all(as.character)
  })

df <- df_raw %>% 
  rename(arduino = arduinoNumber) %>% 
  mutate(
    pc = str_extract(file_name, "data_pc_[0-9]"),
    pc = stri_sub(pc,-1),
    pcTime = as.numeric(pcTime),
    time = hms::as_hms(strptime(pcTime, format = "%H%M%S")),
    date = lubridate::ymd(date)
  )

# add metadata
df_meta <- df %>%
  left_join(
    metadata %>% select(-random_spout),
    by = c("pc", "date", "arduino")
  ) %>% 
  # only get data within session and only files without errors
  filter(
    time >= start, 
    time <= end,
    complete_data == 1
  ) %>% 
  mutate(
    n_sesion = as.numeric(n_sesion)
  )

# add experimental groups and phases
df_meta <- df_meta %>% 
  mutate(
    exp_phase = if_else(n_sesion <= 12, "BASAL", "EXPERIMENTAL"),
    exp_group = case_when(
      ID %in% c(217, 219, 223, 224) ~ "UNC",
      TRUE ~ "CONTROL"
    ),
    spoutType = if_else(randomSpout == 1, "UNC", "CER"),
    spoutNumber = as.numeric(spoutNumber)
    ) %>% 
  select(-...2)

# fix all column variables
# add reward and non rewarded

# plots

# plot 1: baseline licks and events per spout

df_plot <- df_meta %>% 
  group_by(
    ID,
    date,
    spoutType
  ) %>% 
  mutate(
    licksCum = as.numeric(licksCum) - as.numeric(licksCum)[1],
    eventsCum = as.numeric(eventsCum) - as.numeric(eventsCum)[1]
  ) %>% 
  ungroup()

df_plot <- df_plot %>% 
  group_by(
    ID,
    date,
    spoutType,
    exp_phase,
    exp_group,
    n_sesion
  ) %>% 
  summarise(
    licksMax = max(licksCum),
    eventsMax = max(eventsCum)
  ) %>% 
  ungroup() %>% 
  group_by(
    n_sesion,
    exp_phase,
    exp_group,
    spoutType
  ) %>% 
  summarise(
    licksMean = mean(licksMax),
    eventsMean = mean(eventsMax),
    licksE = sd(licksMax) / sqrt(n()),
    eventsE = sd(eventsMax) / sqrt(n())
  ) %>% 
  ungroup()

#licks
df_plot %>% 
  ggplot(aes(
    n_sesion,
    licksMean,
    color = exp_group,
    group = exp_group
  )) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = licksMean - licksE, ymax = licksMean + licksE)) +
  facet_wrap(exp_phase~spoutType~exp_group, scales = "free_x")
#licks by time
df_plot %>% 
  filter(exp_phase == "EXPERIMENTAL", exp_group == "UNC") %>% 
  ggplot(aes(
    n_sesion,
    licksMean,
    color = spoutType,
    group = spoutType
  )) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = licksMean - licksE, ymax = licksMean + licksE))

#events
df_plot %>% 
  filter(exp_phase == "EXPERIMENTAL", exp_group == "UNC") %>% 
  ggplot(aes(
    n_sesion,
    eventsMean,
    color = spoutType,
    group = spoutType
  )) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = eventsMean - eventsE, ymax = eventsMean + eventsE))

# correct by baseline licks


