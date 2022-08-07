# load all libs
pacman::p_load(
  tidyverse,
  stringi,
  here
)

# disable scientific notation
options(scipen = 999) 

# this assumes that the repo is on the home folder
setwd('~/PHD_data')

# load metadata
metadata_dir <- './objective_2/data/meta/objective_2_metadata_pilot.csv'
metadata <- read_csv(metadata_dir) %>% 
  # imported all as character to avoid data compatibility issues
  mutate_all(as.character)

# set columns data types
metadata <- metadata %>% 
  mutate(
    start = hms::as_hms(start),
    end = hms::as_hms(end),
    date = lubridate::ymd(date),
    random_spout = case_when(
      random_spout == "0" ~ "LEFT",
      random_spout == "2" ~ "RIGHT",
      TRUE ~ "NO_RANDOM"
    ),
    complete_data = as.logical(as.numeric(complete_data)),
    n_sesion = as.numeric(n_sesion),
    ID = as.factor(ID),
    lickometer_number = as.factor(lickometer_number),
    com = as.factor(com),
    arduino = as.factor(arduino),
    pc = as.factor(pc),
    across(init_licks_0:end_events_2, ~as.numeric(.x))
  )

# load data
data_dir <- 'objective_2/data/raw/objective_2_pool_1.csv'
data <- read_csv(data_dir)

# set column data types
data <- data %>% 
  mutate(
    date = lubridate::ymd(date),
    pcTime = hms::as_hms(strptime(as.numeric(pcTime), format = "%H%M%S")),
    msFromStart = as.numeric(msFromStart),
    arduino = as.factor(arduino),
    spoutNumber = case_when(
      spoutNumber == "0" ~ "LEFT",
      spoutNumber == "2" ~ "RIGHT",
      TRUE ~ "ERROR"
    ), 
    licksCum = as.numeric(licksCum),
    eventsCum = as.numeric(eventsCum),
    rewardsCum = as.numeric(rewardsCum),
    # TRUE in random spout means that this spout was with a prob of 0.5
    randomSpout = as.logical(randomSpout),
    pc = as.factor(pc)
  ) %>% 
  select(-file_name)

# join data with metadata
df <- data %>% 
  left_join(
    metadata,
    by = c("pc", "date", "arduino")
  ) %>% 
  # only get data within session and only files without errors
  filter(
    pcTime >= start,
    pcTime <= end,
    complete_data == TRUE
  )

# add experimental groups and phases
df <- df %>% 
  mutate(
    exp_phase = if_else(n_sesion <= 12, "BASAL", "EXPERIMENTAL"),
    exp_group = case_when(
      ID %in% c(217, 219, 223, 224) ~ "UNC",
      TRUE ~ "CONTROL"
    )
  )

# rename position of random spout and random spout bool
df <- df %>% 
  rename(
    is_random = randomSpout,
    random_spout_pos = random_spout
  )

# add column to detect if current event was rewarded or not
df <- df %>% 
  group_by(
    ID,
    n_sesion,
    spoutNumber
  ) %>% 
  mutate(
    is_reward = if_else(eventsCum - lag(eventsCum) == 1 & rewardsCum - lag(rewardsCum) == 1, "REWARDED", "NOT_REWARDED")
  ) %>% 
  ungroup()

# correct baseline licks and events
df <- df %>% 
  group_by(
    ID,
    n_sesion,
    spoutNumber
  ) %>% 
  mutate(
    licks_corr = licksCum - licksCum[1],
    events_corr = eventsCum - eventsCum[1],
    rewards_corr = rewardsCum - rewardsCum[1]
  )

# msFromStart for each ID
df <- df %>% 
  group_by(ID, n_sesion) %>% 
  mutate(
    msFromStart_corr = msFromStart - msFromStart[1]
  ) %>% 
  ungroup()

# save final data
write_rds(df, "./objective_2/data/objective_2_pool_1.RDS")
