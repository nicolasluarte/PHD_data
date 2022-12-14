pacman::p_load(
  tidyverse,
  ggplot2,
  ggpubr
)

# weights

weight_data <- read_csv("https://raw.githubusercontent.com/nicolasluarte/PHD_data/main/objective_1/data/raw/weights/weights_pool3.csv") %>% 
  mutate(
    ID = as.factor(ID),
    date = lubridate::ymd(date),
    weight = as.numeric(weight)
  )

# intake

intake_files <- list.files(path = "~/repos/nbolab_FED/raw_data/",
                          pattern = "^4",
                          recursive = TRUE,
                          full.names = TRUE)
intake_data <- read_csv(intake_files) %>% 
  mutate(
    time = lubridate::as_datetime(time),
    date = lubridate::date(time),
    animal = as.factor(animal)
  ) %>% 
  filter(time >= "2022-09-26 00:00:00") %>% 
  rename(ID = animal)

intake_data_daily <- intake_data %>% 
  group_by(date, ID) %>% 
  summarise(
    pellets = n()
  ) %>% 
  ungroup()

intake_data_m <- intake_data_daily %>% 
  group_by(date) %>% 
  summarise(
    pellets_ = mean(pellets),
    e = sd(pellets) / sqrt(n())
  ) %>% 
  rename(pellets = pellets_) %>% 
  mutate(ID = "Mean intake")

weight_data_m <- weight_data %>% 
  group_by(date) %>% 
  summarise(
    weight_ = mean(weight),
    e = sd(weight) / sqrt(n())
  ) %>% 
  rename(weight = weight_) %>% 
  mutate(ID = "Mean weight")

# weights plots

weight_data %>% 
  ggplot(aes(
    date,
    weight,
    color = ID
  )) +
  geom_line() +
  geom_point() +
  geom_line(data = weight_data_m, color = "black") +
  geom_point(data = weight_data_m, color = "black") +
  geom_errorbar(data = weight_data_m,
                aes(ymin = weight - e, ymax = weight + e),
                color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-26"))) +
  theme_pubr()

# intake plot

intake_data_daily %>% 
  filter(date < "2022-09-30") %>% 
  ggplot(aes(date, pellets, color = ID)) +
  geom_line() +
  geom_point() +
  geom_line(
    data = intake_data_m %>% filter(date < "2022-09-30"),
    color = "black"
  ) +
  geom_errorbar(
    data = intake_data_m %>% filter(date < "2022-09-30"),
    color = "black",
    aes(ymin = pellets - e, ymax = pellets + e)
  ) +
  geom_point(
    data = intake_data_m %>% filter(date < "2022-09-30"),
    color = "black"
  ) +
  theme_pubr()
