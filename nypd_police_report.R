library(tidyverse)
library(lubridate)
library(ggplot2)
options(width=60)

# importing
url <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"
nypd_shooting_incident <- read_csv(url)
summary(nypd_shooting_incident)
nypd_shooting_incident <- nypd_shooting_incident %>% 
    select(OCCUR_DATE,BORO,PERP_AGE_GROUP,PERP_RACE,
           VIC_AGE_GROUP,VIC_SEX,VIC_RACE) 

# Tidying and Transforming Data
nypd_si_all <- nypd_shooting_incident %>%
    select(BORO, OCCUR_DATE) %>%
    mutate(OCCUR_DATE = mdy(OCCUR_DATE), INCIDENTS_all = 1,
           YEAR = format_ISO8601(OCCUR_DATE, precision = "y"),
           YEAR_MONTH = format_ISO8601(OCCUR_DATE, precision = "ym"))

nypd_si_all_global <- nypd_si_all %>% 
    select(YEAR, INCIDENTS_all) %>%
    group_by(YEAR) %>%
    summarise(INCIDENTS_all = sum(INCIDENTS_all), .groups = "keep") %>%
    ungroup()

nypd_si_all_global_monthly <- nypd_si_all %>% 
    select(YEAR_MONTH, INCIDENTS_all) %>%
    group_by(YEAR_MONTH) %>%
    summarise(INCIDENTS_all = sum(INCIDENTS_all), .groups = "keep") %>%
    ungroup()

nypd_si_2020 <- nypd_shooting_incident %>%
    select(BORO, OCCUR_DATE) %>%
    mutate(OCCUR_DATE = mdy(OCCUR_DATE), INCIDENTS_2020 = 1,
           MONTH = format(OCCUR_DATE, "%b")) %>%
    filter(OCCUR_DATE >= as.Date("2020-01-01") & OCCUR_DATE <= as.Date("2020-12-31"))

nypd_si_2020_global <- nypd_si_2020 %>% 
    select(MONTH, INCIDENTS_2020) %>%
    group_by(MONTH) %>%
    summarise(INCIDENTS_2020 = sum(INCIDENTS_2020), .groups = "keep") %>%
    ungroup()
    
nypd_si_2019 <- nypd_shooting_incident %>%
    select(BORO, OCCUR_DATE) %>%
    mutate(OCCUR_DATE = mdy(OCCUR_DATE), INCIDENTS_2019 = 1,
           MONTH = format(OCCUR_DATE, "%b")) %>%
    filter(OCCUR_DATE >= as.Date("2019-01-01") & OCCUR_DATE <= as.Date("2019-12-31"))

nypd_si_2019_global <- nypd_si_2019 %>% 
    select( MONTH, INCIDENTS_2019) %>%
    group_by(MONTH) %>%
    summarise(INCIDENTS_2019 = sum(INCIDENTS_2019), .groups = "keep") %>%
    ungroup()

nypd_si_2018 <- nypd_shooting_incident %>%
    select(BORO, OCCUR_DATE) %>%
    mutate(OCCUR_DATE = mdy(OCCUR_DATE), INCIDENTS_2018 = 1,
           MONTH = format(OCCUR_DATE, "%b")) %>%
    filter(OCCUR_DATE >= as.Date("2018-01-01") & OCCUR_DATE <= as.Date("2018-12-31"))

nypd_si_2018_global <- nypd_si_2018 %>% 
    select(MONTH, INCIDENTS_2018) %>%
    group_by(MONTH) %>%
    summarise(INCIDENTS_2018 = sum(INCIDENTS_2018), .groups = "keep") %>%
    ungroup()

# Visualizing Data
nypd_si_all_global %>%
    ggplot(aes(x = YEAR, y = INCIDENTS_all, group = 1)) +
    geom_line(aes(color = "Incidents")) +
    geom_point(aes(color = "Incidents")) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90)) +
    labs(title = "NYPD Shooting Incidents in New York", y = "Incidents", x="Year")

# Analyzing Data
nypd_si_last_3_year <- full_join(nypd_si_2018_global,
                                 nypd_si_2019_global, by="MONTH")

nypd_si_last_3_year <- full_join(nypd_si_last_3_year,
                                 nypd_si_2020_global, by="MONTH")

level_order <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
nypd_si_last_3_year %>%
    ggplot(aes(x = factor(MONTH, level = level_order), y = INCIDENTS_2018, group = 1)) +
    geom_line(aes(color = "Incidents_2018")) +
    geom_point(aes(color = "Incidents_2018")) +
    geom_line(aes(y = INCIDENTS_2019, color = "Incidents_2019")) +
    geom_point(aes(y = INCIDENTS_2019, color = "Incidents_2019")) +
    geom_line(aes(y = INCIDENTS_2020, color = "Incidents_2020")) +
    geom_point(aes(y = INCIDENTS_2020, color = "Incidents_2020")) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90)) +
    labs(title = "NYPD Shooting Incidents for the last three years", y = "Incidents", x="Month")

# Modeling Data
mod <- lm(INCIDENTS_all ~ as.integer(YEAR)-INCIDENTS_all, data = nypd_si_all_global)
summary(mod)

nypd_si_all_global_pred <- nypd_si_all_global %>%
    mutate(pred = predict(mod))
nypd_si_all_global_pred %>% ggplot(aes(x = YEAR, INCIDENTS_all, group = 1)) +
    geom_point(aes(y = INCIDENTS_all, x = YEAR), 
               color = "blue") +
    geom_line(aes(y = pred, x = YEAR),
               color = "red") +
    labs(title = "Shooting Incidents 2006-2020", x = "Year", y="Incients")

