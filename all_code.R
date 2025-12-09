#full code for project
library(tidyverse)
library(ggplot2)
library(dplyr)
library(data.table)
library(haven)
gss <- read_sas("gss7224_r2.sas7bdat")
head(gss)
gss$YEAR <- as.numeric(gss$YEAR)

#Variables of interest: 
#CONFED - CONFID. IN EXEC BRANCH OF FED GOVT 
#CONPRESS - CONFIDENCE IN PRESS 
#CONJUDGE - CONFID. IN UNITED STATES SUPREME COURT 
#CONLEGIS - CONFIDENCE IN CONGRESS 
#CONARMY - CONFIDENCE IN MILITARY 
#CONGOVT - CONFIDENCE IN GOVERNMENT DEPARTMENTS 
#CONCONG - CONFIDENCE IN US CONGRESS (diff between this and CONLEGIS?)
#CONFINAN - CONFID IN BANKS & FINANCIAL INSTITUTIONS
#graphs of these over time
conf_vars <- c("CONFED","CONPRESS","CONJUDGE","CONLEGIS","CONARMY",
               "CONGOVT","CONCONG","CONFINAN")
conf_vars %in% names(gss)

gss <- gss %>%
       mutate(across(all_of(conf_vars), ~replace(., . %in% c(8, 9), NA)))

rm(years_post)
years_pre  <- 2012:2018
years_dur <- c(2021, 2022, 2023, 2024)
gss_pre <- gss %>%
           filter(YEAR %in% years_pre)
#head(gss_pre)

rm(gss_post)
gss_dur <- gss %>%
            filter(YEAR %in% years_dur)

#need wights if we want this to be representative of the US population

# basic scatter-plot for CONJUDGE and CONLEGIS
gss_summary <- gss_pre %>%
               group_by(YEAR) %>%
               summarize(mean_conf = mean(CONJUDGE, na.rm = TRUE)) %>%
               ungroup()

ggplot(gss_summary, aes(x = YEAR, y = mean_conf)) +
  geom_point(size = 3) +
  geom_line() +
  scale_y_continuous(breaks = 1:3, labels = c("Great deal", "Some", "Hardly any")) +
  labs(title = "Average Confidence in US Supreme Court",
       x = "Year", y = "Average Confidence") +
  theme_minimal()

gss_summary1 <- gss_pre %>%
                group_by(YEAR) %>%
                summarize(mean_confi = mean(CONLEGIS, na.rm = TRUE)) %>%
                ungroup()

ggplot(gss_summary1, aes(x = YEAR, y = mean_confi)) +
  geom_point(size = 3) +
  geom_line() +
  scale_y_continuous(breaks = 1:3, labels = c("Great deal", "Some", "Hardly any")) +
  labs(title = "Average Confidence in Congress",
       x = "Year", y = "Average Confidence") +
  theme_minimal()
