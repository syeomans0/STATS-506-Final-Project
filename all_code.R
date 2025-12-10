#Full code for project
#Libraries I will need for this project
library(tidyverse)
library(ggplot2)
library(dplyr)
library(data.table)
library(haven)

#Load in my data set
gss <- read_sas("gss7224_r2.sas7bdat")
#Preview
head(gss)
#Make sure the year variable is numeric
gss$YEAR <- as.numeric(gss$YEAR)

#Variables of interest: that relate to confidence in institutions 
#CONFED - CONFID. IN EXEC BRANCH OF FED GOVT 
#CONPRESS - CONFIDENCE IN PRESS 
#CONJUDGE - CONFID. IN UNITED STATES SUPREME COURT 
#CONLEGIS - CONFIDENCE IN CONGRESS 
#CONARMY - CONFIDENCE IN MILITARY 
#CONGOVT - CONFIDENCE IN GOVERNMENT DEPARTMENTS 
#CONCONG - CONFIDENCE IN US CONGRESS (diff between this and CONLEGIS?)
#CONFINAN - CONFID IN BANKS & FINANCIAL INSTITUTIONS

#Make sure these vars are in the data set
conf_vars <- c("CONFED","CONPRESS","CONJUDGE","CONLEGIS","CONARMY",
               "CONGOVT","CONCONG","CONFINAN")
conf_vars %in% names(gss)
#Get rid of the Missing values in these variables and the not answered numbers, 8 & 9
gss <- gss %>%
       mutate(across(all_of(conf_vars), ~replace(., . %in% c(8, 9), NA)))

#Set up years of interest
#rm(years_post)
years_pre  <- 2012:2018
years_dur <- c(2021, 2022, 2023, 2024)

#Make a subsetted pre-pandemic data set
gss_pre <- gss %>%
           filter(YEAR %in% years_pre)
#head(gss_pre) <- previewed to see if it worked

#Subset for the during and post pandemic dates
#rm(gss_post)
gss_dur <- gss %>%
            filter(YEAR %in% years_dur)

#Basic scatter-plot for CONJUDGE and CONLEGIS (without weights so just raw data)
#Since there are tons of IDs (aka respondents) find the average confidence
#This first one is for the CONJUDGE (confidence in supreme court)
gss_summary <- gss_pre %>%
               group_by(YEAR) %>%
               summarize(mean_conf = mean(CONJUDGE, na.rm = TRUE)) %>%
               ungroup()

ggplot(gss_summary, aes(x = YEAR, y = mean_conf)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = round(mean_conf, 2)),
            vjust = -0.8, hjust = 0.5,           
            size = 3) +
  scale_y_continuous(breaks = 1:3, labels = c("Great deal", "Some", "Hardly any")) +
  labs(title = "Average Confidence in US Supreme Court",
       x = "Year", 
       y = "Average Confidence",
       caption = "Key: 1 = 'Hardly any', 2 = 'Some', 3 = 'Great deal'") +
  theme_minimal()

#Same thing as above but for the confidence in congress
gss_summary1 <- gss_pre %>%
                group_by(YEAR) %>%
                summarize(mean_confi = mean(CONLEGIS, na.rm = TRUE)) %>%
                ungroup()

ggplot(gss_summary1, aes(x = YEAR, y = mean_confi)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = round(mean_confi, 2)),
            vjust = -0.8, hjust = 0.5,           
            size = 3) +                   
  scale_y_continuous(breaks = 1:3, labels = c("Great deal", "Some", "Hardly any")) +
  labs(
    title = "Average Confidence in Congress",
    x = "Year", 
    y = "Average Confidence (Mean Score, unweighted)",
    caption = "Key: 1 = 'Hardly any', 2 = 'Some', 3 = 'Great deal'" #Added key
  ) +
  theme_minimal()
#low in 2012 (recession so makes sense), then goes up since we get out of it, 
#then right back down after election

#Want to quick check another variable of interest: CONPRESS and want to do dur/post year quick
gss_sum <- gss_pre %>%
           group_by(YEAR) %>%
           summarize(mean_confid = mean(CONPRESS, na.rm = TRUE)) %>%
           ungroup()

ggplot(gss_sum, aes(x = YEAR, y = mean_confid)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = round(mean_confid, 2)),
            vjust = -0.8, hjust = 0.5,           
            size = 3) +
  scale_y_continuous(breaks = 1:3, labels = c("Great deal", "Some", "Hardly any")) +
  labs(title = "Average Confidence in the Press",
       x = "Year", 
       y = "Average Confidence",
       caption = "Key: 1 = 'Hardly any', 2 = 'Some', 3 = 'Great deal'") +
  theme_minimal()

gss_sum1 <- gss_dur %>%
            group_by(YEAR) %>%
            summarize(mean_con = mean(CONPRESS, na.rm = TRUE)) %>%
            ungroup()

ggplot(gss_sum1, aes(x = YEAR, y = mean_con)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = round(mean_con, 2)),
            vjust = -0.8, hjust = 0.5,           
            size = 3) +
  scale_y_continuous(breaks = 1:3, labels = c("Great deal", "Some", "Hardly any")) +
  labs(title = "Average Confidence in the Press",
       x = "Year", 
       y = "Average Confidence",
       caption = "Key: 1 = 'Hardly any', 2 = 'Some', 3 = 'Great deal'") +
  theme_minimal()

#Need wights if we want this to be representative of the US population
#For the pre-pandemic we need to use the WTSS weight
#For the during/post we need to use the WTSSPS weight
#These are already in the data set but want to give them the same name for ease of later use
gss_pre <- gss_pre %>%
           mutate(weight = WTSS,
                  period = "Pre-pandemic")

gss_dur <- gss_dur %>%
           mutate(weight = WTSSPS,
                  period = "During/Post-Pandemic")
#rm(gss_sum, gss_sum1, gss_summary, gss_summary1)
#How does adding these weights change our plots we originally made?
#Combine the two data sets first (the ones with years labeled)
gss_fin <- bind_rows(gss_pre, gss_dur)

#Clean all the confidence vars (already set) in the combined
gss_fin <- gss_fin %>%
           mutate(across(all_of(conf_vars), ~replace(., . %in% c(8,9), NA)))

#Find the mean confidence level over time (1-3 scale; labels above)
#SUPREME COURT
summary_court <- gss_fin %>%
                 summarize(mean_conf = weighted.mean(CONJUDGE, 
                                      weight, na.rm=TRUE), .by=c(period, YEAR))

ggplot(summary_court, aes(YEAR, mean_conf, color=period)) +
  geom_point(size=3) +
  geom_line()+
  geom_text(aes(label = round(mean_conf, 2)),
            vjust = -0.8, hjust = 0.5,           
            size = 3) +
  scale_x_continuous(breaks = summary_court$YEAR) +
  scale_y_continuous(breaks=1:3, labels=c("Great deal","Some","Hardly any")) +
  labs(title="Weighted Mean Confidence in Supreme Court",
       x = "Year", 
       y = "Weighted Confidence") +
  theme_minimal()

#EXEC BRANCH
summary_exec <- gss_fin %>%
  summarize(mean_conf = weighted.mean(CONFED, 
                                      weight, na.rm=TRUE), .by=c(period, YEAR))

ggplot(summary_exec, aes(YEAR, mean_conf, color=period)) +
  geom_point(size=3) +
  geom_line()+
  geom_text(aes(label = round(mean_conf, 2)),
            vjust = -0.8, hjust = 0.5,           
            size = 3) +
  scale_x_continuous(breaks = summary_exec$YEAR) +
  scale_y_continuous(breaks=1:3, labels=c("Great deal","Some","Hardly any")) +
  labs(title="Weighted Mean Confidence in the Executive Branch",
       x = "Year", 
       y = "Weighted Confidence") +
  theme_minimal()

#PRESS (none have been really low but maybe this will show bigger changes?)
summary_press <- gss_fin %>%
                 summarize(mean_conf = weighted.mean(CONPRESS, 
                                        weight, na.rm=TRUE), .by=c(period, YEAR))

ggplot(summary_press, aes(YEAR, mean_conf, color=period)) +
  geom_point(size=3) +
  geom_line()+
  geom_text(aes(label = round(mean_conf, 2)),
            vjust = -0.8, hjust = 0.5,           
            size = 3) +
  scale_x_continuous(breaks = summary_press$YEAR) +
  scale_y_continuous(breaks=1:3, labels=c("Great deal","Some","Hardly any")) +
  labs(title="Weighted Mean Confidence in the Press/Media",
       x = "Year", 
       y = "Weighted Confidence") +
  theme_minimal()


#Formal testing to see if there is a difference:
#Null: the confidence levels of gov orgs are the same for pre and during/post pandemic years
#Alternative: the confidence levels are different
#Some more packages, now for hypo test
install.packages("survey")
library(survey)
install.packages("multcomp")
library(multcomp)
install.packages("emmeans")
library(emmeans)

#Chi square test with the survey package (since we have weights)
gss_survey <- svydesign(id = ~1, weights = ~weight, data = gss_fin)

court_chi <- svychisq(~ period + CONJUDGE, design = gss_survey)
court_chi
execbranch_chi <- svychisq(~ period + CONFED, design = gss_survey)
execbranch_chi

















