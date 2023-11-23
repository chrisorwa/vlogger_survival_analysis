
# load required libraries
library("survival")
library("survminer")
library(lubridate)
library(dplyr)

# load data
youtube_data <- read.csv('vlog_data.csv')
youtube_data$date_diff = as.numeric(as.Date(now()) - as.Date(youtube_data$Last_Date))

youtube_data = youtube_data %>% mutate(act = case_when(date_diff < 91 ~ 0,
                                        date_diff > 90 ~ 1))
# fit survival model
fit <- survfit(Surv(Years_Active, act) ~ Category, data = youtube_data)
print(fit)


# Summary of survival curves
summary(fit)

ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_light())# Change ggplot2 theme
