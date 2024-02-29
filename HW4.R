---
  Title: HW 4
  Author: Yetsy Santa
  Date: Feb 15, 2024
  Output: 
---

brfss22[1:10,1:6]
attach(brfss22)
summary(brfss22)

library(ggplot2)
library(tidyverse)

load("BRFSS2022_rev.RData")
brfss22$Age_midpt <- fct_recode(brfss22$X_AGEG5YR, "21" = "Age 18 to 24",
                                "27" = "Age 25 to 29", "32" = "Age 30 to 34",
                                "37" = "Age 35 to 39", "42" = "Age 40 to 44",
                                "47" = "Age 45 to 49", "52" = "Age 50 to 54",
                                "57" = "Age 55 to 59", "62" = "Age 60 to 64",
                                "67" = "Age 65 to 69", "72" = "Age 70 to 74",
                                "77" = "Age 75 to 79", "82" = "Age 80 or older",
                                NULL = "Dont know/Refused/Missing")
brfss22$Age_midpt <- as.numeric(levels(brfss22$Age_midpt))[brfss22$Age_midpt]

select_tristate <- (brfss22$X_STATE == "New York") | (brfss22$X_STATE == "New Jersey") | (brfss22$X_STATE == "Connecticut")
brfss_tristate <- subset(brfss22,select_tristate)

p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = X_BMI5))
p_tri + geom_smooth()

p_tri + geom_jitter(width = 2.5, height = NULL, alpha = 0.05)

p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = X_BMI5,
                              color = X_STATE,
                              fill = X_STATE))
p_tri + geom_smooth()


ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                  as.numeric(is.na(brfss22$ACEDRINK)) +
                  as.numeric(is.na(brfss22$ACEDRUGS)) +
                  as.numeric(is.na(brfss22$ACEPRISN)) +
                  as.numeric(is.na(brfss22$ACEDIVRC)) +
                  as.numeric(is.na(brfss22$ACEPUNCH)) +
                  as.numeric(is.na(brfss22$ACEHURT1)) +
                  as.numeric(is.na(brfss22$ACESWEAR)) +
                  as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) # with zero missing values for any of ACE questions
brfss_ACE <- subset(brfss22, select_ACE)


xtabs(~ brfss_ACE$ACEDRINK + brfss_ACE$ACEDRUGS)

ftable(xtabs(~ brfss_ACE$ACEDRINK + brfss_ACE$ACEDRUGS))

brfss_ACE$ACEDRINK_recode <- fct_recode(brfss_ACE$ACEDRINK, 
                                        " " = "Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic",
                                        " " = "No", 
                                        " " = "dont know not sure",
                                        " " = "refused"
)

# might want to set some values as missing,
#                                 NULL = "dont know not sure"

ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                  as.numeric(is.na(brfss22$ACEDRINK)) +
                  as.numeric(is.na(brfss22$ACEDRUGS)) +
                  as.numeric(is.na(brfss22$ACEPRISN)) +
                  as.numeric(is.na(brfss22$ACEDIVRC)) +
                  as.numeric(is.na(brfss22$ACEPUNCH)) +
                  as.numeric(is.na(brfss22$ACEHURT1)) +
                  as.numeric(is.na(brfss22$ACESWEAR)) +
                  as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) # with zero missing values for any of ACE questions

summary(ACEdidntask)

quantile(ACEdidntask, probs = c(0.01,0.05,0.1,0.15,0.2))

summary(brfss22)
summary(brfss_ACE)

select_tristate <- (brfss22$X_STATE == "New York") | (brfss22$X_STATE == "Washington") | (brfss22$X_STATE == "Ohio")
brfss_tristate <- subset(brfss22,select_tristate)

p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = MENTHLTH))
p_tri + geom_smooth()

p_tri + geom_jitter(width = 2.5, height = NULL, alpha = 0.05)

p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = MENTHLTH,
                              color = X_STATE,
                              fill = X_STATE))
p_tri + geom_smooth()



ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                  as.numeric(is.na(brfss22$ACEDRINK)) +
                  as.numeric(is.na(brfss22$ACEDRUGS)) +
                  as.numeric(is.na(brfss22$ACEPRISN)) +
                  as.numeric(is.na(brfss22$ACEDIVRC)) +
                  as.numeric(is.na(brfss22$ACEPUNCH)) +
                  as.numeric(is.na(brfss22$ACEHURT1)) +
                  as.numeric(is.na(brfss22$ACESWEAR)) +
                  as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) # with zero missing values for any of ACE questions
brfss_ACE <- subset(brfss22, select_ACE)


xtabs(~ brfss_ACE$ACEDEPRS + brfss_ACE$ACEHURT1)

ftable(xtabs(~ brfss_ACE$ACEDEPRS + brfss_ACE$ACEHURT1))


















