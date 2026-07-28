###############################################################################
# Membership Growth and Retention Analysis - LIFE Fitness
# MSc Statistical Data Science, University College Dublin
# Hemanth Sai Kumar Vadlapudi (25259069)
#
# This script runs the whole analysis end to end: it reads the raw Master Sheet,
# cleans it, builds the member-level features, defines churn, and then works
# through the four research questions. Run it top to bottom with the Master
# Sheet in the working directory. Each block below says what it does and, where
# the choice isn't obvious, why it's done that way.
###############################################################################


## Packages ------------------------------------------------------------------
# Everything here is standard. tidyverse covers the data wrangling and plots;
# lubridate handles the dates; survival/survminer do the Kaplan-Meier work;
# pROC gives us the ROC curve and AUC for the logistic model.
library(readxl)      # read the .xlsx sheets
library(dplyr)       # wrangling
library(stringr)     # tidy up the site names
library(lubridate)   # dates
library(ggplot2)     # plots
library(pROC)        # ROC / AUC
library(survival)    # Kaplan-Meier, log-rank
library(survminer)   # survival plots


## 1. Read the data ----------------------------------------------------------
# The Master Sheet has several tabs. For the core analysis we only need the
# member records and the visit log. The other tabs (exercises, body
# measurements, 1RM) are only needed for the workout-mix feature later.
members <- read_excel("Master_Sheet.xlsx", sheet = "Member Information")
visits  <- read_excel("Master_Sheet.xlsx", sheet = "Visits")


## 2. Fix the dates ----------------------------------------------------------
# The dates come in as plain text, so nothing date-related works until we parse
# them. Doing this first means every later calculation is on real dates.
members <- members %>%
  mutate(
    `Date of birth`    = as_date(`Date of birth`),
    `Last visit`       = as_date(`Last visit`),
    `Last interaction` = as_date(`Last interaction`),
    `Joined on`        = as_date(`Joined on`)
  )

# Visits carry a full timestamp. Anything that won't parse gets dropped, since a
# visit with no usable time can't be placed on the timeline anyway.
visits <- visits %>%
  mutate(CheckInDate = as_datetime(CheckInDate)) %>%
  filter(!is.na(CheckInDate))


## 3. Remove duplicate members -----------------------------------------------
# There are 156 duplicated member IDs in the sheet. Owen told us the newest
# record is the one to trust, so for each ID we sort newest-first (by last
# interaction, then join date) and keep the top row. This takes us from 2,248
# rows down to 2,092 real members.
members_clean <- members %>%
  arrange(`Cloud Id`, desc(`Last interaction`), desc(`Joined on`)) %>%
  distinct(`Cloud Id`, .keep_all = TRUE)


## 4. Tidy the site names ----------------------------------------------------
# After the Matchbox -> LIFE Fitness rebrand the same gym shows up under a few
# different names. A quick pattern match puts every visit into one of the two
# sites so we can split by location later.
visits_clean <- visits %>%
  mutate(site = case_when(
    str_detect(FacilityName, "Jetland")  ~ "Jetland",
    str_detect(FacilityName, "Corbally") ~ "Corbally",
    TRUE ~ "Other"
  ))


## 5. Build the first-month engagement feature -------------------------------
# This is the main behavioural feature. It counts how many times someone came in
# during their first 30 days. Because it's fixed early in the membership, it's
# safe to use as a predictor: it can't be "contaminated" by whatever they do
# later, unlike total visits.

# rename the key so it matches the visits table, then grab the join dates
members_clean <- members_clean %>% rename(CloudId = `Cloud Id`)
join_dates <- members_clean %>% select(CloudId, `Joined on`)

# work out how many days after joining each visit happened, keep the first 30
early_visits <- visits_clean %>%
  inner_join(join_dates, by = "CloudId") %>%
  mutate(days_since_join =
           as.numeric(difftime(CheckInDate, `Joined on`, units = "days"))) %>%
  filter(days_since_join >= 0, days_since_join <= 30)

# count those early visits per member
first_month <- early_visits %>%
  group_by(CloudId) %>%
  summarise(first_month_visits = n(), .groups = "drop")


## 6. Put the member feature table together ----------------------------------
# Join the counts back on. Members who never visited in month one simply don't
# appear in first_month, so we fill their count with 0 (which is the true value,
# not missing data).
member_features <- members_clean %>%
  left_join(first_month, by = "CloudId") %>%
  mutate(first_month_visits = coalesce(first_month_visits, 0))


## 7. Define churn -----------------------------------------------------------
# There's no cancellation date anywhere in the data, so we can't just read off
# who left. Instead we call someone churned if they've been inactive for more
# than 30 days as of the latest date in the data. The 30 days isn't arbitrary -
# it matches how the gym itself defines an "active" member.
REFERENCE_DATE <- max(members_clean$`Last visit`, na.rm = TRUE)
CHURN_DAYS <- 30

member_features <- member_features %>%
  mutate(
    days_inactive = as.numeric(difftime(REFERENCE_DATE, `Last visit`,
                                         units = "days")),
    churned = if_else(days_inactive > CHURN_DAYS, "Yes", "No"),
    # duration = join to last visit. This is the survival time for RQ2.
    membership_duration = as.numeric(difftime(`Last visit`, `Joined on`,
                                              units = "days"))
  )

# Sanity check: does the churn rate move much if we shift the threshold? If it
# barely changes, our definition isn't sitting on a knife-edge. It comes out at
# roughly 77% / 73% / 70% for 30 / 60 / 90 days, so we're fine.
member_features %>%
  filter(!is.na(days_inactive)) %>%
  summarise(
    churn_30 = mean(days_inactive > 30),
    churn_60 = mean(days_inactive > 60),
    churn_90 = mean(days_inactive > 90)
  )


## 8. Build the modelling table ----------------------------------------------
# Add age, turn churn into a 0/1 column, make gender a factor, and drop a few
# impossible ages. That leaves 2,064 members to model on.
model_data <- member_features %>%
  filter(!is.na(churned)) %>%
  mutate(
    age = as.numeric(difftime(REFERENCE_DATE, `Date of birth`,
                              units = "days")) / 365.25,
    churned_binary = if_else(churned == "Yes", 1, 0),
    Gender = as.factor(Gender)
  ) %>%
  filter(age >= 16, age <= 90)


## 9. RQ3 - who churns? (logistic regression) --------------------------------
# Predict churn from age, first-month visits and gender. Note what's NOT in
# here: total visits and the drop-out flag. Both are built from visit behaviour,
# same as churn itself, so putting them in would just be circular and inflate
# the fit without telling us anything real.
churn_model <- glm(
  churned_binary ~ first_month_visits + age + Gender,
  data = model_data, family = binomial
)
summary(churn_model)                                  # coefficients + p-values

# odds ratios with confidence intervals - easier to talk about than log-odds
exp(cbind(OR = coef(churn_model), confint(churn_model)))

# how well does it separate churners from stayers? use AUC, not accuracy -
# with 77% churn, "predict everyone churns" already scores 77%, so accuracy lies
model_data <- model_data %>%
  mutate(pred_prob = predict(churn_model, type = "response"))
roc_obj <- roc(model_data$churned_binary, model_data$pred_prob)
auc(roc_obj)

# confusion matrix at 0.5 - shows the imbalance problem: it flags nearly
# everyone as churned, which is why we use the model for ranking, not labelling
table(Predicted = model_data$pred_prob >= 0.5,
      Actual    = model_data$churned_binary)


## 10. RQ2 - does early engagement mean longer membership? (survival) --------
# Split members by whether they showed up at all in month one, then compare how
# long each group lasts. Survival analysis is right here because it uses *when*
# people churn and copes with members who haven't churned yet.
survival_data <- model_data %>%
  filter(!is.na(membership_duration), membership_duration >= 0) %>%
  mutate(early_engaged = if_else(first_month_visits >= 1,
                                 "Engaged", "Not engaged"),
         event = churned_binary)

km_fit <- survfit(Surv(membership_duration, event) ~ early_engaged,
                  data = survival_data)

# log-rank test: are the two curves genuinely different?
survdiff(Surv(membership_duration, event) ~ early_engaged,
         data = survival_data)

# plot the curves with confidence bands
ggsurvplot(km_fit, data = survival_data, conf.int = TRUE,
           legend.title = "First month", xlab = "Days as member",
           ylab = "Proportion still active")

# NB: the result comes out backwards (engaged look shorter). That's a
# measurement artefact - duration is defined from the last visit, which is
# entangled with the engagement split. Written up honestly rather than spun.


## 11. RQ4 - is there a seasonal effect? (chi-square) ------------------------
# Only Corbally has a full year of data, so the seasonal test uses Corbally
# alone. Pooling both sites would confound season with which gym was recording.
season_of <- function(m) {
  case_when(m %in% 3:5   ~ "Spring",
            m %in% 6:8   ~ "Summer",
            m %in% 9:11  ~ "Autumn",
            TRUE         ~ "Winter")
}

corbally_season <- visits_clean %>%
  filter(site == "Corbally") %>%
  mutate(season = season_of(month(CheckInDate))) %>%
  count(season)

# goodness-of-fit against "same number of visits every season"
chisq.test(corbally_season$n)

# significant, but check the effect size too - the season-to-season spread is
# only about 10%, so it's a real but tiny effect, not the big swing the pooled
# data seemed to show
sd(corbally_season$n) / mean(corbally_season$n)   # coefficient of variation


## 12. RQ1 - joins vs churn over time (Corbally) -----------------------------
# Give each member a home site (wherever they visited most), then for Corbally
# count how many joined each month against how many churned each month.
home_site <- visits_clean %>%
  filter(site %in% c("Corbally", "Jetland")) %>%
  group_by(CloudId) %>%
  summarise(home = names(which.max(table(site))), .groups = "drop")

corbally_members <- member_features %>%
  left_join(home_site, by = "CloudId") %>%
  filter(home == "Corbally")

joins <- corbally_members %>%
  mutate(m = floor_date(`Joined on`, "month")) %>%
  count(m, name = "joins")

churns <- corbally_members %>%
  filter(churned == "Yes") %>%
  mutate(m = floor_date(`Last visit`, "month")) %>%
  count(m, name = "churns")

# line up the two series and look at whether the gap (churn minus joins) grows
monthly <- inner_join(joins, churns, by = "m") %>%
  mutate(net = churns - joins, t = row_number())

# is there a trend in that gap over time?
summary(lm(net ~ t, data = monthly))

# quick plot of the two lines
monthly_long <- monthly %>%
  select(m, joins, churns) %>%
  tidyr::pivot_longer(c(joins, churns), names_to = "type", values_to = "n")

ggplot(monthly_long, aes(m, n, colour = type)) +
  geom_line(linewidth = 1) +
  labs(x = NULL, y = "Members", colour = NULL,
       title = "Corbally: monthly joins vs churn")


###############################################################################
# End. Objects worth keeping: member_features (everyone + churn flag),
# model_data (modelling sample), churn_model (the fitted GLM), km_fit (survival).
###############################################################################
