library('ipumsr')
library(here)
library(stargazer)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(gtsummary)
library(sampleSelection)
library(rcompanion)

ddi <- read_ipums_ddi(here("usa_00010.xml"))
data <- read_ipums_micro(ddi)


# Convert interval weeks worked to numeric using case when
# I use the max of the interval
data$weeks <- case_when(
  data$WKSWORK2 == 0 ~ 0,
  data$WKSWORK2 == 1 ~ 13,
  data$WKSWORK2 == 2 ~ 26,
  data$WKSWORK2 == 3 ~ 39,
  data$WKSWORK2 == 4 ~ 47,
  data$WKSWORK2 == 5 ~ 49,
  data$WKSWORK2 == 6 ~ 52,
)

# DROP non wage earners, unemployed
data <- data |> filter(
  !is.na(INCWAGE_CPIU_2010) & !is.na(UHRSWORK) & !is.na(weeks) &
    INCWAGE_CPIU_2010 > 0 & UHRSWORK > 0 & weeks > 0 &
    EMPSTAT == 1
)

# Approximate wage by dividing salary income by approximate hours worked
data$wage <- data$INCWAGE_CPIU_2010 / (data$UHRSWORK * data$weeks)
data$log_wage <- log(data$wage)

# Create 'other' income variable, reflecting all household
data$non_wage_income <- data$HHINCOME_CPIU_2010 - data$INCWAGE_CPIU_2010

# Flags
data$flag_inmigrate <- ifelse(data$MIGRATE1 == 3, 1, 0)

data$flag_female         <- ifelse(data$SEX == 2, 1, 0)
data$flag_ever_married   <- ifelse(data$MARST < 6, 1, 0)
data$flag_veteran        <- ifelse(data$VETSTAT == 2, 1, 0)
data$flag_veteran_spouse <- ifelse(data$VETSTAT_SP == 2, 1, 0)
data$flag_single_mother  <- ifelse(data$flag_female == 1 & data$NCHILD>0 & data$flag_ever_married == 0, 1, 0)

data$flag_employed        <- ifelse(data$EMPSTAT == 1, 1, 0)
data$flag_employed_spouse <- ifelse(data$EMPSTAT_SP == 1, 1, 0)
data$flag_self_employed   <- ifelse(data$CLASSWKR == 1, 1, 0)

data$flag_work_from_home <- ifelse(data$TRANWORK == 80, 1, 0)

data$flag_less_than_high_school <- ifelse(data$EDUCD < 63, 1, 0)
data$flag_more_than_college     <- ifelse(data$EDUCD > 113, 1, 0)
data$flag_spouse_college        <- ifelse(data$EDUC_SP > 6, 1, 0) # Some college
data$flag_power_couple          <- ifelse(data$EDUC > 6 & data$EDUC_SP > 6, 1, 0)

data$flag_work_insurance <- ifelse(data$HINSEMP == 2, 1, 0) # this accounts for spouse's work insurance too

data$flag_from_metro <- ifelse(data$MIGMETRO1 %in% c(2, 3, 4), 1, 0)
data$flag_in_metro   <- ifelse(data$METRO     %in% c(2, 3, 4), 1, 0)

data$flag_asian <- ifelse(data$RACASIAN == 2, 1, 0)
data$flag_white <- ifelse(data$RACWHT   == 2, 1, 0)
data$flag_black <- ifelse(data$RACBLK   == 2, 1, 0)

data$age_18_24   <- ifelse(data$AGE >= 18 & data$AGE <= 24, 1, 0)
data$age_25_34   <- ifelse(data$AGE >= 25 & data$AGE <= 34, 1, 0)
data$age_35_44   <- ifelse(data$AGE >= 35 & data$AGE <= 44, 1, 0)
data$age_45_plus <- ifelse(data$AGE >= 45, 1, 0)

# Replace NAs for some variables
data$flag_veteran_spouse[is.na(data$flag_veteran_spouse)] <- 0
data$flag_spouse_college[is.na(data$flag_spouse_college)] <- 0
data$flag_power_couple[is.na(data$flag_power_couple)] <- 0
data$flag_employed_spouse[is.na(data$flag_employed_spouse)] <- 0

data$age_squared = data$AGE^2

# Show summary statistics of all variables in the "data" table
summary(data)

## Data cleaning
data <- subset(data, 
			HHINCOME_CPIU_2010 >= 0 &
			data$MIGRATE1 != 4 & data$MIGPLAC1 <= 56 & # remove those who lived abroad last year
   (UHRSWORK == 0 | is.na(UHRSWORK) | (UHRSWORK >= 10 & UHRSWORK <= 80)) & # Filter out strange hour jobs
   	HHINCOME_CPIU_2010 < 200000 & INCWAGE_CPIU_2010 < 200000 &
   ((wage >= 5 & wage <= 100)) &
     flag_self_employed == 0 # remove self-employed
   )

# Aggregate the data for summary
data_agg <- data |>
  group_by(flag_inmigrate) |>
  summarize(
    count                     = n(),
    represented_population    = sum(PERWT),
    avg_wage                  = weighted.mean(wage, PERWT, na.rm = TRUE), 
    avg_real_hh_income        = weighted.mean(HHINCOME_CPIU_2010, PERWT), 
    avg_real_work_income      = weighted.mean(INCWAGE_CPIU_2010, PERWT), 
    avg_children              = weighted.mean(NCHILD, PERWT), 
    avg_hrs_per_wk            = weighted.mean(UHRSWORK, PERWT),
    avg_age                   = weighted.mean(AGE, PERWT),  
    pc_asian                  = 100*weighted.mean(flag_asian          , PERWT),       
    pc_white                  = 100*weighted.mean(flag_white          , PERWT),
    pc_black                  = 100*weighted.mean(flag_black          , PERWT),
    pc_female                 = 100*weighted.mean(flag_female         , PERWT),
    pc_veteran                = 100*weighted.mean(flag_veteran        , PERWT),
    pc_less_than_highschool   = 100*weighted.mean(flag_less_than_high_school, PERWT),
    pc_more_than_college      = 100*weighted.mean(flag_more_than_college, PERWT),
    pc_employed_spouse        = 100*weighted.mean(flag_employed_spouse, PERWT),  
    pc_power_couple           = 100*weighted.mean(flag_power_couple   , PERWT),  
  )

# Aggregate the data by reg variables
data_reg <- data |>
  group_by(
    flag_inmigrate, age_18_24, age_25_34, age_35_44, age_45_plus
    , flag_employed, flag_female, flag_ever_married, flag_veteran, flag_single_mother
    , flag_asian, flag_white, flag_black, flag_less_than_high_school, flag_more_than_college
    , flag_work_from_home, flag_work_insurance
    , flag_employed_spouse, flag_spouse_college, flag_veteran_spouse
    , flag_from_metro, flag_in_metro
    , YEAR, STATEFIP
    ) |>
  summarize(
    weight = sum(PERWT),
    wage                  = weighted.mean(wage, PERWT, na.rm = TRUE), 
    real_hh_income        = weighted.mean(HHINCOME_CPIU_2010, PERWT), 
    real_work_income      = weighted.mean(INCWAGE_CPIU_2010, PERWT), 
    children              = weighted.mean(NCHILD, PERWT), 
    children_under_5      = weighted.mean(NCHLT5, PERWT), 
    hrs_per_wk            = weighted.mean(UHRSWORK, PERWT),
    age                   = weighted.mean(AGE, PERWT),  
  ) 

data_reg$log_wage = log(data_reg$wage)
hist(data$log_wage)

# use ggplot to make a pretty histogram of log_wage
ggplot(data_reg, aes(x = log_wage)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log Wage", x = "Log Wage", y = "Frequency") +
  theme_minimal()
ggsave(here("histogram_log_wage.png"), width = 8, height = 6)

uncorrected <- lm(log_wage ~ 
      flag_female*(age_18_24 + age_25_34 + age_35_44 + children + children_under_5 
                   + flag_ever_married + flag_employed_spouse)
    + flag_veteran + flag_single_mother
    + flag_asian + flag_white + flag_black # race
    + flag_less_than_high_school + flag_more_than_college # education
    + flag_spouse_college + flag_veteran_spouse # spouse characteristics
    + factor(YEAR)
    , data = data_reg[data_reg$flag_inmigrate == 1,]
    , weights = data_reg[data_reg$flag_inmigrate == 1,]$weight)

heckit_avg <- selection(
  flag_inmigrate ~ 
    age_18_24 + age_25_34 + age_35_44 # age
  + children + children_under_5 # age, children
  + flag_female + flag_ever_married + flag_veteran + flag_single_mother
  + flag_asian + flag_white + flag_black # race
  + flag_less_than_high_school + flag_more_than_college # education
  + flag_work_from_home + flag_work_insurance # job characteristics
  + flag_employed_spouse + flag_spouse_college + flag_veteran_spouse # spouse characteristics
  + flag_in_metro
  + factor(YEAR) + factor(STATEFIP) # year and state fixed effects
  , log_wage ~ 
    flag_female*(age_18_24 + age_25_34 + age_35_44 + children + children_under_5 
                 + flag_ever_married + flag_employed_spouse)
  + flag_veteran + flag_single_mother
  + flag_asian + flag_white + flag_black # race
  + flag_less_than_high_school + flag_more_than_college # education
  + flag_spouse_college + flag_veteran_spouse # spouse characteristics
  + factor(YEAR)
  , data = data_reg
  , method = "2step"
  , weights = data_reg$weight)

# Run model on pre-COVID only
heckit_pre <- selection(
  flag_inmigrate ~ 
    age_18_24 + age_25_34 + age_35_44 # age
  + children + children_under_5 # age, children
  + flag_female + flag_ever_married + flag_veteran + flag_single_mother
  + flag_asian + flag_white + flag_black # race
  + flag_less_than_high_school + flag_more_than_college # education
  + flag_work_from_home + flag_work_insurance # job characteristics
  + flag_employed_spouse + flag_spouse_college + flag_veteran_spouse # spouse characteristics
  + flag_in_metro
  + factor(YEAR) + factor(STATEFIP) # year and state fixed effects
  , log_wage ~ 
    flag_female*(age_18_24 + age_25_34 + age_35_44 + children + children_under_5 
                 + flag_ever_married + flag_employed_spouse)
  + flag_veteran + flag_single_mother
  + flag_asian + flag_white + flag_black # race
  + flag_less_than_high_school + flag_more_than_college # education
  + flag_spouse_college + flag_veteran_spouse # spouse characteristics
  + factor(YEAR)
  , data = data_reg[data_reg$YEAR < 2020,]
  , method = "2step"
  , weights = data_reg[data_reg$YEAR < 2020,]$weight)

# Run model on post-COVID only
heckit_post <- selection(
  flag_inmigrate ~ 
    age_18_24 + age_25_34 + age_35_44 # age
  + children + children_under_5 # age, children
  + flag_female + flag_ever_married + flag_veteran + flag_single_mother
  + flag_asian + flag_white + flag_black # race
  + flag_less_than_high_school + flag_more_than_college # education
  + flag_work_from_home + flag_work_insurance # job characteristics
  + flag_employed_spouse + flag_spouse_college + flag_veteran_spouse # spouse characteristics
  + flag_in_metro
  + factor(YEAR) + factor(STATEFIP) # year and state fixed effects
  , log_wage ~ 
    flag_female*(age_18_24 + age_25_34 + age_35_44 + children + children_under_5 
                 + flag_ever_married + flag_employed_spouse)
  + flag_veteran + flag_single_mother
  + flag_asian + flag_white + flag_black # race
  + flag_less_than_high_school + flag_more_than_college # education
  + flag_spouse_college + flag_veteran_spouse # spouse characteristics
  + factor(YEAR)
  , data = data_reg[data_reg$YEAR >= 2020,]
  , method = "2step"
  , weights = data_reg[data_reg$YEAR >= 2020,]$weight)

stargazer(uncorrected, heckit_avg, heckit_pre, heckit_post
          , type="latex"
          , dep.var.caption = ""
          , report = "vc*"
          , align=TRUE, no.space=TRUE
          , digits=2
          , column.labels = c("Uncorrected", "Corrected", "Pre-COVID", "Post-COVID")
          , dep.var.labels = "Dependent Variable: Log(Wage)"
          , title=""
          , model.numbers=FALSE
          , omit="YEAR"
          , star.cutoffs = c(0.05, 0.01, 0.005)
          , covariate.labels=c(
            "Female","Age 18-24", "Age 25-34", "Age 35-44",
            "# Children","# Children Under 5 Yrs","Ever Married",
            "Employed Spouse", "Veteran", "Single Mother", 
            "Asian", "White", "Black", "Less than High School", "Post-College Education",
            "College-Educated Spouse", "Veteran Spouse",
            "Female X Age 18-24", "Female X Age 25-34", "Female X Age 35-44",
            "Female X # Children","Female X # Children Under 5 Yrs","Female X Ever Married","Female X Employed Spouse"
          )
          , add.lines=list(
             c('Year Fixed effects', 'Yes', 'Yes', 'Yes', 'Yes')
            ,c('State Fixed effects', 'No', 'No', 'No', 'No')
            )
          , omit.stat = c("f", "rsq", "ser")
          , notes.align = "l"
          , model.names = FALSE
          , out = here("wage_reg_table.tex")
      )

stargazer(uncorrected, heckit_avg, heckit_pre, heckit_post
          , type="text"
          , dep.var.caption = ""
          , report = "vc*"
          , align=TRUE, no.space=TRUE
          , digits=2
          , column.labels = c("Uncorrected", "Corrected", "Pre-COVID", "Post-COVID")
          , dep.var.labels = "Dependent Variable: Log(Wage)"
          , title=""
          , model.numbers=FALSE
          , omit="YEAR"
          , star.cutoffs = c(0.05, 0.01, 0.005)
          , covariate.labels=c(
            "Female","Age 18-24", "Age 25-34", "Age 35-44",
            "# Children","# Children Under 5 Yrs","Ever Married",
            "Employed Spouse", "Veteran", "Single Mother", 
            "Asian", "White", "Black", "Less than High School", "Post-College Education",
            "College-Educated Spouse", "Veteran Spouse",
            "Female X Age 18-24", "Female X Age 25-34", "Female X Age 35-44",
            "Female X # Children","Female X # Children Under 5 Yrs","Female X Ever Married","Female X Employed Spouse"
          )
          , add.lines=list(
            c('Year Fixed effects', 'Yes', 'Yes', 'Yes', 'Yes')
            ,c('State Fixed effects', 'No', 'No', 'No', 'No')
          )
          , omit.stat = c("f", "rsq", "ser")
          , notes.align = "l"
          , model.names = FALSE
)

stargazer(heckit_avg, heckit_pre, heckit_post
          , type="latex"
          , dep.var.caption = ""
          , report = "vc*"
          , align=TRUE, no.space=TRUE
          , digits=2
          , column.labels = c("Full Sample", "Pre-COVID", "Post-COVID")
          , dep.var.labels = "Dependent Variable: In-migrant Status"
          , title=""
          , model.numbers=FALSE
          , omit="YEAR|STATEFIP"
          , star.cutoffs = c(0.05, 0.01, 0.005)
          , selection.equation=TRUE
          , covariate.labels=c(
            "Age 18-24", "Age 25-34", "Age 35-44",
            "# Children","# Children Under 5 Yrs","Female","Ever Married",
            "Veteran", "Single Mother", 
            "Asian", "White", "Black", "Less than High School", "Post-College Education",
            "Works from Home", "Health Insurance through Work",
            "Employed Spouse","College-Educated Spouse", "Veteran Spouse",
            "Lives in Metro. Area"
          )
          , add.lines=list(
            c('Year Fixed effects', 'Yes', 'Yes', 'Yes')
            ,c('State Fixed effects', 'Yes', 'Yes', 'Yes')
          )
          , omit.stat = c("f", "rsq", "ser")
          , notes.align = "l"
          , out = here("selection_reg_table.tex")
)

presentable_table <- data_agg %>%
  pivot_longer(cols = -flag_inmigrate, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = flag_inmigrate, values_from = Value, 
              names_prefix = "In-migrant Status = ") %>%
  # Rename the statistics to be more readable
  mutate(Statistic = case_when(
    Statistic == "sum_weight" ~ "Sum of weights",
    Statistic == "avg_wage" ~ "Average wage",
    Statistic == "avg_age" ~ "Average age",
    Statistic == "avg_real_hh_income" ~ "Average real household income",
    Statistic == "avg_real_work_income" ~ "Average real wage income",
    Statistic == "avg_children" ~ "Average # of children",
    Statistic == "avg_hrs_per_wk" ~ "Average hours worked per week",
    Statistic == "pc_power_couple" ~ "Percentage of power couples",
    Statistic == "pc_more_than_college" ~ "Percentage with post-college education",
    Statistic == "pc_employed_spouse" ~ "Percentage with employed spouse",
    Statistic == "pc_asian" ~ "Percentage Asian",
    Statistic == "pc_white" ~ "Percentage White",
    Statistic == "pc_veteran" ~ "Percentage veteran",
    Statistic == "pc_black" ~ "Percentage Black",
    Statistic == "pc_female" ~ "Percentage female",
    Statistic == "pc_less_than_highschool" ~ "Percentage with less than high school education",
    TRUE ~ Statistic
  ))

# Output as a enhanced LaTeX table
latex_table <- presentable_table %>%
  kable(format = "latex", 
        digits = 2,
        booktabs = TRUE, 
        caption = "Summary Statistics by In-migrant Status",
        col.names = c("Statistic", "Stayers", "Movers")) %>%
  add_header_above(c(" " = 1, "In-migrant Status" = 2))

# Save the table to a .tex file
save_kable(latex_table, file = here("summary_statistics_table.tex"))
