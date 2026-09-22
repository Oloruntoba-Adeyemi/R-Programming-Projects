
# Packages

library(readxl)
library(dplyr)
library(ggplot2)

# Importing Excel workbook.

loans <- read_excel("PDA - Zappy Loan Data.xlsx", sheet = "CW2")

# Viewing the data

head(loans)
str(loans)
summary(loans)

# read_excel() imports the worksheet. head() displays the first records, 
# str() shows column types, and summary() gives descriptive statistics.

# Checking the  data quality

# Number of applications

nrow(loans)

# Numbrs of unique IDs

n_distinct(loans$Loan_ID)

# Duplicated IDs

sum(duplicated(loans$Loan_ID))

# Missing values in each column

colSums(is.na(loans))

# This verifies that the data is suitable for analysis. 
# In this dataset there are 247 applications, no duplicate loan IDs, and no missing values.

# Creating useful analysis variables

loans <- loans %>%
  mutate(
    approved = ifelse(Loan_Status == "Y", 1, 0),
    total_income = ApplicantIncome + CoapplicantIncome,
    loan_to_income = LoanAmount / pmax(total_income, 1),
    log_total_income = log1p(total_income),
    log_loan_amount = log1p(LoanAmount)
  )

# approved changes Y/N into 1/0, which a logistic model can use.
# total_income combines applicant and co-applicant income.
# loan_to_income compares the requested loan with available income.
# pmax(..., 1) prevents division by zero.
# Log transformations reduce the effect of unusually 
# large income or loan figures.

# Overall appoval summary

# Overall approval summary
loans %>%
  summarise(
    applications = n(),
    approvals = sum(approved),
    rejections = sum(approved == 0),
    approval_rate = mean(approved),
    median_income = median(total_income),
    median_loan_amount = median(LoanAmount)
  )

# This produces the headline result: 247 applications, 167 approvals, 
# and a 67.6% approval rate.

# Approval rate by credit-history code

approval_by_credit <- loans %>%
  group_by(Credit_History) %>%
  summarise(applications = n(),
            approval_rate = mean(approved),
            .groups = "drop")

approval_by_credit

# This compares historical approval outcomes by Credit_History. 
# In the supplied data, code 1 has an 81.2% approval rate, 
# compared with 26.2% for code 0.

# Chart: approval rate by credit history

ggplot(approval_by_credit,
       aes(x = factor(Credit_History), y = approval_rate,
           fill = factor(Credit_History))) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100), "%"),
    limits = c(0, 1)
  ) +
  labs(
    title = "Historical approval rate by credit history",
    x = "Credit-history code",
    y = "Approval rate"
  ) +
  theme_minimal()

# This creates a bar chart for management. geom_col() draws the bars 
# and scale_y_continuous() shows the outcome as percentages.

# Creating an 80% training set and 20% test set

set.seed(20260922)

train_index <- unlist(
  tapply(
    seq_len(nrow(loans)),
    loans$approved,
    function(i) sample(i, floor(0.80 * length(i)))
  )
)

train <- loans[train_index, ]
test <- loans[-train_index, ]

# The model learns from 80% of the records and is checked against 
# the remaining 20%. set.seed() makes the split reproducible. 
# The split is stratified, 
# meaning it preserves the mix of approved and rejected cases.

# Fitting a logistic regression model

model <- glm(
  approved ~ Graduate + Self_Employed + Credit_History +
    log_total_income + log_loan_amount +
    Loan_Amount_Term + loan_to_income,
  data = train,
  family = binomial()
)

summary(model)

#glm(..., family = binomial()) creates a logistic regression model 
# that estimates the probability of approval.
# The model intentionally excludes Gender, Married, 
# Dependents and Property_Area. These attributes should not be used 
# to automate credit decisions because they are sensitive attributes 
# or possible proxies for protected characteristics. 

# Estimating  an approval probability for each test application

test$approval_probability <- predict(
  model,
  newdata = test,
  type = "response"
)

head(test[, c("Loan_ID", "Loan_Status", "approval_probability")])

# predict(..., type = "response") returns probabilities between 0 and 1. 
# For example, 0.85 means the model estimates an 85% likelihood of 
# a historical approval decision.

# Creating a human-review queue and not an automatic decision

test <- test %>%
  mutate(
    triage = case_when(
      approval_probability >= 0.80 ~ "Expedite human review",
      approval_probability <= 0.20 ~ "Enhanced human review",
      TRUE ~ "Standard human review"
    )
  )

test %>%
  select(Loan_ID, approval_probability, triage) %>%
  head()

# This is the recommended operational use. 
# The system does not approve or reject loans. Instead, 
# it helps staff decide which cases need fast, enhanced, or normal review.

# Fairness monitoring only - these fields are not model inputs

fairness_monitor <- bind_rows(
  loans %>%
    group_by(group = Gender) %>%
    summarise(
      attribute = "Gender",
      applications = n(),
      historical_approval_rate = mean(approved),
      .groups = "drop"
    ),
  
  loans %>%
    group_by(group = Married) %>%
    summarise(
      attribute = "Married",
      applications = n(),
      historical_approval_rate = mean(approved),
      .groups = "drop"
    ),
  
  loans %>%
    group_by(group = Property_Area) %>%
    summarise(
      attribute = "Property_Area",
      applications = n(),
      historical_approval_rate = mean(approved),
      .groups = "drop"
    )
)

fairness_monitor

# This checks whether historical approval rates differ across groups. 
# It is a compliance and monitoring control, not a basis for a lending decision.

# Important limitation: Loan_Status reflects previous staff approvals, 
# rather than whether a customer repaid their loan. Therefore, 
# this model can only replicate and prioritise historical review patterns. 
# Before using predictive automation for lending, 
# ZFS needs repayment/default outcome data, compliance review, 
# documented override procedures, audit logs, access controls, 
# and ongoing fairness and performance monitoring.