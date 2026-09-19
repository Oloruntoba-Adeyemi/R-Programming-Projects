
# Packages

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(VIM)

# Import Student.CSV file

student <- read_csv("student.csv", show_col_types = FALSE)
student_original <- student

# student_original <- student saves an unchanged copy, 
# so you can compare it with the cleaned dataset later.

student_no_index <- student %>% select(-any_of(c("...1", "X", "index")))

# This removes an unwanted index column if the CSV has one.  
# %>% means take the result from the left and 
# use it in the function on the right.

# Check data types and missing values.

View(student)
str(student)
colSums(is.na(student))

# Clean column names and text.

names(student) <- names(student) %>% str_to_lower() %>% 
  str_replace_all(" ", "_")

# This converts column names to lowercase 
# and replaces spaces with underscores.

student$country[is.na(student$country)] <- "United Kingdom"

# This replaces missing country values with "United Kingdom".

student$sname <- str_to_lower(student$sname)

# This changes names in the sname column to lowercase.

# Convert marks, age, and dates

student$marks <- parse_number(as.character(student$marks))
student$age <- parse_number(as.character(student$age))

# These convert marks and age into numeric values.

student$enrollment_date <- parse_date_time(
  as.character(student$enrollment_date),
  orders = c("dmy", "mdy", "ymd")
) %>% as.Date()

# This converts the enrollment-date column into a real R date. 
# It accepts common formats such as 12/08/2021, 08/12/2021, or 2021-08-12.

# Find outliers

find_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  x[x < q1 - 1.5 * iqr_value | x > q3 + 1.5 * iqr_value]
}

# This creates a reusable function where q1 is the lower quartile,
# q3 is the upper quartile and iqr_value is the spread between them.
# Values below Q1 - 1.5 × IQR or above Q3 + 1.5 × IQR are outliers.

find_outliers(student$marks)
find_outliers(student$age)

# Make sure the columns are numeric

student$marks <- as.numeric(student$marks)
student$age <- as.numeric(student$age)

# Replace outliers and missing values with the mean

replace_outliers_with_mean <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  x[x < (q1 - 1.5 * iqr_value) | x > (q3 + 1.5 * iqr_value)] <- NA
  x[is.na(x)] <- mean(x, na.rm = TRUE)
   return(x)
}

names(student) <- tolower(trimws(names(student)))
names(student)

student$marks <- replace_outliers_with_mean(student$marks)
student$age <- replace_outliers_with_mean(student$age)
student$gender <- str_to_lower(str_trim(student$gender))

# This removes extra spaces and converts entries to lowercase.

student$gender[student$gender %in% c("m", "male")] <- "male"
student$gender[student$gender %in% c("f", "female")] <- "female"
student$gender[!student$gender %in% c("male", "female")] <- NA

# Remove incomplete rows

student_modified <- drop_na(student)

# This deletes any row that still contains a missing value.

# Filter rows

students_12_aug_2021 <- filter(
  student_modified,
  enrollment_date == as.Date("2021-08-12")
)

# This selects students enrolled on 12 August 2021.

students_below_50 <- filter(student_modified, marks < 50)

# This selects students whose marks are below 50.

students_starting_a <- filter(student_modified,
  str_starts(sname, regex("a", ignore_case = TRUE))
)

# This selects names beginning with A,
# ignore_case = TRUE means both A and a are accepted.

# Status and grade loop

student_modified <- student_modified %>%
  mutate(status = ifelse(enrollment_date > as.Date("2021-11-01"), 
      "Needs Validation", "Validated"),
    grades = case_when(
      marks > 70  ~ "A",
      marks >= 60 ~ "B",
      marks >= 50 ~ "C",
      TRUE        ~ "F"
    )
  )

table(student_modified$grades)

# Create a chart between age and marks.

  ggplot(student_modified, aes(x = age, y = marks)) +
    geom_point(size = 3, colour = "blue") +
    labs(title = "Relationship between Marks and Age", x = "Age", y = "Marks") +
    theme_minimal()

# This makes a scatter plot:
# x-axis: age
# y-axis: marks
# one dot: one student 
  
  write_csv(student_modified, "student_modified.csv")
