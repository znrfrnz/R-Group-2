#BSIT 2-1 | GROUP 2 | ASSIGNMENT #2

#----Load necessary libraries-----
library(readxl) # to read the excel file
library(dplyr) # for data manipulation
library(ggplot2) # to visualize the data

#-----DATA-----
German_Credit_Risk <- read_excel("c:/Users/benid/Downloads/German Credit Risk.xlsx") #change this to the local file path mo # nolint
View(German_Credit_Risk)
DATA <- German_Credit_Risk
attach(DATA)

#-----Task 1 – Custom R Function-----
summarise_credit_segment <- function(df, segment_name = "All Applicants") {
  # Check if column missing then stop
  required_cols <- c("Risk", "Credit_amount", "Duration")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", "))) # nolint: line_length_linter.
  }
  if (nrow(df) == 0) stop("Data frame is empty – nothing to summarise.")

  # Compute metrics
  n_total <- nrow(df)
  n_bad <- sum(df$Risk == "bad", na.rm = TRUE)
  pct_bad <- round(n_bad / n_total * 100, 1)
  avg_amount <- round(mean(df$Credit_amount, na.rm = TRUE), 2)
  median_amount <- round(median(df$Credit_amount, na.rm = TRUE), 2)
  avg_duration <- round(mean(df$Duration, na.rm = TRUE), 1)

  # Summary
  cat("\n  Credit Risk Summary –", segment_name, "\n")
  cat(sprintf("  Total applicants  : %d\n", n_total))
  cat(sprintf("  Bad-risk count    : %d\n", n_bad))
  cat(sprintf("  Bad-risk rate     : %.1f%%\n", pct_bad))
  cat(sprintf("  Avg loan amount   : %.2f\n", avg_amount))
  cat(sprintf("  Median loan amount: %.2f\n", median_amount))
  cat(sprintf("  Avg loan duration : %.1f months\n", avg_duration))


  # Named list
  invisible(list(
    segment       = segment_name,
    n_total       = n_total,
    n_bad         = n_bad,
    pct_bad       = pct_bad,
    avg_amount    = avg_amount,
    median_amount = median_amount,
    avg_duration  = avg_duration
  ))
}

##-----Demo-----
# show all applicants
all_metrics <- summarise_credit_segment(DATA, "All Applicants")

# female
female_metrics <- summarise_credit_segment(
  df           = DATA[DATA$Sex == "female", ],
  segment_name = "Female Applicants"
)

# male
male_metrics <- summarise_credit_segment(
  df           = DATA[DATA$Sex == "male", ],
  segment_name = "Male Applicants"
)

# no checking account
no_checking_metrics <- summarise_credit_segment(
  df           = DATA[is.na(DATA$Checking_account) |
                        DATA$Checking_account == "NA", ],
  segment_name = "No Checking Account"
)

cat("\n")
# check metrics
cat("Overall bad-risk rate:", all_metrics$pct_bad, "%\n")
cat("Female bad-risk rate :", female_metrics$pct_bad, "%\n")
cat("Male bad-risk rate   :", male_metrics$pct_bad, "%\n")



#-----Task 2 – Data Structures-----

# 1. A vector
# Extracting the 'Credit_amount' column to create a numeric vector
credit_vector <- DATA$Credit_amount
cat("1. Vector created: 'credit_vector' of length", length(credit_vector), "\n")

# 2. An unordered factor
# The 'Housing' column has categories (rent, own, free) with no specific rank
housing_unordered <- factor(DATA$Housing)
cat("2. Unordered Factor created with levels:", paste(levels(housing_unordered), collapse = ", "), "\n")

# 3. An ordered factor
# The 'Saving_accounts' column has a natural hierarchy, so we define the levels explicitly
saving_levels <- c("little", "moderate", "quite rich", "rich")
saving_ordered <- factor(DATA$Saving_accounts, 
                         levels = saving_levels, 
                         ordered = TRUE)
cat("3. Ordered Factor created with levels:", paste(levels(saving_ordered), collapse = " < "), "\n\n")

# 4. A table
# Creating a 2D frequency table (cross-tabulation) comparing Housing status against Credit Risk
housing_risk_table <- table(Housing = DATA$Housing, Risk = DATA$Risk)
cat("4. Table created (Housing vs. Risk):\n")
print(housing_risk_table)
cat("\n")

# 5. A data frame
# Creating a custom, smaller data frame using a subset of the first 5 records
mini_credit_df <- data.frame(
  Applicant_Age = DATA$Age[1:5],
  Loan_Duration = DATA$Duration[1:5],
  Loan_Amount = DATA$Credit_amount[1:5],
  Risk_Status = DATA$Risk[1:5]
)
cat("5. Data Frame created (Preview of first 5 rows):\n")
print(mini_credit_df)


#-----Task 3 – Data Manipulation with dplyr----
# Filter: Find high-risk applicants with large credit amounts (> 5000)
high_risk_large_loans <- DATA %>%
  filter(Risk == "bad", Credit_amount > 5000)

cat("1. Filtered: Found", nrow(high_risk_large_loans), "bad-risk applicants with loans > 5000.\n")

# Select: Keep only identifying and financial columns
financial_profile <- DATA %>%
  select(Age, Sex, Job, Credit_amount, Risk)

cat("2. Selected: Created a profile with", ncol(financial_profile), "specific columns.\n")

# Mutate: Create a new column for 'Loan_to_Age_Ratio' and a flag for 'Senior' applicants (Age > 60)
DATA <- DATA %>%
  mutate(
    Loan_to_Age_Ratio = Credit_amount / Age,
    Is_Senior = ifelse(Age > 60, "Yes", "No")
  )

cat("3. Mutated: Added 'Loan_to_Age_Ratio' and 'Is_Senior' flag.\n")

# Arrange: Sort data by Credit Amount (Descending) to see highest loans first
sorted_data <- DATA %>%
  arrange(desc(Credit_amount))

cat("4. Arranged: Highest loan amount is", sorted_data$Credit_amount[1], "\n")

# Group & Summarize: Analyze average risk and loan amount by Housing status
housing_analysis <- DATA %>%
  group_by(Housing) %>%
  summarize(
    Avg_Loan = mean(Credit_amount),
    Median_Age = median(Age),
    Total_Count = n()
  )

cat("5. Summarized Analysis by Housing:\n")
print(housing_analysis)


#-----Task 4 – Base R Visualizations-----
# Create meaningful visualizations of the German Credit Dataset using Base R graphics only
# Create meaningful visualizations of the German Credit Dataset using Base R graphics only

### Average Age by Savings Category
avg_age <- tapply(DATA$Age,
                  DATA$Saving_accounts,
                  mean)

barplot(avg_age,
        col = "skyblue",
        main = "Average Age by Savings Category",
        xlab = "Savings Category",
        ylab = "Average Age")

#### Age vs Savings
DATA$Saving_accounts <- as.factor(DATA$Saving_accounts)


barplot(sex_counts,
        main="Number of Borrowers by Gender",
        xlab="Gender",
        ylab="Count",
        col=c("pink","skyblue"))

# CREDIT RISK DISTRIBUTION
risk_counts <- table(DATA$Risk)

barplot(risk_counts,
        main="Distribution of Credit Risk",
        xlab="Risk Category",
        ylab="Number of Clients",
        col=c("red","green"))

# AGE VS CREDIT AMOUNT

plot(DATA$Age,
     as.numeric(DATA$Saving_accounts),
     col = ifelse(DATA$Risk == "good", "green", "red"),
     pch = 19,
     xlab = "Age",
     ylab = "Savings Category",
     main = "Age and Savings vs Credit Risk",
     yaxt = "n")

axis(2,
     at = 1:length(levels(DATA$Saving_accounts)),
     labels = levels(DATA$Saving_accounts))

legend("topright",
       legend = c("Good Risk", "Bad Risk"),
       col = c("green","red"),
       pch = 19)

### Gender vs Credit Risk
gender_risk <- table(DATA$Sex,
                     DATA$Risk)

barplot(gender_risk,
        beside=TRUE,
        col=c("skyblue","pink"),
        legend=TRUE,
        main="Credit Risk by Gender",
        xlab="Risk Category",
        ylab="Number of Borrowers")

#-----Task 5 – ggplot2 Visualizations-----
# Create meaningful visualizations of the German Credit Dataset using ggplot2 only
ggplot(DATA, aes(x = Job, y = Credit_amount)) +

  # Layer 1: Points
  geom_jitter(aes(color = Sex), alpha = 0.6) +
  
  # Layer 2: Color of points
  scale_color_manual(values = c("male" = "skyblue1", "female" = "hotpink1")) +

  # Layer 3: Smooth regression line
  geom_smooth(method = "lm", se = FALSE, color = "violetred2") +

  # Layer 4: Facet by purpose
  facet_wrap(~Purpose) +

  # Layer 5: Customize theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, face = "italic", color = "darkgrey")) +

  # Layer 6: Titles and labels
  labs(title = "Job Classification vs. Credit Amount",
       subtitle = "Colored by Sex and Faceted by Purpose",
       x = "Job Classification",
       y = "Credit Amount",
       caption = "Note: 0 = Unskilled & Non-resident | 1 = Unskilled & Resident | 2 = Skilled | 3 = Highly Skilled")
