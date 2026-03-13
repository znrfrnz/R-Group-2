#BSIT 2-1 | GROUP 2 | ASSIGNMENT #2

#----Load necessary libraries-----
library(readxl) # to read the excel file
library(dplyr) # for data manipulation
library(ggplot2) # to visualize the data

#-----DATA-----
German_Credit_Risk <- read_excel("D:/Personal Projects/R/R-Group-2/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk
attach(DATA)

#-----Task 1 – Custom Function-----
summarise_credit_segment <- function(df, segment_name = "All Applicants") {
  # check if column missing then stop
  required_cols <- c("Risk", "Credit_amount", "Duration")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  if (nrow(df) == 0) stop("Data frame is empty – nothing to summarise.")

  # Compute metrics
  n_total <- nrow(df)
  n_bad <- sum(df$Risk == "bad", na.rm = TRUE)
  pct_bad <- round(n_bad / n_total * 100, 1)
  avg_amount <- round(mean(df$Credit_amount, na.rm = TRUE), 2)
  median_amount <- round(median(df$Credit_amount, na.rm = TRUE), 2)
  avg_duration <- round(mean(df$Duration, na.rm = TRUE), 1)

  # sumamary
  cat("\n  Credit Risk Summary –", segment_name, "\n")
  cat(sprintf("  Total applicants  : %d\n", n_total))
  cat(sprintf("  Bad-risk count    : %d\n", n_bad))
  cat(sprintf("  Bad-risk rate     : %.1f%%\n", pct_bad))
  cat(sprintf("  Avg loan amount   : DM %.2f\n", avg_amount))
  cat(sprintf("  Median loan amount: DM %.2f\n", median_amount))
  cat(sprintf("  Avg loan duration : %.1f months\n", avg_duration))


  # named list
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

#Demo
# show all applicants
all_metrics <- summarise_credit_segment(DATA, "All Applicants")

# female
female_metrics <- summarise_credit_segment(
  df           = DATA[DATA$Sex == "female", ],
  segment_name = "Female Applicants"
)

# no checking account
no_checking_metrics <- summarise_credit_segment(
  df           = DATA[is.na(DATA$Checking_account) |
                               DATA$Checking_account == "NA", ],
  segment_name = "No Checking Account"
)

# check metrics
cat("Overall bad-risk rate:", all_metrics$pct_bad, "%\n")
cat("Female bad-risk rate :", female_metrics$pct_bad, "%\n")



#-----Task 2 – Data Structures-----
# Create and assign all required data structures: a vector, an ordered factor, an unordered factor, a table, and a data frame (using the German Credit Dataset where applicable).



#-----Task 3 – Data Manipulation with dplyr----



#-----Task 4 – Base R Visualizations-----
# Create meaningful visualizations of the German Credit Dataset using Base R graphics only



#-----Task 5 – ggplot2 Visualizations-----
# Create meaningful visualizations of the German Credit Dataset using ggplot2 only
ggplot(DATA, aes(x = Job, y = Credit_amount)) +

  # Points
  geom_jitter(aes(color = Sex), alpha = 0.6) +

  # Color of points
  scale_color_manual(values = c("male" = "skyblue1", "female" = "hotpink1")) +

  # Smooth regression line
  geom_smooth(method = "lm", se = FALSE, color = "violetred2") +

  # Facet by purpose
  facet_wrap(~Purpose) +

  # Customize theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, face = "italic", color = "darkgrey")) +

  # Titles and labels
  labs(title = "Job Classification vs. Credit Amount",
       subtitle = "Colored by Sex and Faceted by Purpose",
       x = "Job Classification",
       y = "Credit Amount",
       caption = "Note: 0 = Unskilled & Non-resident | 1 = Unskilled & Resident | 2 = Skilled | 3 = Highly Skilled")
