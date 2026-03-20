#  INSTRUCTIONS BEFORE RUNNING:
#  1. Download the CSV from Kaggle and save it somewhere on your machine
#  2. Update DATA_PATH (line 16) to where you saved the CSV
#  3. Update PDF_PATH (line 17) to where you want the PDF saved
#  4. Uncomment lines 28-33 if packages are not yet installed
# ============================================================

# ── PATHS — UPDATE THESE ────────────────────────────────────
DATA_PATH <- "D:/Personal Projects/R/R-Group-2/enhanced_student_habits_performance_dataset.csv"
PDF_PATH <- "EDA_Student_Habits_Graphs.pdf"
# The PDF will be saved in the same folder as this script by default.
# To save elsewhere: "C:/Users/YourName/Desktop/EDA_Student_Habits_Graphs.pdf"

# ── INSTALL PACKAGES (uncomment if needed) ──────────────────
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("gridExtra")
# install.packages("scales")

library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(scales)

# ── LOAD DATA ───────────────────────────────────────────────
data <- read.csv(DATA_PATH, stringsAsFactors = TRUE)

cat("Dataset loaded:", nrow(data), "rows,", ncol(data), "columns\n")
cat("Saving all graphs to:", PDF_PATH, "\n\n")


eda_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = "#1F3864"),
    plot.subtitle    = element_text(size = 10, color = "#555555"),
    plot.caption     = element_text(size = 8, color = "#888888", hjust = 0),
    axis.title       = element_text(face = "bold", size = 11),
    axis.text        = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(15, 20, 15, 20)
  )

# ── OPEN PDF DEVICE ─────────────────────────────────────────
pdf(PDF_PATH, width = 11, height = 8.5) # change this pag hindi nagkasya yung graph


# ── SECTION DIVIDER HELPER ──────────────────────────────────
section_page <- function(title, subtitle, color = "#2E75B6") {
  grid.newpage()
  grid.rect(gp = gpar(fill = color, col = NA))
  grid.text(title,
    x = 0.5, y = 0.55, just = "center",
    gp = gpar(col = "white", fontsize = 28, fontface = "bold")
  )
  grid.text(subtitle,
    x = 0.5, y = 0.42, just = "center",
    gp = gpar(col = "white", fontsize = 14)
  )
}


m1 <- round(mean(data$study_hours_per_day, na.rm = TRUE), 2)
md1 <- median(data$study_hours_per_day, na.rm = TRUE)
sd1 <- round(sd(data$study_hours_per_day, na.rm = TRUE), 2)
cat("Mean:", m1, "| Median:", md1, "| SD:", sd1, "\n")

p1a <- ggplot(data, aes(x = study_hours_per_day)) +
  geom_histogram(aes(y = after_stat(density)),
    bins = 30,
    fill = "#2E75B6", color = "white", alpha = 0.85
  ) +
  geom_density(color = "#1F3864", linewidth = 1.2) +
  geom_vline(xintercept = m1, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text",
    x = m1 + 0.3, y = Inf,
    label = paste("Mean =", m1), vjust = 2, color = "red", size = 3.5
  ) +
  labs(
    title = "Distribution of Daily Study Hours",
    subtitle = paste("Mean =", m1, "| Median =", md1, "| SD =", sd1),
    x = "Study Hours per Day", y = "Density",
    caption = "Red dashed line = mean"
  ) +
  eda_theme

p1b <- ggplot(data, aes(y = study_hours_per_day)) +
  geom_boxplot(
    fill = "#AED6F1", color = "#1F3864",
    outlier.color = "red", outlier.alpha = 0.5
  ) +
  labs(
    title = "Study Hours — Box Plot",
    subtitle = "Median, IQR, whiskers, and outliers",
    y = "Study Hours per Day"
  ) +
  eda_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p1a, p1b,
  ncol = 2,
  top = grid::textGrob("Daily Study Hours",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)



m2 <- round(mean(data$exam_score, na.rm = TRUE), 2)
md2 <- median(data$exam_score, na.rm = TRUE)
sd2 <- round(sd(data$exam_score, na.rm = TRUE), 2)
cat("Mean:", m2, "| Median:", md2, "| SD:", sd2, "\n")

p2a <- ggplot(data, aes(x = exam_score)) +
  geom_histogram(aes(y = after_stat(density)),
    bins = 30,
    fill = "#1E8449", color = "white", alpha = 0.85
  ) +
  geom_density(color = "#145A32", linewidth = 1.2) +
  geom_vline(xintercept = m2, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text",
    x = m2 + 1, y = Inf,
    label = paste("Mean =", m2), vjust = 2, color = "red", size = 3.5
  ) +
  labs(
    title = "Distribution of Exam Scores",
    subtitle = paste("Mean =", m2, "| Median =", md2, "| SD =", sd2),
    x = "Exam Score (0–100)", y = "Density",
    caption = "Red dashed line = mean"
  ) +
  eda_theme

p2b <- ggplot(data, aes(y = exam_score)) +
  geom_boxplot(
    fill = "#A9DFBF", color = "#1E8449",
    outlier.color = "red", outlier.alpha = 0.5
  ) +
  labs(
    title = "Exam Scores — Box Plot",
    subtitle = "5-number summary + outliers",
    y = "Exam Score"
  ) +
  eda_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p2a, p2b,
  ncol = 2,
  top = grid::textGrob("Exam Score Distribution",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)



freq3 <- as.data.frame(table(data$extracurricular_participation))
names(freq3) <- c("Participation", "Count")
freq3$Pct <- round(freq3$Count / sum(freq3$Count) * 100, 1)
freq3$Label <- paste0(freq3$Count, "\n(", freq3$Pct, "%)")
print(freq3)

p3a <- ggplot(freq3, aes(x = Participation, y = Count, fill = Participation)) +
  geom_col(width = 0.5, color = "white") +
  geom_text(aes(label = Label), vjust = -0.4, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("#E74C3C", "#2980B9")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Extracurricular Participation — Bar Chart",
    subtitle = "Count and percentage of students",
    x = "Participates in Extracurriculars", y = "Number of Students"
  ) +
  eda_theme +
  theme(legend.position = "none")

p3b <- ggplot(freq3, aes(x = "", y = Count, fill = Participation)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Pct, "%")),
    position = position_stack(vjust = 0.5),
    fontface = "bold", color = "white", size = 6
  ) +
  scale_fill_manual(values = c("#E74C3C", "#2980B9")) +
  labs(title = "Participation — Pie Chart", fill = "Extracurriculars") +
  eda_theme +
  theme(
    axis.text = element_blank(), axis.title = element_blank(),
    panel.grid = element_blank()
  )

grid.arrange(p3a, p3b,
  ncol = 2,
  top = grid::textGrob("Extracurricular Activity Participation",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)



m4 <- round(mean(data$mental_health_rating, na.rm = TRUE), 2)
md4 <- median(data$mental_health_rating, na.rm = TRUE)
sd4 <- round(sd(data$mental_health_rating, na.rm = TRUE), 2)
cat("Mean:", m4, "| Median:", md4, "| SD:", sd4, "\n")

mh_df <- as.data.frame(table(data$mental_health_rating))
names(mh_df) <- c("Rating", "Count")

p4 <- ggplot(mh_df, aes(x = Rating, y = Count, fill = Count)) +
  geom_col(color = "white") +
  scale_fill_gradient(low = "#FADBD8", high = "#922B21") +
  geom_text(aes(label = Count), vjust = -0.5, fontface = "bold", size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Mental Health Rating Distribution",
    subtitle = paste(
      "Mean =", m4, "| Median =", md4,
      "| SD =", sd4, "| Scale: 1 (poor) to 10 (excellent)"
    ),
    x = "Mental Health Rating (1–10)", y = "Number of Students",
    caption = "Darker bars = higher frequency"
  ) +
  eda_theme +
  theme(legend.position = "none")

print(p4)



m5 <- round(mean(data$sleep_hours, na.rm = TRUE), 2)
md5 <- median(data$sleep_hours, na.rm = TRUE)
sd5 <- round(sd(data$sleep_hours, na.rm = TRUE), 2)
pct5 <- round(sum(data$sleep_hours < 7, na.rm = TRUE) / nrow(data) * 100, 1)
cat("Mean:", m5, "| Median:", md5, "| SD:", sd5, "| Under 7 hrs:", pct5, "%\n")

p5a <- ggplot(data, aes(x = sleep_hours)) +
  geom_histogram(aes(y = after_stat(density)),
    bins = 25,
    fill = "#8E44AD", color = "white", alpha = 0.85
  ) +
  geom_density(color = "#4A235A", linewidth = 1.2) +
  geom_vline(xintercept = 7, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 9, color = "orange", linetype = "dashed", linewidth = 1) +
  annotate("text",
    x = 8, y = Inf, label = "Recommended\n7–9 hrs",
    vjust = 2, color = "darkorange", fontface = "bold", size = 3.5
  ) +
  labs(
    title = "Distribution of Sleep Hours",
    subtitle = paste(
      "Mean =", m5, "| Median =", md5,
      "| SD =", sd5, "| Under 7 hrs:", pct5, "%"
    ),
    x = "Sleep Hours per Day", y = "Density",
    caption = "Orange dashed lines = recommended range (7–9 hrs)"
  ) +
  eda_theme

p5b <- ggplot(data, aes(y = sleep_hours)) +
  geom_boxplot(
    fill = "#D2B4DE", color = "#6C3483",
    outlier.color = "red", outlier.alpha = 0.5
  ) +
  geom_hline(yintercept = 7, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 9, color = "orange", linetype = "dashed", linewidth = 1) +
  labs(title = "Sleep Hours — Box Plot", y = "Sleep Hours per Day") +
  eda_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p5a, p5b,
  ncol = 2,
  top = grid::textGrob("Daily Sleep Hours",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)



#  SECTION B — BIVARIATE ANALYSIS
section_page(
  "Section B: Bivariate Analysis",
  "Questions 6–10  |  Examining relationships between two variables",
  color = "#1E8449"
)



r6 <- round(cor(data$study_hours_per_day, data$exam_score, use = "complete.obs"), 4)
cat("Pearson r:", r6, "\n")

p6 <- ggplot(data, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(alpha = 0.3, color = "#2E75B6", size = 1.8) +
  geom_smooth(
    method = "lm", color = "#1F3864", linewidth = 1.4,
    se = TRUE, fill = "#AED6F1"
  ) +
  annotate("label",
    x = max(data$study_hours_per_day, na.rm = TRUE) * 0.75,
    y = min(data$exam_score, na.rm = TRUE) + 6,
    label = paste0("Pearson r = ", r6),
    fontface = "bold", color = "#1F3864", size = 4.5,
    fill = "white", label.size = 0.5
  ) +
  labs(
    title = "Study Hours per Day vs. Exam Score",
    subtitle = "Positive relationship: more study hours → higher exam scores",
    x = "Study Hours per Day", y = "Exam Score (0–100)",
    caption = "Shaded band = 95% confidence interval"
  ) +
  eda_theme

print(p6)



job_summary <- data %>%
  group_by(part_time_job) %>%
  summarise(
    Mean = round(mean(exam_score, na.rm = TRUE), 2),
    SD = round(sd(exam_score, na.rm = TRUE), 2),
    n = n(), .groups = "drop"
  )
print(job_summary)

p7 <- ggplot(data, aes(x = part_time_job, y = exam_score, fill = part_time_job)) +
  geom_boxplot(
    outlier.color = "gray40", outlier.alpha = 0.5,
    alpha = 0.85, width = 0.5
  ) +
  stat_summary(
    fun = mean, geom = "point", shape = 18,
    size = 5, color = "white", show.legend = FALSE
  ) +
  stat_summary(
    fun = mean, geom = "point", shape = 18,
    size = 4, color = "#1F3864", show.legend = FALSE
  ) +
  scale_fill_manual(values = c("#E74C3C", "#2980B9")) +
  labs(
    title = "Exam Scores by Part-Time Job Status",
    subtitle = "Comparing academic performance between employed and non-employed students",
    x = "Has Part-Time Job", y = "Exam Score (0–100)",
    fill = "Part-Time Job",
    caption = "Diamond = group mean"
  ) +
  eda_theme

print(p7)

r8 <- round(cor(data$mental_health_rating, data$exam_score, use = "complete.obs"), 4)
cat("Pearson r:", r8, "\n")

mh_means <- data %>%
  group_by(mental_health_rating) %>%
  summarise(Mean_Score = round(mean(exam_score, na.rm = TRUE), 2), .groups = "drop")

p8a <- ggplot(data, aes(x = mental_health_rating, y = exam_score)) +
  geom_point(alpha = 0.25, color = "#922B21", size = 1.5) +
  geom_smooth(
    method = "lm", color = "#641E16", linewidth = 1.4,
    se = TRUE, fill = "#F5B7B1"
  ) +
  annotate("label",
    x = 2, y = max(data$exam_score, na.rm = TRUE) - 4,
    label = paste0("Pearson r = ", r8),
    fontface = "bold", color = "#641E16", size = 4.5,
    fill = "white", label.size = 0.5
  ) +
  labs(
    title = "Mental Health Rating vs. Exam Score",
    subtitle = "Scatterplot with linear regression line",
    x = "Mental Health Rating (1–10)", y = "Exam Score (0–100)",
    caption = "Shaded band = 95% confidence interval"
  ) +
  eda_theme

p8b <- ggplot(mh_means, aes(x = mental_health_rating, y = Mean_Score)) +
  geom_line(color = "#922B21", linewidth = 1.4) +
  geom_point(color = "#641E16", size = 3.5) +
  geom_text(aes(label = round(Mean_Score, 1)),
    vjust = -0.9, size = 3.2,
    fontface = "bold"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    title = "Mean Exam Score per Mental Health Rating",
    subtitle = "Average score at each rating level (1–10)",
    x = "Mental Health Rating", y = "Mean Exam Score"
  ) +
  eda_theme

grid.arrange(p8a, p8b,
  ncol = 2,
  top = grid::textGrob("Mental Health Rating vs. Exam Score",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)

r9 <- round(cor(data$social_media_hours, data$exam_score, use = "complete.obs"), 4)
cat("Pearson r:", r9, "\n")

data$sm_bin <- cut(data$social_media_hours,
  breaks = c(-Inf, 1, 2, 3, 4, Inf),
  labels = c("0–1 hr", "1–2 hrs", "2–3 hrs", "3–4 hrs", "4+ hrs")
)
sm_means <- data %>%
  group_by(sm_bin) %>%
  summarise(
    Mean_Score = round(mean(exam_score, na.rm = TRUE), 2),
    Count = n(), .groups = "drop"
  )

p9a <- ggplot(data, aes(x = social_media_hours, y = exam_score)) +
  geom_point(alpha = 0.3, color = "#E67E22", size = 1.8) +
  geom_smooth(
    method = "lm", color = "#784212", linewidth = 1.4,
    se = TRUE, fill = "#FAD7A0"
  ) +
  annotate("label",
    x = max(data$social_media_hours, na.rm = TRUE) * 0.72,
    y = max(data$exam_score, na.rm = TRUE) - 4,
    label = paste0("Pearson r = ", r9),
    fontface = "bold", color = "#784212", size = 4.5,
    fill = "white", label.size = 0.5
  ) +
  labs(
    title = "Social Media Hours vs. Exam Score",
    subtitle = "Negative relationship expected: more social media → lower scores",
    x = "Social Media Hours per Day", y = "Exam Score (0–100)",
    caption = "Shaded band = 95% confidence interval"
  ) +
  eda_theme

p9b <- ggplot(sm_means, aes(x = sm_bin, y = Mean_Score, fill = Mean_Score)) +
  geom_col(color = "white", width = 0.6) +
  geom_text(aes(label = round(Mean_Score, 1)),
    vjust = -0.6,
    fontface = "bold", size = 4.2
  ) +
  scale_fill_gradient(low = "#FDEBD0", high = "#E67E22") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Mean Exam Score by Social Media Usage",
    subtitle = "Binned daily social media hours",
    x = "Daily Social Media Usage", y = "Mean Exam Score"
  ) +
  eda_theme +
  theme(legend.position = "none")

grid.arrange(p9a, p9b,
  ncol = 2,
  top = grid::textGrob("Social Media Hours vs. Exam Score",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)


data$diet_quality <- factor(data$diet_quality,
  levels = c("Poor", "Average", "Good"),
  ordered = TRUE
)

diet_summary <- data %>%
  group_by(diet_quality) %>%
  summarise(
    Mean_Attendance = round(mean(attendance_percentage, na.rm = TRUE), 2),
    SD_Attendance = round(sd(attendance_percentage, na.rm = TRUE), 2),
    Mean_Score = round(mean(exam_score, na.rm = TRUE), 2),
    SD_Score = round(sd(exam_score, na.rm = TRUE), 2),
    Count = n(), .groups = "drop"
  )
print(diet_summary)

p10a <- ggplot(
  diet_summary,
  aes(x = diet_quality, y = Mean_Attendance, fill = diet_quality)
) +
  geom_col(color = "white", width = 0.55) +
  geom_errorbar(
    aes(
      ymin = Mean_Attendance - SD_Attendance,
      ymax = Mean_Attendance + SD_Attendance
    ),
    width = 0.2, color = "gray30", linewidth = 0.8
  ) +
  geom_text(aes(label = paste0(Mean_Attendance, "%")),
    vjust = -2.2, fontface = "bold", size = 4.2
  ) +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#27AE60")) +
  scale_y_continuous(limits = c(0, 115)) +
  labs(
    title = "Diet Quality vs. Mean Attendance",
    subtitle = "Higher diet quality → better class attendance",
    x = "Diet Quality", y = "Mean Attendance (%)",
    caption = "Error bars = ±1 SD"
  ) +
  eda_theme +
  theme(legend.position = "none")

p10b <- ggplot(data, aes(x = diet_quality, y = exam_score, fill = diet_quality)) +
  geom_boxplot(outlier.alpha = 0.4, alpha = 0.85, width = 0.5) +
  stat_summary(
    fun = mean, geom = "point", shape = 18,
    size = 5, color = "white", show.legend = FALSE
  ) +
  stat_summary(
    fun = mean, geom = "point", shape = 18,
    size = 4, color = "#1F3864", show.legend = FALSE
  ) +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#27AE60")) +
  labs(
    title = "Exam Score by Diet Quality",
    subtitle = "Better diet quality → higher exam scores",
    x = "Diet Quality", y = "Exam Score (0–100)",
    caption = "Diamond = group mean"
  ) +
  eda_theme +
  theme(legend.position = "none")

grid.arrange(p10a, p10b,
  ncol = 2,
  top = grid::textGrob("Diet Quality vs. Attendance & Exam Score",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)


# ── CLOSE PDF ───────────────────────────────────────────────
dev.off()

cat("\n==============================\n")
cat(" PDF saved successfully!\n")
cat(" File:", PDF_PATH, "\n")
cat("==============================\n")
