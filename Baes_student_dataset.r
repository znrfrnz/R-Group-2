#  INSTRUCTIONS BEFORE RUNNING:
#  1. Download the CSV from Kaggle and save it somewhere on your machine
#  2. Update DATA_PATH (line 9) to where you saved the CSV
#  3. Update PDF_PATH (line 10) to where you want the PDF saved
#  4. Uncomment lines 14-17 if packages are not yet installed
# ============================================================

# ── PATHS — UPDATE THESE ────────────────────────────────────
DATA_PATH <- "D:/Personal Projects/R/R-Group-2/enhanced_student_habits_performance_dataset.csv"
PDF_PATH  <- "EDA_Student_Habits_Graphs.pdf"
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
pdf(PDF_PATH, width = 11, height = 8.5)


# ── SECTION DIVIDER HELPER ──────────────────────────────────
section_page <- function(title, subtitle, color = "#2E75B6") {
  grid.newpage()
  grid.rect(gp = gpar(fill = color, col = NA))
  grid.text(title,
    x = 0.5, y = 0.55, just = "center",
    gp = gpar(col = "white", fontsize = 28, fontface = "bold")
  )
}


# ── SECTION A — UNIVARIATE ANALYSIS ─────────────────────────
section_page(
  "Univariate Analysis",
  color = "#2E75B6"
)


# ── Q1: Daily Study Hours ────────────────────────────────────
m1  <- round(mean(data$study_hours_per_day, na.rm = TRUE), 2)
md1 <- median(data$study_hours_per_day, na.rm = TRUE)
sd1 <- round(sd(data$study_hours_per_day, na.rm = TRUE), 2)
cat("Q1 | Mean:", m1, "| Median:", md1, "| SD:", sd1, "\n")

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
    title    = "Distribution of Daily Study Hours",
    subtitle = paste("Mean =", m1, "| Median =", md1, "| SD =", sd1),
    x        = "Study Hours per Day", y = "Density",
    caption  = "Red dashed line = mean"
  ) +
  eda_theme

p1b <- ggplot(data, aes(y = study_hours_per_day)) +
  geom_boxplot(
    fill = "#AED6F1", color = "#1F3864",
    outlier.color = "red", outlier.alpha = 0.5
  ) +
  labs(
    title    = "Study Hours — Box Plot",
    subtitle = "Median, IQR, whiskers, and outliers",
    y        = "Study Hours per Day"
  ) +
  eda_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p1a, p1b,
  ncol = 2,
  top  = grid::textGrob("Daily Study Hours",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)


# ── Q2: Exam Score Distribution ──────────────────────────────
m2  <- round(mean(data$exam_score, na.rm = TRUE), 2)
md2 <- median(data$exam_score, na.rm = TRUE)
sd2 <- round(sd(data$exam_score, na.rm = TRUE), 2)
cat("Q2 | Mean:", m2, "| Median:", md2, "| SD:", sd2, "\n")

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
    title    = "Distribution of Exam Scores",
    subtitle = paste("Mean =", m2, "| Median =", md2, "| SD =", sd2),
    x        = "Exam Score (0–100)", y = "Density",
    caption  = "Red dashed line = mean"
  ) +
  eda_theme

p2b <- ggplot(data, aes(y = exam_score)) +
  geom_boxplot(
    fill = "#A9DFBF", color = "#1E8449",
    outlier.color = "red", outlier.alpha = 0.5
  ) +
  labs(
    title    = "Exam Scores — Box Plot",
    subtitle = "5-number summary + outliers",
    y        = "Exam Score"
  ) +
  eda_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p2a, p2b,
  ncol = 2,
  top  = grid::textGrob("Exam Score Distribution",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)


# ── Q3: Sleep Hours ──────────────────────────────────────────
m5   <- round(mean(data$sleep_hours, na.rm = TRUE), 2)
md5  <- median(data$sleep_hours, na.rm = TRUE)
sd5  <- round(sd(data$sleep_hours, na.rm = TRUE), 2)
pct5 <- round(sum(data$sleep_hours < 7, na.rm = TRUE) / nrow(data) * 100, 1)
cat("Q3 | Mean:", m5, "| Median:", md5, "| SD:", sd5, "| Under 7 hrs:", pct5, "%\n")

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
    title    = "Distribution of Sleep Hours",
    subtitle = paste(
      "Mean =", m5, "| Median =", md5,
      "| SD =", sd5, "| Under 7 hrs:", pct5, "%"
    ),
    x       = "Sleep Hours per Day", y = "Density",
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
  top  = grid::textGrob("Daily Sleep Hours",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)


# ── SECTION B — BIVARIATE ANALYSIS ──────────────────────────
section_page(
  "Bivariate Analysis",
  color = "#1E8449"
)


# ── Q4: Social Media Hours vs. Exam Score ───────────────────
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
    Count      = n(), .groups = "drop"
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
    label     = paste0("Pearson r = ", r9),
    fontface  = "bold", color = "#784212", size = 4.5,
    fill      = "white", label.size = 0.5
  ) +
  labs(
    title    = "Social Media Hours vs. Exam Score",
    subtitle = "Negative relationship expected: more social media → lower scores",
    x        = "Social Media Hours per Day", y = "Exam Score (0–100)",
    caption  = "Shaded band = 95% confidence interval"
  ) +
  eda_theme

p9b <- ggplot(sm_means, aes(x = sm_bin, y = Mean_Score, fill = Mean_Score)) +
  geom_col(color = "white", width = 0.6) +
  geom_text(aes(label = round(Mean_Score, 1)),
    vjust    = -0.6,
    fontface = "bold", size = 4.2
  ) +
  scale_fill_gradient(low = "#FDEBD0", high = "#E67E22") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Mean Exam Score by Social Media Usage",
    subtitle = "Binned daily social media hours",
    x        = "Daily Social Media Usage", y = "Mean Exam Score"
  ) +
  eda_theme +
  theme(legend.position = "none")

grid.arrange(p9a, p9b,
  ncol = 2,
  top  = grid::textGrob("Social Media Hours vs. Exam Score",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)


# ── Q5: Diet Quality vs. Attendance & Exam Score ────────────
data$diet_quality <- factor(data$diet_quality,
  levels  = c("Poor", "Average", "Good"),
  ordered = TRUE
)

diet_summary <- data %>%
  group_by(diet_quality) %>%
  summarise(
    Mean_Attendance = round(mean(attendance_percentage, na.rm = TRUE), 2),
    SD_Attendance   = round(sd(attendance_percentage,   na.rm = TRUE), 2),
    Mean_Score      = round(mean(exam_score,             na.rm = TRUE), 2),
    SD_Score        = round(sd(exam_score,               na.rm = TRUE), 2),
    Count           = n(), .groups = "drop"
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
    title    = "Diet Quality vs. Mean Attendance",
    subtitle = "Higher diet quality → better class attendance",
    x        = "Diet Quality", y = "Mean Attendance (%)",
    caption  = "Error bars = ±1 SD"
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
    title    = "Exam Score by Diet Quality",
    subtitle = "Better diet quality → higher exam scores",
    x        = "Diet Quality", y = "Exam Score (0–100)",
    caption  = "Diamond = group mean"
  ) +
  eda_theme +
  theme(legend.position = "none")

grid.arrange(p10a, p10b,
  ncol = 2,
  top  = grid::textGrob("Diet Quality vs. Attendance & Exam Score",
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1F3864")
  )
)


# ── CLOSE PDF ───────────────────────────────────────────────
dev.off()

cat("\n==============================\n")
cat(" PDF saved successfully!\n")
cat(" File:", PDF_PATH, "\n")
cat("==============================\n")
