library(medicaldata)
data(package = "medicaldata")
covid <- medicaldata::covid_testing
s1 <- medicaldata::strep_tb


# Load Required Libraries
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(lme4) # For mixed-effects modeling
library(ggpubr) # For combining plots

# Load Dataset
# Replace `your_file_path` with the actual path to the dataset
data <- s1

# Step 1: Data Integrity Check
# Ensure all variables and observations are intact
print("Data Summary:")
summary(data)

# Validate dataset structure
print("Dataset Structure:")
str(data)

# Step 2: Exploratory Data Analysis
# Descriptive statistics for numeric variables
numeric_summary <- data %>%
  select_if(is.numeric) %>%
  summary()
print("Numeric Variables Summary:")
print(numeric_summary)

# Group-wise analysis of 'improved' by 'arm' and 'gender'
group_summary <- data %>%
  group_by(arm, gender) %>%
  summarise(
    count = n(),
    improved_rate = mean(improved, na.rm = TRUE)
  )
print("Group-wise Summary:")
print(group_summary)

# Step 3: Statistical Modeling
# Logistic regression to predict 'improved' using baseline conditions and treatment arm
log_model <- glm(improved ~ arm + gender + baseline_condition + baseline_esr,
                 data = data, family = binomial)
print("Logistic Regression Model Summary:")
summary(log_model)

# Model to account for patient-level variability in radiologic response
fixed_model <- lm(rad_num ~ arm + gender + baseline_condition + patient_id, data = data)
summary(fixed_model)

# Survival analysis to test time-to-improvement (assuming a 'time' column exists)
# Creating synthetic time-to-event data for testing if not available
if (!"time" %in% colnames(data)) {
  set.seed(123)
  data$time <- sample(1:365, nrow(data), replace = TRUE) # Random time-to-event data
  data$status <- sample(0:1, nrow(data), replace = TRUE) # Random censoring data
}
surv_fit <- survfit(Surv(time, status) ~ arm, data = data)
print("Survival Model Summary:")
summary(surv_fit)

# Step 4: Visualizations
# Logistic regression effect plot
log_plot <- ggplot(data, aes(x = arm, y = improved, color = gender)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  theme_minimal() +
  labs(title = "Logistic Regression Effect Plot", y = "Probability of Improvement")

# Kaplan-Meier survival plot
km_plot <- ggsurvplot(surv_fit, data = data, 
                      palette = c("blue", "red"), 
                      pval = TRUE, 
                      title = "Kaplan-Meier Survival Analysis")

# Mixed model diagnostic plot
diag_plot <- ggplot(data, aes(x = baseline_condition, y = rad_num, color = arm)) +
  geom_boxplot() +
  facet_wrap(~ gender) +
  theme_minimal() +
  labs(title = "Radiologic Response by Baseline Condition and Treatment Arm")

# Combine all plots
combined_plot <- ggarrange(
  log_plot, km_plot$plot, diag_plot,
  ncol = 2, nrow = 2, labels = c("A", "B", "C")
)

# Save the combined plot
ggsave("PQ_Test_Plots.png", combined_plot, width = 12, height = 10)

# Step 5: Response Time Testing
start_time <- Sys.time()
# Run multiple logistic regression iterations for performance testing
for (i in 1:1000) {
  glm(improved ~ arm + gender + baseline_condition + baseline_esr, data = data, family = binomial)
}
end_time <- Sys.time()
response_time <- end_time - start_time
print(paste("Total Time for 1000 Logistic Models:", response_time))

# Step 6: Audit and Access Control (Example Placeholder)
# Placeholder for integrating audit trail or access control testing
print("Audit Trail Testing: Ensure data modification logs are being captured.")
print("Access Control Testing: Verify user roles and permissions.")

# Final Message
print("PQ Testing Completed. Check summary, models, and plots for results.")
