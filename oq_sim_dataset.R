# Load required libraries
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
library(rmarkdown)
library(writexl)

# 1. Create a Simulated Data Frame
set.seed(123)
simulated_data <- data.frame(
  ID = 1:100,
  Group = sample(c("Control", "Treatment"), 100, replace = TRUE),
  Age = round(runif(100, 20, 60), 0),
  Score_Pre = round(runif(100, 50, 80), 1),
  Score_Post = round(runif(100, 60, 90), 1)
)

# Display the first few rows of the simulated data
head(simulated_data)

# 2. Data Manipulation
# Create a new column for score difference
manipulated_data <- simulated_data %>%
  mutate(Score_Diff = Score_Post - Score_Pre)

# Merge the data with a new table
extra_data <- data.frame(
  ID = sample(1:100, 50),
  Extra_Info = sample(c("High", "Low"), 50, replace = TRUE)
)
merged_data <- manipulated_data %>%
  left_join(extra_data, by = "ID")

# Filter for Treatment group only
filtered_data <- merged_data %>%
  filter(Group == "Treatment")

# View the manipulated data
head(filtered_data)

# 3. Statistical Analysis
# Perform a t-test on Score_Diff between Control and Treatment groups
t_test_result <- t.test(
  Score_Diff ~ Group,
  data = manipulated_data
)
print(t_test_result)

# Perform linear regression
regression_model <- lm(Score_Post ~ Score_Pre + Group, data = simulated_data)
summary(regression_model)

# Perform ANOVA
anova_result <- aov(Score_Post ~ Group, data = simulated_data)
summary(anova_result)

# 4. Graphical Representation
# Scatter plot of Score_Pre vs Score_Post
scatter_plot <- ggplot(simulated_data, aes(x = Score_Pre, y = Score_Post, color = Group)) +
  geom_point() +
  labs(title = "Score Pre vs Post", x = "Score Pre", y = "Score Post") +
  theme_minimal()
print(scatter_plot)

# Save the plot
ggsave("scatter_plot.png", scatter_plot, width = 7, height = 5)

# 5. Data Export and Reporting
# Export data to Excel
write_xlsx(manipulated_data, "manipulated_data.xlsx")

# # Create and Knit R Markdown Report (This requires an Rmd file setup)
# rmarkdown::render(
#   input = "oq_sym_dataset.R",  # Replace with your RMarkdown file path
#   output_format = "pdf_document",
#   output_file = "OQ_Report_sym.pdf"
# )

# 6. System Integration
# Set up a connection to an SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
dbWriteTable(con, "SimulatedData", manipulated_data)

# Fetch data back from the database
db_data <- dbReadTable(con, "SimulatedData")
head(db_data)

# Disconnect from the database
dbDisconnect(con)

# API Integration Example (Pseudo-code, requires API setup)
# library(httr)
# response <- GET("https://api.example.com/data")
# api_data <- content(response, as = "parsed")

# 7. Record Test Results
# Store test results as comments or print statements
cat("\nAll OQ steps executed successfully.\n")