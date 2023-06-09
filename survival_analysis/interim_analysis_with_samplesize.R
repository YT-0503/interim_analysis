# Load the required packages
library(survival)
library(survminer)

# Set the parameters
set.seed(123)  # Seed value for reproducibility
n_control <- 500  # Sample size of the control group
n_treatment <- 250  # Sample size of the treatment group
alpha <- 0.025  # Significance level
power <- 0.9  # Power
interim_time <- 10
# Generate survival time data
control_survival <- rexp(n_control, rate = 0.1)  # Survival times of the control group
treatment_survival <- rexp(n_treatment, rate = 0.08)  # Survival times of the treatment group

# Create a data frame combining survival times and event information
control_data <- data.frame(time = control_survival, event = rep(1, n_control), group = rep("Control", n_control))
treatment_data <- data.frame(time = treatment_survival, event = rep(1, n_treatment), group = rep("Treatment", n_treatment))
data <- rbind(control_data, treatment_data)

# Perform interim analysis at the specified time point
interim_data <- data[data$time <= interim_time, ]

# Calculate the Kaplan-Meier estimates for the interim analysis
fit_interim <- survfit(Surv(time, event) ~ group, data = interim_data)

# Plot the Kaplan-Meier curves for the interim analysis
ggsurvplot(fit_interim, data = interim_data, pval = TRUE, conf.int = TRUE, surv.median.line = "hv",
           ggtheme = theme_minimal(), xlab = "Time", ylab = "Survival Probability",
           title = paste("Interim Analysis (Time Point:", interim_time, ")"))
# Get the sample sizes for each group at the interim analysis time point
sample_sizes <- summary(fit_interim)$n

# Print the sample sizes
cat("Sample size at interim analysis (Time Point:", interim_time, "):\n")
cat("Control Group:", sample_sizes[1], "\n")
cat("Treatment Group:", sample_sizes[2], "\n")
