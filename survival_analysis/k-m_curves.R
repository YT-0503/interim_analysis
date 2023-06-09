# Load the required packages
library(survival)
library(survminer)

# Set the parameters
set.seed(123)  # Seed value for reproducibility
n_control <- 250  # Sample size of the control group
n_treatment <- 500  # Sample size of the treatment group
alpha <- 0.025  # Significance level
power <- 0.9  # Power

# Generate survival time data
control_survival <- rexp(n_control, rate = 0.1)  # Survival times of the control group
treatment_survival <- rexp(n_treatment, rate = 0.08)  # Survival times of the treatment group

# Create a data frame combining survival times and event information
control_data <- data.frame(time = control_survival, event = rep(1, n_control), group = rep("Control", n_control))
treatment_data <- data.frame(time = treatment_survival, event = rep(1, n_treatment), group = rep("Treatment", n_treatment))
data <- rbind(control_data, treatment_data)

# Calculate the Kaplan-Meier estimates
fit <- survfit(Surv(time, event) ~ group, data = data)
summary(fit)

# Plot the Kaplan-Meier curves
ggsurvplot(fit, data = data, pval = TRUE, conf.int = TRUE, surv.median.line = "hv",
           ggtheme = theme_minimal(), xlab = "Time", ylab = "Survival Probability")
