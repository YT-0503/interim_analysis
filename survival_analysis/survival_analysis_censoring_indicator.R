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

# Generate censoring indicators
control_censor <- rbinom(n_control, size = 1, prob = 0.2)  # Censoring indicators for the control group
treatment_censor <- rbinom(n_treatment, size = 1, prob = 0.3)  # Censoring indicators for the treatment group

# Create a data frame combining survival times, event information, and censoring indicators
control_data <- data.frame(time = control_survival, event = 1 - control_censor, group = rep("Control", n_control))
treatment_data <- data.frame(time = treatment_survival, event = 1 - treatment_censor, group = rep("Treatment", n_treatment))
data <- rbind(control_data, treatment_data)

# Calculate the Kaplan-Meier estimates with censoring
fit <- survfit(Surv(time, event) ~ group, data = data)
summary(fit)

# Plot the Kaplan-Meier curves with censoring
ggsurvplot(fit, data = data, pval = TRUE, conf.int = TRUE, surv.median.line = "hv",
           ggtheme = theme_minimal(), xlab = "Time", ylab = "Survival Probability")
