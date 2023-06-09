# Function to calculate the alpha-spending function
of_alpha_spending <- function(t, alpha){
    2 * (1 - pnorm(qnorm(alpha/2, lower = FALSE) / sqrt(t)))
} 

# Number of analyses
n_analyses <- 4

# Overall alpha level
overall_alpha <- 0.02500

# Information times for each analysis
information_times <- c(0, 0.4, 0.5, 1.0)

# Calculate alpha values for each analysis
alpha_values <- rep(0, n_analyses)
for (i in 1:n_analyses) {
    alpha_values[i] <- of_alpha_spending(information_times[i], overall_alpha)
}

# Calculate alpha differences
alpha_differences <- c(alpha_values[1], diff(alpha_values))

# Display alpha values and differences for each analysis
for (i in 1:n_analyses){
    cat(sprintf("Analysis %d: Information Time = %.1f, Alpha = %.10f, Difference = %.10f\n", i, information_times[i], alpha_values[i], alpha_differences[i]))
}

library(ggplot2)

# Create a data frame for the results
results <- data.frame(
    Analysis = 1:n_analyses,
    Information_Time = information_times,
    Alpha = alpha_values,
    Difference = alpha_differences
)

# Plot the results using ggplot2
ggplot(results, aes(x = Information_Time, y = Alpha)) +
    geom_point() +
    geom_line() +
    geom_text(aes(label = sprintf("%.5f", Alpha)), hjust = -0.2, vjust = 0.5) +
    labs(title = "Alpha Allocation in Each Analysis",
         x = "Information Time",
         y = "Alpha") +
    theme_minimal() +
    scale_x_continuous(limits = c(0, max(information_times)))  # Set x-axis limits from 0 to maximum information time