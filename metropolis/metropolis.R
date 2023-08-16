# Source the model file
source("Documents/Masters/STATS 731/bayesian-inference/metropolis/metropolis-model.R")
library(ggplot2)

# Number of iterations to do
steps = 100000

# Thinning
thin = 10

# Initialise the particle somewhere
particle = starting_point
logp = log_prob(particle)

# Start the output file
header = ""
for(i in 1:num_params)
    header = paste(header, parameter_names[i], ",", sep="")
header = paste(header, "log_prob", sep="")
fileConn = file("metropolis-output.csv", open="w")
writeLines(header, fileConn)
close(fileConn)

# Storage of results for trace plot
keep = rep(NA, floor(steps/thin))

# Count the number of accepted proposals
accepted = 0

# Main Metropolis loop
for(iteration in 1:steps)
{
    # Generate proposal and measure how good it is
    proposal = generate_proposal(particle)
    proposal_logp = log_prob(proposal)

    # Compute acceptance probability
    log_alpha = proposal_logp - logp
    if(is.nan(log_alpha))
        log_alpha = -Inf
    if(log_alpha >= 0.0)
        alpha = 1.0
    else
        alpha = exp(log_alpha)


    # Make accept/reject decision
    if(runif(1) <= alpha)
    {
        # Accept the proposal
        particle = proposal
        logp = proposal_logp
        accepted = accepted + 1
    }

    if(iteration %% thin == 0)
    {
        # Save particle to disk
        fileConn = file("metropolis-output.csv", open="a")
        line = ""
        for(i in 1:num_params)
            line = paste(line, particle[i], ",", sep="")
        line = paste(line, logp, sep="")
        writeLines(line, fileConn)
        close(fileConn)

        # Print a message to screen.
        accept_frac = accepted/iteration
        cat(paste("Iteration", iteration, ". Acceptance rate =", accept_frac))
        cat("\n")
    }
}

output_df = read.csv("metropolis-output.csv")

# trace plots for different parameters
# todo: clip burnin
plot(1:10000, output_df$m, xlab = "Iterations", ylab = "m", title("Trace Plot of m"))
plot(1:10000, output_df$b, xlab = "Iterations", ylab = "b", title("Trace Plot of b"))
plot(1:10000, output_df$sigma, xlab = "Iterations", ylab = "sigma", title("Trace Plot of Sigma"))

# marginal posterior distribution of m
hist(output_df$m, breaks = 100, xlab = "m", title("Marginal POsterior Distribution of m"))

# joint posterior distribution of m and b (scatter)
plot(output_df$m, output_df$b, xlab = "m", ylab = "b", title("Joint Posterior Distribution of m and b"))

# general scatter plot
pairs(output_df)

# autocorrelation check
acf(output_df$m)
acf(output_df$b)
acf(output_df$sigma)

# significant correlation until 50 steps. Would we have roughly the same number of effective independent posterior samples?

data = list(x=c(1.17, 2.97, 3.26, 4.69, 5.83, 6, 6.41),
            y=c(78.93, 58.2, 67.47, 37.47, 45.65, 32.92, 29.97),
            N=7)

{
plot(data$x, data$y)

for (i in 1:1000) {
  abline(a = output_df$b[i], b = output_df$m[i], col = rgb(0, 0, 1, alpha = 0.05))
}
}
