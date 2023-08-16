# Define the number of parameters
num_params = 3

# Define the names of the parameters.
# If you can't be bothered, use parameter_names=rep(NA, num_params).
parameter_names = c("m", "b", "sigma")

# Step sizes for proposals
step_sizes = c(1, 1, 1)

# If these are nonzero, they set the scale of a log-t
# distribution for the step sizes.
# don't need to change step size significantly as Brendon has added enough ... what???
scale_step_sizes = c(3, 3, 3)

# A dataset
# data = list(x = c(0.044103595329532, -0.922186227194728, 0.0960010565013454, 
# -1.12828522333698, 1.36291783960003, -2.23065969145022, -0.93027082294735, 
# -2.07175801570109, 1.20069146950599, 3.3937627148158), N = 10)

data = list(x=c(1.17, 2.97, 3.26, 4.69, 5.83, 6, 6.41),
            y=c(78.93, 58.2, 67.47, 37.47, 45.65, 32.92, 29.97),
            N=7)

# A sensible starting point
starting_point = c(1.0, 0.0, 1.0)
names(starting_point) = parameter_names

# Function that takes a vector of parameters and returns the log posterior,
# up to a normalising constant.
log_prob = function(params)
{
    log_prior = 0.0
    
    # Improper flat prior for mu requires no action
    
    # Improper log-uniform for sigma requires this
    if(params["sigma"] <= 0.0)
        return(-Inf)
    log_prior = log_prior - log(params["sigma"])

    log_likelihood = sum(dnorm(data$y,
                         mean=params["m"]*data$x + params["b"], sd=params["sigma"], log=TRUE))

    return(log_prior + log_likelihood)
}

# Function that takes the current parameter values as argument
# and returns a proposed new set of parameter values. Remember
# that in R the original params has been passed by value, i.e., copied,
# so I can modify "params" without destroying the original
generate_proposal = function(params)
{
    # Choose which parameter to change
    k = sample(1:length(params), 1)

    # Change it
    step_size = step_sizes[k]*exp(scale_step_sizes[k]*rt(1, df=2))
    params[k] = params[k] + step_size*rnorm(1)

    # Return modified vector
    return(params)
}

