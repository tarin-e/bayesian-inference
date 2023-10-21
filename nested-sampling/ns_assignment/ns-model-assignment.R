# Define the number of parameters
num_params = 1

# Define the names of the parameters.
# If you can't be bothered, use parameter_names=rep(NA, num_params).
parameter_names = c("theta")

# A dataset
# This could easily be loaded from an external file
data = list(x=c(0.610164901707321, 1.99984208494425, 1.50817369576544, 0.707493807654828,
                           1.49413506453857), N=5)

# Function that takes a vector of Uniform(0, 1) variables
# and returns a vector of the actual parameters. This
# function implicitly defines the prior for the parameters.
us_to_params = function(us)
{
    # Vector to be returned as the result of the function
    params = rep(NA, num_params)

    # Apply the names
    names(params) = parameter_names
    ### 1
    params["theta"] = exp(qnorm(us[1], 0, 1))
    ### 1
    ### 2
    
    ### 2
    return(params)
}

# Function that takes a vector of parameters and returns the
# log likelihood.
log_likelihood = function(params)
{
    ### 1
    # for p(x | U)
    # logL = sum(dunif(x = data$x, min = 0, max = params["b"], log = TRUE)[is.finite(dunif(x = data$x, min = 0, max = params["b"], log = TRUE))])
    # for p(x | E)
    logL = sum(dexp(x = data$x, rate = params["theta"], log = TRUE))
    ### 1
    ### 2
    
    ### 2
    return(logL)
}

