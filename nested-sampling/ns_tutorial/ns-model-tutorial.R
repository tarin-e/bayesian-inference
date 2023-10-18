# Define the number of parameters
num_params = 2

# Define the names of the parameters.
# If you can't be bothered, use parameter_names=rep(NA, num_params).
parameter_names = c("mu", "sigma")

# A dataset
# This could easily be loaded from an external file
data = data = list(x=c(340, 180, 210, 100, 100, 225, 90,
                       225, 240, 55, -35, 5), N=12)

# Function that takes a vector of Uniform(0, 1) variables
# and returns a vector of the actual parameters. This
# function implicitly defines the prior for the parameters.
us_to_params = function(us)
{
    # Vector to be returned as the result of the function
    params = rep(NA, num_params)

    # Apply the names
    names(params) = parameter_names

    # transform from Uniform(0 ,1) to the actual priors we want
    # 2a)
    # params["mu"] = -1E6 + 2E6*us[1]
    # 2b)
    # params["mu"] = 0
    # 2d)
    params["sigma"] = exp(qnorm(us[2], 0, 5))
    
    kappa = qt(us[1], df=1)
    params["mu"] = kappa * params["sigma"]

    return(params)
}

# Function that takes a vector of parameters and returns the
# log likelihood.
log_likelihood = function(params)
{
    logL = sum(dnorm(data$x, params["mu"], params["sigma"], log=TRUE))
    return(logL)
}

