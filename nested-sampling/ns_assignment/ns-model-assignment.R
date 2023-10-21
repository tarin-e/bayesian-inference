# Define the number of parameters

### 1
num_params = 1
### 1

### 2A
num_params = 3
### 2A

### 2B, C
num_params = 4
### 2B, C

# Define the names of the parameters.
# If you can't be bothered, use parameter_names=rep(NA, num_params).

### 1
parameter_names = c("theta")
### 1

### 2A
parameter_names = c("beta_0", "beta_1", "sigma")
### 2A

### 2B, C
parameter_names = c("beta_0", "beta_1", "beta_2", "sigma")
### 2B, C

# A dataset
# This could easily be loaded from an external file

### 1
data = list(x=c(0.610164901707321, 1.99984208494425, 1.50817369576544, 0.707493807654828,
                           1.49413506453857), N=5)
### 1

### 2
colorado_df = read.csv("../data/colorado.csv")
### 2


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
    # params["theta"] = exp(qnorm(us[1], 0, 1))
    ### 1
    ### 2A
    # params["beta_0"] = exp(qnorm(us[1], 0, 5))
    # params["beta_1"] = qnorm(us[2], 0, params["beta_0"])
    # params["sigma"] = exp(qnorm(us[3], log(params["beta_0"]),1))
    ### 2A
    
    ### 2B, C
    params["beta_0"] = exp(qnorm(us[1], 0, 5))
    params["beta_1"] = qnorm(us[2], 0, params["beta_0"])
    params["beta_2"] = qnorm(us[3], 0, params["beta_0"])
    params["sigma"] = exp(qnorm(us[4], log(params["beta_0"]),1))
    ### 2B, C
    
    
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
    # logL = sum(dexp(x = data$x, rate = params["theta"], log = TRUE))
    ### 1
  
    ### 2A
    # mu = params["beta_0"] + params["beta_1"]*colorado_df$t
    # logL = sum(dnorm(x = colorado_df$y, mean = mu, sd = params["sigma"], log = TRUE))
    ### 2A
    
    ### 2B
    # mu = params["beta_0"] + params["beta_1"]*colorado_df$t + (colorado_df$t >= 0.875)*params["beta_2"]*(colorado_df$t - 0.875)
    # logL = sum(dnorm(x = colorado_df$y, mean = mu, sd = params["sigma"], log = TRUE))
    ### 2B
    
    ### 2C
    mu = params["beta_0"] + params["beta_1"]*colorado_df$t + params["beta_2"]*(colorado_df$t)^2
    logL = sum(dnorm(x = colorado_df$y, mean = mu, sd = params["sigma"], log = TRUE))
    ### 2C
    
    return(logL)
}

