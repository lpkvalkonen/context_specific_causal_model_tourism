#-------------------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------------------

library(dplyr)
library(brms)
library(posterior)
library(ggplot2)


#-------------------------------------------------------------------------------
# Notations (Cinelli & Hazlett (2020))
#   * Y = expenditure (Y)
#   * X = length of stay (D)
#   * I = income (Z)


# Set seed
set.seed(2425)



#-------------------------------------------------------------------------------
# Load models
#-------------------------------------------------------------------------------
# * Full model
# * Model for the length of stay

### Load the model and data
### Load the model and data
#model_m0 <- readRDS(".../path/model_m0_final.rds")
#model_m1 <- readRDS(".../path/model_m1_final.rds")

load("data_M0_final.RData")
load("data_M1_final.RData")

model_m0_LOS <- readRDS(".../path/model_m0_LOS_final.rds")
model_m1_LOS <- readRDS(".../path/model_m1_LOS_final.rds")

load("data_M0_LOS_model.RData")
load("data_M1_LOS_model.RData")


#-------------------------------------------------------------------------------
### Funciton to calculate bias
#-------------------------------------------------------------------------------

calc_bias <- function(model, model_X, data, data_X, M) {

  #-------------------------------------------------------------------------------
  # Posterior samples into data frame
  #-------------------------------------------------------------------------------
  post_samples <- brms::as_draws(model) %>% as_draws_df()
  
  # Number of posterior samples
  n_post <- nrow(post_samples)
  
  
  
  #-------------------------------------------------------------------------------
  # Priors for correlations: cor(I,Y) and cor(I,X)
  #-------------------------------------------------------------------------------
  
  if(M==0) {
    # effect of I on X (linear)
    effect_I_X <- rnorm(n_post, 0.1, 0.15)

    # effect of I on Y (linear)
    effect_I_Y <- rnorm(n_post, 0.45, 0.1)
  }
  if(M==1) {
    # effect of I on X (linear)
    effect_I_X <- rnorm(n_post, 0.05, 0.15)

    # effect of I on Y (linear)
    effect_I_Y <- rnorm(n_post, 0.2, 0.1)

  }
  
  # Fisher's z-transformation
  inv_fisher_transf <- function(z){
    r <- (exp(2*z)-1)/(exp(2*z)+1)
    r
  }
  
  
  # Correlation between I and X
  corr_I_X <- inv_fisher_transf(effect_I_X)

  # Correlation between I and Y
  corr_I_Y <- inv_fisher_transf(effect_I_Y)

  # Append into data frame
  post_samples$corr_I_Y <- corr_I_Y
  post_samples$corr_I_X <- corr_I_X
  
  
  #-------------------------------------------------------------------------------
  # Cinelli & Hazlett (2020)
  #-------------------------------------------------------------------------------
  # Formula (8) p.48
  
  
  # R^2_y~z|d,x
  r2_y_zdx <- post_samples$corr_I_Y^2

  # R^2_d~z|x
  r2_d_zx <- post_samples$corr_I_X^2

  # sd(Y_xd)
  res <- residuals(model)
  sd_y_xd <- sd(res[,1])
  
  # sd(D_x)
  res2 <- residuals(model_X)
  sd_d_x <- sd(res2[,1])
  
  
  # Bias (formula (8))
  bias <- sign(post_samples$corr_I_Y*post_samples$corr_I_X)*sqrt(
    ((r2_y_zdx*r2_d_zx)/(1-r2_d_zx)))*(sd_y_xd/sd_d_x)
  
  return(bias)
  
}

bias_M0 <- calc_bias(model_m0, model_m0_LOS, data_M0_model, data_M0_LOS_model, M=0)
hist(bias_M0, breaks = 20)
summary(bias_M0)
quantile(bias_M0, probs = c(0.025,0.975))

bias_M1 <- calc_bias(model_m1, model_m1_LOS, data_M1_model, data_M1_LOS_model, M=1)
hist(bias_M1, breaks = 20)
summary(bias_M1)
quantile(bias_M1, probs = c(0.025,0.975))


save(bias_M0, file = "bias_M0.RData")
save(bias_M1, file = "bias_M1.RData")
