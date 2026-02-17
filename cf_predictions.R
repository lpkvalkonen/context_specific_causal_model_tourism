### Load packages
library(brms)
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bayesplot)
library(tidybayes)
library(brmsmargins)
library(xtable)
library(scales)


### Load the model and data
model_m0 <- readRDS(".../path/model_m0_final.rds")
model_m1 <- readRDS(".../path/model_m1_final.rds")

load("data_M0_final.RData")
load("data_M1_final.RData")

model_m0_LOS <- readRDS(".../path/model_m0_LOS_final.rds")
model_m1_LOS <- readRDS(".../path/model_m1_LOS_final.rds")

load("data_M0_LOS_model.RData")
load("data_M1_LOS_model.RData")


### Load bias
load("bias_M0.RData")
load("bias_M1.RData")

#-------------------------------------------------------------------------------

# Average causal effect
cf_predictions <- function(data, model, bias, M){  

  # Number of posterior draws
  n_draws <- ndraws(model)
  
  # Initialize the data frame where the results are saved
  effects <- data.frame(expand.grid(
    Length_of_stay = seq(1, 14),
    x0 = NA,
    x1 = NA,
    mean_xdiff = NA,
    lwr_xdiff = NA,
    upr_xdiff = NA)
  )
  
  # Iterate over 'effects'
  for(i in 1:nrow(effects)) {
    
    LOS <- effects$Length_of_stay[i]
    
    # Subset data
    data_filt_x0 <- data[data$Length_of_stay == LOS, ]
    
    # If the length is zero, skip
    if(nrow(data_filt_x0) == 0) {
      next
    }
    
    # Subset data for LOS+1
    data_filt_x1 <- data_filt_x0
    data_filt_x1$Length_of_stay <- data_filt_x1$Length_of_stay + 1
    
    # Calculate weights
    w <- data_filt_x0$PainoKk
    
    # Normalize the weights within LOS
    w_norm <- w / sum(w)

    # Calculate conditional expected values
    pred_x0 <- posterior_epred(model, 
                              newdata = data_filt_x0,
                              re_formula = NA) 

    # For increased x values 
    pred_x1 <- posterior_epred(model, 
                               newdata = data_filt_x1,
                               re_formula = NA)


    # Apply weights
    means_x0 <- pred_x0 %*% w_norm
    means_x1 <- pred_x1 %*% w_norm

    # difference
    means_xdiff <- (means_x1 - means_x0)

    # Add bias to correct the ACE
    if(bias) {
      if(M==0) {
        means_xdiff <- means_xdiff + bias_M0
      }
      if(M==1) {
        means_xdiff <- means_xdiff + bias_M1
      }
    }
    
    # average over posterior samples
    effects$mean_xdiff[i] <- mean(means_xdiff)

    # Posterior quantiles
    effects$lwr_xdiff[i] <- quantile(means_xdiff, 0.025)
    effects$upr_xdiff[i] <- quantile(means_xdiff, 0.975)
    
    effects$x0[i] <- means_x0 %>% mean()
    effects$x1[i] <- means_x1 %>% mean()

  }
  return(list(effects = effects))
}


# ACE without OVB
ace_M0 <- cf_predictions(data_M0_model, model_m0, bias = FALSE, M = NULL)
ace_M1 <- cf_predictions(data_M1_model, model_m1, bias = FALSE, M = NULL)

# ACE with OVB
ace_M0_bias <- cf_predictions(data_M0_model, model_m0, bias = TRUE, M = 0)
ace_M1_bias <- cf_predictions(data_M1_model, model_m1, bias = TRUE, M = 1)
