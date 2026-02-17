library(future)
library(brms)
library(dplyr)
library(rstan)

rm(list = ls())

### Load data
load("data.RData") 

#-------------------------------------------------------------------------------
# Models
#-------------------------------------------------------------------------------

### Set the number of cores used
options(mc.cores = parallel::detectCores())

### Save models into files
rstan_options(auto_write = TRUE) 
rstan_options(threads_per_chain = 4)

### Set seed for reproducibility
options(future.seed = TRUE)

# Setup for parallel computing
plan(multisession)


n_iter <- 2000
n_warmup <- 1000

n_chains <- 4


run_models <- function() {
  
  # Model for personal trips
  model_m0_final <- brm(
    
    bf(Euros ~ 
         Main_destination +
         (1 | Country_of_residence) +
         mo(Length_of_stay) +
         Overnight_stays_in_secondary_destinations +
         Purpose_of_the_trip + 
         Quarter + 
         Accommodation + 
         Travel_group + 
         First_reservation + 
         Age_group + 
         Gender + 
         Mode_of_transportation + 
         Experienced_nature + 
         Experienced_sports + 
         Experienced_wellbeing + 
         Experienced_culture + 
         Experienced_city_life + 
         Experienced_events + 
         Experienced_shopping + 
         Experienced_road_trip +
         Over_50km_trips, 
       shape ~ 1
    ),
    data = data_m0,
    iter = n_iter,
    warmup = n_warmup,
    chains = n_chains,
    prior = 
      c(
        set_prior("normal(0, 0.5)", class = "b"),
        set_prior("normal(0, 2)", class = "Intercept"),
        set_prior("student_t(3, 0, 1)", class = "sd"),
        set_prior("dirichlet(2)", class = "simo", 
                  coef = "moLength_of_stay1")
      ),
    family = Gamma(link = "log"),
    control = list(adapt_delta = 0.99),
    future = TRUE,
    file = paste0("model_m0_final")
    )

  # Model for work-related trips
  
  model_m1_final <- brm(
    bf(Euros ~ 
         Main_destination +  
         (1 | Country_of_residence) +
         mo(Length_of_stay) +
         Purpose_of_the_trip + 
         Quarter + 
         Age_group + 
         Gender,
       shape ~ 1
    ),
    data = data_m1,
    iter = n_iter,
    warmup = n_warmup,
    chains = n_chains,
    prior = 
      c(
        set_prior("normal(0, 0.5)", class = "b"),
        set_prior("normal(0, 2)", class = "Intercept"),
        set_prior("student_t(3, 0, 1)", class = "sd"),
        set_prior("dirichlet(2)", class = "simo", 
                  coef = "moLength_of_stay1")
      ),
    family = Gamma(link = "log"),
    control = list(adapt_delta = 0.99),
    future = TRUE,
    file = paste0("model_m1_final")
    )
  
  # Models for the length of stay (for sensitivity analysis)
  model_m1_LOS_final <- brm(
    bf(Length_of_stay ~ 
         Main_destination +
          (1 | Country_of_residence) +
         Purpose_of_the_trip + 
         Quarter + 
         Age_group + 
         Gender,
       shape ~1
    ),
    data = data_m1,
    iter = n_iter,
    warmup = n_warmup, 
    chains = n_chains,
    prior = 
      c(set_prior("normal(0, 2)", class = "b")),
    family = Gamma(link = "log"),
    control = list(adapt_delta = 0.99),
    future = TRUE,
    file = paste0("model_m1_LOS_final")
  )
  
  model_m0_LOS_final <- brm(
    bf(Length_of_stay ~ 
         Main_destination +
         (1 | Country_of_residence) +
         Overnight_stays_in_secondary_destinations +
         Purpose_of_the_trip + 
         Quarter + 
         Accommodation + 
         Travel_group + 
         First_reservation + 
         Age_group + 
         Gender + 
         Mode_of_transportation + 
         Experienced_nature + 
         Experienced_sports + 
         Experienced_wellbeing + 
         Experienced_culture + 
         Experienced_city_life + 
         Experienced_events + 
         Experienced_shopping + 
         Experienced_road_trip +
         Over_50km_trips,
       shape ~1
    ),
    data = data_m0,
    iter = n_iter,
    warmup = n_warmup,
    chains = n_chains,
    prior = 
      c(set_prior("normal(0, 2)", class = "b")),
    family = Gamma(link = "log"),
    control = list(adapt_delta = 0.99),
    future = TRUE,
    file = paste0("model_m0_LOS_final")
  )

}

# Record the calculating time
start_time <- Sys.time()

# Run models parallel
models %<-% run_models() %seed% TRUE
models 

stop_time <- Sys.time()
print(stop_time - start_time)

# Models
model_m0_final <- readRDS(".../path/model_m0_final.rds")
model_m1_final <- readRDS(".../path/model_m1_final.rds")

## Rows used
model_m0_rows <- as.numeric(
  rownames(model.frame(
    model_m0_final
    ))
  )

model_m1_rows <- as.numeric(
  rownames(model.frame(
    model_m1_final
  ))
)

## Save the datas
data_M0_model <- data_m0[model_m0_rows, ]
data_M1_model <- data_m1[model_m1_rows, ]

save(data_M0_model, file = "data_M0_final.RData")
save(data_M1_model, file = "data_M1_final.RData")


model_m0_LOS_final <- readRDS(".../path/model_m0_LOS_final.rds")
model_m1_LOS_final <- readRDS(".../path/model_m1_LOS_final.rds")

# Rows used
model_m0_LOS_rows <- as.numeric(
  rownames(model.frame(
    model_m0_LOS_final
  ))
)

model_m1_LOS_rows <- as.numeric(
  rownames(model.frame(
    model_m1_LOS_final
  ))
)

# Save the data
data_M0_LOS_model <- data_m0[model_m0_LOS_rows, ]
data_M1_LOS_model <- data_m1[model_m1_LOS_rows, ]

save(data_M0_LOS_model, file = "data_M0_LOS_model.RData")
save(data_M1_LOS_model, file = "data_M1_LOS_model.RData")

