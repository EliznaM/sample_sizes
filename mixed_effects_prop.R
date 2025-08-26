

# design that is going to use mixed modelling
# simulation approach


# design
# 3 readers read a diagram, some based on a scan, some based on histology
# each reader defines a surgery plan according to the diagram
# the outcome is proportion of scans that differ between scan diagram and histology diagram
# problem - THERE IS NO PREDICTOR FOR THE MODEL

# Question: is it different from a coin flip?

# will analyse with a logistic regression with mixed effects, 
# both reader and scan will be random effects, as below
library(tidyverse)
library(lme4)
# 
# model <- glmer(outcome ~ predictor1 + (1 | reader) + (1 | scan_id), 
#                data = your_data_frame, 
#                family = binomial(link = "logit"))


# create data for simulation

set.seed(986976)
age <- sample(40:70, 100, replace = TRUE)

# START HERE

reader_data <- function(rdr = "A"){
       df <- tibble(pt_id = paste0("id", 1:100),
       scan = "x",
       age = age,
       hist = sample(c("x", "y"), 100, prob = c(0.66, 0.33), replace = T),
       change = NA,
       reader = rdr
       ) |> 
  mutate(across(change, ~if_else(scan == hist, "no", "yes"))) 
       
       return(df)

}

set.seed(938373)
x <- 3 # nr of readers

ll <- lapply(LETTERS[1:x], reader_data)

dta <- ll |> 
  bind_rows() |> 
  arrange(pt_id, reader)

dta$change <- factor(dta$change, levels = c("no", "yes"))

# model

fit <- glmer(change ~ age + (1 | reader) + (1 | pt_id), 
                              data = dta, 
                              family = binomial(link = "logit"))

str(fit)
coefficients(fit)

