

# design that is going to use mixed modelling
# simulation approach


# design
# 3 readers read a diagram, some based on a scan, some based on histology
# each reader defines a surgery plan according to the diagram
# the outcome is proportion of scans that differ between scan diagram and histology diagram


# will analyse with a logistic regression with mixed effects, 
# both reader and scan will be random effects, as below

library(lme4)
# 
# model <- glmer(outcome ~ predictor1 + (1 | reader) + (1 | scan_id), 
#                data = your_data_frame, 
#                family = binomial(link = "logit"))


# create data for simulation

set.seed(938373)

# START HERE
tibble(pt_id = paste0("id", 1:100),
       scan = "x",
       hist = sample(c("x", "y"), 100, prob = c(0.66, 0.33), replace = T),
       change = NA,
       reader = "A"
       ) |> 
  mutate(across(change, ~if_else(scan == hist, "no", "yes")))




