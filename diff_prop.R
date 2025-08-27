

library(tidyverse)


power.prop.test(p1=0.3, p2=0,power=0.8, alternative = "two.sided")



# simulate
# one-sample prop.test (expected change vs 0)

# data


set.seed(986976)

reader_data <- function(rdr = "A", n = 100){
  df <- tibble(pt_id = paste0("id", 1:n),
               scan = "x",
               hist = sample(c("x", "y"), n, prob = c(0.66, 0.33), replace = T),
               change = NA,
               reader = rdr
  ) |> 
    mutate(across(change, ~if_else(scan == hist, "no", "yes"))) 
  
  return(df)
  
}



x_tests <- function(x = 3, nn = 100){
 # x =  nr of readers

ll <- lapply(LETTERS[1:x], reader_data, n = nn)

dta <- ll |> 
  bind_rows() |> 
  arrange(pt_id, reader)

dta$change <- factor(dta$change, levels = c("no", "yes"))

# combined readers result

dta_combo <- dta |> 
  group_by(pt_id) |> 
  count(change) |> 
  pivot_wider(names_from = change, values_from = n) |> 
  mutate(across(c(yes, no), ~if_else(is.na(.x), 0, .x)),
                change_combo = if_else(yes > no, "yes", "no")) |> 
  ungroup()
  
tmp <- dta_combo |> 
  count(change_combo) |> 
  mutate(tot = sum(n),
         prop = n/tot) |> 
  filter(change_combo == "yes")


# start here
tmp <- prop.test(tmp$n, tmp$tot, p = NULL, alternative = "two.sided", correct = TRUE)

res_test <- tibble(est = tmp$estimate,
       ci_lwr = tmp$conf.int[1],
       ci_upr = tmp$conf.int[2],
       p_val = tmp$p.value)

return(res_test)

}

set.seed(938373)
x <- seq(40, 140, by = 10)
gr <- expand_grid(it = 1:1000, x)

ll <- map2(gr$x, gr$it,
    ~x_tests(x = 3, nn = .x) |> 
      mutate(nn = .x,
             it = .y))


ll |> 
  bind_rows()








