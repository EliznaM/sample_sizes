


# proportion estimate
library(tidyverse)
library(samplingbook)

# estimate a proportion of around 0.3, with CI width of 5% either side of the estimate
# in function below, e = half of full width of CI

x <- seq(0.05, 0.25, by = 0.01)

ll <- lapply(x, sample.size.prop, P = 0.3)
str(ll[[1]])

plotdta <- map(ll, 
    ~tibble(half_width = .x[[1]]$e,
            n = .x$n)) |> 
  bind_rows()

plotdta |> 
  ggplot(aes(n, half_width))+
  geom_point()


