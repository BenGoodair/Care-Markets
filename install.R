install.packages("brms", dependencies = TRUE)
install.packages(c(
  "dplyr", "tidyr", "broom.mixed",
  "kableExtra", "officer", "flextable",
  "gt", "gtsummary", "lubridate"", "ggplot2"",
  "viridis", "cowplot", "sf", "scales"
))

# Optional: if you want cmdstanr (may be easier than rstan on Binder)
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()
