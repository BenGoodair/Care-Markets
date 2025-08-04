# Install required R packages for Binder
if (!require("pacman")) install.packages("pacman")

# Use pacman to install and load packages
pacman::p_load(
  # Add all your required packages here
  tidyverse,
  ggplot2,
  dplyr,
  readr,
  stringr,
  lubridate,
  # Add any other packages your code uses
  rmarkdown,
  knitr,
  DT,
  plotly
)

# If you have specific package versions in renv.lock, consider using renv
if (file.exists("renv.lock")) {
  if (!require("renv")) install.packages("renv")
  renv::restore()
}