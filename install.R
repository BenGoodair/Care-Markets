# Enhanced install.R for Binder - ensures brms and gtsummary work properly
options(repos = c(CRAN = "https://cran.rstudio.com/"))
options(Ncpus = 1)  # Prevent memory issues during compilation

# Function to safely install packages with better error handling
safe_install <- function(pkg, method = "cran") {
  cat("Attempting to install", pkg, "...\n")
  
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    tryCatch({
      if (method == "source") {
        install.packages(pkg, dependencies = TRUE, type = "source")
      } else {
        install.packages(pkg, dependencies = TRUE)
      }
      library(pkg, character.only = TRUE)
      cat("✓", pkg, "installed successfully\n")
      return(TRUE)
    }, error = function(e) {
      cat("✗", pkg, "installation failed:", conditionMessage(e), "\n")
      return(FALSE)
    })
  } else {
    cat("✓", pkg, "already installed\n")
    return(TRUE)
  }
}

# Install renv first if using it
if (file.exists("renv.lock")) {
  safe_install("renv")
  
  # Try to restore renv environment
  tryCatch({
    cat("Attempting to restore renv environment...\n")
    renv::restore(prompt = FALSE)
    cat("renv restore completed successfully!\n")
  }, error = function(e) {
    cat("renv restore failed, falling back to manual installation\n")
    cat("Error:", conditionMessage(e), "\n")
  })
}

# Install pacman first
safe_install("pacman")

# Core packages that should install easily
basic_packages <- c(
  "curl", "httr", "jsonlite", "xml2", "rvest",
  "dplyr", "readr", "readxl", "stringr", "lubridate", 
  "forcats", "purrr", "tibble", "tidyr",
  "ggplot2", "scales", "viridis", "RColorBrewer",
  "knitr", "rmarkdown", "DT", "plotly"
)

# Install basic packages first
cat("Installing basic packages...\n")
for (pkg in basic_packages) {
  safe_install(pkg)
}

# Special handling for tidyverse (can be problematic in Binder)
cat("Installing tidyverse...\n")
if (!safe_install("tidyverse")) {
  cat("tidyverse failed, but core tidyverse packages should be available\n")
}

# Now handle the problematic packages with special care

# 1. Install Stan ecosystem for brms
cat("\n=== Installing Stan ecosystem for brms ===\n")

# Set environment for Stan compilation
Sys.setenv("MAKEFLAGS" = "-j1")  # Single-threaded compilation to save memory
Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "FALSE")

# Install V8 first (required for Stan)
if (!safe_install("V8")) {
  cat("V8 installation failed - trying alternative approach\n")
  system("apt-get update && apt-get install -y libv8-dev", ignore.stderr = TRUE)
  safe_install("V8")
}

# Install Stan dependencies in order
stan_deps <- c("StanHeaders", "RcppEigen", "RcppParallel", "inline")
for (pkg in stan_deps) {
  safe_install(pkg)
}

# Install rstan with special configuration
cat("Installing rstan...\n")
if (!require("rstan", quietly = TRUE)) {
  tryCatch({
    # Try with source installation and specific configuration
    install.packages("rstan", 
                     type = "source",
                     configure.args = "--with-eigen=/usr/include/eigen3")
    library("rstan")
    cat("✓ rstan installed successfully\n")
  }, error = function(e) {
    cat("✗ rstan installation failed:", conditionMessage(e), "\n")
    cat("Trying alternative rstan installation...\n")
    
    # Alternative approach
    tryCatch({
      install.packages("rstan")
      library("rstan")
      cat("✓ rstan installed with alternative method\n")
    }, error = function(e2) {
      cat("✗ All rstan installation attempts failed\n")
    })
  })
}

# Install brms if rstan succeeded
cat("Installing brms...\n")
if (require("rstan", quietly = TRUE)) {
  brms_success <- safe_install("brms")
  if (brms_success) {
    cat("🎉 brms installed successfully!\n")
  }
} else {
  cat("⚠️  Skipping brms installation (rstan not available)\n")
}

# 2. Install gtsummary and dependencies
cat("\n=== Installing gtsummary ecosystem ===\n")

# Install gtsummary dependencies first
gtsummary_deps <- c(
  "broom", "broom.helpers", "cards", "cli", "glue",
  "rlang", "vctrs", "withr", "gt", "flextable"
)

for (pkg in gtsummary_deps) {
  safe_install(pkg)
}

# Install gtsummary
gtsummary_success <- safe_install("gtsummary")
if (gtsummary_success) {
  cat("🎉 gtsummary installed successfully!\n")
}

# 3. Install plotting packages that can be problematic
cat("\n=== Installing specialized plotting packages ===\n")

# cowplot often has dependency issues in Binder
cat("Installing cowplot...\n")
if (!safe_install("cowplot")) {
  cat("cowplot failed with standard method, trying alternatives...\n")
  
  # Try installing dependencies first
  cowplot_deps <- c("gtable", "grid")
  for (dep in cowplot_deps) {
    safe_install(dep)
  }
  
  # Try again
  if (!safe_install("cowplot")) {
    cat("⚠️  cowplot installation failed - you can use gridExtra as alternative\n")
    safe_install("gridExtra")  # Alternative for plot arrangements
  }
}

# Install other potentially problematic plotting packages
plotting_packages <- c("patchwork", "gridExtra", "egg")
for (pkg in plotting_packages) {
  safe_install(pkg)
}

# Final verification
cat("\n=== Final Package Verification ===\n")
critical_packages <- c("brms", "gtsummary", "dplyr", "ggplot2")
plotting_packages_check <- c("cowplot", "gridExtra", "patchwork")

all_working <- TRUE
for (pkg in critical_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "is working\n")
  } else {
    cat("✗", pkg, "is NOT working\n")
    all_working <- FALSE
  }
}

# Check plotting packages
cat("\nPlotting packages status:\n")
for (pkg in plotting_packages_check) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "is working\n")
  } else {
    cat("○", pkg, "is not available (but may not be critical)\n")
  }
}

if (all_working) {
  cat("\n🎉 All critical packages installed successfully!\n")
} else {
  cat("\n⚠️  Some packages failed to install. Check the log above for details.\n")
}

# Use pacman to load all successfully installed packages
tryCatch({
  pacman::p_load(char = c(basic_packages, "brms", "gtsummary"), 
                 install = FALSE,  # Don't try to install again
                 character.only = TRUE)
}, error = function(e) {
  cat("Some packages couldn't be loaded with pacman, but may still be available\n")
})

cat("\nPackage installation process completed!\n")

# Clean up memory
gc()