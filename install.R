# Enhanced install.R for Binder - ensures all packages work properly
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Function to safely install packages
safe_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
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

# Install pacman and essential packages regardless
safe_install("pacman")

# Core packages that are commonly needed
essential_packages <- c(
  "curl",          # Often needed for web requests
  "httr",          # HTTP requests
  "jsonlite",      # JSON handling
  "xml2",          # XML parsing
  "rvest",         # Web scraping
  "tidyverse",     # Data manipulation suite
  "dplyr",         # Data manipulation
  "ggplot2",       # Plotting
  "readr",         # Reading data
  "readxl",        # Excel files
  "stringr",       # String manipulation
  "lubridate",     # Date handling
  "forcats",       # Factor handling
  "purrr",         # Functional programming
  "tibble",        # Modern data frames
  "tidyr",         # Data tidying
  "knitr",         # Document generation
  "rmarkdown",     # R Markdown
  "DT",            # Interactive tables
  "plotly",        # Interactive plots
  "scales",        # Plot scaling
  "viridis",       # Color palettes
  "RColorBrewer"   # Color palettes
)

# Install all essential packages
cat("Installing essential packages...\n")
for (pkg in essential_packages) {
  cat("Installing", pkg, "...\n")
  safe_install(pkg)
}

# Use pacman to load packages (this also installs if missing)
pacman::p_load(char = essential_packages, install = TRUE)

cat("Package installation completed!\n")
cat("Installed packages:\n")
print(essential_packages)