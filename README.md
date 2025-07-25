# Care-Markets
 
Welcome to a holding repository for reproduction files for a research paper on children's home ownership, quality and location.

# How to use?

## Coders

The folder is structured with (hopefully) fully replicable code. Data and each figure are produced from separate functions in the Code folder.

For example... to create the data simply run in R...

```

  source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")
  
  df <- create_home_data()

```

The file in the folder Manuscript code - is a simple way to replicate all the results with 10 lines of code, running all the pre-prepared functions.

## Non-coders

For non-coders or R users, a final version of the data used for all tables and figures is provided in the Final Data folder.

# Notes and considerations

 - Some underlying data is not shareable, and so for some variables like profits and ownership, the data provided here is pre-aggregated.

 - The code uses Pacman to make it as reproducible as possible - but therefore if you run the code, expect many R packages to be downloaded onto your machines.

 - Table 2 and figure 3 involve heavy models, and take over 30 minutes to run on my little laptop! be warned!


Please reach out to b.goodair@lse.ac.uk for questions - I always want to improve my work (and fix my broken code!) :)
