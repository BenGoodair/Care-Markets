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

Each function should work independently of eachother - so you don't need to produce the data first.

The file in the folder Manuscript code is a simple way to replicate all the results with 10 lines of code, running all the pre-prepared functions.

## Non-coders

For non-coders or R users, a final version of the data used for all tables and figures is provided in the Final Data folder.

## For replication

In it's current stage the packages gtsummary and brms are preventing replication via binder - I am working on a file which will produce results without using bayesian methods.

# Notes and considerations

 - Our data will have some error. The only way available to us to know the companies running children's homes is via the text reported by Ofsted inspections. Comapnies are often named with the same names as old companies, they change their names, and small typos can make it hard to identify the correct company. We have done our best to make sure it is precise - but some error is likely.
   
 - Some underlying data is not shareable, and so for some variables like profits and ownership, the data provided here is pre-aggregated.

 - The code uses Pacman to make it as reproducible as possible - but therefore if you run the code, expect many R packages to be downloaded onto your machines.

 - Table 2 and figure 3 involve some computation, and take over 10 minutes to run on my little laptop! 


Please reach out to b.goodair@lse.ac.uk for questions - I always want to improve my work (and fix my broken code!) :)
