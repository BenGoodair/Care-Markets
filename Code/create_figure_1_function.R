create_figure_1 <- function(){
  
  if (!require("pacman")) install.packages("pacman")
  
  
  pacman::p_load(tidyverse, cowplot, viridis, lubridate)
  
  source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")
  
  df <- create_home_data()
  
  
  #### figure1 ####
  # Load necessary libraries

  
  
  # Create a full sequence of times
  all_times <- tibble(time = -607:0)
  
  # Process the start dataset
  carehomesstart <- df %>%
    mutate(
      Registration.date = as.Date(Registration.date, format = "%d/%m/%Y"),
      Join = as.Date(Join),
      location_start = coalesce(Registration.date, Join, as.Date("2014-03-15")),
      date = location_start,
      time = as.integer(time_length(difftime(date, as.Date("2023-12-01")), "months"))
    ) %>%
    distinct(URN, .keep_all = TRUE) %>%
    dplyr::select(time, Sector_merge, URN)
  
  # Count observations by group
  nobsByIdih <- carehomesstart %>%
    count(time, Sector_merge, name = "nobs")
  
  # Ensure all time periods exist for each Sector_merge
  nobsBySector <- nobsByIdih %>%
    complete(Sector_merge, time = all_times$time, fill = list(nobs = 0)) %>%
    group_by(Sector_merge) %>%
    mutate(cumulative = cumsum(nobs)) %>%
    ungroup()
  
  # Process the end dataset
  carehomesend <- df %>%
    filter(Leave != "") %>%
    mutate(
      date = as.Date(Leave),
      time = as.integer(time_length(difftime(date, as.Date("2023-12-01")), "months"))
    ) %>%
    distinct(URN, .keep_all = TRUE) %>%
    dplyr::select(time, Sector_merge, URN)
  
  # Count observations by group
  nobsEndByIdih <- carehomesend %>%
    count(time, Sector_merge, name = "nobs")
  
  # Ensure all time periods exist for each Sector_merge
  nobsEndBySector <- nobsEndByIdih %>%
    complete(Sector_merge, time = all_times$time, fill = list(nobs = 0)) %>%
    group_by(Sector_merge) %>%
    mutate(cumulative_end = cumsum(nobs)) %>%
    ungroup()
  
  # Merge start and end datasets and compute the running total
  nobser <- full_join(nobsBySector, nobsEndBySector, by = c("Sector_merge", "time")) %>%
    mutate(
      cumulative = replace_na(cumulative, 0),
      cumulative_end = replace_na(cumulative_end, 0),
      runningsum = cumulative - cumulative_end
    )
  
  
  nobser$Sector_merge <- factor(nobser$Sector_merge, levels = c("Local Authority", "LA owned company", "Third sector", "Individual owned", "Corporate owned", "Investment owned", "Unidentified for-profit"))
  
  # Create the time-series plot
  d <- ggplot(nobser %>% filter(time > -155,
                                Sector_merge != "LA owned company",
                                Sector_merge != "Unidentified for-profit"),
              aes(x = time, y = runningsum, group = Sector_merge,
                  fill = Sector_merge, colour = Sector_merge, alpha = Sector_merge)) +
    geom_point(size = 2.5) +  # brighter dots (larger size)
    theme_bw() +
    scale_alpha_manual(values = c(0.4, 0.4, 1, 1, 1, 0.2, 0.2)) +
    scale_color_manual(values = c("#008F5D", "#F0AB00", "#E4007C", "#FF5E5E", "#5B0000")) +
    labs(x = "Year", y = "Number of children homes", 
         title = "Number of children's homes owned by ownership",
         fill = "Ownership", color = "Ownership", alpha = "Ownership") +
    scale_x_continuous(breaks = c(-12, -24, -36, -48, -60, -72, -84, -96, -108, -120, -132, -144, -156),
                       labels = c("2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011")) +
    coord_cartesian(xlim = c(-110, -22)) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold")
    )
  
  
 # ggsave(plot=d, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_1_revised.jpeg", width=9.5, height=7, dpi=600)
  
}
