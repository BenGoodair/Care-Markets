source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")

df <- create_characteristics_data()


#### figure1 ####
# Load necessary libraries
library(tidyverse)
library(cowplot)
library(viridis)
library(lubridate)


# Create a full sequence of times
all_times <- tibble(time = -238:0)

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

# Create the time-series plot
d <- ggplot(nobser %>% filter(time > -155,
                              Sector_merge != "LA owned company",
                              Sector_merge != "Unidentified for-profit"),
            aes(x = time, y = runningsum, group = Sector_merge,
                fill = Sector_merge, colour = Sector_merge, alpha = Sector_merge)) +
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("#008F5D", "#F0AB00", "#E4007C", "#FF5E5E", "#5B0000", "#1F77B4", "#9467BD")) +
  scale_alpha_manual(values = c(0.2, 0.2, 1, 1, 1, 0.2, 0.2)) +
  labs(x = "Year", y = "Number of children homes", 
       title = "Number of children's homes owned for-profit",
       fill = "Ownership", color = "Ownership", alpha = "Ownership") +
  scale_x_continuous(breaks = c(-12, -24, -36, -48, -60, -72, -84, -96, -108, -120, -132, -144, -156),
                     labels = c("2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11, face = "bold")  ) +
  coord_cartesian(xlim = c(-110, -22)) +
  theme(legend.position = "bottom")

# Reusable function for enhanced box/violin plots
create_plot_improved <- function(data, filter_col, filter_val, title, ylab_text, var_name, ylims = NULL) {
  ggplot(
    data %>% filter({{ filter_col }} == filter_val),
    aes(x = as.factor({{ filter_col }}), y = as.numeric(get(var_name)))
  ) +
    # Violin plot shows density distribution
    geom_violin(width = 0.8, fill = "#2A6EBB", alpha = 0.3, trim = TRUE) +
    # Box plot for median and quartiles
    geom_boxplot(width = 0.15, color = "#2A6EBB", fill = "white", outlier.shape = NA) +
    # Jittered points to show individual data points
    #    geom_jitter(width = 0.1, size = 1.5, color = "black",fill="darkgrey", alpha = 0.1) +
    # Summary statistic: Mean (red diamond)
    #    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_blank()
    ) +
    ggtitle(title) +
    xlab("") +
    ylab(ylab_text) +
    (if (!is.null(ylims)) coord_cartesian(ylim = ylims) else coord_cartesian())
}

# Generate improved plots for the "profit margin %" metric
fplow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Individual owned", 
  title = "\nIndividual owned",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

forlow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE),
  Sector_merge, "Corporate owned", 
  title = "\nCorporate owned",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

investlow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE),
  Sector_merge, "Investment owned", 
  title = "\nInvestment owned",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

lalow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE),
  Sector_merge, "Local Authority", 
  title = "Profit margins by sector\nLocal Authority",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

vollow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE),
  Sector_merge, "Third sector", 
  title = "\nThird Sector",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

# Combine improved profit margin plots into one composite row
yip_improved_profit <- cowplot::plot_grid(
  lalow_improved, vollow_improved, fplow_improved, forlow_improved, investlow_improved, 
  nrow = 1
)

# If needed, join inspection data (assuming 'inspection_data' exists)
data <- data %>%
  left_join(inspection_data, by = "URN")

# Generate improved plots for the "overall quality" metric (inspection scores)
lalow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Local Authority", 
  title = "Quality by sector\nLocal Authority",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

vollow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Third sector", 
  title = "\nThird Sector",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

fplow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Individual owned", 
  title = "\nIndividual owned",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

forlow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Corporate owned", 
  title = "\nCorporate owned",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

investlow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Investment owned", 
  title = "\nInvestment owned",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

# Combine improved overall quality plots into one composite row
yip_improved_overall <- cowplot::plot_grid(
  lalow_improved_overall, vollow_improved_overall, fplow_improved_overall,
  forlow_improved_overall, investlow_improved_overall, 
  nrow = 1
)

# Combine both types of improved distribution plots with labels "D" and "E"
yip_both_improved <- cowplot::plot_grid(
  yip_improved_profit, yip_improved_overall, 
  ncol = 1, labels = c("B", "C")
)


final_plot <- cowplot::plot_grid(
  d, yip_both_improved, 
  ncol = 1, rel_heights = c(0.5, 0.5), labels = c("A", "")
)

# Display the final composite plot
print(final_plot)

ggsave(plot=final_plot, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_1.jpeg", width=12, height=14, dpi=600)





