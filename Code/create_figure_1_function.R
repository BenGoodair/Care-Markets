create_figure_1 <- function(){
  
  

source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")

df <- create_home_data()


# 0. Load required packages
library(dplyr)
library(ggplot2)
library(estimatr)    # for lm_robust()
library(emmeans)     # for emmeans & contrasts
library(cowplot)     # for plot_grid() & get_legend()

# 1. Create quartile categories for age and places
mlm2 <- mlm %>%
  mutate(
    age_q = factor(ntile(age_years, 4),
                   labels = c("Q1 (youngest)",
                              "Q2",
                              "Q3",
                              "Q4 (oldest)")),
    places_q = factor(ntile(Places, 4),
                      labels = c("Small",
                                 "Medium",
                                 "Large",
                                 "XL"))
  )

# 2. Fit two interaction models
reg_age <- lm_robust(
  overall.average ~ Sector_merge * age_q +
    number_of_inspections +
    Local.authority,
  data     = mlm2,
  clusters = Organisation
)

reg_places <- lm_robust(
  overall.average ~ Sector_merge * places_q +
    number_of_inspections +
    Local.authority,
  data     = mlm2,
  clusters = Organisation
)

# 3. Helper to get difference‐from‐Local Authority
get_diff <- function(model, by_factor) {
  # Build an emmeans formula: "~ Sector_merge | age_q" or "~ Sector_merge | places_q"
  fmla <- as.formula(paste("~ Sector_merge |", by_factor))
  
  # Get emmeans, then contrast each sector vs. Local Authority
  ctr_df <- emmeans(model, fmla) %>%
    contrast(method = "trt.vs.ctrl", ref = "Local Authority") %>%
    summary(infer = TRUE) %>%
    as.data.frame()
  
  # Rename the grouping column to "bin"
  names(ctr_df)[ names(ctr_df) == by_factor ] <- "bin"
  ctr_df
}

df_age    <- get_diff(reg_age,    "age_q")
df_places <- get_diff(reg_places, "places_q")

# Define color scheme to match your time-series plot
sector_colors <- c(
  "Third sector - Local Authority" = "#F0AB00",
  "Individual owned - Local Authority" = "#E4007C", 
  "Corporate owned - Local Authority" = "#FF5E5E",
  "Investment owned - Local Authority" = "#5B0000"
)

# 4a. Plot for age quartiles
p_age <- ggplot(df_age, aes(x = bin, y = estimate, colour = contrast)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = sector_colors) +
  labs(
    x     = "Age quartile",
    y     = "Difference vs Local Authority",
    colour = "Sector − LA",
    title = "Sector differences by Age category"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# 4b. Plot for place‐size quartiles
p_places <- ggplot(df_places, aes(x = bin, y = estimate, colour = contrast)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = sector_colors) +
  labs(
    x     = "Place‑size quartile",
    y     = "Difference vs Local Authority",
    colour = "Sector − LA",
    title = "Sector differences by Place‑size category"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# 5. Combine with cowplot
#    - extract shared legend
shared_leg <- cowplot::get_legend(p_age)

#    - remove legends from individual plots
p1 <- p_age    + theme(legend.position = "none")
p2 <- p_places + theme(legend.position = "none")

#    - assemble panels + legend
final_plot <- cowplot::plot_grid(
  cowplot::plot_grid(p1, p2, labels = c("A", "B"), ncol = 2),
  shared_leg,
  ncol        = 1,
  rel_heights = c(1, 0.1)
)

# Print the combined plot
print(final_plot)
#ggsave(plot=d, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_1_revised.jpeg", width=9.5, height=7, dpi=600)

}
