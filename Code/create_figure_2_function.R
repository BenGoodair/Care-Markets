

create_figure_2 <- function(){
  
  if (!require("pacman")) install.packages("pacman")
  
  
  pacman::p_load(dplyr, tidyr, brms, sf, ggplot2, scales)
  
  
  source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")
  
  mlm  <- create_home_data()



library(dplyr)        # data wrangling  
library(tidyr)        # data wrangling  
library(brms)         # Bayesian multilevel modeling  
library(sf)           # spatial data  
library(ggplot2)      # plotting  
library(scales)       # axis formatting  


la_shp   <-  st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2022_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")%>%
  dplyr::rename(Local.authority = CTYUA22NM)%>%
  dplyr::filter(Local.authority!="Wales",
                Local.authority!="Scotland",
                grepl('^E', CTYUA22CD))%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())

obs_la <- mlm %>%
  dplyr::filter(closed==0)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(O = sum(Places, na.rm=T))%>%
  dplyr::ungroup()


region_data <- obs_la %>%
  left_join(
    mlm %>%
      group_by(Local.authority) %>%
      summarise(
        children_in_care_s            = mean(children_in_care_s,            na.rm=TRUE)      ),
    by = "Local.authority"
  )

count_priors <- c(
  prior(normal(0, 1), class = "b"),            # slopes
  prior(student_t(3, 0, 2.5), class = "Intercept"),  # intercept
  prior(student_t(3, 0, 2.5), class = "sd")         # random‐effect SD
)

# 4a. Poisson  
model_poisson <- brm(
  bf(O ~ children_in_care_s 
     + (1 | Local.authority)
  ),
  data    = region_data,
  family  = poisson(),
  prior   = count_priors,
  cores   = 4,
  iter    = 2000,
  control = list(adapt_delta = 0.95)
)



# Choose best (e.g. model_negbin) for E

fitted_E <- fitted(
  model_poisson,        # or model_poisson
  newdata    = region_data,
  re_formula = NA,     # marginalise over LA random effects
  scale      = "response"
)

region_data <- region_data %>%
  mutate(
    E   = fitted_E[, "Estimate"],
    O_E = O / E
  )

#–– 7. Summary Statistics of O:E  
summary_stats <- region_data %>%
  summarise(
    median_OE = median(O_E, na.rm=T),
    IQR_OE    = IQR(O_E, na.rm=T),
    min_OE    = min(O_E, na.rm=T),
    max_OE    = max(O_E, na.rm=T)
  )
print(summary_stats)

map_df <- la_shp %>%
  left_join(region_data, by = "Local.authority")

library(scales)

map_df$O_E_log <- ifelse(map_df$O_E < 1, -log(1 / map_df$O_E), log(map_df$O_E))

# Define the log-transformed function
log_transform <- function(x) ifelse(x < 1, -log(1 / x), log(x))

# Define breaks in the original O/E scale
breaks_original <- c(0.1, 0.5, 1, 2, 3)

# Map them to the log-transformed scale for color
breaks_transformed <- log_transform(breaks_original)

p_map <- ggplot(map_df) +
  geom_sf(aes(fill = log_transform(O_E)), colour = "grey30", size = 0.1) +
  scale_fill_gradient2(
    name     = "O / E ratio",
    low      = "#045a8d",  # darker blue
    mid      = "#f7f7f7",  # light grey
    high     = "#d7301f",  # coral/red
    midpoint = 0,
    breaks   = breaks_transformed,
    labels   = breaks_original,
    limits = c(log(0.05), log(5))
  ) +
  labs(
    title    = "",
    subtitle = "Map of O:E",
    caption  = "Expected calculated from Bayesian Poisson model\nAssumes that all areas need the same number of places per child\nSee supplement for London-only map and funnel plot including outlier, Birmingham."
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    legend.position = "bottom"
  )



# print or save
# ggsave("Figure2_choropleth_OE.png", p_map, width=8, height=6)





# --- Exact Poisson funnel lines CIs for each LA --------------------

# choose alphas for funnel lines
alpha_95   <- 0.05    # 95% two-sided
alpha_998  <- 0.002   # 99.8% two-sided

# grid of expected counts to draw the funnel lines over
E_grid <- seq(from = max(0.1, min(region_data$E, na.rm = TRUE)), 
              to   = max(region_data$E, na.rm = TRUE) * 1.15, 
              length.out = 2000)

# For each E on the grid compute Poisson limits for observed counts, then convert to O/E
# Note: qpois returns integer quantiles; dividing by E gives the exact (discrete) funnel steps.
funnel_df <- data.frame(
  E = E_grid,
  lower95 = qpois(alpha_95/2,    lambda = E_grid) / E_grid,
  upper95 = qpois(1 - alpha_95/2, lambda = E_grid) / E_grid,
  lower998 = qpois(alpha_998/2,    lambda = E_grid) / E_grid,
  upper998 = qpois(1 - alpha_998/2, lambda = E_grid) / E_grid
)

# Replace infinite/NaN (if any) with small/large finite values to avoid plotting issues
funnel_df <- funnel_df %>%
  mutate(
    lower95  = ifelse(is.na(lower95) | is.infinite(lower95), 0, lower95),
    upper95  = ifelse(is.na(upper95) | is.infinite(upper95), max(upper95, na.rm = TRUE), upper95),
    lower998 = ifelse(is.na(lower998) | is.infinite(lower998), 0, lower998),
    upper998 = ifelse(is.na(upper998) | is.infinite(upper998), max(upper998, na.rm = TRUE), upper998)
  )

# --- Per-LA exact  CIs for O/E (useful for labels / tables) -----------
# CI for Poisson mean mu given observed count O:
# lower_mu = 0.5 * qchisq(alpha/2, 2*O)       (with lower_mu = 0 when O=0)
# upper_mu = 0.5 * qchisq(1 - alpha/2, 2*(O+1))

alpha <- 0.05
O <- region_data$O
E <- region_data$E

lower_O <- ifelse(O == 0, 0, qchisq(alpha/2, 2 * O) / 2)
upper_O <- qchisq(1 - alpha/2, 2 * (O + 1)) / 2

region_data <- region_data %>%
  mutate(
    OE_lower = lower_O / E,
    OE_upper = upper_O / E,
    lower_998 = ifelse(O == 0, 0, qpois(alpha_998/2, E) / E),
    upper_998 = qpois(1 - alpha_998/2, E) / E,
    outlier = case_when(
      O_E > upper_998 ~ "High",
      O_E < lower_998 ~ "Low",
      TRUE           ~ "Normal"
    )
  )

# --- Plot: replace the previous funnel limit lines with exact Poisson lines -------
library(ggplot2)
library(ggrepel)

p_funnel_exact <- ggplot(region_data, aes(x = E, y = O_E)) +
  # exact Poisson funnel lines (95% dashed, 99.8% solid)
  geom_line(data = funnel_df, aes(x = E, y = upper95),  linetype = "dashed") +
  geom_line(data = funnel_df, aes(x = E, y = lower95),  linetype = "dashed") +
  geom_line(data = funnel_df, aes(x = E, y = upper998), linetype = "solid") +
  geom_line(data = funnel_df, aes(x = E, y = lower998), linetype = "solid") +
  
  # points colored by simple outlier marker (recompute based on exact 99.8% lines)
  # (we compute whether each observed O_E lies outside the exact 99.8% limits at that E)
  geom_point(aes(col = outlier), size = 1.6) +
  
  # labels only for extreme points
  geom_text_repel(
    data = subset(region_data %>% dplyr::filter(Local.authority != "BIRMINGHAM"), O_E > 3 | O_E < 0.3),
    aes(label = Local.authority),
    size = 2,
    max.overlaps = 15,
    box.padding  = 0.3,
    point.padding = 0.4
  ) +
  
  scale_x_continuous("Expected count of places (E)", labels = scales::comma) +
  scale_y_continuous("Observed / Expected (O/E)", trans = "log10", limits = c(0.1, NA), labels = scales::comma) +
  
  labs(
    title    = "Local Authority observed to expected ratios",
    subtitle = "Funnel plot: dashed = 95%, solid = 99.8% limits (standard Poisson limits)."  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 300))



plot<- cowplot::plot_grid(p_funnel_exact,p_map, rel_widths = c(0.42,0.58))


#ggsave(plot=plot, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_2_final.pdf", width=11, height=6, dpi=600)
}
