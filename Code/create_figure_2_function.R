

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

p_map <- ggplot(map_df) +
  geom_sf(aes(fill = O_E), colour = "grey30", size = 0.1) +
  scale_fill_gradient2(
    name   = "O / E ratio",
    low    = "#2c7fb8",    # teal
    mid    = "#f7f7f7",    # light grey
    high   = "#d7301f",    # coral/red
    midpoint = 1,
    limits = c(0, 3),
    oob    = scales::squish
  ) +
  labs(
    title   = "",
    subtitle = "Map of O:E",
    caption = "Expected calculated from Bayesian Poission model\nAssumes that all areas need the same number of places per child"
  ) +
  theme_bw()+
  theme(plot.title      = element_text(face = "bold"),
  plot.subtitle   = element_text(size = 9) ,
  legend.position = "bottom"
)

# print or save
# ggsave("Figure2_choropleth_OE.png", p_map, width=8, height=6)

region_data <- region_data %>%
  mutate(
    z95     = qnorm(0.975),
    z998    = qnorm(0.999),
    lower95  = 1 - z95  / sqrt(E),
    upper95  = 1 + z95  / sqrt(E),
    lower998 = 1 - z998 / sqrt(E),
    upper998 = 1 + z998 / sqrt(E),
    outlier  = case_when(
      O_E > upper998 ~ "High",
      O_E < lower998 ~ "Low",
      TRUE           ~ "Normal"
    )
  )



region_data$outlier <- factor(region_data$outlier, levels = c("High", "Normal", "Low"))

library(ggplot2)
library(scales)
library(ggrepel)
library(grid)    # for unit()

# find a good x‑position (just inside the left margin)
x0 <- min(region_data$E) * 40.45

# pick the corresponding y for the 99.8% lines at that x
y_hi <- region_data$upper998[which.min(region_data$E)]
y_lo <- region_data$lower998[which.min(region_data$E)]

p_funnel_clean <- ggplot(region_data, aes(x = E, y = O_E)) +
  # funnel limits
  geom_line(aes(y = upper95),  linetype = "dashed") +
  geom_line(aes(y = lower95),  linetype = "dashed") +
  geom_line(aes(y = upper998), linetype = "solid") +
  geom_line(aes(y = lower998), linetype = "solid") +
  # points colored by outlier status
  geom_point(aes(col = outlier), size = 1.4) +
  # labels only for true outliers
  geom_text_repel(
    data = subset(region_data, O_E > 3 | O_E < 0.3),
    aes(label = Local.authority),
    size         = 1.5,
    max.overlaps = 15,
    box.padding  = 0.3,
    point.padding= 0.4
  ) +
  # annotate the top/bottom limits at left margin
  annotate(
    "text",
    x      = x0,
    y      = y_hi,
    label  = "More than expected\nchildren's home places",
    colour = "red",
    hjust  = 0,           # flush‐left
    size   = 2
  ) +
  annotate(
    "text",
    x      = x0,
    y      = y_lo,
    label  = "Fewer than expected\nchildren's home places",
    colour = "blue",
    hjust  = 0,
    size   = 2
  ) +
  # extend x‑axis so labels fit
  expand_limits(x = max(region_data$E) * 1.15) +
  # manual colours
  scale_colour_manual(
    values = c("High"   = "red",
               "Normal" = "black",
               "Low"    = "blue"),
    guide  = guide_legend(title = "O/E ratio")
  ) +
  scale_x_continuous("Expected count of places (E)", labels = comma) +
  scale_y_continuous("Observed/Expected (O/E)", limits = c(0, NA)) +
  labs(
    title    = "Local Authority observed to expected ratios",
    subtitle = "Funnel plot: dashed = 95% limits; solid = 99.8% limits",
    caption = " \n "
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(size = 9)  # more space on right
  )





plot<- cowplot::plot_grid(p_funnel_clean,p_map, rel_widths = c(0.42,0.58))

#ggsave(plot=plot, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_2_revised.jpeg", width=11, height=6, dpi=600)
}
