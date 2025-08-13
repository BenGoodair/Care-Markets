
source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")

mlm <- create_home_data()



####APPENDIX####

####prior diagnostics####

priors <- c(
  # Slopes for each category
  prior(normal(0, 1), class = "b", dpar = "muCorporateowned"),
  prior(normal(0, 1), class = "b", dpar = "muIndividualowned"),
  prior(normal(0, 1), class = "b", dpar = "muInvestmentowned"),
  prior(normal(0, 1), class = "b", dpar = "muThirdsector"),
  # Intercepts
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muCorporateowned"),
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muIndividualowned"),
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muInvestmentowned"),
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muThirdsector"),
  # Random effect SDs
  prior(student_t(3, 0, 2.5), class = "sd", dpar = "muCorporateowned"),
  prior(student_t(3, 0, 2.5), class = "sd", dpar = "muIndividualowned"),
  prior(student_t(3, 0, 2.5), class = "sd", dpar = "muInvestmentowned"),
  prior(student_t(3, 0, 2.5), class = "sd", dpar = "muThirdsector")
)

# Data for Model 1: Net Loss
data_netloss <- mlm %>%
  drop_na(Sector_merge, net_loss_s, children_in_care_s, Local.authority) %>%
  select(Sector_merge, net_loss_s, children_in_care_s, Local.authority, closed) %>%
  mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed)
  )
data_netloss$Sector_merge <- droplevels(data_netloss$Sector_merge)
n1 <- nrow(data_netloss)

model_multilevel <- brm(
  formula = Sector_merge ~ net_loss_s + closed + children_in_care_s + (1 | Local.authority),
  data = data_netloss,
  family = categorical(),
  prior = priors,
  cores = 4,
  iter = 2000
)

# Data for Model 3: House Price
data_house <- mlm %>%
  drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s, Local.authority) %>%
  select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s, Local.authority, closed) %>%
  mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed)
  )
data_house$Sector_merge <- droplevels(data_house$Sector_merge)
n3 <- nrow(data_house)

model_multilevel_house <- brm(
  formula = Sector_merge ~ average_house_price_per_sq_m_s + closed + children_in_care_s + (1 | Local.authority),
  data = data_house,
  family = categorical(),
  prior = priors,
  cores = 4,
  iter = 2000
)




one <- pp_check(model_multilevel, type = "bars")           # bar plot comparison for categories
two <- pp_check(model_multilevel_house, type = "bars")           # bar plot comparison for categories

ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/prior_diag_need.png", one, width = 7, height = 7, dpi = 600)
ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/prior_diag_house.png", two, width = 7, height = 7, dpi = 600)

####Simple MLM####

mlm$sector_third <- ifelse(mlm$Sector_merge=="Local Authority", 0,
                           ifelse(mlm$Sector_merge=="Third sector", 1, NA
                           ))

mlm$sector_invest <- ifelse(mlm$Sector_merge=="Local Authority", 0,
                            ifelse(mlm$Sector_merge=="Investment owned", 1, NA
                            ))

mlm$sector_corp <- ifelse(mlm$Sector_merge=="Local Authority", 0,
                          ifelse(mlm$Sector_merge=="Corporate owned", 1, NA
                          ))

mlm$sector_indiv <- ifelse(mlm$Sector_merge=="Local Authority", 0,
                           ifelse(mlm$Sector_merge=="Individual owned", 1, NA
                           ))


reg1 <- lmerTest::lmer(sector_third ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg2 <- lmerTest::lmer(sector_indiv ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm)
reg3 <- lmerTest::lmer(sector_corp ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg4 <- lmerTest::lmer(sector_invest ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm)

reg5 <- lmerTest::lmer(sector_third ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg6 <- lmerTest::lmer(sector_indiv ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm)
reg7 <- lmerTest::lmer(sector_corp ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg8 <- lmerTest::lmer(sector_invest ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm)





modelsummary(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
             stars = TRUE,
             coef_omit = "Intercept",
             output = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/simple_mlm.docx")


####plotting openings according to 2014 levels####

# Function to create plot for each ownership type
create_ownership_plot <- function(data, ownership_type, title_suffix) {
  
changeplot <- data %>%
  dplyr::select(Registration.date, Sector_merge, Local.authority, Join) %>%
  left_join(., data %>%
              dplyr::select(Local.authority, net_loss_2014) %>%
              dplyr::distinct(.keep_all = TRUE) %>%
              dplyr::mutate(needQuint = factor(ntile(net_loss_2014, 5))) %>%
              dplyr::select(-net_loss_2014)
  ) %>%
  mutate(location_start = dmy(Registration.date),
         location_start = ifelse(is.na(location_start), Join, as.character(location_start)),
         location_start = as.Date(location_start))%>%
  filter(location_start >= as.Date("2014-03-01"),
         Sector_merge == ownership_type,
         !is.na(needQuint)) %>%
    mutate(month = floor_date(location_start, "month")) %>%
    group_by(month, needQuint) %>%
    summarise(new_care_homes = n(), .groups = "drop") %>%
    complete(
      month = seq(min(month), max(month), by = "month"),
      needQuint = unique(needQuint),
      fill = list(new_care_homes = 0)
    ) %>%
    group_by(needQuint) %>%
    arrange(month) %>%
    mutate(cumulative_homes = cumsum(new_care_homes),
           needQuint = case_when(
             needQuint == 5 ~ "5 (High need)",
             needQuint == 4 ~ "4",
             needQuint == 3 ~ "3",
             needQuint == 2 ~ "2",
             needQuint == 1 ~ "1 (Low need)"
           ))
  
  zero_rows <- changeplot %>%
    group_by(needQuint) %>%
    summarise(month = min(month) - months(1), cumulative_homes = 0, .groups = "drop") %>%
    mutate(new_care_homes = 0)
  
  changeplot <- bind_rows(changeplot, zero_rows) %>%
    arrange(needQuint, month)
  
  ggplot(changeplot, aes(x = month, y = cumulative_homes, color = needQuint)) +
    geom_line(size = 1.5) +
    labs(
      title = title_suffix,
      x = NULL,
      y = "Cumulative new homes opened",
      color = "Area Need\n(Net loss, Quintile)"
    ) +
    theme_bw() +
    scale_color_viridis_d(option = "viridis") +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 12)
    )
}

ownership_types <- c("Individual owned", "Corporate owned", "Investment owned", "Local Authority", "Third sector")
display_names   <- c("Individual owned",   "Corporate owned",   "Investment owned",   "Local Authority",   "Third sector")

plots_list <- Map(function(own, lab) create_ownership_plot(mlm, own, lab),
                  ownership_types, display_names)



# 1) force your guide into a single row at the bottom
legend_plot <- ggplot(plots_list[[1]]$data, aes(month, cumulative_homes, color = needQuint)) +
  geom_line(size = 1.5, show.legend = TRUE) +
  labs(color = "Area Need\n(Net loss 2014, Quintile)") +
  theme_bw() +
  scale_color_viridis_d(option = "viridis") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.4, "cm"))

# 2) pull out the one "guide-box" grob that now definitely contains your legend
g <- ggplotGrob(legend_plot)
legend_index <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
legend <- g$grobs[[legend_index]]



# And then you just return or print final_plot as before.


# section labels
commercial_label    <- cowplot::ggdraw() + cowplot::draw_label("Commercial entities",    fontface='bold', size=14)
noncommercial_label <- cowplot::ggdraw() + cowplot::draw_label("Non commercial entities", fontface='bold', size=14)

# thick black separator as a tiny plot with a horizontal line
separator <- ggplot() +
  geom_hline(yintercept = 0, size = 2) +
  xlim(0,1) +
  theme_void() 

# stack everything vertically, with sep between C and non‑comm label
stacked_plots <- cowplot::plot_grid(
  commercial_label,
  plots_list[[1]],
  plots_list[[2]],
  plots_list[[3]],
  separator,
  noncommercial_label,
  plots_list[[4]],
  plots_list[[5]],
  ncol = 1,
  rel_heights = c(0.05, 1, 1, 1, 0.02, 0.05, 1, 1),
  labels = c("", "A", "B", "C", "", "", "D", "E"),
  label_size = 12,
  align = "v"
)

# now put legend at the bottom
final_plot <- cowplot::plot_grid(
  stacked_plots,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.08)
)

# Save the plot if needed
ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/Figure3_2014_need.png", final_plot, width=7, height=15, dpi = 600)

####opening for house prices####


create_ownership_plot <- function(data, ownership_type, title_suffix) {
  changeplot <- data %>%
    dplyr::select(Registration.date, Sector_merge, Local.authority, Join) %>%
    left_join(., data %>%
                dplyr::select(Local.authority, house_price_2014) %>%
                dplyr::distinct(.keep_all = TRUE) %>%
                dplyr::mutate(needQuint = factor(ntile(house_price_2014, 5))) %>%
                dplyr::select(-house_price_2014)
    ) %>%
    mutate(location_start = dmy(Registration.date),
           location_start = ifelse(is.na(location_start), Join, as.character(location_start)),
           location_start = as.Date(location_start)) %>%
    filter(location_start >= as.Date("2014-03-01"),
           Sector_merge == ownership_type,
           !is.na(needQuint)) %>%
    mutate(month = floor_date(location_start, "month")) %>%
    group_by(month, needQuint) %>%
    summarise(new_care_homes = n(), .groups = "drop") %>%
    complete(
      month = seq(min(month), max(month), by = "month"),
      needQuint = unique(needQuint),
      fill = list(new_care_homes = 0)
    ) %>%
    group_by(needQuint) %>%
    arrange(month) %>%
    mutate(cumulative_homes = cumsum(new_care_homes),
           needQuint = case_when(
             needQuint == 5 ~ "5 (High price)",
             needQuint == 4 ~ "4",
             needQuint == 3 ~ "3",
             needQuint == 2 ~ "2",
             needQuint == 1 ~ "1 (Low price)"
           ))
  
  zero_rows <- changeplot %>%
    group_by(needQuint) %>%
    summarise(month = min(month) - months(1), cumulative_homes = 0, .groups = "drop") %>%
    mutate(new_care_homes = 0)
  
  changeplot <- bind_rows(changeplot, zero_rows) %>%
    arrange(needQuint, month)
  
  ggplot(changeplot, aes(x = month, y = cumulative_homes, color = needQuint)) +
    geom_line(size = 1.5) +
    labs(
      title = title_suffix,
      x = NULL,
      y = "Cumulative new homes opened",
      color = "House price\n(2014, Quintile)"
    ) +
    theme_bw() +
    scale_color_viridis_d(option = "viridis") +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 12)
    )
}

ownership_types <- c("Individual owned", "Corporate owned", "Investment owned", "Local Authority", "Third sector")
display_names   <- c("Individual owned",   "Corporate owned",   "Investment owned",   "Local Authority",   "Third sector")

plots_list <- Map(function(own, lab) create_ownership_plot(mlm2, own, lab),
                  ownership_types, display_names)



# 1) force your guide into a single row at the bottom
legend_plot <- ggplot(plots_list[[1]]$data, aes(month, cumulative_homes, color = needQuint)) +
  geom_line(size = 1.5, show.legend = TRUE) +
  labs(color = "House price\n(2014, Quintile)") +
  theme_bw() +
  scale_color_viridis_d(option = "viridis") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.4, "cm"))

# 2) pull out the one "guide-box" grob that now definitely contains your legend
g <- ggplotGrob(legend_plot)
legend_index <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
legend <- g$grobs[[legend_index]]



# And then you just return or print final_plot as before.


# section labels
commercial_label    <- cowplot::ggdraw() + cowplot::draw_label("Commercial entities",    fontface='bold', size=14)
noncommercial_label <- cowplot::ggdraw() + cowplot::draw_label("Non commercial entities", fontface='bold', size=14)

# thick black separator as a tiny plot with a horizontal line
separator <- ggplot() +
  geom_hline(yintercept = 0, size = 2) +
  xlim(0,1) +
  theme_void() 

# stack everything vertically, with sep between C and non‑comm label
stacked_plots <- cowplot::plot_grid(
  commercial_label,
  plots_list[[1]],
  plots_list[[2]],
  plots_list[[3]],
  separator,
  noncommercial_label,
  plots_list[[4]],
  plots_list[[5]],
  ncol = 1,
  rel_heights = c(0.05, 1, 1, 1, 0.02, 0.05, 1, 1),
  labels = c("", "A", "B", "C", "", "", "D", "E"),
  label_size = 12,
  align = "v"
)

# now put legend at the bottom
final_plot <- cowplot::plot_grid(
  stacked_plots,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.08)
)

# Save the plot if needed
ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/Figure3_2014_house.png", final_plot, width = 7, height = 15, dpi = 600)



create_ownership_plot <- function(data, ownership_type, title_suffix) {
  changeplot <- data %>%
    dplyr::select(Registration.date, Sector_merge, Local.authority, Join) %>%
    left_join(., data %>%
                dplyr::select(Local.authority, average_house_price_per_sq_m_s) %>%
                dplyr::distinct(.keep_all = TRUE) %>%
                dplyr::mutate(needQuint = factor(ntile(average_house_price_per_sq_m_s, 5))) %>%
                dplyr::select(-average_house_price_per_sq_m_s)
    ) %>%
    mutate(location_start = dmy(Registration.date),
           location_start = ifelse(is.na(location_start), Join, as.character(location_start)),
           location_start = as.Date(location_start)) %>%
    filter(location_start >= as.Date("2014-03-01"),
           Sector_merge == ownership_type,
           !is.na(needQuint)) %>%
    mutate(month = floor_date(location_start, "month")) %>%
    group_by(month, needQuint) %>%
    summarise(new_care_homes = n(), .groups = "drop") %>%
    complete(
      month = seq(min(month), max(month), by = "month"),
      needQuint = unique(needQuint),
      fill = list(new_care_homes = 0)
    ) %>%
    group_by(needQuint) %>%
    arrange(month) %>%
    mutate(cumulative_homes = cumsum(new_care_homes),
           needQuint = case_when(
             needQuint == 5 ~ "5 (High price)",
             needQuint == 4 ~ "4",
             needQuint == 3 ~ "3",
             needQuint == 2 ~ "2",
             needQuint == 1 ~ "1 (Low price)"
           ))
  
  zero_rows <- changeplot %>%
    group_by(needQuint) %>%
    summarise(month = min(month) - months(1), cumulative_homes = 0, .groups = "drop") %>%
    mutate(new_care_homes = 0)
  
  changeplot <- bind_rows(changeplot, zero_rows) %>%
    arrange(needQuint, month)
  
  ggplot(changeplot, aes(x = month, y = cumulative_homes, color = needQuint)) +
    geom_line(size = 1.5) +
    labs(
      title = title_suffix,
      x = NULL,
      y = "Cumulative new homes opened",
      color = "House price\n(average, Quintile)"
    ) +
    theme_bw() +
    scale_color_viridis_d(option = "viridis") +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 12)
    )
}

ownership_types <- c("Individual owned", "Corporate owned", "Investment owned", "Local Authority", "Third sector")
display_names   <- c("Individual owned",   "Corporate owned",   "Investment owned",   "Local Authority",   "Third sector")

plots_list <- Map(function(own, lab) create_ownership_plot(mlm2, own, lab),
                  ownership_types, display_names)



# 1) force your guide into a single row at the bottom
legend_plot <- ggplot(plots_list[[1]]$data, aes(month, cumulative_homes, color = needQuint)) +
  geom_line(size = 1.5, show.legend = TRUE) +
  labs(color = "House price\n(average, Quintile)") +
  theme_bw() +
  scale_color_viridis_d(option = "viridis") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.4, "cm"))

# 2) pull out the one "guide-box" grob that now definitely contains your legend
g <- ggplotGrob(legend_plot)
legend_index <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
legend <- g$grobs[[legend_index]]



# And then you just return or print final_plot as before.


# section labels
commercial_label    <- cowplot::ggdraw() + cowplot::draw_label("Commercial entities",    fontface='bold', size=14)
noncommercial_label <- cowplot::ggdraw() + cowplot::draw_label("Non commercial entities", fontface='bold', size=14)

# thick black separator as a tiny plot with a horizontal line
separator <- ggplot() +
  geom_hline(yintercept = 0, size = 2) +
  xlim(0,1) +
  theme_void() 

# stack everything vertically, with sep between C and non‑comm label
stacked_plots <- cowplot::plot_grid(
  commercial_label,
  plots_list[[1]],
  plots_list[[2]],
  plots_list[[3]],
  separator,
  noncommercial_label,
  plots_list[[4]],
  plots_list[[5]],
  ncol = 1,
  rel_heights = c(0.05, 1, 1, 1, 0.02, 0.05, 1, 1),
  labels = c("", "A", "B", "C", "", "", "D", "E"),
  label_size = 12,
  align = "v"
)

# now put legend at the bottom
final_plot <- cowplot::plot_grid(
  stacked_plots,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.08)
)

# Save the plot if needed
ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/Figure3_average_house.png", final_plot, width = 7, height = 15, dpi = 600)





####Map of area need and house price####





map <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2022_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")%>%
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
                  str_trim())%>%
  dplyr::full_join(., mlm %>%
                     dplyr::select(Local.authority, net_loss_s),
                   by="Local.authority")%>%
  st_as_sf(.)




no_classes <- 6


quantiles <- quantile(as.double(map$net_loss_s), 
                      probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " - ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]




a <- map %>%  dplyr::mutate(unreg_quantiles =cut(net_loss_s, 
                                                 breaks = quantiles, 
                                                 labels = labels, 
                                                 include.lowest = T) )%>%
  ggplot(.) +
  geom_sf(aes(fill = unreg_quantiles), color = "black") +
  theme_map()+
  labs(x = NULL, 
       y = NULL)+
  scale_fill_brewer(
    palette = "OrRd",
    name = "Area need\n(net loss, average, 2014-23)", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))


ggsave(plot=a, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/map_need.jpeg", width=8, height=6, dpi=600)






map <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2022_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")%>%
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
                  str_trim())%>%
  dplyr::full_join(., mlm %>%
                     dplyr::select(Local.authority, average_house_price_per_sq_m_s),
                   by="Local.authority")%>%
  st_as_sf(.)




no_classes <- 6


quantiles <- quantile(as.double(map$average_house_price_per_sq_m_s), 
                      probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " - ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]




a <- map %>%  dplyr::mutate(unreg_quantiles =cut(average_house_price_per_sq_m_s, 
                                                 breaks = quantiles, 
                                                 labels = labels, 
                                                 include.lowest = T) )%>%
  ggplot(.) +
  geom_sf(aes(fill = unreg_quantiles), color = "black") +
  theme_map()+
  labs(x = NULL, 
       y = NULL)+
  scale_fill_brewer(
    palette = "OrRd",
    name = "House price\n(per sq m, average, 2014-23)", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))


ggsave(plot=a, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/map_house.jpeg", width=8, height=6, dpi=600)





####percent of beds in top 5 LAs####
LA_panel_data$all_places <- LA_panel_data$Places_Individual_owned+
  
  LA_panel_data$Places_Corporate_owned+
  
  LA_panel_data$Places_Investment_owned+
  
  LA_panel_data$Places_Third_sector+
  
  LA_panel_data$Places_Local_Authority+
  
  LA_panel_data$Places_Unidentified_for_profit+
  
  LA_panel_data$Places_LA_owned_company

LA_panel_data %>%
  dplyr::filter(year == 2023) %>%
  dplyr::distinct(Local.authority, .keep_all = TRUE) %>%
  dplyr::filter(all_places > 300) %>%
  dplyr::summarise(total_all_places = sum(all_places, na.rm = TRUE)) %>%
  dplyr::pull(total_all_places)

LA_panel_data %>%
  dplyr::filter(year == 2023) %>%
  dplyr::distinct(Local.authority, .keep_all = TRUE) %>%
  dplyr::filter(all_places < 300) %>%
  dplyr::summarise(total_all_places = sum(all_places, na.rm = TRUE)) %>%
  dplyr::pull(total_all_places)

2308/10127

LA_panel_data %>%
  dplyr::filter(year == 2023) %>%
  dplyr::distinct(Local.authority, .keep_all = TRUE) %>%
  dplyr::filter(all_places > 300) %>%
  dplyr::summarise(children_in_care = sum(children_in_care, na.rm = TRUE)) %>%
  dplyr::pull(children_in_care)

LA_panel_data %>%
  dplyr::filter(year == 2023) %>%
  dplyr::distinct(Local.authority, .keep_all = TRUE) %>%
  dplyr::filter(all_places < 300) %>%
  dplyr::summarise(children_in_care = sum(children_in_care, na.rm = TRUE)) %>%
  dplyr::pull(children_in_care)

605/6042




as.data.frame(LA_panel_data)%>%
  mutate(Local.authority = as.character(Local.authority))%>%
  filter(year == 2023) %>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(variable=="Secure homes and children's homes", category=="child characteristic at 31st March", year==2023)%>%
                     dplyr::select(LA_Name, number)%>%
                     dplyr::rename(Local.authority= LA_Name)
  )%>%
  mutate(
    children_in_care = as.numeric(number),
    ratio = children_in_care / all_places
  ) %>%
  dplyr::filter(ratio>1)




####changing the comparison to invest firm####

mlm$sector_third <- ifelse(mlm$Sector_merge=="Investment owned", 0,
                           ifelse(mlm$Sector_merge=="Third sector", 1, NA
                           ))

mlm$sector_la <- ifelse(mlm$Sector_merge=="Local Authority", 1,
                        ifelse(mlm$Sector_merge=="Investment owned", 0, NA
                        ))

mlm$sector_corp <- ifelse(mlm$Sector_merge=="Investment owned", 0,
                          ifelse(mlm$Sector_merge=="Corporate owned", 1, NA
                          ))

mlm$sector_indiv <- ifelse(mlm$Sector_merge=="Investment owned", 0,
                           ifelse(mlm$Sector_merge=="Individual owned", 1, NA
                           ))



reg1 <- lmerTest::lmer(sector_third ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg2 <- lmerTest::lmer(sector_indiv ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm)
reg3 <- lmerTest::lmer(sector_corp ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg4 <- lmerTest::lmer(sector_la ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm)

reg5 <- lmerTest::lmer(sector_third ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg6 <- lmerTest::lmer(sector_indiv ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm)
reg7 <- lmerTest::lmer(sector_corp ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg8 <- lmerTest::lmer(sector_la ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm)





modelsummary(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
             stars = TRUE,
             coef_omit = "Intercept",
             output = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/simple_mlm_invest_as_ref.docx")



####council politics####

mlm <- mlm %>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/history2016-2025.csv"))%>%
                     dplyr::mutate(Local.authority = authority %>%
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
                                     str_trim())%>%
                     dplyr::filter(year==2023)%>%
                     dplyr::select(Local.authority, majority)
                   
                   
  )




priors <- c(
  # Slopes for each category
  prior(normal(0, 1), class = "b", dpar = "muCorporateowned"),
  prior(normal(0, 1), class = "b", dpar = "muIndividualowned"),
  prior(normal(0, 1), class = "b", dpar = "muInvestmentowned"),
  prior(normal(0, 1), class = "b", dpar = "muThirdsector"),
  
  # Intercepts
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muCorporateowned"),
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muIndividualowned"),
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muInvestmentowned"),
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muThirdsector"),
  
  # Random effect SDs
  prior(student_t(3, 0, 2.5), class = "sd", dpar = "muCorporateowned"),
  prior(student_t(3, 0, 2.5), class = "sd", dpar = "muIndividualowned"),
  prior(student_t(3, 0, 2.5), class = "sd", dpar = "muInvestmentowned"),
  prior(student_t(3, 0, 2.5), class = "sd", dpar = "muThirdsector")
)


brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, net_loss_s, children_in_care_s,
                Local.authority, closed, majority) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 






### Fit the Hierarchical Multinomial Model ###
model_multilevel <- brm(
  formula   = Sector_merge ~ net_loss_s  + closed + majority+
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  prior     = priors,
  cores     = 4,
  iter      = 2000
)





brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,majority,
                Local.authority, closed) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 

### Fit the Hierarchical Multinomial Model ###
model_multilevel_house <- brm(
  formula   = Sector_merge ~ average_house_price_per_sq_m_s  + closed + majority+
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  prior     = priors,
  cores     = 4,
  iter      = 2000
)

extract_bayes_table <- function(model) {
  draws <- as_draws_df(model) %>%
    select(starts_with("b_mu")) %>%
    pivot_longer(everything(), names_to = "orig_term", values_to = "logodds") %>%
    mutate(term_clean = orig_term %>% sub("^b_", "", .) %>% sub("^mu", "", .),
           OR = exp(logodds))
  summary_stats <- draws %>%
    group_by(term_clean) %>%
    summarise(
      OR_estimate = mean(OR),
      OR_sd       = sd(OR),
      OR_conf.low = quantile(OR, 0.025),
      OR_conf.high= quantile(OR, 0.975),
      Pr_GT_1     = mean(OR > 1)
    )
  labels <- tidy(model, effects = "fixed") %>%
    mutate(term_clean = term %>% sub("^b_", "", .) %>% sub("^mu", "", .),
           Category   = sub("_.*$", "", term_clean),
           Predictor  = sub("^[^_]+_", "", term_clean)) %>%
    select(term_clean, Category, Predictor)
  labels %>%
    left_join(summary_stats, by = "term_clean") %>%
    mutate(Value = sprintf(
      "%.3f\n(SD=%.3f)\n[%.3f, %.3f]\nPr>1=%.2f",
      OR_estimate, OR_sd, OR_conf.low, OR_conf.high, Pr_GT_1
    )) %>%
    select(Category, Predictor, Value)
}

# Compile models and tables
bayes_models <- list(
  `Model 1: Net Loss`    = model_multilevel,
  `Model 3: House Price` = model_multilevel_house
)
combined_bayes <- do.call(rbind, lapply(names(bayes_models), function(mn) {
  df <- extract_bayes_table(bayes_models[[mn]])
  df$Model <- mn
  df
}))


combined_bayes <- combined_bayes %>%
  filter(Predictor %in% c("net_loss_s", "average_house_price_per_sq_m_s",
                          "majorityConplurality", "majorityLab", "majorityLabplurality", 
                          "majorityLD", "majorityLDplurality", "majorityOther", "majorityOtherplurality")) %>%
  mutate(
    Predictor = recode(Predictor,
                       net_loss_s = "Net Loss (std)",
                       average_house_price_per_sq_m_s = "House Price (std)",
                       majorityConplurality = "Political Control: Conservative plurality",
                       majorityLab = "Political Control: Labour majority",
                       majorityLabplurality = "Political Control: Labour plurality",
                       majorityLD = "Political Control: Liberal Democrat majority",
                       majorityLDplurality = "Political Control: Liberal Democrat plurality",
                       majorityOther = "Political Control: Other majority",
                       majorityOtherplurality = "Political Control: Other plurality"),
    Category = factor(Category, levels = c("Thirdsector", "Individualowned", "Corporateowned", "Investmentowned"))
  ) %>%
  arrange(Category, Predictor)



wide_bayes <- combined_bayes %>%
  pivot_wider(names_from = Model, values_from = Value)
# Build flextable with sample sizes
ft_bayes <- flextable(wide_bayes) %>%
  # Add a top header row for sample size
  add_header_row(values = c("", "", paste0("N=", n1), paste0("N=", n3)), colwidths = c(1, 1, 1, 1), top = TRUE) %>%
  # Add the main header row
  add_header_row(values = c("", "", "Posterior Summary", ""), colwidths = c(1, 1, 1, 1)) %>%
  set_header_labels(
    Category               = "Ownership Type",
    Predictor              = "Predictor",
    `Model 1: Net Loss`    = "Model 1: Net Loss",
    `Model 3: House Price` = "Model 3: House Price"
  ) %>%
  theme_booktabs() %>%
  align(j = 1:2, align = "left") %>%
  align(j = 3:4, align = "center")

ft_bayes

# Save directly to Word
save_as_docx(ft_bayes, path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table3_council_cont.docx")


####non informative priors####


# Data for Model 1: Net Loss
data_netloss <- mlm %>%
  drop_na(Sector_merge, net_loss_s, children_in_care_s, Local.authority) %>%
  select(Sector_merge, net_loss_s, children_in_care_s, Local.authority, closed) %>%
  mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed)
  )
data_netloss$Sector_merge <- droplevels(data_netloss$Sector_merge)
n1 <- nrow(data_netloss)

model_multilevel <- brm(
  formula = Sector_merge ~ net_loss_s + closed + children_in_care_s + (1 | Local.authority),
  data = data_netloss,
  family = categorical(),
  cores = 4,
  iter = 2000
)

# Data for Model 3: House Price
data_house <- mlm %>%
  drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s, Local.authority) %>%
  select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s, Local.authority, closed) %>%
  mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed)
  )
data_house$Sector_merge <- droplevels(data_house$Sector_merge)
n3 <- nrow(data_house)

model_multilevel_house <- brm(
  formula = Sector_merge ~ average_house_price_per_sq_m_s + closed + children_in_care_s + (1 | Local.authority),
  data = data_house,
  family = categorical(),
  cores = 4,
  iter = 2000
)

# Function to extract OR table
extract_bayes_table <- function(model) {
  draws <- as_draws_df(model) %>%
    select(starts_with("b_mu")) %>%
    pivot_longer(everything(), names_to = "orig_term", values_to = "logodds") %>%
    mutate(term_clean = orig_term %>% sub("^b_", "", .) %>% sub("^mu", "", .),
           OR = exp(logodds))
  summary_stats <- draws %>%
    group_by(term_clean) %>%
    summarise(
      OR_estimate = mean(OR),
      OR_sd       = sd(OR),
      OR_conf.low = quantile(OR, 0.025),
      OR_conf.high= quantile(OR, 0.975),
      Pr_GT_1     = mean(OR > 1)
    )
  labels <- tidy(model, effects = "fixed") %>%
    mutate(term_clean = term %>% sub("^b_", "", .) %>% sub("^mu", "", .),
           Category   = sub("_.*$", "", term_clean),
           Predictor  = sub("^[^_]+_", "", term_clean)) %>%
    select(term_clean, Category, Predictor)
  labels %>%
    left_join(summary_stats, by = "term_clean") %>%
    mutate(Value = sprintf(
      "%.3f\n(SD=%.3f)\n[%.3f, %.3f]\nPr>1=%.2f",
      OR_estimate, OR_sd, OR_conf.low, OR_conf.high, Pr_GT_1
    )) %>%
    select(Category, Predictor, Value)
}

# Compile models and tables
bayes_models <- list(
  `Model 1: Net Loss`    = model_multilevel,
  `Model 3: House Price` = model_multilevel_house
)
combined_bayes <- do.call(rbind, lapply(names(bayes_models), function(mn) {
  df <- extract_bayes_table(bayes_models[[mn]])
  df$Model <- mn
  df
}))
combined_bayes <- combined_bayes %>%
  filter(Predictor %in% c("net_loss_s", "average_house_price_per_sq_m_s")) %>%
  mutate(
    Predictor = recode(Predictor,
                       net_loss_s = "Net Loss (std)",
                       average_house_price_per_sq_m_s = "House Price (std)"),
    # Reorder categories: Thirdsector, Individualowned, Corporateowned, Investmentowned
    Category = factor(Category, levels = c("Thirdsector","Individualowned","Corporateowned","Investmentowned"))
  ) %>%
  arrange(Category, Predictor)
wide_bayes <- combined_bayes %>%
  pivot_wider(names_from = Model, values_from = Value)
# Build flextable with sample sizes
ft_bayes <- flextable(wide_bayes) %>%
  # Add a top header row for sample size
  add_header_row(values = c("", "", paste0("N=", n1), paste0("N=", n3)), colwidths = c(1, 1, 1, 1), top = TRUE) %>%
  # Add the main header row
  add_header_row(values = c("", "", "Posterior Summary", ""), colwidths = c(1, 1, 1, 1)) %>%
  set_header_labels(
    Category               = "Ownership Type",
    Predictor              = "Predictor",
    `Model 1: Net Loss`    = "Model 1: Net Loss",
    `Model 3: House Price` = "Model 3: House Price"
  ) %>%
  theme_booktabs() %>%
  align(j = 1:2, align = "left") %>%
  align(j = 3:4, align = "center")





# Save directly to Word
save_as_docx(ft_bayes, path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table3_no_priors_rev.docx")

one <- pp_check(model_multilevel, type = "bars")           # bar plot comparison for categories
two <- pp_check(model_multilevel_house, type = "bars")           # bar plot comparison for categories

ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/prior_diag_need_rev.png", one, width = 7, height = 7, dpi = 600)
ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/diag_house_rev.png", two, width = 7, height = 7, dpi = 600)


####all owenership cats - hashtagged out because requires a different dataset to see all cats####


# 
# # Create a full sequence of times
# all_times <- tibble(time = -238:0)
# 
# # Process the start dataset
# carehomesstart <- df %>%
#   mutate(
#     Registration.date = as.Date(Registration.date, format = "%d/%m/%Y"),
#     Join = as.Date(Join),
#     location_start = coalesce(Registration.date, Join, as.Date("2014-03-15")),
#     date = location_start,
#     time = as.integer(time_length(difftime(date, as.Date("2023-12-01")), "months"))
#   ) %>%
#   distinct(URN, .keep_all = TRUE) %>%
#   dplyr::select(time, Sector_merge, URN)
# 
# # Count observations by group
# nobsByIdih <- carehomesstart %>%
#   count(time, Sector_merge, name = "nobs")
# 
# # Ensure all time periods exist for each Sector_merge
# nobsBySector <- nobsByIdih %>%
#   complete(Sector_merge, time = all_times$time, fill = list(nobs = 0)) %>%
#   group_by(Sector_merge) %>%
#   mutate(cumulative = cumsum(nobs)) %>%
#   ungroup()
# 
# # Process the end dataset
# carehomesend <- df %>%
#   filter(Leave != "") %>%
#   mutate(
#     date = as.Date(Leave),
#     time = as.integer(time_length(difftime(date, as.Date("2023-12-01")), "months"))
#   ) %>%
#   distinct(URN, .keep_all = TRUE) %>%
#   dplyr::select(time, Sector_merge, URN)
# 
# # Count observations by group
# nobsEndByIdih <- carehomesend %>%
#   count(time, Sector_merge, name = "nobs")
# 
# # Ensure all time periods exist for each Sector_merge
# nobsEndBySector <- nobsEndByIdih %>%
#   complete(Sector_merge, time = all_times$time, fill = list(nobs = 0)) %>%
#   group_by(Sector_merge) %>%
#   mutate(cumulative_end = cumsum(nobs)) %>%
#   ungroup()
# 
# # Merge start and end datasets and compute the running total
# nobser <- full_join(nobsBySector, nobsEndBySector, by = c("Sector_merge", "time")) %>%
#   mutate(
#     cumulative = replace_na(cumulative, 0),
#     cumulative_end = replace_na(cumulative_end, 0),
#     runningsum = cumulative - cumulative_end
#   )
# 
# # Create the time-series plot
# d <- ggplot(nobser %>% filter(time > -155),
#             aes(x = time, y = runningsum, group = Sector_merge,
#                 fill = Sector_merge, colour = Sector_merge, alpha = Sector_merge)) +
#   geom_point() +
#   theme_minimal() +
#   scale_color_manual(values = c("#008F5D", "#F0AB00", "#E4007C", "#FF5E5E", "#5B0000", "#1F77B4", "#9467BD")) +
#   scale_alpha_manual(values = c(0.2, 0.2, 1, 1, 1, 0.2, 0.2)) +
#   labs(x = "Year", y = "Number of children homes", 
#        title = "Number of children's homes owned for-profit",
#        fill = "Ownership", color = "Ownership", alpha = "Ownership") +
#   scale_x_continuous(breaks = c(-12, -24, -36, -48, -60, -72, -84, -96, -108, -120, -132, -144, -156),
#                      labels = c("2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011")) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     plot.title = element_text(size = 11, face = "bold")  ) +
#   coord_cartesian(xlim = c(-110, -13)) +
#   theme(legend.position = "bottom")
# 
# 
# ggsave(plot=d, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/all_cats.png", width=8, height=6, dpi=600)
# 

####comparing unidentified fp with others####

library(dplyr)
library(ggplot2)

# Compute the average Places by Sector_merge

df <- df %>%
  dplyr::filter(!is.na(URN))

df_avg <- df %>%
  dplyr::mutate(closed = ifelse(is.na(Leave), 0,1),
                Overall.experiences.and.progress.of.children.and.young.people = ifelse(
    Overall.experiences.and.progress.of.children.and.young.people=="Outstanding", 4,
    ifelse( Overall.experiences.and.progress.of.children.and.young.people=="Good", 3,
            ifelse( Overall.experiences.and.progress.of.children.and.young.people=="Requires improvement", 2,
                    ifelse( Overall.experiences.and.progress.of.children.and.young.people=="Inadequate", 1,
                            NA)) )    )  )%>%
  group_by(Sector_merge) %>%
  summarise(avg_places = mean(Places, na.rm = TRUE),
            avg_inspection = mean(as.numeric(as.character(Overall.experiences.and.progress.of.children.and.young.people)), na.rm=T),
            closed = mean(closed, na.rm=T))%>%
  dplyr::ungroup()
# Plot the averages
one <- ggplot(df_avg, aes(x = Sector_merge, y = avg_places)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Sector") +
  ylab("Average Places") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

two <- ggplot(df_avg, aes(x = Sector_merge, y = avg_inspection)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Sector") +
  ylab("Average Inspection score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

three <- ggplot(df_avg, aes(x = Sector_merge, y = closed)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Sector") +
  ylab("Proportion of homes closed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Ensure Sector_merge has the correct factor levels
df$Sector_merge <- factor(df$Sector_merge, levels = c(
  "Local Authority", 
  "LA owned company", 
  "Third sector", 
  "Individual owned", 
  "Corporate owned", 
  "Investment owned", 
  "Unidentified for-profit"
))

# Join Region.Country.name into df
df_reg <- df %>%
  left_join(
    mlm %>%
      select(Local.authority, Region.Country.name) %>%
      distinct(),
    by = "Local.authority"
  )

# Count and calculate proportions by Region and Sector
df_grouped <- df_reg %>%
  group_by(Region.Country.name, Sector_merge) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Region.Country.name) %>%
  mutate(percent = count / sum(count))

# Plot with percentages
four <- ggplot(df_grouped, aes(x = Region.Country.name, y = percent, fill = Sector_merge)) +
  geom_bar(stat = "identity", position = "fill") +
  xlab("Region") +
  ylab("Proportion of Observations") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(
    "#008F5D", # Local Authority
    "#F0AB00", # LA owned company
    "#E4007C", # Third sector
    "#FF5E5E", # Individual owned
    "#5B0000", # Corporate owned
    "#1F77B4", # Investment owned
    "#9467BD"  # Unidentified for-profit
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Sector")



compare <- cowplot::plot_grid(one, two, three, four, ncol=1)


ggsave(plot=compare, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/unident_comp.png", width=6, height=14, dpi=600)






####missing data analysis####
# Required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Calculate missing counts and proportions
missing_df <- mlm %>%
  select(Sector_merge, Places, age_years, overall.average, net_loss_s, chain_size, closed,
         average_house_price_per_sq_m_s, reqs_per, profit_margin_average, children_in_care_s) %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "miss_{col}")) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "n_missing") %>%
  mutate(
    variable = sub("^miss_", "", variable),
    total_n = nrow(mlm),
    p_missing = n_missing / total_n
  ) %>%
  arrange(desc(n_missing)) %>%
  mutate(variable = fct_inorder(variable))

# Plot
ggplot(missing_df, aes(x = n_missing, y = variable)) +
  geom_col(fill = "grey50", width = 0.6) +
  geom_text(aes(label = n_missing),
            hjust = -0.1, size = 4, family = "Helvetica") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Number of Missing Values",
    y = NULL,
    title = "Missing Data per Variable",
    subtitle = paste0("N = ", nrow(mlm), " observations"),
    caption = "Data source: mlm dataset"
  ) +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 9),
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed")
  )

####compare house price and need ####

comp <- ggplot(mlm %>%
         dplyr::select(Local.authority, average_house_price_per_sq_m_s, net_loss_s, Region.Country.name)%>%
         dplyr::distinct(.keep_all = T),
       aes(x= net_loss_s, y=average_house_price_per_sq_m_s ))+
  geom_point()+
  #facet_grid(~Region.Country.name)+
  geom_smooth(method="lm")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_bw()

ggsave(plot=comp, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/comp.png", width=7, height=6, dpi=600)


#### compare openings and ratings by opening date ####

mlm %>% dplyr::select(Places, overall.average, Registration.date)%>%
  dplyr::filter(!is.na(Registration.date))

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

# Clean and transform
mlm_clean <- mlm %>%
  filter(!is.na(Registration.date)) %>%
  mutate(
    Registration.date = dmy(Registration.date),
    Year = year(Registration.date)
  ) %>%
  filter(!is.na(overall.average), !is.nan(overall.average), !is.na(Places))

# Aggregate: yearly average
mlm_yearly <- mlm_clean %>%
  group_by(Year) %>%
  summarise(
    avg_quality = mean(overall.average, na.rm = TRUE),
    avg_size = mean(Places, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(Year) %>%
  mutate(
    roll_quality = rollmean(avg_quality, 3, fill = NA, align = "right"),
    roll_size = rollmean(avg_size, 3, fill = NA, align = "right")
  )%>%
  dplyr::filter(Year>1995)
# Plot 1: Quality over time
p1 <- ggplot(mlm_yearly, aes(x = Year, y = roll_quality)) +
  geom_line(color = "darkred", size = 1) +
  labs(
    title = "Average inspection ratings by year of registration",
    x = "Year of Registration",
    y = "3-Year Rolling Mean\nQuality Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Plot 2: Size over time
p2 <- ggplot(mlm_yearly, aes(x = Year, y = roll_size)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Average size of homes by year of registration",
    x = "Year of Registration",
    y = "3-Year Rolling Mean\nNumber of Places"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))
combined_plot <- cowplot::plot_grid(
  p1, p2,
  labels = c("A", "B"),
  label_size = 16,
  ncol = 2,
  align = "v"
)

# Print the plot
print(combined_plot)

ggsave(plot=combined_plot, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/size_qual.png", width=7, height=6, dpi=600)

