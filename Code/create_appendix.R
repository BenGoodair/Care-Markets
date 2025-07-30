
source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")

mlm2 <- create_home_data()



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
    dplyr::select(Registration.date, Sector_merge, Local.authority) %>%
    left_join(., data %>%
                dplyr::select(Local.authority, net_loss_2014) %>%
                dplyr::distinct(.keep_all = TRUE) %>%
                dplyr::mutate(needQuint = factor(ntile(net_loss_2014, 5))) %>%
                dplyr::select(-net_loss_2014)
    ) %>%
    mutate(location_start = dmy(Registration.date)) %>%
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







####care home falsification####


updata <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_Materials/Data/complete inspection and location data_ben_feb2025v2.csv")%>%
  filter(closed_complete!=1)%>%
  dplyr::distinct(locationid, .keep_all = T)%>%
  dplyr::select(locationlocalauthority, ownership)%>%
  dplyr::mutate(
    ownership = ifelse(ownership==0, "For-profit",
                       ifelse(ownership==1, "Local Authority",
                              ifelse(ownership==2, "Third sector", NA))))%>%
  dplyr::filter(!ownership=="")%>%
  dplyr::mutate(Local.authority = locationlocalauthority %>%
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
  left_join(., mlm %>%
              dplyr::select(Local.authority,children_in_care_s, net_loss_s, median_house_price_detached_s, Unemployment.rate...aged.16.64._s, residential_expenditure_s, claimant_count_rate_s, median_wage_s, Region.Country.name)%>%
              dplyr::distinct(.keep_all = T))%>%
  mutate(ownership = factor(ownership, levels = c("Local Authority","Third sector","For-profit")))%>%
  drop_na(ownership,Local.authority,children_in_care_s, net_loss_s, median_house_price_detached_s, Unemployment.rate...aged.16.64._s, )%>%
  dplyr::mutate(third  = ifelse(ownership=="Local Authority", 0,
                                ifelse(ownership=="Third sector", 1, NA)),
                profit  = ifelse(ownership=="Local Authority", 0,
                                 ifelse(ownership=="For-profit", 1, NA)))


model_multilevel <- brm(
  formula = ownership ~ net_loss_s +  children_in_care_s +
    residential_expenditure_s + median_house_price_detached_s + median_wage_s +
    Region.Country.name + (1 | Local.authority),
  data = updata,
  family = categorical(),
  cores = 4,
  iter = 2000
)

summary(model_multilevel)


### Function to Extract Multinomial Regression Table ###
extract_regression_table <- function(model, reference_level) {
  summary_model <- summary(model)
  results_table <- data.frame(
    Category = character(),
    Predictor = character(),
    Estimate = numeric(),
    SE = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Get outcome categories (excluding the specified reference)
  categories <- levels(model$data$ownership)
  categories <- categories[categories != reference_level]
  
  for (cat in categories) {
    # Remove both spaces and hyphens from category names.
    cat_clean <- gsub("[ -]", "", cat)
    pattern <- paste0("^mu", cat_clean, "_")
    
    # Debug: print the pattern and available rownames
    message("Processing category: ", cat, " with pattern: ", pattern)
    print(rownames(summary_model$fixed))
    
    fixed_effects <- summary_model$fixed[grep(pattern, rownames(summary_model$fixed)), ]
    
    # Check if any fixed effects were found for this category
    if(nrow(fixed_effects) == 0){
      warning(paste("No fixed effects found for category:", cat))
      next
    }
    
    for (i in 1:nrow(fixed_effects)) {
      param_name <- rownames(fixed_effects)[i]
      predictor <- gsub(pattern, "", param_name)
      
      results_table <- rbind(results_table, data.frame(
        Category = cat,
        Predictor = predictor,
        Estimate = fixed_effects[i, "Estimate"],
        SE = fixed_effects[i, "Est.Error"],
        CI_Lower = fixed_effects[i, "l-95% CI"],
        CI_Upper = fixed_effects[i, "u-95% CI"],
        P_value = 2 * (1 - pnorm(abs(fixed_effects[i, "Estimate"] / fixed_effects[i, "Est.Error"]))),
        stringsAsFactors = FALSE
      ))
    }
  }
  return(results_table)
}


# Adjust the reference level as needed (here we assume "Local Authority" is the reference)
reg_table_multinom <- extract_regression_table(model_multilevel, "Local Authority")

formatted_multinom <- reg_table_multinom %>%
  mutate(
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01  ~ "**",
      P_value < 0.05  ~ "*",
      P_value < 0.1   ~ ".",
      TRUE ~ ""
    ),
    Estimate_formatted = sprintf("%.3f%s", Estimate, Significance),
    CI_formatted = sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper)
  ) %>%
  dplyr::select(Category, Predictor, Estimate_formatted, CI_formatted, P_value)




# Pivot the multinomial table so that each category becomes a column
multinom_wide <- formatted_multinom %>%
  pivot_wider(names_from = Category, values_from = c(Estimate_formatted, CI_formatted, P_value))

# Rename Predictor to term to merge by predictor name
multinom_wide <- multinom_wide %>% rename(term = Predictor)

library(dplyr)
library(stringr)
library(gt)

# Merge overall and profit margin estimates by predictor ("term")
final_table <- multinom_wide %>%
  filter(!str_detect(term, "Region"),
         term != "Intercept") %>%
  select(term, 
         `Estimate_formatted_Third sector`, `CI_formatted_Third sector`, 
         `Estimate_formatted_For-profit`, `CI_formatted_For-profit`)

# Create a prettier version of the table with clear column names and cleaned predictors
pretty_table <- final_table %>%
  rename(
    Predictor = term,
    `Third Sector (Estimate)` = `Estimate_formatted_Third sector`,
    `Third Sector (95% CI)` = `CI_formatted_Third sector`,
    `For-profit (Estimate)` = `Estimate_formatted_For-profit`,
    `For-profit (95% CI)` = `CI_formatted_For-profit`
  ) %>%
  mutate(
    Predictor = case_when(
      Predictor == "net_loss_s" ~ "Area Need (Net Loss)",
      Predictor == "children_in_care_s" ~ "Children in Care",
      Predictor == "residential_expenditure_s" ~ "Local Authority Expenditure (Residential care)",
      Predictor == "median_house_price_detached_s" ~ "Median House Price (detached)",
      Predictor == "median_wage_s" ~ "Median Wage (Hourly)",
      TRUE ~ Predictor
    )
  )

# # Calculate sample sizes for each model
# n_multinom <- nrow(brmdata_multinom)
# n_overall  <- nrow(brmdata_overall)
# n_profit   <- nrow(brmdata_profit)
# 
# # Add rows for fixed/random effects and sample sizes
# # Using NA for cells that are not applicable provides more consistent formatting.
# pretty_table <- pretty_table %>%
#   add_row(Predictor = "Regional Fixed Effects", 
#           `Third Sector (Estimate)` = "Included", 
#           `Third Sector (95% CI)` = NA,
#           `Individual Owned (Estimate)` = "Included",
#           `Individual Owned (95% CI)` = NA,
#           `Corporate Owned (Estimate)` = "Included",
#           `Corporate Owned (95% CI)` = NA,
#           `Investment Owned (Estimate)` = "Included",
#           `Investment Owned (95% CI)` = NA,
#           `Overall Rating (Estimate)` = "Included",
#           `Overall Rating (95% CI)` = NA,
#           `Profit Margin (Estimate)` = "Included",
#           `Profit Margin (95% CI)` = NA
#   ) %>%
#   add_row(Predictor = "Local Authority Random Effects", 
#           `Third Sector (Estimate)` = "Included", 
#           `Third Sector (95% CI)` = NA,
#           `Individual Owned (Estimate)` = "Included",
#           `Individual Owned (95% CI)` = NA,
#           `Corporate Owned (Estimate)` = "Included",
#           `Corporate Owned (95% CI)` = NA,
#           `Investment Owned (Estimate)` = "Included",
#           `Investment Owned (95% CI)` = NA,
#           `Overall Rating (Estimate)` = "Included",
#           `Overall Rating (95% CI)` = NA,
#           `Profit Margin (Estimate)` = "Included",
#           `Profit Margin (95% CI)` = NA
#   ) %>%
#   add_row(Predictor = "Observations (n)", 
#           `Third Sector (Estimate)` = as.character(n_multinom), 
#           `Third Sector (95% CI)` = NA,
#           `Individual Owned (Estimate)` = NA, 
#           `Individual Owned (95% CI)` = NA,
#           `Corporate Owned (Estimate)` = NA, 
#           `Corporate Owned (95% CI)` = NA,
#           `Investment Owned (Estimate)` = NA, 
#           `Investment Owned (95% CI)` = NA,
#           `Overall Rating (Estimate)` = as.character(n_overall), 
#           `Overall Rating (95% CI)` = NA,
#           `Profit Margin (Estimate)` = as.character(n_profit), 
#           `Profit Margin (95% CI)` = NA
#   )
# 
# # Combine each estimate with its 95% CI into one column per model/metric
# pretty_table <- pretty_table %>%
#   mutate(
#     `Third Sector (Estimate) [95% CI]` = paste0(`Third Sector (Estimate)`, " ", `Third Sector (95% CI)`, ""),
#     `Individual Owned (Estimate) [95% CI]` = paste0(`Individual Owned (Estimate)`, " ", `Individual Owned (95% CI)`, ""),
#     `Corporate Owned (Estimate) [95% CI]` = paste0(`Corporate Owned (Estimate)`, " ", `Corporate Owned (95% CI)`, ""),
#     `Investment Owned (Estimate) [95% CI]` = paste0(`Investment Owned (Estimate)`, " ", `Investment Owned (95% CI)`, ""),
#     `Quality (Estimate) [95% CI]` = paste0(`Overall Rating (Estimate)`, " ", `Overall Rating (95% CI)`, ""),
#     `Profit Margin (Estimate) [95% CI]` = paste0(`Profit Margin (Estimate)`, " ", `Profit Margin (95% CI)`, "")
#   ) %>%
#   dplyr::select(Predictor,
#                 `Third Sector (Estimate) [95% CI]`,
#                 `Individual Owned (Estimate) [95% CI]`,
#                 `Corporate Owned (Estimate) [95% CI]`,
#                 `Investment Owned (Estimate) [95% CI]`,
#                 `Quality (Estimate) [95% CI]`,
#                 `Profit Margin (Estimate) [95% CI]`)

# Build the gt table
regression_table <- pretty_table %>%
  gt() %>%
  tab_header(
    title = "Hierarchical Regression Models Examining Predictors of Children Home Ownership and Performance"
  ) %>%
  tab_spanner(
    label = c("Reference category for ownership type: Local Authority"),
    columns = matches("Third Sector|For-profit")
  )  %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    source_notes.font.size = px(10),
    footnotes.font.size = px(10),
    table.width = pct(100)
  )

# Save the table as HTML
regression_table %>%
  gtsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/care_home")




####other quality reg table ####
# Fit models with clustered standard errors
reg5 <- lm_robust(ever_outstanding ~ Sector_merge + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg6 <- lm_robust(ever_outstanding ~ Sector_merge + Places + number_of_inspections + age_years +factor(left)+ chain_size_0s + Local.authority, data = mlm, clusters = Organisation)
reg7 <- lm_robust(ever_outstanding ~ profit_margin_average + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg8 <- lm_robust(ever_outstanding ~ profit_margin_average + Places + number_of_inspections + age_years +factor(left)+ chain_size_0s + Local.authority, data = mlm, clusters = Organisation)
reg51 <- lm_robust(recs_per ~ Sector_merge + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg61 <- lm_robust(recs_per ~ Sector_merge + Places + number_of_inspections + age_years +factor(left)+ chain_size_0s + Local.authority, data = mlm, clusters = Organisation)
reg71 <- lm_robust(recs_per ~ profit_margin_average + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg81 <- lm_robust(recs_per ~ profit_margin_average + Places + number_of_inspections + age_years +factor(left)+ chain_size_0s + Local.authority, data = mlm, clusters = Organisation)

library(modelsummary)
library(kableExtra)
library(gt)

# Try using modelsummary's built-in CI formatting
table_html <- modelsummary(
  list( reg5, reg6, reg7, reg8,
        reg51, reg61, reg71, reg81),
  stars = TRUE,
  coef_map = c(
    "Sector_mergeIndividual owned" = "Ownership [Individual-owned]", 
    "Sector_mergeCorporate owned" = "Ownership [Corporate-owned]",
    "Sector_mergeInvestment owned" = "Ownership [Investment-owned]",
    "Sector_mergeThird sector" = "Ownership [Third Sector]",
    "profit_margin_average" = "Profit Margin Average (%)",
    "Places" = "Places (n)",
    "age_years" = "Age (years)",
    "chain_size_0s" = "Chain Size (n, 10s)",
    "factor(left)1" = "De-registered home (closed or taken over dummy)"
  ),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
  fmt = "%.3f",
  estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
  statistic = NULL,  # Remove separate standard error row
  conf_level = 0.95, # 95% confidence intervals
  output = "gt"
) %>%
  tab_header(
    title = "Regression Models of Inspection Ratings and Outstanding Status"
  ) %>%
  tab_spanner(
    label = "Ever Rated Outstanding",
    columns = 2:5
  )%>%
  tab_spanner(
    label = "Recommendations per inspection",
    columns = 6:9
  ) %>%
  tab_footnote(
    footnote = "Reference category for ownership is Local Authority",
    locations = cells_stub(rows = contains("Sector"))  # Apply to all ownership rows
  ) %>%
  tab_footnote(
    footnote = "* p < 0.1, ** p < 0.05, *** p < 0.01",
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "Number of inspections and Local Authority fixed effects are controlled for in all models.",
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "Standard errors are clustered at the chain level.",
    locations = cells_title()
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    source_notes.font.size = px(10),
    footnotes.font.size = px(10),
    table.width = pct(100)
  )

# Save the table as HTML
table_html %>%
  gtsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table2_quality_others.html")

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

####Outcomes of profit and quality ####


### Prepare Data for the Numerical BRMS Models ###
brmdata_overall <- mlm %>%
  tidyr::drop_na(overall.average,number_of_inspections, age_s, net_loss_s, Places_s,rel_pov_per_s, children_in_care_s,
                 residential_expenditure_s, average_house_price_per_sq_m_s, median_wage_s,
                 Region.Country.name, Local.authority) %>%
  dplyr::mutate(Region.Country.name = factor(Region.Country.name),
                Local.authority = factor(Local.authority))

brmdata_profit <- mlm %>%
  tidyr::drop_na(profit_margin_average, age, net_loss_s, Places, children_in_care_s,rel_pov_per_s,
                 residential_expenditure_s, average_house_price_per_sq_m_s, median_wage_s,
                 Region.Country.name, Local.authority) %>%
  dplyr::mutate(Region.Country.name = factor(Region.Country.name),
                Local.authority = factor(Local.authority))



### Fit Hierarchical Gaussian Models for the Numerical Outcomes ###

# Model for overall.average
model_overall <- brm(
  formula = overall.average ~ net_loss_s +  children_in_care_s +closed+ average_house_price_per_sq_m_s  + (1 | Local.authority),
  data = brmdata_overall,
  family = gaussian(),
  cores = 4,
  iter = 2000
)

# Model for profit_margin_average
model_profit <- brm(
  formula = profit_margin_average ~ net_loss_s +  children_in_care_s +closed+ average_house_price_per_sq_m_s  + (1 | Local.authority),
  data = brmdata_profit,
  family = gaussian(),
  cores = 4,
  iter = 2000
)

# required packages
library(dplyr)
library(tidyr)
library(flextable)
library(broom.mixed)

# 1. Function to extract and tidy a single brms model
tidy_brm <- function(model, model_name) {
  # broom.mixed::tidy() gives Estimate, std.error, conf.low, conf.high
  tbl <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    filter(term != "sigma") %>%     # drop the residual sd
    mutate(
      # compute p-value (approximate z-test)
      p.value = 2 * (1 - pnorm(abs(estimate / std.error))),
      # significance stars
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      ),
      # formatted strings
      Estimate_fmt = sprintf("%.2f%s", estimate, sig),
      CI_fmt       = sprintf("[%.2f, %.2f]", conf.low, conf.high),
      Model        = model_name
    ) %>%
    select(Model, Predictor = term, Estimate_fmt, CI_fmt, p.value)
  return(tbl)
}

# 2. Extract tables for both models
tbl_overall <- tidy_brm(model_overall, model_name = "Overall Average")
tbl_profit  <- tidy_brm(model_profit,  model_name = "Profit Margin")

# 3. Combine and reshape to wide
combined <- bind_rows(tbl_overall, tbl_profit) %>%
  # rename predictors for clarity
  mutate(Predictor = case_when(
    Predictor == "(Intercept)"                ~ "Intercept",
    Predictor == "net_loss_s"                 ~ "Net Loss (std)",
    Predictor == "children_in_care_s"         ~ "Children in Care (std)",
    Predictor == "closed1"                    ~ "Closed (Yes)",
    Predictor == "average_house_price_per_sq_m_s" ~ "House Price (std)",
    TRUE ~ Predictor
  )) %>%
  unite("Coef [95% CI]", Estimate_fmt, CI_fmt, sep = " ") %>%
  pivot_wider(
    names_from  = Model,
    values_from = `Coef [95% CI]`,
    values_fill = "-"
  ) %>%
  # reorder predictors how you like
  mutate(order = case_when(
    Predictor == "Intercept"           ~ 1,
    Predictor == "Net Loss (std)"      ~ 2,
    Predictor == "House Price (std)"   ~ 3,
    Predictor == "Closed (Yes)"        ~ 4,
    Predictor == "Children in Care (std)" ~ 5,
    TRUE ~ 6
  )) %>%
  arrange(order) %>%
  select(-order)

# 4. Build flextable
ft <- flextable(combined) %>%
  set_header_labels(
    Predictor          = "Predictor",
    `Overall Average`  = "Overall Avg model",
    `Profit Margin`    = "Profit Margin model"
  )%>%
  theme_booktabs() %>%
  align(j = 2:3, align = "center") %>%
  bold(part = "header") %>%
  fontsize(size = 9) %>%
  width(j = 1, width = 2.5) %>%
  width(j = 2:3, width = 1.5) %>%
  set_caption("Table: Fixed‐effect estimates (with 95% CIs) from two brms models") %>%
  add_footer_lines("Note: Standardized continuous predictors; *p<0.05; **p<0.01; ***p<0.001.")

# 5. Save to Word (uncomment & adjust path)
save_as_docx(ft, path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table3_qual_prof.docx")

# print to Viewer
ft




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


# Create extraction function if it doesn't already exist
extract_regression_table <- function(model, reference_level) {
  summary_model <- summary(model)
  results_table <- data.frame(
    Category = character(),
    Predictor = character(),
    Estimate = numeric(),
    SE = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Get outcome categories (excluding the specified reference)
  categories <- levels(model$data$Sector_merge)
  categories <- categories[categories != reference_level]
  
  for (cat in categories) {
    cat_clean <- gsub(" ", "", cat)
    fixed_effects <- summary_model$fixed[grep(paste0("^mu", cat_clean, "_"), rownames(summary_model$fixed)), ]
    
    for (i in 1:nrow(fixed_effects)) {
      param_name <- rownames(fixed_effects)[i]
      predictor <- gsub(paste0("^mu", cat_clean, "_"), "", param_name)
      
      results_table <- rbind(results_table, data.frame(
        Category = cat,
        Predictor = predictor,
        Estimate = fixed_effects[i, "Estimate"],
        SE = fixed_effects[i, "Est.Error"],
        CI_Lower = fixed_effects[i, "l-95% CI"],
        CI_Upper = fixed_effects[i, "u-95% CI"],
        P_value = 2 * (1 - pnorm(abs(fixed_effects[i, "Estimate"] / fixed_effects[i, "Est.Error"]))),
        stringsAsFactors = FALSE
      ))
    }
  }
  return(results_table)
}

# Extract regression tables for both models
reg_table_netLoss <- extract_regression_table(model_multilevel, "Local Authority")
reg_table_housePrice <- extract_regression_table(model_multilevel_house, "Local Authority")

# Format tables function
format_table <- function(table) {
  table %>%
    mutate(
      Significance = case_when(
        P_value < 0.001 ~ "***",
        P_value < 0.01  ~ "**",
        P_value < 0.05  ~ "*",
        P_value < 0.1   ~ ".",
        TRUE ~ ""
      ),
      Estimate_formatted = sprintf("%.3f%s", Estimate, Significance),
      CI_formatted = sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper)
    ) %>%
    dplyr::select(Category, Predictor, Estimate_formatted, CI_formatted, P_value)
}

# Apply formatting
reg_table_netLoss <- format_table(reg_table_netLoss)
reg_table_housePrice <- format_table(reg_table_housePrice)

# Add model identifiers
reg_table_1 <- reg_table_netLoss %>%
  mutate(Model = "Model 1: Net Loss")

reg_table_2 <- reg_table_housePrice %>%
  mutate(Model = "Model 2: House Price")

# Format predictor names for clarity
format_predictors <- function(table) {
  table %>%
    mutate(Predictor = case_when(
      Predictor == "Intercept" ~ "Intercept",
      Predictor == "net_loss_s" ~ "Net Loss (std)",
      Predictor == "average_house_price_per_sq_m_s" ~ "House Price per sq.m (std)",
      Predictor == "closed1" ~ "Closed (Yes)",
      Predictor == "children_in_care_s" ~ "Children in Care (std)",
      Predictor == "majorityConplurality" ~ "Political Control: Conservative plurality",
      Predictor == "majorityLab" ~ "Political Control: Labour majority",
      Predictor == "majorityLabplurality" ~ "Political Control: Labour plurality",
      Predictor == "majorityLD" ~ "Political Control: Liberal Democrat majority",
      Predictor == "majorityLDplurality" ~ "Political Control: Liberal Democrat plurality",
      Predictor == "majorityOther" ~ "Political Control: Other majority",
      Predictor == "majorityOtherplurality" ~ "Political Control: Other plurality",
      TRUE ~ Predictor
    ))
}

reg_table_1 <- format_predictors(reg_table_1)
reg_table_2 <- format_predictors(reg_table_2)

# Combine the tables
combined_table <- bind_rows(reg_table_1, reg_table_2)

# Create a wide-format table with models as columns
wide_table <- combined_table %>%
  select(Category, Predictor, Model, Estimate_formatted, CI_formatted) %>%
  unite("Value", Estimate_formatted, CI_formatted, sep = " ") %>%
  pivot_wider(
    names_from = Model, 
    values_from = Value,
    values_fill = " "  # Use empty space for missing values
  ) %>%
  arrange(Category, case_when(
    grepl("^Intercept", Predictor) ~ 1,
    grepl("^Net Loss", Predictor) ~ 2,
    grepl("^House Price", Predictor) ~ 3,
    grepl("^Closed", Predictor) ~ 4,
    grepl("^Children in Care", Predictor) ~ 5,
    grepl("^Political Control:", Predictor) ~ 6,
    TRUE ~ 7
  ), Predictor)

# Create flextable
ft <- flextable(wide_table) %>%
  set_header_labels(
    Category = "Sector Type",
    Predictor = "Predictor",
    `Model 1: Net Loss` = "Model 1: Net Loss",
    `Model 2: House Price` = "Model 2: House Price"
  )

# Add header rows for grouping
ft <- add_header_row(
  ft,
  values = c("", "", "Coefficient Estimates [95% CI]"),
  colwidths = c(1, 1, 2)
)

# Filter out political control rows if they don't have significant results
# This is optional - you might want to keep all variables regardless of significance
# wide_table_filtered <- wide_table %>%
#   filter(!(grepl("^Political Control:", Predictor) & 
#            !grepl("\\*", `Model 1: Net Loss`) & 
#            !grepl("\\*", `Model 2: House Price`)))

# Style the table for Word
ft <- ft %>%
  theme_booktabs() %>%
  align(j = 1:2, align = "left") %>%
  align(j = 3:4, align = "center") %>%
  fontsize(size = 9) %>%  # Smaller font for better fit in Word
  width(j = 1, width = 1.5) %>%
  width(j = 2, width = 2.5) %>%  # Wider to accommodate longer political control names
  width(j = 3:4, width = 1.6) %>%
  bold(j = 1) %>%
  italic(j = 2) %>%
  border(i = 1, border.top = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(i = 2, border.bottom = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(border.bottom = fp_border(color = "black", width = 1), part = "body") %>%
  bg(bg = "#f2f2f2", part = "header") %>%
  # Conditional formatting for rows containing "Political Control:" to be slightly indented
  padding(i = which(grepl("^Political Control:", wide_table$Predictor)), padding.left = 15, j = 2)


# Set caption and footnote
ft <- set_caption(ft, 
                  caption = "Multinomial Logistic Regression Models Comparing Care Home Ownership Types (Reference: Local Authority)")

ft <- add_footer_lines(ft, 
                       values = "Note: Standardized continuous predictors. Political control is included as control variable. *p<0.05; **p<0.01; ***p<0.001.")

# Save directly to Word
save_as_docx(ft, path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table3_council_cont.docx")


####non informative priors####



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
                Local.authority, closed) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 






### Fit the Hierarchical Multinomial Model ###
model_multilevel <- brm(
  formula   = Sector_merge ~ net_loss_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  cores     = 4,
  iter      = 2000
)



brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, net_loss_s, children_in_care_s,
                Local.authority, closed, age_years) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed),
    age_s = scale(age_years))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 


### Fit the Hierarchical Multinomial Model ###
model_multilevel_int <- brm(
  formula   = Sector_merge ~ net_loss_s*age_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  cores     = 4,
  iter      = 2000
)






brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,
                Local.authority, closed) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 

### Fit the Hierarchical Multinomial Model ###
model_multilevel_house <- brm(
  formula   = Sector_merge ~ average_house_price_per_sq_m_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  cores     = 4,
  iter      = 2000
)

brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,
                Local.authority, closed, age_years) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed),
    age_s = scale(age_years))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 

### Fit the Hierarchical Multinomial Model ###
model_multilevel_house_int <- brm(
  formula   = Sector_merge ~ average_house_price_per_sq_m_s*age_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  cores     = 4,
  iter      = 2000
)





library(dplyr)
library(kableExtra)
library(tidyr)

# Create extraction function if it doesn't already exist
if (!exists("extract_regression_table")) {
  extract_regression_table <- function(model, reference_level) {
    summary_model <- summary(model)
    results_table <- data.frame(
      Category = character(),
      Predictor = character(),
      Estimate = numeric(),
      SE = numeric(),
      CI_Lower = numeric(),
      CI_Upper = numeric(),
      P_value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Get outcome categories (excluding the specified reference)
    categories <- levels(model$data$Sector_merge)
    categories <- categories[categories != reference_level]
    
    for (cat in categories) {
      cat_clean <- gsub(" ", "", cat)
      fixed_effects <- summary_model$fixed[grep(paste0("^mu", cat_clean, "_"), rownames(summary_model$fixed)), ]
      
      for (i in 1:nrow(fixed_effects)) {
        param_name <- rownames(fixed_effects)[i]
        predictor <- gsub(paste0("^mu", cat_clean, "_"), "", param_name)
        
        results_table <- rbind(results_table, data.frame(
          Category = cat,
          Predictor = predictor,
          Estimate = fixed_effects[i, "Estimate"],
          SE = fixed_effects[i, "Est.Error"],
          CI_Lower = fixed_effects[i, "l-95% CI"],
          CI_Upper = fixed_effects[i, "u-95% CI"],
          P_value = 2 * (1 - pnorm(abs(fixed_effects[i, "Estimate"] / fixed_effects[i, "Est.Error"]))),
          stringsAsFactors = FALSE
        ))
      }
    }
    return(results_table)
  }
}

# Extract regression tables for all four models if they don't exist
if (!exists("reg_table_multinom_1") || !exists("reg_table_multinom_int") || 
    !exists("reg_table_multinom_2") || !exists("reg_table_multinom_house_int")) {
  
  # Model 1: Base net loss model
  reg_table_multinom_1 <- extract_regression_table(model_multilevel, "Local Authority")
  
  # Model 2: Net loss with age interaction
  reg_table_multinom_int <- extract_regression_table(model_multilevel_int, "Local Authority")
  
  # Model 3: House price model
  reg_table_multinom_2 <- extract_regression_table(model_multilevel_house, "Local Authority")
  
  # Model 4: House price with age interaction
  reg_table_multinom_house_int <- extract_regression_table(model_multilevel_house_int, "Local Authority")
  
  # Format all tables
  format_table <- function(table) {
    table %>%
      mutate(
        Significance = case_when(
          P_value < 0.001 ~ "***",
          P_value < 0.01  ~ "**",
          P_value < 0.05  ~ "*",
          P_value < 0.1   ~ ".",
          TRUE ~ ""
        ),
        Estimate_formatted = sprintf("%.3f%s", Estimate, Significance),
        CI_formatted = sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper)
      ) %>%
      dplyr::select(Category, Predictor, Estimate_formatted, CI_formatted, P_value)
  }
  
  reg_table_multinom_1 <- format_table(reg_table_multinom_1)
  reg_table_multinom_int <- format_table(reg_table_multinom_int)
  reg_table_multinom_2 <- format_table(reg_table_multinom_2)
  reg_table_multinom_house_int <- format_table(reg_table_multinom_house_int)
}

# First, let's combine the four regression tables
# Create copies with model identifiers
reg_table_1 <- reg_table_multinom_1 %>%
  mutate(Model = "Model 1: Net Loss")

reg_table_2 <- reg_table_multinom_int %>%
  mutate(Model = "Model 2: Net Loss × Age")

reg_table_3 <- reg_table_multinom_2 %>%
  mutate(Model = "Model 3: House Price")

reg_table_4 <- reg_table_multinom_house_int %>%
  mutate(Model = "Model 4: House Price × Age")

# Replace predictor names for clarity in all tables
format_predictors <- function(table) {
  table %>%
    mutate(Predictor = case_when(
      Predictor == "Intercept" ~ "Intercept",
      Predictor == "net_loss_s" ~ "Net Loss (std)",
      Predictor == "average_house_price_per_sq_m_s" ~ "House Price per sq.m (std)",
      Predictor == "closed1" ~ "Closed (Yes)",
      Predictor == "children_in_care_s" ~ "Children in Care (std)",
      Predictor == "age_s" ~ "Age (std)",
      Predictor == "net_loss_s:age_s" ~ "Net Loss × Age",
      Predictor == "average_house_price_per_sq_m_s:age_s" ~ "House Price × Age",
      TRUE ~ Predictor
    ))
}

reg_table_1 <- format_predictors(reg_table_1)
reg_table_2 <- format_predictors(reg_table_2)
reg_table_3 <- format_predictors(reg_table_3)
reg_table_4 <- format_predictors(reg_table_4)

# Combine the tables
combined_table <- bind_rows(reg_table_1, reg_table_2, reg_table_3, reg_table_4)

# Create a wide-format table with models as columns
wide_table <- combined_table %>%
  select(Category, Predictor, Model, Estimate_formatted, CI_formatted) %>%
  unite("Value", Estimate_formatted, CI_formatted, sep = " ") %>%
  pivot_wider(
    names_from = Model, 
    values_from = Value,
    values_fill = " "  # Use empty space for missing values
  ) %>%
  arrange(Category, match(Predictor, c(
    "Intercept", 
    "Net Loss (std)", 
    "House Price per sq.m (std)", 
    "Age (std)",
    "Net Loss × Age", 
    "House Price × Age",
    "Closed (Yes)", 
    "Children in Care (std)"
  )))%>%
  dplyr::filter(Predictor!="Intercept",
                Predictor!="Closed (Yes)",
                Predictor!="Children in Care (std)",
  )

# Create flextable
ft <- flextable(wide_table) %>%
  set_header_labels(
    Category = "Sector Type",
    Predictor = "Predictor",
    `Model 1: Net Loss` = "Model 1",
    `Model 2: Net Loss × Age` = "Model 2",
    `Model 3: House Price` = "Model 3",
    `Model 4: House Price × Age` = "Model 4"
  )

# Add header rows for grouping
ft <- add_header_row(
  ft,
  values = c("", "", "Base Models", "Interaction Models"),
  colwidths = c(1, 1, 2, 2)
)

ft <- add_header_row(
  ft,
  values = c("", "", "Coefficient Estimates [95% CI]"),
  colwidths = c(1, 1, 4)
)

# Style the table for Word
ft <- ft %>%
  theme_booktabs() %>%
  align(j = 1:2, align = "left") %>%
  align(j = 3:6, align = "center") %>%
  fontsize(size = 9) %>%  # Smaller font for better fit in Word
  width(j = 1, width = 1.5) %>%
  width(j = 2, width = 1.75) %>%
  width(j = 3:6, width = 1.6) %>%
  bold(j = 1) %>%
  italic(j = 2) %>%
  border(i = 1, border.top = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(i = 3, border.bottom = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(border.bottom = fp_border(color = "black", width = 1), part = "body") %>%
  bg(bg = "#f2f2f2", part = "header")

# Group rows by sector type
sector_types <- unique(wide_table$Category)
for (sector in sector_types) {
  indices <- which(wide_table$Category == sector)
  if (length(indices) > 1) {
    ft <- merge_at(ft, i = indices, j = 1)
  }
}

# Set caption and footnote
ft <- set_caption(ft, 
                  caption = "Multinomial Logistic Regression Models Comparing Care Home Ownership Types (Reference: Local Authority)")

ft <- add_footer_lines(ft, 
                       values = "Note: Standardized continuous predictors. *p<0.05; **p<0.01; ***p<0.001.")

# Save directly to Word
save_as_docx(ft, path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table3_no_priors.docx")

# Return the flextable object
ft

####all owenership cats####



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
d <- ggplot(nobser %>% filter(time > -155),
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


ggsave(plot=d, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/all_cats.png", width=8, height=6, dpi=600)


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



####tables for figu 3####


# Load required packages
library(tidyverse)
library(broom.mixed)
library(knitr)
library(kableExtra)

# Extract coefficient tables from the Bayesian models
# House price model coefficients
house_price_coefs <- fixef(model_multilevel_house_price) %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  # Ensure nice formatting for parameter names
  mutate(parameter = gsub("mu", "", parameter)) %>%
  mutate(parameter = gsub("_", " ", parameter)) %>%
  # Add significance stars
  mutate(sig = case_when(
    `Q2.5` > 0 | `Q97.5` < 0 ~ "*",
    TRUE ~ ""
  ))

# Need (net loss) model coefficients
need_coefs <- fixef(model_multilevel_need) %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  # Ensure nice formatting for parameter names
  mutate(parameter = gsub("mu", "", parameter)) %>%
  mutate(parameter = gsub("_", " ", parameter)) %>%
  # Add significance stars
  mutate(sig = case_when(
    `Q2.5` > 0 | `Q97.5` < 0 ~ "*",
    TRUE ~ ""
  ))

# Create formatted table for House Price model
house_price_table <- house_price_coefs %>%
  mutate(
    estimate_fmt = sprintf("%.4f%s", Estimate, sig),
    std_error = sprintf("(%.4f)", Est.Error)
  ) %>%
  select(parameter, estimate_fmt, std_error) %>%
  pivot_wider(
    names_from = parameter,
    values_from = c(estimate_fmt, std_error),
    names_sep = "_"
  )

# Create formatted table for Need model
need_table <- need_coefs %>%
  mutate(
    estimate_fmt = sprintf("%.4f%s", Estimate, sig),
    std_error = sprintf("(%.4f)", Est.Error)
  ) %>%
  select(parameter, estimate_fmt, std_error) %>%
  pivot_wider(
    names_from = parameter,
    values_from = c(estimate_fmt, std_error),
    names_sep = "_"
  )

# Print nicely formatted tables
cat("Table 1: House Price Model Results\n")
house_price_coefs %>%
  mutate(
    Estimate = sprintf("%.4f%s", Estimate, sig),
    `Std. Error` = sprintf("%.4f", Est.Error),
    `Lower 95% CI` = sprintf("%.4f", Q2.5),
    `Upper 95% CI` = sprintf("%.4f", Q97.5)
  ) %>%
  select(parameter, Estimate, `Std. Error`, `Lower 95% CI`, `Upper 95% CI`) %>%
  kable(format = "simple", align = "lrrrr")

cat("\nTable 2: Need (Net Loss) Model Results\n")
need_coefs %>%
  mutate(
    Estimate = sprintf("%.4f%s", Estimate, sig),
    `Std. Error` = sprintf("%.4f", Est.Error),
    `Lower 95% CI` = sprintf("%.4f", Q2.5),
    `Upper 95% CI` = sprintf("%.4f", Q97.5)
  ) %>%
  select(parameter, Estimate, `Std. Error`, `Lower 95% CI`, `Upper 95% CI`) %>%
  kable(format = "simple", align = "lrrrr")

# Alternative visualization: sector-specific effects
# Extract just the coefficients for the predictors (not intercepts)
house_price_effects <- house_price_coefs %>%
  filter(str_detect(parameter, "average house price per sq m")) %>%
  mutate(sector = str_extract(parameter, "^[^_]+"))

need_effects <- need_coefs %>%
  filter(str_detect(parameter, "net loss")) %>%
  mutate(sector = str_extract(parameter, "^[^_]+"))

# Create a simple dot plot of coefficients with error bars
ggplot(house_price_effects, aes(x = sector, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Effect of House Price on Sector",
    x = "Sector",
    y = "Coefficient Estimate"
  ) +
  theme_minimal() +
  coord_flip()

ggplot(need_effects, aes(x = sector, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Effect of Net Loss on Sector",
    x = "Sector",
    y = "Coefficient Estimate"
  ) +
  theme_minimal() +
  coord_flip()

# If you want more traditional regression tables with standard frequentist logic
# We can create simplified tables that mimic lm() output
# Extract and format key coefficients

# Function to format coefficient table in a traditional way
format_coef_table <- function(model_summary, model_name) {
  coefs <- model_summary %>%
    filter(!str_detect(parameter, "Intercept")) %>%
    mutate(
      significance = case_when(
        abs(Q2.5) > 0 & Q2.5 * Q97.5 > 0 ~ "*",
        TRUE ~ ""
      ),
      formatted_estimate = sprintf("%.4f%s", Estimate, significance),
      formatted_se = sprintf("(%.4f)", Est.Error)
    ) %>%
    select(parameter, formatted_estimate, formatted_se)
  
  # Add intercepts as separate rows
  intercepts <- model_summary %>%
    filter(str_detect(parameter, "Intercept")) %>%
    mutate(
      significance = case_when(
        abs(Q2.5) > 0 & Q2.5 * Q97.5 > 0 ~ "*",
        TRUE ~ ""
      ),
      formatted_estimate = sprintf("%.4f%s", Estimate, significance),
      formatted_se = sprintf("(%.4f)", Est.Error)
    ) %>%
    select(parameter, formatted_estimate, formatted_se)
  
  # Combine for final table
  data.frame(
    Model = model_name,
    Parameter = c("Intercepts", intercepts$parameter, "Coefficients", coefs$parameter),
    Estimate = c("", intercepts$formatted_estimate, "", coefs$formatted_estimate),
    `Std.Error` = c("", intercepts$formatted_se, "", coefs$formatted_se)
  )
}

# Create traditional-style regression tables
house_price_table_trad <- format_coef_table(house_price_coefs, "House Price Model")
need_table_trad <- format_coef_table(need_coefs, "Net Loss Model")

# Print the tables
cat("\nTraditional Style Regression Tables\n")
cat("\nHouse Price Model\n")
house_price_table_trad %>%
  kable(format = "simple", align = "lllr")

cat("\nNet Loss Model\n")
need_table_trad %>%
  kable(format = "simple", align = "lllr")

# Note: * indicates significance at 95% confidence level
cat("\nNote: * indicates significance at 95% confidence level (CI doesn't cross zero)")


# For the House Price Model
house_price_table_html <- house_price_coefs %>%
  mutate(
    Estimate = sprintf("%.4f%s", Estimate, sig),
    `Std. Error` = sprintf("%.4f", Est.Error),
    `Lower 95% CI` = sprintf("%.4f", Q2.5),
    `Upper 95% CI` = sprintf("%.4f", Q97.5)
  ) %>%
  select(parameter, Estimate, `Std. Error`, `Lower 95% CI`, `Upper 95% CI`) %>%
  kable(format = "html", align = "lrrrr", caption = "House Price Model Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE)

# Save to HTML file
writeLines(house_price_table_html, "house_price_model_table.html")

# For the Net Loss Model
need_table_html <- need_coefs %>%
  mutate(
    Estimate = sprintf("%.4f%s", Estimate, sig),
    `Std. Error` = sprintf("%.4f", Est.Error),
    `Lower 95% CI` = sprintf("%.4f", Q2.5),
    `Upper 95% CI` = sprintf("%.4f", Q97.5)
  ) %>%
  select(parameter, Estimate, `Std. Error`, `Lower 95% CI`, `Upper 95% CI`) %>%
  kable(format = "html", align = "lrrrr", caption = "Net Loss Model Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE)

# Save to HTML file
writeLines(need_table_html, "net_loss_model_table.html")

# For traditional style tables (combined in one file)
combined_tables_html <- bind_rows(
  house_price_table_trad %>% mutate(Model = "House Price Model"),
  data.frame(Model = "", Parameter = "", Estimate = "", Std.Error = ""),  # Empty row as separator
  need_table_trad %>% mutate(Model = "Net Loss Model")
) %>%
  kable(format = "html", align = "llrr", caption = "Combined Regression Tables") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) %>%
  add_footnote("* indicates significance at 95% confidence level (CI doesn't cross zero)")

# Save to HTML file
writeLines(combined_tables_html, "combined_regression_tables.html") 


####to delete no evidence#####



# Load necessary libraries
library(lmerTest)
library(dplyr)
library(ggplot2)
library(purrr)
library(broom.mixed)  # For tidying mixed-effects models

# Define the outcome variables
outcomes <- c("sector_third", "sector_indiv", "sector_corp", 
              "sector_invest", "overall.average", "profit_margin_average")

# Define the potential control variables (to be varied)
controls <- c("Places", "age_years", "closed", "chain_size_0s", 
              "children_in_care_s", "residential_expenditure_s", 
              "median_wage_s", 
              "Region.Country.name")

# A list to collect results from all specifications
results <- list()

# Loop over each outcome variable
for (outcome in outcomes) {
  
  # For each possible number of controls (from 0 to all available)
  for (k in 0:length(controls)) {
    
    # Get all combinations of controls of size k 
    ctrl_combinations <- if(k == 0) list(character(0)) else combn(controls, k, simplify = FALSE)
    
    # Loop over each specific combination of controls
    for (ctrl_set in ctrl_combinations) {
      
      # Always include 'net_loss_s' and add the current set of controls
      fixed_effects <- if(length(ctrl_set) > 0) {
        paste(c("median_house_price_detached_s", ctrl_set), collapse = " + ")
      } else {
        "median_house_price_detached_s"
      }
      
      # Construct the model formula: outcome ~ net_loss_s + ... + (1|Local.authority)
      formula_str <- paste(outcome, "~", fixed_effects, "+ (1 | Local.authority)")
      formula_model <- as.formula(formula_str)
      
      # Fit the model using lmer (wrap in try() in case of convergence or data issues)
      fit <- try(lmer(formula_model, data = mlm), silent = TRUE)
      if (inherits(fit, "try-error")) next
      
      # Extract the net_loss_s coefficient details using broom.mixed::tidy with confidence intervals
      tidy_fit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
      net_loss_coef <- tidy_fit %>% filter(term == "median_house_price_detached_s")
      
      # If confidence intervals are missing, compute them using a normal approximation
      if(nrow(net_loss_coef) > 0) {
        if (!("conf.low" %in% names(net_loss_coef)) ||
            !("conf.high" %in% names(net_loss_coef))) {
          net_loss_coef <- net_loss_coef %>%
            mutate(conf.low = estimate - 1.96 * std.error,
                   conf.high = estimate + 1.96 * std.error)
        }
        results[[length(results) + 1]] <- data.frame(
          outcome      = outcome,
          controls     = if(length(ctrl_set) == 0) "None" else paste(ctrl_set, collapse = ", "),
          num_controls = length(ctrl_set),
          estimate     = net_loss_coef$estimate,
          std.error    = net_loss_coef$std.error,
          conf.low     = net_loss_coef$conf.low,
          conf.high    = net_loss_coef$conf.high
        )
      }
    }
  }
}

# Combine the list into one data frame if at least one model was successful
if(length(results) > 0) {
  results_df <- do.call(rbind, results)
} else {
  stop("No model specifications were successfully fit.")
}

results_df$outcome <- factor(ifelse(results_df$outcome=="sector_third", "Third sector",
                                    ifelse(results_df$outcome=="sector_indiv", "Individual-owned",
                                           ifelse(results_df$outcome=="sector_corp", "Corporate-owned",
                                                  ifelse(results_df$outcome=="sector_invest", "Investment-owned",
                                                         ifelse(results_df$outcome=="overall.average", "Quality score",
                                                                ifelse(results_df$outcome=="profit_margin_average", "Profit Margin (%)", NA)))))))

# Create a specification curve plot:
y <- ggplot(results_df, aes(x = factor(num_controls), y = estimate)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, alpha = 0.3) +
  facet_wrap(~ outcome, scales = "free_y") +
  geom_hline(yintercept = 0)+
  labs(x = "Number of Controls Included",
       y = "Coefficient Estimate for Area Need (net loss)",
       title = "Specification Curve: Effect of Area Need, varying controls") +
  theme_bw()
ggsave(plot=y, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/specification_curve.jpeg", width=8, height=8, dpi=600)








# Load necessary libraries
library(lmerTest)
library(dplyr)
library(ggplot2)
library(purrr)
library(broom.mixed)  # For tidying mixed-effects models

# Define the outcome variables
outcomes <- c("sector_third", "sector_indiv", "sector_corp", 
              "sector_invest", "overall.average", "profit_margin_average")

# Define the potential control variables (to be varied)
controls <- c("Places", "age_years", "closed", "chain_size_0s", 
              "children_in_care_s", "residential_expenditure_s", 
              "average_house_price_per_sq_m_s", "median_wage_s", 
              "Region.Country.name")

# A list to collect results from all specifications
results <- list()

# Loop over each outcome variable
for (outcome in outcomes) {
  
  # For each possible number of controls (from 0 to all available)
  for (k in 0:length(controls)) {
    
    # Get all combinations of controls of size k 
    ctrl_combinations <- if(k == 0) list(character(0)) else combn(controls, k, simplify = FALSE)
    
    # Loop over each specific combination of controls
    for (ctrl_set in ctrl_combinations) {
      
      # Always include 'net_loss_s' and add the current set of controls
      fixed_effects <- if(length(ctrl_set) > 0) {
        paste(c("net_loss_s", ctrl_set), collapse = " + ")
      } else {
        "net_loss_s"
      }
      
      # Construct the model formula: outcome ~ net_loss_s + ... + (1|Local.authority)
      formula_str <- paste(outcome, "~", fixed_effects, "+ (1 | Local.authority)")
      formula_model <- as.formula(formula_str)
      
      # Fit the model using lmer (wrap in try() in case of convergence or data issues)
      fit <- try(lmer(formula_model, data = mlm), silent = TRUE)
      if (inherits(fit, "try-error")) next
      
      # Extract the net_loss_s coefficient details using broom.mixed::tidy with confidence intervals
      tidy_fit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
      net_loss_coef <- tidy_fit %>% filter(term == "net_loss_s")
      
      # If confidence intervals are missing, compute them using a normal approximation
      if(nrow(net_loss_coef) > 0) {
        if (!("conf.low" %in% names(net_loss_coef)) ||
            !("conf.high" %in% names(net_loss_coef))) {
          net_loss_coef <- net_loss_coef %>%
            mutate(conf.low = estimate - 1.96 * std.error,
                   conf.high = estimate + 1.96 * std.error)
        }
        results[[length(results) + 1]] <- data.frame(
          outcome      = outcome,
          controls     = if(length(ctrl_set) == 0) "None" else paste(ctrl_set, collapse = ", "),
          num_controls = length(ctrl_set),
          estimate     = net_loss_coef$estimate,
          std.error    = net_loss_coef$std.error,
          conf.low     = net_loss_coef$conf.low,
          conf.high    = net_loss_coef$conf.high
        )
      }
    }
  }
}

# Combine the list into one data frame if at least one model was successful
if(length(results) > 0) {
  results_df <- do.call(rbind, results)
} else {
  stop("No model specifications were successfully fit.")
}

results_df$outcome <- factor(ifelse(results_df$outcome=="sector_third", "Third sector",
                                    ifelse(results_df$outcome=="sector_indiv", "Individual-owned",
                                           ifelse(results_df$outcome=="sector_corp", "Corporate-owned",
                                                  ifelse(results_df$outcome=="sector_invest", "Investment-owned",
                                                         ifelse(results_df$outcome=="overall.average", "Quality score",
                                                                ifelse(results_df$outcome=="profit_margin_average", "Profit Margin (%)", NA)))))))

# Create a specification curve plot:
y <- ggplot(results_df, aes(x = factor(num_controls), y = estimate)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, alpha = 0.3) +
  facet_wrap(~ outcome, scales = "free_y") +
  geom_hline(yintercept = 0)+
  labs(x = "Number of Controls Included",
       y = "Coefficient Estimate for Area Need (net loss)",
       title = "Specification Curve: Effect of Area Need, varying controls") +
  theme_bw()
ggsave(plot=y, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/specification_curve.jpeg", width=8, height=8, dpi=600)







nbm <- as.data.frame(LA_panel_data) %>%
  dplyr::mutate(needQuint = factor(ntile(net_loss, 5),
                                   levels=c(5,4,3,2,1)),
                needQuint = ifelse(needQuint==1, "1 (Low need)",
                                   ifelse(needQuint==2, "2",
                                          ifelse(needQuint==3,"3",
                                                 ifelse(needQuint==4, "4",
                                                        ifelse(needQuint==5, "5 (High need)", NA))))),
                needQuint = factor(needQuint,
                                   levels = c("5 (High need)", "4", "3", "2", "1 (Low need)")))



nbm$net_loss <- as.numeric(nbm$net_loss_lagged)
nbm$median_wage <- as.numeric(nbm$median_wage)
nbm$median_house_price_detached <- as.numeric(nbm$median_house_price_detached)
nbm$occupancy_rate_fte <- as.numeric(nbm$occupancy_rate_fte)
nbm$Unemployment.rate...aged.16.64. <- as.numeric(nbm$Unemployment.rate...aged.16.64.)
nbm$children_in_care <- as.numeric(as.numeric(nbm$out_of_area_per)*as.numeric(nbm$out_of_area))
nbm$claimant_count_rate <- as.numeric(nbm$claimant_count_rate)
nbm$white <- as.numeric(nbm$X..of.white...aged.16.)
nbm$residential_expenditure <- as.numeric(nbm$residential_expenditure)


# Create a scaled version in the data frame
nbm$net_loss_s <- scale(nbm$net_loss)
nbm$median_wage_s <- scale(nbm$median_wage)
nbm$median_house_price_detached_s <- scale(nbm$median_house_price_detached)
nbm$occupancy_rate_fte_s <- scale(nbm$occupancy_rate_fte)
nbm$Unemployment.rate...aged.16.64._s <- scale(nbm$Unemployment.rate...aged.16.64.)
nbm$claimant_count_rate_s <- scale(nbm$claimant_count_rate)
nbm$children_in_care_s <- scale(nbm$children_in_care)
nbm$white_s <- scale(nbm$white)
nbm$residential_expenditure_s <- scale(nbm$residential_expenditure)


nbm$non_invest = scale(as.numeric(nbm$All_homes-nbm$Homes_Investment_owned))
nbm$non_corp = scale(as.numeric(nbm$All_homes-nbm$Homes_Corporate_owned))
nbm$non_indiv = scale(as.numeric(nbm$All_homes-nbm$Homes_Individual_owned))
nbm$non_la = scale(as.numeric(nbm$All_homes-nbm$Homes_Local_Authority))
nbm$non_3rd = scale(as.numeric(nbm$All_homes-nbm$Homes_Third_sector))

reg1 <- plm(residential_expenditure_s ~  net_loss_s  + median_house_price_detached_s +median_wage_s +Unemployment.rate...aged.16.64._s + white_s+ occupancy_rate_fte_s+claimant_count_rate_s + children_in_care_s  +  Region.Country.name, 
            index = c("Local.authority", "year"), data = nbm%>%
              dplyr::filter(Region.Country.name!="London"), model = "pooling")
coef_test(reg1, vcov = "CR2", cluster = nbm%>%
            dplyr::filter(Region.Country.name!="London")$Local.authority, test = "Satterthwaite")

nbm_filtered <- nbm %>% dplyr::filter(Region.Country.name != "London")

# Run the PLM model
reg1 <- plm(residential_expenditure_s ~ net_loss_s + median_house_price_detached_s + 
              median_wage_s + Unemployment.rate...aged.16.64._s + white_s + 
              occupancy_rate_fte_s + claimant_count_rate_s + children_in_care_s + 
              Region.Country.name, 
            index = c("Local.authority", "year"), 
            data = nbm_filtered, 
            model = "pooling")

# Run coefficient test with clustering
coef_test(reg1, vcov = "CR2", cluster = nbm_filtered$Local.authority, test = "Satterthwaite")

reg1 <- plm(as.numeric(unregulated) ~  net_loss_s+ as.numeric(Asylum_per)  + median_house_price_detached_s +median_wage_s +Unemployment.rate...aged.16.64._s + white_s+ occupancy_rate_fte_s+claimant_count_rate_s + children_in_care_s  +  Region.Country.name, 
            index = c("Local.authority", "year"), data = nbm, model = "pooling")
coef_test(reg1, vcov = "CR2", cluster = nbm$Local.authority, test = "Satterthwaite")






# Check its length
length(nbm$net_loss_s)  # Should match nrow(nbm)

rownames(nbm) <- NULL


library(dplyr)
library(plm)
library(sjPlot)
library(clubSandwich)
library(cowplot)

# Prepare the panel data
nbm <- nbm %>% distinct(.keep_all = TRUE)

# Load required libraries if not already loaded
# library(plm)
# library(dplyr)
# library(sjPlot)
# library(lmtest)
# library(sandwich)
# library(clubSandwich)  # For coef_test function

# Create a named vector for variable labels
var_labels <- c(
  "needQuint" = "Area need\n(net loss)",
  "median_wage_s" = "Median Wage\n(£, hourly)",
  "median_house_price_detached_s" = "Median House Price\n(Detached)",
  "Unemployment.rate...aged.16.64._s" = "Unemployment Rate\n(16-64, %)",
  "occupancy_rate_fte_s" = "Social Worker Occupancy Rate\n(FTE, %)",
  "white_s" = "White Population\n(%)",
  "claimant_count_rate_s" = "Claimant Count Rate\n(%)",
  "children_in_care_s" = "Children in Residential Care"
)

# List of variables to include in models
vars_to_include <- c("needQuint", "median_wage_s", "median_house_price_detached_s", 
                     "Unemployment.rate...aged.16.64._s", "occupancy_rate_fte_s", 
                     "white_s", "claimant_count_rate_s", "children_in_care_s")

# Set up the panel data
nbm <- pdata.frame(nbm %>% distinct(.keep_all = TRUE), 
                   index = c("Local.authority", "year"))

add_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  if (p_value < 0.1) return(".")
  return("")
}

# Function to create model and plot with clustered standard errors
create_model_plot <- function(dependent_var, title) {
  # Run the model
  model_formula <- as.formula(paste(
    dependent_var, "~", 
    paste(vars_to_include, collapse = " + "), 
    "+ factor(Region.Country.name)"
  ))
  
  model <- lm(model_formula, data = nbm)
  
  # Compute clustered standard errors
  clustered <- coef_test(model, vcov = "CR2", 
                         cluster = nbm$Local.authority, 
                         test = "Satterthwaite")
  
  # Extract coefficients and SEs from clustered results
  # Convert the clustered results to a data frame
  coefs_data <- data.frame(
    term = clustered$Coef,
    estimate = clustered$beta,
    std.error = clustered$SE,
    satt.freedom = clustered$df_Satt,
    statistic = clustered$tstat,
    p.value = clustered$p_Satt,
    stringsAsFactors = FALSE
  )
  
  
  # Set significance level (for a 95% confidence interval)
  alpha <- 0.05
  
  # Compute the lower and upper bounds of the confidence interval for each coefficient
  coefs_data$conf.low <- coefs_data$estimate - qt(1 - alpha/2, df = coefs_data$satt.freedom) * coefs_data$std.error
  coefs_data$conf.high <- coefs_data$estimate + qt(1 - alpha/2, df = coefs_data$satt.freedom) * coefs_data$std.error
  
  # Add significance stars
  coefs_data$stars <- sapply(coefs_data$p.value, add_significance_stars)
  
  # Format estimates for display (2 decimal places)
  coefs_data$est_label <- paste0(format(round(coefs_data$estimate, 2), nsmall = 2), coefs_data$stars)
  
  # Filter to only include the variables we want to plot
  coefs_filtered <- coefs_data[coefs_data$term %in% vars_to_include, ]
  
  # Create factor with levels in desired order for plotting
  coefs_filtered$term <- factor(coefs_filtered$term, levels = rev(vars_to_include))
  
  plot <- ggplot(coefs_filtered, aes(x = term, y = estimate, 
                                     ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    coord_flip() +
    scale_x_discrete(labels = var_labels[as.character(coefs_filtered$term)]) +
    # Add coefficient values with stars
    geom_text(aes(label = est_label), vjust = -0.7, hjust = ifelse(coefs_filtered$estimate > 0, -0.1, 1.1), size = 3) +
    labs(
      title = title,
      x = "",
      y = "Coefficient"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  
  return(list(model = model, clustered = clustered, plot = plot))
}


# Create all models and plots
models <- list(
  all = create_model_plot("all_change", "All Homes"),
  ind = create_model_plot("ind_change", "Individual Owned"),
  corp = create_model_plot("corp_change", "Corporate Owned"),
  invest = create_model_plot("invest_change", "Investment Owned"),
  la = create_model_plot("la_change", "Local Authority"),
  third = create_model_plot("third_change", "Third Sector")
)

# Extract just the plots for the grid
plots <- list(
  models$all$plot,
  models$la$plot,
  models$third$plot,
  models$ind$plot,
  models$corp$plot,
  models$invest$plot
)

# Create the combined plot grid
combined_plot <- cowplot::plot_grid(plotlist = plots, ncol = 3)

# Display the combined plot
combined_plot

# To access the clustered standard errors results for any model:
# models$all$clustered
# models$ind$clustered
# etc.


ggsave(plot=combined_plot, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_2.jpeg", width=14, height=7, dpi=600)









corr <- LA_panel_data %>% dplyr::select(Homes_Individual_owned, Homes_Corporate_owned, Homes_Investment_owned, Homes_Third_sector, Homes_Local_Authority,
                                        median_wage, out_of_area_per,  net_loss, vacancy_rate_fte, average_house_price_per_sq_m, children_in_care, residential_expenditure, 
                                        Asylum_per, unregulated, claimant_count_rate, Unemployment.rate...aged.16.64.)%>%
  dplyr::distinct(.keep_all = T)%>%
  mutate(across(everything(), as.numeric))%>%
  dplyr::mutate(unregulated_per = unregulated/children_in_care)%>%
  dplyr::select(-unregulated)



# Calculate the correlation matrix
cor_matrix <- cor(corr, use = "complete.obs")

# Create a more attractive visualization using corrplot
par(mar = c(1, 1, 2, 1))  # Adjust margins


# Set up a high-resolution image file
png("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/correlation_matrix_full.png", width = 5000, height = 5000, res = 500)

# Set the plotting parameters for a full-size plot
par(mar = c(2, 2, 2, 2), # Minimal margins
    oma = c(0, 0, 0, 0), # No outer margins
    mfrow = c(1, 1))     # Single plot

# Generate the correlation plot with enhanced visual features
corrplot(cor_matrix,
         method = "color",      # Color-coded squares
         type = "upper",        # Upper triangular matrix
         order = "hclust",      # Hierarchical clustering to group similar variables
         tl.col = "black",      # Text label color
         tl.srt = 45,           # Text label rotation
         addCoef.col = "black", # Add correlation coefficients to the plot
         number.cex = 0.7,      # Size of correlation coefficients
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), # Custom color palette
         diag = FALSE,          # Hide the diagonal
         mar = c(0, 0, 1, 0),   # Minimal margins
         tl.cex = 1.2)          # Text label size increased

# Close the device to save the image
dev.off()







corr <- mlm %>% dplyr::select(age_years, Places, left, profit_margin_average, ebitda_margin_average, salaries_turnover_average, overall.average, ever_inadequate, ever_outstanding, chain_size_0s)%>%
  dplyr::distinct(.keep_all = T)%>%
  mutate(across(everything(), as.numeric))



# Calculate the correlation matrix
cor_matrix <- cor(corr, use = "complete.obs")

# Create a more attractive visualization using corrplot
par(mar = c(1, 1, 2, 1))  # Adjust margins


# Set up a high-resolution image file
png("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/correlation_matrix_full_home_level.png", width = 5000, height = 5000, res = 500)

# Set the plotting parameters for a full-size plot
par(mar = c(2, 2, 2, 2), # Minimal margins
    oma = c(0, 0, 0, 0), # No outer margins
    mfrow = c(1, 1))     # Single plot

# Generate the correlation plot with enhanced visual features
corrplot(cor_matrix,
         method = "color",      # Color-coded squares
         type = "upper",        # Upper triangular matrix
         order = "hclust",      # Hierarchical clustering to group similar variables
         tl.col = "black",      # Text label color
         tl.srt = 45,           # Text label rotation
         addCoef.col = "black", # Add correlation coefficients to the plot
         number.cex = 0.7,      # Size of correlation coefficients
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), # Custom color palette
         diag = FALSE,          # Hide the diagonal
         mar = c(0, 0, 1, 0),   # Minimal margins
         tl.cex = 1.2)          # Text label size increased

# Close the device to save the image
dev.off()




