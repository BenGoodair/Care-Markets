create_figure_3 <- function(){
  
  if (!require("pacman")) install.packages("pacman")
  
  pacman::p_load(tidyverse, brms, patchwork, ggdist, scales)

  
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(brms, dplyr, tidyr, broom.mixed, kableExtra, officer)
  
  source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")
  
  mlm  <- create_home_data()
  
  
  
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
  
  
  # Model 1: Net Loss
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
    prior     = priors,
    cores     = 4,
    iter      = 2000
  )
  
  
  # Model 3: House Price
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
    prior     = priors,
    cores     = 4,
    iter      = 2000
  )
  
  
  # Helper: extract posterior OR draws for an effect
  extract_posterior_OR <- function(model, effect_name){
    draws <- as_draws_df(model)
    sectors <- c("Corporateowned","Individualowned","Investmentowned","Thirdsector")
    map_dfr(sectors, ~{
      term <- paste0("b_mu", .x, "_", effect_name)
      if(term %in% names(draws)){
        tibble(sector = .x, OR = exp(draws[[term]]))
      } else tibble()
    }) %>%
      filter(!is.na(OR)) %>%
      mutate(
        sector = factor(sector, levels = sectors),
        label = recode(sector,
                       Investmentowned = "Investment owned",
                       Corporateowned  = "Corporate owned",
                       Individualowned = "Individual owned",
                       Thirdsector     = "Third sector")
      )
  }
  
  # Extract OR draws for need and price
  or_need  <- extract_posterior_OR(model_multilevel,        "net_loss_s")
  or_price <- extract_posterior_OR(model_multilevel_house,  "average_house_price_per_sq_m_s")
  
  # Panel 1: Half‑eye plots of OR distributions (unchanged)
  p1_need <- ggplot(or_need,  aes(x = OR, y = label, fill = label)) +
    stat_halfeye(.width = c(0.5,0.95), slab_alpha = 0.8, slab_size = 1) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_continuous(trans = "log10", labels = label_number()) +
    scale_fill_manual(values = c(
      "Investment owned" = "#D32F2F",
      "Corporate owned"  = "#757575",
      "Individual owned" = "#757575",
      "Third sector"     = "#757575"
    ), guide = "none") +
    labs(title = "Odds Ratios: Area Need",
         x = "Odds Ratio", y = "Sector") +
    theme_minimal(base_size = 12) +
    coord_cartesian(xlim = c(0.1, 3))
  
  p1_price <- ggplot(or_price, aes(x = OR, y = label, fill = label)) +
    stat_halfeye(.width = c(0.5,0.95), slab_alpha = 0.8, slab_size = 1) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_continuous(trans = "log10", labels = label_number()) +
    scale_fill_manual(values = c(
      "Investment owned" = "#D32F2F",
      "Corporate owned"  = "#757575",
      "Individual owned" = "#757575",
      "Third sector"     = "#757575"
    ), guide = "none") +
    labs(title = "Odds Ratios: House Price",
         x = "Odds Ratio", y = "") +
    theme_minimal(base_size = 12) +
    coord_cartesian(xlim = c(0.1, 3))
  
  # Shared colors
  sector_colors <- c(
    "Local Authority"  = "#008F5D",
    "Investment owned" = "#5B0000",
    "Corporate owned"  = "#FF5E5E",
    "Individual owned" = "#E4007C",
    "Third sector"     = "#F0AB00"
  )
  
  # PANEL 2: Predicted probabilities by Area Need
  grid_need <- expand_grid(
    net_loss_s                   = c(-1.5, 1.5),
    average_house_price_per_sq_m_s = 0,
    children_in_care_s           = 0,
    closed                       = 0
  )
  preds_need <- posterior_epred(
    model_multilevel,
    newdata         = grid_need,
    re_formula      = NA,
    allow_new_levels = TRUE
  )
  df_need <- as_tibble(reshape2::melt(preds_need)) %>%
    rename(draw = Var1, row = Var2, category = Var3, prob = value) %>%
    mutate(
      need = ifelse(grid_need[row, ]$net_loss_s < 0, "Low need", "High need"),
      label = recode(category,
                     b_muInvestmentowned = "Investment owned",
                     b_muCorporateowned  = "Corporate owned",
                     b_muIndividualowned = "Individual owned",
                     b_muThirdsector     = "Third sector")
    )
  # Summarize both 50% and 95% intervals
  df_need_sum <- df_need %>%
    group_by(need, label) %>%
    summarize(
      median  = median(prob),
      lower95 = quantile(prob, 0.025),
      upper95 = quantile(prob, 0.975),
      lower50 = quantile(prob, 0.25),
      upper50 = quantile(prob, 0.75),
      .groups = "drop"
    )
  
  p2 <- ggplot(df_need_sum, aes(x = need, y = median, color = label)) +
    # 95% CI
    geom_errorbar(aes(ymin = lower95, ymax = upper95),
                  position = position_dodge(width = 0.5),
                  width = 0.2) +
    # 50% CI (thicker)
    geom_errorbar(aes(ymin = lower50, ymax = upper50),
                  position = position_dodge(width = 0.5),
                  width = 0.4, size = 1.2) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    labs(title    = "Predicted Probability by Area Need",
         subtitle = "(holding house price and number of children at average)",
         x        = "Area Need",
         y        = "Probability") +
    scale_color_manual(values = sector_colors) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  
  # PANEL 3: Predicted probabilities by House Price
  grid_price <- expand_grid(
    net_loss_s                   = 0,
    average_house_price_per_sq_m_s = c(-1.5, 1.5),
    children_in_care_s           = 0,
    closed                       = 0
  )
  preds_price <- posterior_epred(
    model_multilevel_house,
    newdata         = grid_price,
    re_formula      = NA,
    allow_new_levels = TRUE
  )
  df_price <- as_tibble(reshape2::melt(preds_price)) %>%
    rename(draw = Var1, row = Var2, category = Var3, prob = value) %>%
    mutate(
      price = ifelse(grid_price[row, ]$average_house_price_per_sq_m_s < 0,
                     "Low price", "High price"),
      label = recode(category,
                     b_muInvestmentowned = "Investment owned",
                     b_muCorporateowned  = "Corporate owned",
                     b_muIndividualowned = "Individual owned",
                     b_muThirdsector     = "Third sector")
    )
  df_price_sum <- df_price %>%
    group_by(price, label) %>%
    summarize(
      median  = median(prob),
      lower95 = quantile(prob, 0.025),
      upper95 = quantile(prob, 0.975),
      lower50 = quantile(prob, 0.25),
      upper50 = quantile(prob, 0.75),
      .groups = "drop"
    )
  
  p3 <- ggplot(df_price_sum, aes(x = price, y = median, color = label)) +
    geom_errorbar(aes(ymin = lower95, ymax = upper95),
                  position = position_dodge(width = 0.5),
                  width = 0.2) +
    geom_errorbar(aes(ymin = lower50, ymax = upper50),
                  position = position_dodge(width = 0.5),
                  width = 0.4, size = 1.2) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    labs(title    = "Predicted Probability by House Price",
         subtitle = "(holding area need and number of children at average)",
         x        = "House price",
         y        = "Probability") +
    scale_color_manual(values = sector_colors) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  # Combine panels
  final <- (p1_need | p1_price) /
    p2 /
    p3 +
    plot_layout(heights = c(2, 1.2, 1)) &
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(plot=figure_3, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_3_revised.jpeg", width=12, height=11, dpi=600)
  
  return(final)
}