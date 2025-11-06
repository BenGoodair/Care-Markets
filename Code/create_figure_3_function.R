create_figure_3 <- function(){
  
  if (!require("pacman")) install.packages("pacman")
  
  pacman::p_load(tidyverse, brms, patchwork, ggdist, scales, dplyr, tidyr, 
                 broom.mixed, kableExtra, officer)

  
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
  
  levels(or_need$sector) <- c("Investmentowned","Corporateowned" , "Individualowned", "Thirdsector"    ) 
  or_need$label <- factor(
    or_need$label,
    levels = c("Investment owned",
               "Corporate owned",
               "Individual owned",
               "Third sector")
  )
  
  
  # Panel 1: Half‑eye plots of OR distributions (unchanged)
  p1_need <- ggplot(or_need,  aes(x = OR, y = label, fill = label)) +
    stat_halfeye(.width = c(0.5,0.95), slab_alpha = 0.8, slab_size = 1) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_continuous(trans = "log10", labels = label_number()) +
    scale_fill_manual(values = c(
      "Investment owned" = "#5B0000",
      "Corporate owned"  = "#FF5E5E",
      "Individual owned" = "#E4007C",
      "Third sector"     = "#F0AB00"
    ), guide = "none") +
    labs(title = "A) Odds ratios of ownership by area need",
         subtitle = "Reference: Local Authority owned",
         x = "Odds Ratio", y = "Sector") +
    theme_bw(base_size = 12) +
    #coord_cartesian(xlim = c(0.2, 2))+
    theme(        plot.title = element_text(face = "bold", size = 12))
  
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
    labs(title    = "B) Predicted probability of ownership by area need",
         subtitle = "(with number of children fixed at the average and closure status set to open)",
         caption = "High need defined as a net loss 1.5 SD above mean; Low need defined as a net loss 1.5 SD below mean",
         x        = "Area Need",
         y        = "Probability",
         color = "Ownership") +
    scale_color_manual(values = sector_colors) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 12)
    )
  
  
  
  
  
    changeplot <- mlm %>%
      
      dplyr::select(Registration.date, Sector_merge, Local.authority, Join) %>%
      left_join(., mlm %>%
                  dplyr::select(Local.authority, net_loss_s) %>%
                  dplyr::distinct(.keep_all = TRUE) %>%
                  dplyr::mutate(needQuint = factor(ntile(net_loss_s, 5))) %>%
                  dplyr::select(-net_loss_s)
      ) %>%
      mutate(location_start = dmy(Registration.date),
             location_start = ifelse(is.na(location_start), Join, as.character(location_start)),
             location_start = as.Date(location_start))%>%
      filter(location_start >= as.Date("2014-03-01"),
             Sector_merge == "Local Authority"|Sector_merge == "Third sector",
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
    
    non_com <- ggplot(changeplot, aes(x = month, y = cumulative_homes, color = needQuint)) +
      geom_line(size = 1.5) +
      labs(
        title = "D) Non-commercial homes opened by area need",
        subtitle = "Non-commercial = Local Authority and Third sector",
        x = NULL,
        y = "Cumulative homes opened",
        color = "Area Need\n(Net loss, Quintile)"
      ) +
      theme_bw() +
      scale_color_viridis_d(option = "viridis") +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12)
      )
  
    
    changeplot <- mlm %>%
      
      dplyr::select(Registration.date, Sector_merge, Local.authority, Join) %>%
      left_join(., mlm %>%
                  dplyr::select(Local.authority, net_loss_s) %>%
                  dplyr::distinct(.keep_all = TRUE) %>%
                  dplyr::mutate(needQuint = factor(ntile(net_loss_s, 5))) %>%
                  dplyr::select(-net_loss_s)
      ) %>%
      mutate(location_start = dmy(Registration.date),
             location_start = ifelse(is.na(location_start), Join, as.character(location_start)),
             location_start = as.Date(location_start))%>%
      filter(location_start >= as.Date("2014-03-01"),
             Sector_merge == "Individual owned"|Sector_merge == "Corporate owned"|Sector_merge == "Investment owned",
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
    
    com <- ggplot(changeplot, aes(x = month, y = cumulative_homes, color = needQuint)) +
      geom_line(size = 1.5) +
      labs(
        title = "C) Commercial homes opened by area need",
        subtitle = "Commercial = Individual, Corporate and Investment owned",
        x = NULL,
        y = "Cumulative homes opened",
        color = "Area Need\n(Net loss, Quintile)"
      ) +
      theme_bw() +
      scale_color_viridis_d(option = "viridis") +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 12)
      )

 
  # Combine panels
  final <- p1_need / p2 /com/non_com
  
  

  
#  ggsave(plot=final, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_3_revised.jpeg", width=9, height=11, dpi=600)
  
  return(final)
}