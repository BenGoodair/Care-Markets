create_table_2 <- function(){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(brms, dplyr, tidyr, broom.mixed, kableExtra, officer, flextable)
  
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
  
  
  # Define extract_bayes_table to compute ORs
  extract_bayes_table <- function(model) {
    # 1) get posterior draws and exponentiate
    draws <- as_draws_df(model) %>%
      select(starts_with("b_mu")) %>%
      pivot_longer(everything(), names_to = "orig_term", values_to = "logodds") %>%
      mutate(
        term_clean = orig_term %>% sub("^b_", "", .) %>% sub("^mu", "", .),
        OR = exp(logodds)
      )
    
    # 2) summarize OR distribution
    summary_stats <- draws %>%
      group_by(term_clean) %>%
      summarise(
        OR_estimate  = mean(OR),
        OR_sd        = sd(OR),
        OR_conf.low  = quantile(OR, 0.025),
        OR_conf.high = quantile(OR, 0.975),
        Pr_GT_1      = mean(OR > 1)
      )
    
    # 3) get labels and clean term names
    labels <- tidy(model, effects = "fixed") %>%
      mutate(
        term_clean = term %>% sub("^b_", "", .) %>% sub("^mu", "", .),
        Category   = sub("_.*$", "", term_clean),
        Predictor  = sub("^[^_]+_", "", term_clean)
      ) %>%
      select(term_clean, Category, Predictor)
    
    # 4) join and format
    labels %>%
      left_join(summary_stats, by = "term_clean") %>%
      mutate(
        Value = sprintf(
          "%.3f
(SD=%.3f)
[%.3f, %.3f]
Pr>1=%.2f",  
          OR_estimate, OR_sd, OR_conf.low, OR_conf.high, Pr_GT_1
        )
      ) %>%
      select(Category, Predictor, Value)
  }
  
  # Define bayes_models list with only models 1 and 3
  bayes_models <- list(
    `Model 1: Net Loss`    = model_multilevel,
    `Model 3: House Price` = model_multilevel_house
  )
  
  # Extract all tables using the updated function
  combined_bayes <- do.call(
    rbind,
    lapply(names(bayes_models), function(mn) {
      df <- extract_bayes_table(bayes_models[[mn]])
      df$Model <- mn
      df
    })
  )
  
  # Filter to only include Net Loss and House Price predictors
  combined_bayes <- combined_bayes %>%
    filter(Predictor %in% c("net_loss_s", "average_house_price_per_sq_m_s")) %>%
    mutate(
      Predictor = recode(Predictor,
                         net_loss_s = "Net Loss (std)",
                         average_house_price_per_sq_m_s = "House Price (std)",
                         .default = Predictor
      ),
      # Order categories by ownership group
      Category = factor(Category, levels = c("Corporateowned", "Individualowned", "Investmentowned", "Thirdsector"))
    ) %>%
    arrange(Category, Predictor)
  
  # Pivot to wide format
  wide_bayes <- combined_bayes %>%
    pivot_wider(names_from = Model, values_from = Value)
  
  # Build and inspect the flextable
  ft_bayes <- flextable(wide_bayes) %>%
    set_header_labels(
      Category                = "Ownership Type",
      Predictor               = "Predictor",
      `Model 1: Net Loss`     = "Model 1: Net Loss",
      `Model 3: House Price`  = "Model 3: House Price"
    ) %>%
    add_header_row(
      values    = c("", "", "Posterior Summary"),
      colwidths = c(1, 1, 2)
    ) %>%
    theme_booktabs() %>%
    align(j = 1:2, align = "left") %>%
    align(j = 3:4, align = "center")
  
  # Return the flextable
  return(ft_bayes)
}