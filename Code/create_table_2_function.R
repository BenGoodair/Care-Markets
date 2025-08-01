create_table_2 <- function(){
  

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(brms, dplyr, tidyr, broom.mixed, kableExtra, officer, flextable, brms)
  
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
  
  return(ft_bayes)


  
  #save_as_docx(table_2, path = "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Tables/Table_Bayesian_Regressions_revision.docx")
}