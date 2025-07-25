create_figure_4 <- function() {
  source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")
  mlm <- create_home_data()
  
  # Reusable plot function
  create_ownership_plot <- function(data, ownership_type, title_suffix) {
    changeplot <- data %>%
      dplyr::select(Registration.date, Sector_merge, Local.authority) %>%
      left_join(
        data %>%
          dplyr::select(Local.authority, net_loss_s) %>%
          dplyr::distinct() %>%
          dplyr::mutate(needQuint = factor(ntile(net_loss_s, 5))) %>%
          dplyr::select(-net_loss_s),
        by = "Local.authority"
      ) %>%
      mutate(location_start = dmy(Registration.date)) %>%
      filter(
        location_start >= as.Date("2014-03-01"),
        Sector_merge == ownership_type,
        !is.na(needQuint)
      ) %>%
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
      mutate(
        cumulative_homes = cumsum(new_care_homes),
        needQuint = recode_factor(
          needQuint,
          `5` = "5 (High need)",
          `4` = "4",
          `3` = "3",
          `2` = "2",
          `1` = "1 (Low need)"
        )
      ) %>%
      ungroup()
    
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
        y     = "Cumulative new homes opened",
        color = "Area Need\n(Net loss, Quintile)"
      ) +
      theme_bw() +
      scale_color_viridis_d(option = "viridis") +
      theme(
        legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 12),
        axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank()
      )
  }
  
  ownership_types <- c(
    "Individual owned", "Corporate owned", "Investment owned",
    "Local Authority", "Third sector"
  )
  display_names <- c(
    "Individual‑owned", "Corporate‑owned", "Investment‑owned",
    "Local Authority", "Third sector"
  )
  
  plots_list <- map2(ownership_types, display_names, ~
                       create_ownership_plot(mlm, .x, .y)
  )
  
  # Extract shared legend
  legend_plot <- get_legend(
    ggplot_build(plots_list[[1]])$plot +
      theme(legend.position = "bottom")
  )
  
  # Labels and divider
  commercial_label    <- ggdraw() + draw_label("Commercial entities", fontface = "bold", size = 14)
  noncommercial_label <- ggdraw() + draw_label("Non‑commercial entities", fontface = "bold", size = 14)
  divider_line        <- ggplot() + geom_hline(yintercept = 0, size = 2, color = "black") + theme_void()
  
  # Assemble vertical stack
  stacked <- plot_grid(
    commercial_label,
    plots_list[[1]],
    plots_list[[2]],
    plots_list[[3]],
    divider_line,
    noncommercial_label,
    plots_list[[4]],
    plots_list[[5]],
    ncol       = 1,
    rel_heights = c(0.05, rep(1, 3), 0.005, 0.05, rep(1, 2)),
    labels      = c("", "A", "B", "C", "", "", "D", "E"),
    label_size  = 12,
    align       = "v"
  )
  
  # Combine with legend at bottom
  final_plot <- plot_grid(
    stacked,
    legend_plot,
    ncol       = 1,
    rel_heights = c(0.95, 0.05)
  )
  
  return(final_plot)


  
  #ggsave(plot=figure_4, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_4_revised.jpeg", width=8, height=13, dpi=600)


}