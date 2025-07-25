create_figure_4 <- function() {
  source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")
  mlm <- create_home_data()
  
  # Create ownership plot function
  create_ownership_plot <- function(data, ownership_type, title_suffix) {
    changeplot <- data %>%
      dplyr::select(Registration.date, Sector_merge, Local.authority) %>%
      left_join(., data %>%
                  dplyr::select(Local.authority, net_loss_s) %>%
                  dplyr::distinct(.keep_all = TRUE) %>%
                  dplyr::mutate(needQuint = factor(ntile(net_loss_s, 5))) %>%
                  dplyr::select(-net_loss_s)
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
    
    plot <- ggplot(changeplot, aes(x = month, y = cumulative_homes, color = factor(needQuint))) +
      geom_line(size = 1.5) +
      labs(
        title = paste0(title_suffix),
        x = "Year",
        y = "Cumulative new homes opened",
        color = "Area Need\n(Net loss, Quintile)"
      ) +
      theme_bw() +
      scale_color_viridis_d(option = "viridis") +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 12),
        axis.title.x = element_blank()
      )
    
    return(plot)
  }
  
  # Define types
  ownership_types <- c("Individual owned", "Corporate owned", "Investment owned", "Local Authority", "Third sector")
  display_names <- c("Individual-owned", "Corporate-owned", "Investment-owned", "Local Authority", "Third sector")
  
  plots_list <- lapply(seq_along(ownership_types), function(i) {
    create_ownership_plot(mlm, ownership_types[i], display_names[i])
  })
  
  # Extract legend from first plot
  legend_plot <- ggplot(plots_list[[1]]$data, aes(x = month, y = cumulative_homes, color = factor(needQuint))) +
    geom_line(size = 1.5) +
    labs(color = "Area Need\n(Net loss, Quintile)") +
    theme_bw() +
    scale_color_viridis_d(option = "viridis")
  
  legend <- cowplot::get_legend(legend_plot)
  
  # Divider text for sections
  commercial_label <- cowplot::ggdraw() + cowplot::draw_label("Commercial entities", fontface = 'bold', size = 14)
  noncommercial_label <- cowplot::ggdraw() + cowplot::draw_label("Non-commercial entities", fontface = 'bold', size = 14)
  
  # Assemble full vertical figure
  stacked_plots <- cowplot::plot_grid(
    commercial_label,
    plots_list[[1]],
    plots_list[[2]],
    plots_list[[3]],
    noncommercial_label,
    plots_list[[4]],
    plots_list[[5]],
    ncol = 1,
    rel_heights = c(0.05, 1, 1, 1, 0.05, 1, 1),
    labels = c("", "A", "B", "C", "", "D", "E"),
    label_size = 12,
    align = "v"
  )
  
  final_plot <- cowplot::plot_grid(
    stacked_plots,
    legend,
    ncol = 2,
    rel_widths = c(0.92, 0.08)
  )
  
  return(final_plot)
  
  # ggsave("Figure_4_revised.png", final_plot, width = 10, height = 16, dpi = 600)
}
