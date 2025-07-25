create_figure_4 <- function() {
  
  source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")
  
  mlm <- create_home_data()
  
  # Function to create plot for each ownership type
  create_ownership_plot <- function(data, ownership_type, title_suffix) {
    # Parse dates and filter for the specified ownership type
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
      mutate(cumulative_homes = cumsum(new_care_homes),
             needQuint = case_when(
               needQuint == 5 ~ "5 (High need)",
               needQuint == 4 ~ "4",
               needQuint == 3 ~ "3",
               needQuint == 2 ~ "2",
               needQuint == 1 ~ "1 (Low need)"
             ))
    
    # Add a zero baseline for each quintile
    zero_rows <- changeplot %>%
      group_by(needQuint) %>%
      summarise(month = min(month) - months(1), cumulative_homes = 0, .groups = "drop") %>%
      mutate(new_care_homes = 0)
    
    changeplot <- bind_rows(changeplot, zero_rows) %>%
      arrange(needQuint, month)
    
    # Create the ggplot
    plot <- ggplot(changeplot, aes(x = month, y = cumulative_homes, color = factor(needQuint))) +
      geom_line(size = 1.5) +
      labs(
        title = paste0("Cumulative number of new children homes\n(", title_suffix, ")"),
        x = "Year",
        y = paste0(title_suffix, " children homes opened (n, cumulative)"),
        color = "Area Need\n(Net loss, Quintile)"
      ) +
      theme_bw() +
      scale_color_viridis_d(option = "viridis") +
      theme(legend.position = "none")  # Remove individual legends
    
    return(plot)
  }
  
  # Define ownership types and their display names
  ownership_types <- c(
    "Individual owned", 
    "Corporate owned", 
    "Investment owned", 
    "Local Authority", 
    "Third sector"
  )
  
  display_names <- c(
    "Individual-owned", 
    "Corporate-owned", 
    "Investment-owned", 
    "Local Authority", 
    "Third sector"
  )
  
  # Create a list to store the plots
  plots_list <- list()
  
  # Generate all plots
  for (i in 1:length(ownership_types)) {
    plots_list[[i]] <- create_ownership_plot(
      data = mlm, 
      ownership_type = ownership_types[i], 
      title_suffix = display_names[i]
    )
  }
  
  # Extract legend from the first plot (with legend included)
  legend_plot <- ggplot(plots_list[[1]]$data, aes(x = month, y = cumulative_homes, color = factor(needQuint))) +
    geom_line(size = 1.5) +
    labs(color = "Area Need\n(Net loss, Quintile)") +
    theme_bw() +
    scale_color_viridis_d(option = "viridis")
  
  legend <- cowplot::get_legend(legend_plot)
  
  # Combine top 3 plots (commercial) with label
  top_plots <- cowplot::plot_grid(
    plotlist = plots_list[1:3],
    ncol = 3,
    nrow = 1,
    labels = c("A", "B", "C")
  )
  
  # Add commercial label
  commercial_section <- cowplot::plot_grid(
    NULL,
    top_plots,
    NULL,
    ncol = 1,
    rel_heights = c(0.05, 1, 0.02)
  )
  
  # Add the "Commercial" title
  commercial_labeled <- cowplot::ggdraw(commercial_section) +
    cowplot::draw_label("Commercial", x = 0.5, y = 0.97, size = 14, fontface = "bold")
  
  # Combine bottom 2 plots (non-commercial) with label  
  bottom_plots <- cowplot::plot_grid(
    plotlist = plots_list[4:5],
    ncol = 2,
    nrow = 1,
    labels = c("D", "E")
  )
  
  # Add non-commercial label
  noncommercial_section <- cowplot::plot_grid(
    NULL,
    bottom_plots,
    NULL,
    ncol = 1,
    rel_heights = c(0.05, 1, 0.02)
  )
  
  # Add the "Non-commercial" title
  noncommercial_labeled <- cowplot::ggdraw(noncommercial_section) +
    cowplot::draw_label("Non-commercial", x = 0.5, y = 0.97, size = 14, fontface = "bold")
  
  # Combine commercial and non-commercial sections vertically
  combined_plots <- cowplot::plot_grid(
    commercial_labeled,
    noncommercial_labeled,
    ncol = 1,
    nrow = 2,
    rel_heights = c(1, 1)
  )
  
  # Create the final plot with the legend
  final_plot <- cowplot::plot_grid(
    combined_plots,
    legend,
    ncol = 2,
    rel_widths = c(0.9, 0.1)
  )
  
  # Display the final plot
  return(final_plot)
}

# Usage:
final_plot <- create_figure_4_area_need()
final_plot

# Save the plot
# ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Figure_4_revisions.png", 
#        final_plot, width = 15, height = 10, dpi = 600)



}