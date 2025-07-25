create_figure_4 <- function() {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(dplyr,  tidyr,   lubridate, ggplot2,viridis, cowplot)
  
  
  source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")
  mlm <- create_home_data()
  
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
    labs(color = "Area Need\n(Net loss, Quintile)") +
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
  
  # now 'legend' is a single grob you can stick under your plot_grid
  
  final_plot <- cowplot::plot_grid(
    stacked_plots,
    legend,
    ncol = 1,
    rel_heights = c(1, 0.08)
  )
  
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
  
  return(final_plot)
  
  
  
  
}
