create_table_1 <- function(){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(dplyr, gtsummary, gt)
  
  source("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Code/create_data_function.R")
  
  df <- create_home_data()
  
  
  ####table 1####
  

  
  # Create the summary table with renamed variables
  summary_table <- df %>%
    dplyr::distinct(URN, .keep_all = TRUE) %>%
    mutate(Sector_merge = droplevels(Sector_merge)) %>%
    dplyr::select(Sector_merge,
                  overall.average, ever_outstanding, reqs_per, age, Places, left, profit_margin_average,
                  chain_size, Region.Country.name) %>%
    tbl_summary(
      by = Sector_merge,
      missing = "no",
      type = all_continuous() ~ "continuous2",
      statistic = all_continuous() ~ c(
        "{median}, {mean}",
        "({min} : {max}), ({p25} : {p75})",   # <-- Q1 and Q3 displayed here
        "{N_nonmiss}"
      ),
      label = list(
        overall.average ~ "Quality (average inspection score)",
        ever_outstanding ~ "Quality (ever rated Outstanding)",
        reqs_per ~ "Number of Requirements (per inspection)",
        age ~ "Age of Home (months since registration)",
        Places ~ "Children home size (Places, n)",
        left ~ "Closed",
        profit_margin_average ~ "Profit margin (%)",
        chain_size ~ "Chain size (n)",
        Region.Country.name = "Region"
      )
    ) %>%
    add_n() %>%
    modify_spanning_header(
      c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5") ~ "**Ownership**"
    ) %>%
    modify_caption("**Table. Children Home and Area Characteristics**") %>%
    modify_header(label = "**Variable**") %>%
    bold_labels()
  
  # Convert the gtsummary table to a gt table to add sub-headers and styling
  gt_table <- as_gt(summary_table) %>%
    
    
    # Additional styling for a polished look
    tab_options(
      table.font.names = "Calibri",
      table.font.size = 12,
      row_group.background.color = "#E6E6E6",
      table.border.top.style = "solid",
      table.border.top.width = px(2),
      table.border.top.color = "gray"
    )
  
  # # Print the final pretty table
  #  gt_table%>%
  #    gtsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Table_1_revised.html")
  # 
  # 
  
}