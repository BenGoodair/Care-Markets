create_table_1 <- function(){
  
  ####table 1####
  
  library(dplyr)
  library(gtsummary)
  library(gt)
  
  # Create the summary table with renamed variables
  summary_table <- mlm %>%
    dplyr::distinct(URN, .keep_all = T)%>%
    dplyr::mutate(residential_expenditure = residential_expenditure/1000000)%>%
    mutate(Sector_merge = droplevels(Sector_merge)) %>%
    select(Sector_merge,
           overall.average, ever_outstanding, reqs_per, age, Places, left, profit_margin_average,
           chain_size) %>%
    tbl_summary(
      by = Sector_merge,              # split table by ownership type
      missing = "no",
      type = all_continuous() ~ "continuous2",
      statistic = all_continuous() ~ c("{mean} ({median})",
                                       "{N_nonmiss}"#,
                                       #"{min}, {max} ({sd})"
      ),
      label = list(
        overall.average ~ "Quality (average inspection score)",
        ever_outstanding ~ "Quality (ever rated Outstanding)",
        reqs_per ~ "Number of Requirements (per inspection)",
        age ~ "Age of Home (months)",
        Places ~ "Places (n)",
        left ~ "Closed",
        profit_margin_average ~ "Profit margin (%)",
        chain_size ~ "Chain size (n)"
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
  
  # Print the final pretty table
  gt_table%>%
    gtsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Table1_chomes.html")
  
  
  
  
  
  
  
  
  
  
  
  
}