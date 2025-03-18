df <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/anders_df.csv")

# Convert the data to data.table for faster processing
childrens_homes <- as.data.table(df%>%
                                   dplyr::select(URN, Sector, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date, Join, Leave))

childrens_homes[, `:=`(
  # Convert string dates to Date objects
  Latest.full.inspection.date = as.Date(Latest.full.inspection.date),
  Join = as.Date(Join),
  Leave = as.Date(Leave),
  # Create a binary indicator for good or outstanding ratings
  good_outstanding = ifelse(Overall.experiences.and.progress.of.children.and.young.people %in% 
                              c("Good", "Outstanding"), 1, 0)
)]

# Set a future date for homes that never closed (NA values)
future_date <- as.Date("2027-01-01")
childrens_homes[is.na(Leave), Leave := future_date]

# Sort by URN and inspection date
setorder(childrens_homes, URN, Latest.full.inspection.date)

# Create a data.table of all rating periods
rating_periods <- childrens_homes[, {
  # For each location, get all inspection dates
  dates <- Latest.full.inspection.date
  n <- length(dates)
  
  # Determine end date for each rating period (next inspection or leave date)
  if (n > 1) {
    end_dates <- c(dates[2:n] - 1, Leave[n])
  } else {
    end_dates <- Leave
  }
  
  # Return a data.table with start and end date for each rating period
  list(
    start_date = dates,
    end_date = pmin(end_dates, Leave),
    good_outstanding = good_outstanding,
    Sector = Sector
  )
}, by = URN]

# Define the date range for calculation
min_date <- min(childrens_homes$Join)
max_date <- max(childrens_homes$Leave[childrens_homes$Leave < future_date])

# Sample at regular intervals (weekly)
sample_dates <- seq(min_date, max_date, by = "week")

# Calculate the proportion of good/outstanding ratings at each sample date
result_list <- lapply(sample_dates, function(sample_date) {
  # Find all rating periods that cover this date
  active_ratings <- rating_periods[start_date <= sample_date & end_date >= sample_date]
  
  # Calculate proportions by sector
  if (nrow(active_ratings) > 0) {
    summary <- active_ratings[, .(
      total = .N,
      good_outstanding_count = sum(good_outstanding),
      good_outstanding_ratio = sum(good_outstanding) / .N * 100
    ), by = Sector]
    
    # Add the date
    summary$date <- sample_date
    
    return(summary)
  } else {
    return(NULL)
  }
})

# Remove any NULL results and combine
result_list <- Filter(Negate(is.null), result_list)
rolling_summary <- rbindlist(result_list)

# Convert back to tibble for ggplot
rolling_summary <- as_tibble(rolling_summary)

# Create the plot
ggplot(rolling_summary, aes(x=date, y=good_outstanding_ratio, colour=Sector)) +
  geom_line(size=2) +
  labs(x="Year", y="Good or Outstanding (%)", color="Sector") +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  scale_color_manual(values=c("#CD202C","#2A6EBB","#F0AB00"))+
  coord_cartesian(ylim=c(0,100))


