library(tidyverse)

final_data <- read_csv("Downloads/final_data.csv")

random_sample <- final_data %>%
  filter(Sector.x != "Local Authority", Sector.x != "Voluntary") %>%
  sample_n(30)


write.csv(random_sample, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Final Data/manual_check_sample.csv")


final_data <- read_csv("Downloads/final_data.csv")

checks <- read.csv( "Library/CloudStorage/OneDrive-Nexus365/incorrect_corporate_companies.csv")


yep <- final_data%>%dplyr::select(Sector_merge, Company.name, URN) %>%
  dplyr::full_join(., checks%>% dplyr::select(Company.name, Registered.number)%>%dplyr::mutate(charity="yes"))%>%
  dplyr::filter(charity=="yes")

wrong <- yep %>% dplyr::filter(Sector_merge=="Corporate owned")

write.csv(wrong, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Final Data/all_missing_info.csv")


checks <- read.csv( "Library/CloudStorage/OneDrive-Nexus365/incorrect_corporate_companies.csv")


yep <- final_data%>%dplyr::select(Sector_merge, Company.name, URN, Organisation, Organisation_fame_search) %>%
  dplyr::full_join(., checks%>% dplyr::select(Company.name, Registered.number)%>%dplyr::mutate(charity="yes"))%>%
  dplyr::filter(charity=="yes",
                Sector_merge=="Corporate owned")

write.csv(yep, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/final_fame_edit.csv")
