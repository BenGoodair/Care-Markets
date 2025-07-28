
create_home_data <- function(){
  


if (!require("pacman")) install.packages("pacman")

pacman::p_load(MCMCglmm,ordinal,devtools,MASS,  sandwich,  lmtest,  clubSandwich,  
               modelsummary, cowplot,ggmap,googleway,hrbrthemes,viridis,jsonlite,survival, 
               httr, purrr, dplyr,gt, gtsummary, tidyverse,rattle,ggeffects, glmnet,caret, 
               rpart.plot,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, 
               stringr, patchwork,knitr,  sf, clubSandwich, modelsummary, sjPlot,
               lmerTest, lme4, brms, glmmTMB, ggeffects, MCMCglmm, tidybayes, bayesplot,
               gridExtra, nnet, clubSandwich, fixest, patchwork, gtExtras, estimatr)


####proper dataset####



joiners17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_17.csv"))  
joiners18 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_18.csv"))  
joiners19 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_19.csv"))  
joiners20 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_20.csv"))  
joiners21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_21.csv"))  
joiners22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_22.csv"), skip=3)  
joiners23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_23.csv"), skip=3)  

joiners <- rbind( #joiners17 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  #joiners18 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  joiners19 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners20 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners21 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners22 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners23 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.)
)%>%
  dplyr::rename(Date = Registration.date)%>%
  dplyr::mutate(leave_join = "Join",
                provider_status = NA)%>%
  dplyr::filter(Provision.type!="Adoption Support Agency",
                Provision.type!="Further Education College with Residential Accommodation",
                Provision.type!="Boarding School",
                Provision.type!="Residential Family Centre",
                Provision.type!="Residential Special School",
                Provision.type!="Voluntary Adoption Agency",
                Provision.type!="Residential Holiday Scheme for Disabled Children",
                Provision.type!="Independent Fostering Agency",
                Provision.type!="Voluntary Adoption Agency")


Leavers <- rbind( #joiners17 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  #joiners18 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  joiners19 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Date.closed)%>%
    dplyr::rename(Cancelled.or.resigned.date = Date.closed),
  joiners20 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Cancelled.or.resigned.date),
  joiners21 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Resigned.closed.date)%>%
    dplyr::rename(Cancelled.or.resigned.date=Resigned.closed.date),
  joiners22 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Cancelled.or.resigned.date),
  joiners23 %>% dplyr::filter(Leaver.status=="Leaver") %>% 
    dplyr::mutate(Cancelled.or.resigned.date = ifelse(First.effective.date..that.the.provider.became.resigned.!="",First.effective.date..that.the.provider.became.resigned.,
                                                      ifelse(First.effective.date..that.the.provider.became.resigned.==""&First.effective.date..that.the.provider.closed.=="", First.effective.date..that.the.provider.became.cancelled.,
                                                             First.effective.date..that.the.provider.closed.)))%>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,Cancelled.or.resigned.date) 
)%>%
  dplyr::rename(Date = Cancelled.or.resigned.date)%>%
  dplyr::mutate(leave_join = "Leave",
                provider_status = NA)%>%
  dplyr::filter(Provision.type!="Adoption Support Agency",
                Provision.type!="Further Education College with Residential Accommodation",
                Provision.type!="Boarding School",
                Provision.type!="Residential Family Centre",
                Provision.type!="Residential Special School",
                Provision.type!="Voluntary Adoption Agency",
                Provision.type!="Residential Holiday Scheme for Disabled Children",
                Provision.type!="Independent Fostering Agency",
                Provision.type!="Voluntary Adoption Agency")


exits <- rbind(joiners, Leavers)

# pre <- rbind(
#   read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 3, skip=3) %>%
#     dplyr::mutate(leave_join = "Join")%>%
#     dplyr::rename(Date = `Registration date`),
#   read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 4, skip=3) %>%
#     dplyr::mutate(leave_join = "Leave")%>%
#     dplyr::rename(Date = `Date closed`),
#   read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 5, skip=3) %>%
#     dplyr::mutate(leave_join = "Join")%>%
#     dplyr::rename(Date = `Registration date`),
#   read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 6, skip=3) %>%
#     dplyr::mutate(leave_join = "Leave")%>%
#     dplyr::rename(Date = `Date closed`),
#   read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 7, skip=3) %>%
#     dplyr::mutate(leave_join = "Join")%>%
#     dplyr::rename(Date = `Registration date`),
#   read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 8, skip=3) %>%
#     dplyr::mutate(leave_join = "Leave")%>%
#     dplyr::rename(Date = `Date closed`),
#   read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 9, skip=3) %>%
#     dplyr::mutate(leave_join = "Join")%>%
#     dplyr::rename(Date = `Registration date`),
#   read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 10, skip=3) %>%
#     dplyr::mutate(leave_join = "Leave")%>%
#     dplyr::rename(Date = `Date closed`)
# )

pre <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/pre2019_joiners_leavers.csv"))  

all <- rbind(pre %>%
               dplyr::rename(Provision.type = Provider.type) %>%
               dplyr::mutate(Local.authority = Local.authority %>%
                               gsub('&', 'and', .) %>%
                               gsub('[[:punct:] ]+', ' ', .) %>%
                               gsub('[0-9]', '', .) %>%
                               toupper() %>%
                               gsub("CITY OF", "", .) %>%
                               gsub("UA", "", .) %>%
                               gsub("COUNTY OF", "", .) %>%
                               gsub("ROYAL BOROUGH OF", "", .) %>%
                               gsub("LEICESTER CITY", "LEICESTER", .) %>%
                               gsub("UA", "", .) %>%
                               gsub("DARWIN", "DARWEN", .) %>%
                               gsub("COUNTY DURHAM", "DURHAM", .) %>%
                               gsub("AND DARWEN", "WITH DARWEN", .) %>%
                               gsub("NE SOM", "NORTH EAST SOM", .) %>%
                               gsub("N E SOM", "NORTH EAST SOM", .) %>%
                               str_trim()) %>% 
               # Fix Excel serial numbers in pre dataset
               dplyr::mutate(
                 Date = ifelse(
                   grepl("^[0-9]+$", as.character(Date)) & as.numeric(as.character(Date)) > 10000,
                   as.character(as.Date(as.numeric(as.character(Date)), origin = "1899-12-30")),
                   as.character(Date)
                 )
               ) %>%
               dplyr::select(URN, Local.authority, Sector, Places, Date, leave_join), 
             exits %>% 
               dplyr::select(URN, Local.authority, Sector, Places, Date, leave_join) %>%
               dplyr::mutate(Date = as.character(as.Date(Date, format = "%d/%m/%Y"))) %>%
               dplyr::filter(as.Date(Date) >= as.Date("2018-04-01"))  # Filter for dates after 1st April 2018
) %>%  
  dplyr::distinct(URN, leave_join, .keep_all = T) %>%
  tidyr::pivot_wider(id_cols = c("Local.authority", "Sector", "URN", "Places"), 
                     names_from = "leave_join", 
                     values_from = "Date") %>%
  dplyr::mutate(Join = substr(Join, 1, 10),
                Leave = substr(Leave, 1, 10),
                Sector = ifelse(Sector == "Health Authority", "Local Authority", Sector))

leaves <- all %>%
  dplyr::select(URN,Leave)

all <- all %>%
  dplyr::select(-Leave)%>%
  full_join(., leaves, by= "URN")  %>%
  group_by(URN) %>%                      # Group by URN
  fill(Leave, .direction = "downup") %>% 
  fill(Join, .direction = "downup")%>%
  ungroup()%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())







df <-rbind( read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2015.csv"))%>%
              dplyr::mutate(year=2015,
                            Places= NA,
                            Organisation = NA, 
                            Registration.date = NA)%>%
              dplyr::rename(Registration.status=Reg.Status,
                            Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.Date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home")|
                              str_detect(Provision.type, "(?i)day"),
                            !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2016.csv"), skip=1)%>%
              dplyr::mutate(year=2016,
                            Organisation = NA,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"),
                            !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2017.csv"), skip=1)%>%
              dplyr::mutate(year=2017,
                            Organisation = NA,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"),
                            !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018.csv"), skip=1)%>%
              dplyr::mutate(year=2018,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"),
                            !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018_part2.csv"))%>%
              dplyr::mutate(year=2018,
                            Registration.date = NA,
                            Overall.experiences.and.progress.of.children.and.young.people = NA,
                            Latest.full.inspection.date = NA)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"),
                            !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::rename(Organisation = Organisation.name)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2019.csv"), skip=1)%>%
              dplyr::mutate(year=2019)%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"),
                          !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2020.csv"), skip=1)%>%
              dplyr::mutate(year=2020)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"),
                            !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2021.csv"), skip=1)%>%
              dplyr::mutate(year=2021)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"),
                            !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2022_yep.csv"), skip=4)%>%
              dplyr::mutate(year=2022)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"),
                            !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Overall.experiences.and.progress.of.children.and.young.people =    Latest.full.inspection.overall.experiences.and.progress.of.children.and.young.people)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2023.csv"), skip=3)%>%
              dplyr::mutate(year=2023)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"),
                            !str_detect(Provision.type, "(?i)school"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people,Latest.full.inspection.date)
)%>%
  dplyr::mutate(Sector= ifelse(Sector=="Health Authority"|Sector=="Local authority"|Sector=="Health authority", "Local Authority", Sector),
                Homes=1)%>%
  dplyr::distinct(.keep_all = T)%>%
  group_by(URN) %>%                      # Group by URN
  fill(Places, .direction = "downup") %>% 
  fill(Organisation, .direction = "downup") %>% 
  ungroup()%>%
  bind_rows(., all %>% 
              dplyr::select(URN, Places, Local.authority)%>%
              dplyr::mutate(Homes = NA, 
                            Sector=NA,
                            Places = as.numeric(Places)))%>%
  group_by(URN) %>%                      # Group by URN
  fill(Places, .direction = "downup") %>% 
  ungroup()%>%
  dplyr::filter(!is.na(Sector))%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., all%>%
                     dplyr::select(-Places, -Local.authority, -Sector,)%>%
                     dplyr::filter(as.Date(Leave)>="2014-04-01"|
                                     is.na(Leave))%>%
                     dplyr::filter(as.Date(Join)<"2023-04-01"|
                                     is.na(Join)),
  by=c("URN"))%>%
  group_by(URN) %>%
  mutate(
    Registration.date = if_else(
      is.na(Registration.date), 
      NA_character_,  # Use NA_character_ for character type
      Registration.date
    )
  ) %>%
  fill(Registration.date, .direction = "down") %>%  # Fill downwards within each group
  fill(Registration.date, .direction = "up") %>%    # Fill upwards within each group
  ungroup()%>%
  dplyr::mutate(Registration.date = ifelse(URN == "SC022444" & Registration.date != "26/09/2001",
                                           "26/09/2001", Registration.date))


yes <- df %>% dplyr::filter(is.na(Local.authority))%>%
  dplyr::mutate(keep = 1)%>%
  dplyr::select(URN, keep)%>%
  full_join(., all, by="URN")%>%
  dplyr::filter(keep==1)





df <- df%>% dplyr::filter(!is.na(Local.authority))%>%
  bind_rows(., yes%>%
              dplyr::mutate(Places = as.numeric(Places)))%>%
  dplyr::mutate(Latest.full.inspection.date = as.Date(Latest.full.inspection.date, format = "%d/%m/%Y")) %>%
  group_by(URN) %>%
  filter( all(is.na(Latest.full.inspection.date)) |  # Keep if all dates in the group are NA
            Latest.full.inspection.date == max(Latest.full.inspection.date, na.rm = TRUE)) %>%
  ungroup()%>%
  dplyr::mutate(Overall.experiences.and.progress.of.children.and.young.people = ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Requires improvement", "Requires improvement to be good",
                                                                                       ifelse(Overall.experiences.and.progress.of.children.and.young.people == "", NA,
                                                                                              ifelse(Overall.experiences.and.progress.of.children.and.young.people =="Satisfactory", NA,
                                                                                                     ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Adequate", NA,
                                                                                                            Overall.experiences.and.progress.of.children.and.young.people)))))

####needs to be in 3 docs for 500 max in fame####








df <- df %>%
  dplyr::mutate(Organisation_fame_search = Organisation %>%
                  gsub("children's", "childrens", ., ignore.case = TRUE)%>%
                  gsub("Cedarways Residential Child Care  Larkhill House Limited", "Cedarways Residential Child Care Larkhill House Limited", ., ignore.case = TRUE)%>%
                  gsub("Achieving Aspirations  Community Interest Company", "Achieving Aspirations Community Interest Company", ., ignore.case = TRUE)%>%
                  gsub("Oak  house Childrens Home Ltd", "Oak house Childrens Home Ltd", ., ignore.case = TRUE)%>%
                  gsub("Overley Hall  Limited", "Overley Hall Limited", ., ignore.case = TRUE)%>%
                  gsub("Kattz ltd", "Kattz (ah) Ltd", ., ignore.case = TRUE)%>%
                  gsub("the partnership of care today", "Care Today Childrens Services", ., ignore.case = TRUE)%>%
                  gsub("Devon \\& Cornwall Autistic Community Trust \\(t/a Spectrum\\)", "Devon & Cornwall Autistic Community Trust", ., ignore.case = TRUE)%>%
                  gsub("Beacon Child Care Ltd", "BEACON CHILDCARE LTD", ., ignore.case = TRUE)%>%
                  gsub("Tulip Care One Limited t/as TulipCare", "Tulip Care One Limited", ., ignore.case = TRUE)%>%
                  gsub("Inspirations Leicestershire Limited comp number", "Inspirations Leicestershire Limited", ., ignore.case = TRUE)%>%
                  gsub("4 Pure Heart Limited", "4PureHeart Limited", ., ignore.case = TRUE)%>%
                  gsub("Lakeside @ Our Place Limited", "Lakeside@OurPlace Limited", ., ignore.case = TRUE)%>%
                  gsub("Next Stage 4Life LTD", "Next Stage 4 Life LTD", ., ignore.case = TRUE)%>%
                  gsub("The Exeter Royal Academy For Deaf Education", "Exeter Royal Academy For Deaf Education", ., ignore.case = TRUE)%>%
                  gsub("Pathways Residential Child Care Larkhill House Limited", "CEDARWAYS RESIDENTIAL CHILD CARE LARKHILL HOUSE LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Leicester Young Mens Christian Association \\(incorporated\\) \\(the\\)", "YMCA LEICESTERSHIRE", ., ignore.case = TRUE)%>%
                  gsub("Slough childrens Services Trust Limited", "SLOUGH CHILDREN FIRST LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Tjunction childrens Services Limited", "T-junction childrens Services Ltd", ., ignore.case = TRUE)%>%
                  gsub("Crystal Care Solutions Limited Company Number", "Crystal Care Solutions Limited", ., ignore.case = TRUE)%>%
                  gsub("The Partnership of Care Today childrens Services", "CARE TODAY (CHILDRENS SERVICES) LTD", ., ignore.case = TRUE)%>%
                  gsub("Broadwood Education Services", "KEYS EDUCATIONAL SERVICES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("247Bluebell Care LTD", "247 Bluebell Care LTD", ., ignore.case = TRUE)%>%
                  gsub("Willows21 Limited", "Willows 21 Limited", ., ignore.case = TRUE)%>%
                  gsub("iMapcentre Ltd", "iMap centre Ltd", ., ignore.case = TRUE)%>%
                  gsub("solent Childcare LTD", "solent Child care LTD", ., ignore.case = TRUE)%>%
                  gsub("North West Youth Services Limited", "Northwest Youth Services Limited", ., ignore.case = TRUE)%>%
                  gsub("Achieving Aspirations cic", "Achieving Aspirations COMMUNITY INTEREST COMPANY", ., ignore.case = TRUE)%>%
                  gsub("Monach Intervention Services Limited", "Monarch Intervention Services Limited", ., ignore.case = TRUE)%>%
                  gsub("Sbl Care Services", "SBL CARESERVICES", ., ignore.case = TRUE)%>%
                  gsub("Lioncare Ltd Operating As The Lioncare Group", "Lioncare Ltd", ., ignore.case = TRUE)%>%
                  gsub("Smoothstone Care And Education Ltd", "Smooth stone Care And Education Ltd", ., ignore.case = TRUE)%>%
                  gsub("Foundations Children & Family Services Ltd", "STEP UP CHILDREN AND FAMILY SERVICES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("A\\+t Home", "A & T HOME LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Juvenis Care Services Ltd", "ACCALIA CARE SERVICES LTD", ., ignore.case = TRUE)%>%
                  gsub("Juvenis Care Services Ltd.", "ACCALIA CARE SERVICES LTD", ., ignore.case = TRUE)%>%
                  gsub("Stonelake London Limited", "Stone lake London Limited", ., ignore.case = TRUE)%>%
                  gsub("Homes2inspire Limited", "Homes 2 inspire Limited", ., ignore.case = TRUE)%>%
                  gsub("Blossom SC Ltd", "PARKER SC LTD", ., ignore.case = TRUE)%>%
                  gsub("Residential Childcare Community\\(Town Hall\\) LTD", "Residential Child care Community (Town Hall) LTD", ., ignore.case = TRUE)%>%
                  gsub("G&S caring for children and young people ltd", "G&SCARING FORCHILDREN AND YOUNG PEOPLE LTD", ., ignore.case = TRUE)%>%
                  gsub("Smoothstone Care & Education", "SMOOTH STONE CARE AND EDUCATION LIMITED", ., ignore.case = TRUE)%>%
                  gsub("ROC Family Support Ltd", "ROC FAMILY TIME LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Lighthouse childrens Care LTD", "SUSSEX CHILDRENS CARE LTD", ., ignore.case = TRUE)%>%
                  gsub("Catch22 Charity Limited", "Catch 22 Charity Limited", ., ignore.case = TRUE)%>%
                  gsub("Evolve Childcare Ltd", "Evolve Child care Ltd", ., ignore.case = TRUE)%>%
                  gsub("TJY Care lytd", "TJY Care ltd", ., ignore.case = TRUE)%>%
                  gsub("Oasis Young People's Care Services \\(uk\\) Ltd", "OASIS CARE SERVICES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Birtenshaw - company number 02978546", "Birtenshaw", ., ignore.case = TRUE)%>%
                  gsub("Rubicon childrens Homes Limited", "RUBICON CHILDREN & FAMILY SERVICES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Horizon Residential childrens Home", "HORIZON RESIDENTIAL HOMES LIMITED", ., ignore.case = TRUE)%>%
                  gsub("Bright Futures Care Limited T/A Cornerstones", "Bright Futures Care Limited", ., ignore.case = TRUE)%>%
                  gsub("Tree House Care Fostering Solutions Ltd", "TreeHouse Care Fostering Solutions Ltd", ., ignore.case = TRUE)%>%
                  gsub("children assisted in a real environment \\(care\\) ltd", "children assisted in a real environment ltd", ., ignore.case = TRUE)%>%
                  gsub("[0-9]{7,}", "", .)%>%
                  gsub("'", "", .)%>%
                  gsub("T/A.*", "", .)%>%
                  gsub("T/a.*", "", .)%>%
                  str_trim(.)%>%
                  gsub("Southend on Sea Young Mens Christian Association", "SOUTHEND-ON-SEA YOUNG MEN'S CHRISTIAN ASSOCIATION", ., ignore.case = TRUE)  )




#write.csv(save_df[1], "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/orgs1.csv")
#write.csv(save_df[2], "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/orgs2.csv")
#write.csv(save_df[3], "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/orgs3.csv")


#### lookup####


lookup <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/lookup_fame_search_clean_manual.csv"))%>%
  dplyr::mutate(Organisation_fame_search = Organisation_fame_search %>%
                  gsub("Selected", "", .),
                Company.name = Company.name %>%
                  gsub(" In liquidation", "", .)%>%
                  gsub(" Dissolved", "",.)%>%
                  str_trim())%>%
  dplyr::distinct(Organisation_fame_search, .keep_all = T)%>%
  dplyr::bind_rows(., data.frame(Organisation_fame_search = "BEACON CHILDCARE LTD", Company.name = "BEACON CHILDCARE LTD", stringsAsFactors = FALSE))%>%
  dplyr::mutate(Company.name = ifelse(Organisation_fame_search=="Kids", "KIDS", Company.name))



francois_clean <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/c_home_cats.csv"))



df <- df %>%
  full_join(., lookup, by="Organisation_fame_search")%>%
  dplyr::left_join(., francois_clean, by="Company.name") %>%
  dplyr::mutate(Sector_merge = ifelse(Sector.x=="Local Authority", "Local Authority",
                                      ifelse(Sector.x=="Voluntary", "Third sector", Sector.y)),
                Sector_merge = ifelse(is.na(Sector_merge), "Unidentified for-profit", Sector_merge))%>%
  dplyr::filter(!is.na(URN))

df$Sector_merge <- factor(df$Sector_merge, levels = c("Local Authority", "LA owned company", "Third sector", "Individual owned", "Corporate owned", "Investment owned", "Unidentified for-profit"))





chome_netgain <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/FOI%202024-0040813%20part%201.csv"), skip=13)%>%
  dplyr::rename(Local.authority=la_name,
                year=time_period,
                net_gain = number)%>%
  dplyr::filter(!(Local.authority == "Dorset" & net_gain == "z"))%>%
  dplyr::filter(geographic_level=="Local authority")%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::select(Local.authority,year, net_gain)


chom_out_of_area <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/FOI%202024-0040813%20part%202.csv"), skip=13)%>%
  dplyr::filter(geographic_level=="Local authority")%>%
  dplyr::rename(Local.authority=la_name,
                year=time_period,
                out_of_area=number,
                out_of_area_per = percentage)%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::select(Local.authority,year, out_of_area, out_of_area_per)


data <- full_join(chom_out_of_area, chome_netgain, by=c("Local.authority", "year"))



data <- data %>%
  dplyr::full_join(., df, by="Local.authority")%>%
  dplyr::mutate(joined = ifelse(is.na(Join),"Open pre-2014","Opened since 2014"))



data <- data %>%
  full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/processed_profs.csv"))
          
    ,
    by = "Company.name"
  )

houseprice <- bind_rows(read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/detached_house_prices.csv"), skip=2)%>%
                          dplyr::select(Local.authority.name, contains("mar"), Region.Country.name)%>%
                          tidyr::pivot_longer(cols =contains("mar"), names_to = "year", values_to = "median_house_price_detached" )%>%
                          dplyr::mutate(year = as.numeric(gsub("[^0-9]", "", year)),
                                        median_house_price_detached = as.numeric(gsub(",", "", median_house_price_detached)))%>%
                          dplyr::rename(Local.authority = Local.authority.name)%>%
                          dplyr::mutate(Local.authority = Local.authority %>%
                                          gsub('&', 'and', .) %>%
                                          gsub('[[:punct:] ]+', ' ', .) %>%
                                          gsub('[0-9]', '', .)%>%
                                          toupper() %>%
                                          gsub("CITY OF", "",.)%>%
                                          gsub("UA", "",.)%>%
                                          gsub("COUNTY OF", "",.)%>%
                                          gsub("ROYAL BOROUGH OF", "",.)%>%
                                          gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                          gsub("UA", "",.)%>%
                                          gsub("DARWIN", "DARWEN", .)%>%
                                          gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                          gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                          gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                          gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                          str_trim())%>%
                          dplyr::filter(Local.authority!=""),
                        read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/detached_house_prices_county.csv"), skip=2)%>%
                          dplyr::select(County.name, contains("mar"), Region.Country.name)%>%
                          dplyr::rename(Local.authority = County.name)%>%
                          tidyr::pivot_longer(cols =contains("mar"), names_to = "year", values_to = "median_house_price_detached" )%>%
                          dplyr::mutate(year = as.numeric(gsub("[^0-9]", "", year)),
                                        median_house_price_detached = as.numeric(gsub(",", "", median_house_price_detached)))%>%
                          dplyr::mutate(Local.authority = Local.authority %>%
                                          gsub('&', 'and', .) %>%
                                          gsub('[[:punct:] ]+', ' ', .) %>%
                                          gsub('[0-9]', '', .)%>%
                                          toupper() %>%
                                          gsub("CITY OF", "",.)%>%
                                          gsub("UA", "",.)%>%
                                          gsub("COUNTY OF", "",.)%>%
                                          gsub("ROYAL BOROUGH OF", "",.)%>%
                                          gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                          gsub("UA", "",.)%>%
                                          gsub("DARWIN", "DARWEN", .)%>%
                                          gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                          gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                          gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                          gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                          str_trim())%>%
                          dplyr::filter(Local.authority!=""))

house_price_two <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/priceperareadata.csv"), skip=2)%>%
  dplyr::mutate(Location =  LA.name %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2016)_Lookup_in_EW.csv"))%>%
                     dplyr::mutate(Location = LTLA16NM %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim(),
                                   Local.authority = UTLA16NM %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim()))%>%
  dplyr::select(Local.authority, X2014, X2015, X2016)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(X2014 = mean(X2014, na.rm=T),
                   X2015 = mean(X2015, na.rm=T),
                   X2016 = mean(X2016, na.rm=T))%>%
  dplyr::bind_rows(.,  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/priceperareadata.csv"), skip=2)%>%
                     dplyr::mutate(Local.authority =  LA.name %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::select(Local.authority, X2016, X2015, X2014)
  )%>%
  dplyr::distinct(Local.authority, .keep_all = T)%>%
  dplyr::mutate(average_house_price_per_sq_m = (X2014+X2015+X2016)/3)%>%
  dplyr::select(Local.authority, average_house_price_per_sq_m)

workforce <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/csww_indicators_2017_to_2024.csv"))%>%
  dplyr::filter(geographic_level=="Local authority")%>%
  dplyr::select(time_period, la_name, vacancy_rate_fte)%>%
  dplyr::rename(year=time_period,
                Local.authority = la_name)%>%
  dplyr::mutate(vacancy_rate_fte = as.numeric(vacancy_rate_fte))%>%
  dplyr::bind_rows(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/SFR07_2016_UD.csv"))%>%
                     dplyr::select(LA_name, A3_FTE_VacancyRate_2015)%>%
                     dplyr::mutate(year=2015)%>%
                     dplyr::rename(Local.authority= LA_name,
                                   vacancy_rate_fte = A3_FTE_VacancyRate_2015)%>%
                     dplyr::filter(Local.authority!="")%>%
                     dplyr::mutate(vacancy_rate_fte = as.numeric(vacancy_rate_fte)))%>%
  dplyr::bind_rows(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/SFR07_2016_UD.csv"))%>%
                     dplyr::select(LA_name, A3_FTE_VacancyRate_2015)%>%
                     dplyr::mutate(year=2016)%>%
                     dplyr::rename(Local.authority= LA_name,
                                   vacancy_rate_fte = A3_FTE_VacancyRate_2015)%>%
                     dplyr::filter(Local.authority!="")%>%
                     dplyr::mutate(vacancy_rate_fte = as.numeric(vacancy_rate_fte)))%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())


data <- data %>%
  dplyr::full_join(., workforce)


poplookup <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/two_child_limit/refs/heads/main/Data/Raw/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2022)_Lookup_in_England_and_Wales.csv"))%>%
  dplyr::mutate(Location = LTLA22NM %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim(),
                Local.authority = UTLA22NM %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())

pov <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/children_in_rel_pov.csv"), skip=12)%>%
  dplyr::select(-Year)%>%
  dplyr::rename(Local.authority=X)%>%
  pivot_longer(      cols = starts_with("X"),
                     names_to = "year",
                     values_to = "child_rel_pov_n")%>%
  mutate(year = as.numeric(str_extract(year, "\\d{4}")),
         Local.authority = Local.authority %>%
           gsub('&', 'and', .) %>%
           gsub('[[:punct:] ]+', ' ', .) %>%
           gsub('[0-9]', '', .)%>%
           toupper() %>%
           gsub("CITY OF", "",.)%>%
           gsub("UA", "",.)%>%
           gsub("COUNTY OF", "",.)%>%
           gsub("ROYAL BOROUGH OF", "",.)%>%
           gsub("LEICESTER CITY", "LEICESTER",.)%>%
           gsub("UA", "",.)%>%
           gsub("DARWIN", "DARWEN", .)%>%
           gsub("COUNTY DURHAM", "DURHAM", .)%>%
           gsub("AND DARWEN", "WITH DARWEN", .)%>%
           gsub("NE SOM", "NORTH EAST SOM", .)%>%
           gsub("N E SOM", "NORTH EAST SOM", .)%>%
           str_trim())%>%
  dplyr::left_join(read.csv(curl("https://raw.githubusercontent.com/BenGoodair/two_child_limit/refs/heads/main/Data/Raw/myebtablesenglandwales20112023.csv"), skip=1)%>%
                     dplyr::filter(age<19)%>%
                     tidyr::pivot_longer(cols = c("population_2011",
                                                  "population_2012",
                                                  "population_2013",
                                                  "population_2014",
                                                  "population_2015",
                                                  "population_2016",
                                                  "population_2017",
                                                  "population_2018",
                                                  "population_2019",
                                                  "population_2020",
                                                  "population_2021",
                                                  "population_2022",
                                                  "population_2023"), names_to = "year", values_to = "under_19_population")%>%
                     dplyr::select(-age, -sex, -ladcode23, -country)%>%
                     dplyr::mutate(under_19_population = gsub(",", "", under_19_population))%>%
                     dplyr::group_by(year, laname23)%>%
                     dplyr::summarise(under_19_population = sum(as.numeric(under_19_population), na.rm=T))%>%
                     dplyr::ungroup()%>%
                     dplyr::mutate(year = as.numeric(gsub("population_", "", year)),
                                   Location = laname23 %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::full_join(., poplookup, by="Location")%>%
                     dplyr::select(Local.authority, year, under_19_population)%>%
                     dplyr::group_by(Local.authority, year)%>%
                     dplyr::summarise(under_19_population = sum(under_19_population, na.rm=T))%>%
                     dplyr::ungroup()
  )%>%
  dplyr::mutate(rel_pov_per = as.numeric(child_rel_pov_n)/ under_19_population *100)





data <- data %>%
  dplyr::full_join(., pov)

controls <- read.csv(curl("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190593...1774190597,1774190637,1774190646,1774190675...1774190678,1774190691,1774190598...1774190601,1774190638,1774190639,1774190652,1774190653,1774190656...1774190670,1774190734,1774190602...1774190606,1774190654,1774190671...1774190674,1774190686...1774190690,1774190607...1774190610,1774190650,1774190651,1774190726,1774190735,1774190736,1774190738,1774190611...1774190613,1774190640,1774190679...1774190685,1774190740,1774190743,1774190745,1774190621...1774190624,1774190644,1774190645,1774190725,1774190729,1774190732,1774190737,1774190741,1774190692...1774190724,1774190625...1774190636,1774190649,1774190728,1774190731,1774190733,1774190739,1774190742,1774190744,1774190614...1774190620,1774190641...1774190643,1774190647,1774190648,1774190655,1774190727,1774190730,1774190746...1774190799&date=latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=557,84,2011&measures=20599,21001,21002,21003"))%>%
  dplyr::filter(MEASURES_NAME == "Variable")%>%
  dplyr::select(DATE, GEOGRAPHY_NAME, VARIABLE_NAME, OBS_VALUE)%>%
  dplyr::filter(!is.na(OBS_VALUE))%>%
  dplyr::bind_rows(., read.csv(curl("https://www.nomisweb.co.uk/api/v01/dataset/NM_99_1.data.csv?geography=1774190593...1774190597,1774190637,1774190646,1774190675...1774190678,1774190691,1774190598...1774190601,1774190638,1774190639,1774190652,1774190653,1774190656...1774190670,1774190734,1774190602...1774190606,1774190654,1774190671...1774190674,1774190686...1774190690,1774190607...1774190610,1774190650,1774190651,1774190726,1774190735,1774190736,1774190738,1774190611...1774190613,1774190640,1774190679...1774190685,1774190740,1774190743,1774190745,1774190621...1774190624,1774190644,1774190645,1774190725,1774190729,1774190732,1774190737,1774190741,1774190692...1774190724,1774190625...1774190636,1774190649,1774190728,1774190731,1774190733,1774190739,1774190742,1774190744,1774190614...1774190620,1774190641...1774190643,1774190647,1774190648,1774190655,1774190727,1774190730,1774190746...1774190799&date=latestMINUS11-latest&sex=8&item=2&pay=5&measures=20100,20701"))%>%
                     dplyr::filter(MEASURES_NAME == "Value")%>%
                     dplyr::select(DATE, GEOGRAPHY_NAME, OBS_VALUE)%>%
                     dplyr::filter(!is.na(OBS_VALUE))%>%
                     dplyr::mutate(VARIABLE_NAME= "median_wage",
                                   DATE = as.character(DATE)))%>%
  dplyr::bind_rows(., read.csv(curl("https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=1774190593...1774190597,1774190637,1774190646,1774190675...1774190678,1774190691,1774190598...1774190601,1774190638,1774190639,1774190652,1774190653,1774190656...1774190670,1774190734,1774190602...1774190606,1774190654,1774190671...1774190674,1774190686...1774190690,1774190607...1774190610,1774190650,1774190651,1774190726,1774190735,1774190736,1774190738,1774190611...1774190613,1774190640,1774190679...1774190685,1774190740,1774190743,1774190745,1774190621...1774190624,1774190644,1774190645,1774190725,1774190729,1774190732,1774190737,1774190741,1774190692...1774190724,1774190625...1774190636,1774190649,1774190728,1774190731,1774190733,1774190739,1774190742,1774190744,1774190614...1774190620,1774190641...1774190643,1774190647,1774190648,1774190655,1774190727,1774190730,1774190746...1774190810&date=latestMINUS133,latestMINUS121,latestMINUS109,latestMINUS97,latestMINUS85,latestMINUS73,latestMINUS61,latestMINUS49,latestMINUS37,latestMINUS25,latestMINUS13,latestMINUS1&gender=0&age=0&measure=2&measures=20100"))%>%
                     dplyr::select(DATE, GEOGRAPHY_NAME, OBS_VALUE)%>%
                     dplyr::filter(!is.na(OBS_VALUE))%>%
                     dplyr::mutate(VARIABLE_NAME= "claimant_count_rate"))%>%
  dplyr::mutate(year = as.numeric(sub(".*?(\\d{4}).*", "\\1", DATE)),
                Local.authority = GEOGRAPHY_NAME %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::select(-DATE,-GEOGRAPHY_NAME)%>%
  tidyr::pivot_wider(id_cols = c("year", "Local.authority"), names_from = "VARIABLE_NAME", values_from = "OBS_VALUE")%>%
  dplyr::full_join(.,read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(variable=="Total", subcategory=="Age group", category=="child characteristic at 31st March") %>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care = number)%>%
                     dplyr::select(Local.authority, year, children_in_care), 
                   by=c("Local.authority", "year"))



source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/provider_cleaning_function.R")
inspection_data <- create_provider_data()%>%
  dplyr::select(URN, Overall.experiences.and.progress.of.children.and.young.people, Inspection.date)%>%
  dplyr::distinct(.keep_all = T)%>%
  dplyr::mutate(overall.average = ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Outstanding", 4,
                                         ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Good", 3,
                                                ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Requires improvement to be good", 2,
                                                       ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Inadequate", 1,NA
                                                       )))))%>%
  dplyr::mutate(number_of_inspections = 1)%>%
  dplyr::group_by(URN)%>%
  dplyr::mutate(ever_outstanding = ifelse(overall.average==4,1,0),
                ever_inadequate = ifelse(overall.average==1,1,0))%>%
  dplyr::summarise(overall.average = mean(overall.average, na.rm=T),
                   number_of_inspections = sum(number_of_inspections, na.rm=T),
                   ever_outstanding = sum(ever_outstanding, na.rm=T),
                   ever_inadequate = sum(ever_inadequate, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ever_outstanding = ifelse(ever_outstanding>0,1,0),
                ever_inadequate = ifelse(ever_inadequate>0,1,0))



df$Sector_merge <- factor(df$Sector_merge, levels = c("Local Authority", "LA owned company", "Third sector", "Individual owned", "Corporate owned", "Investment owned", "Unidentified for-profit"))
df$Overall.experiences.and.progress.of.children.and.young.people<- factor(df$Overall.experiences.and.progress.of.children.and.young.people, levels = c("Inadequate", "Requires improvement to be good", "Good", "Outstanding"))
df$year = lubridate::year(df$Latest.full.inspection.date)

create_annual_panel <- function(data) {
  # First, ensure we have clean dates and convert to Date type
  data_cleaned <- data %>%
    mutate(
      Join = as.Date(Join, "%Y-%m-%d"),
      Leave = as.Date(Leave, "%Y-%m-%d"),
      # If Leave is NA, assume the organization is still active
      Leave = if_else(is.na(Leave), as.Date("2024-12-31"), Leave),
      Join = if_else(is.na(Join), as.Date("2014-01-31"), Join)
    )
  
  
  
  panel_data <- data_cleaned %>%
    # Create a row for each year the organization was active
    rowwise() %>%
    mutate(
      active_years = list(seq(year(Join), year(Leave), by = 1))
    ) %>%
    unnest(active_years) %>%
    
    # Filter for years within the active period
    filter(
      active_years >= year(Join) & 
        active_years <= year(Leave)
    ) %>%
    
    # Group and summarize
    group_by(Local.authority, active_years, Sector_merge) %>%
    summarise(
      Places = sum(Places, na.rm = TRUE),
      Homes = sum(Homes, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    
    # Pivot wider to create sector-specific columns
    pivot_wider(
      names_from = Sector_merge, 
      values_from = c("Places", "Homes"),
      names_sep = "_",
      # Aggregate in case of multiple rows
      values_fn = sum
    ) %>%
    
    # Clean up column names
    rename_with(
      ~ gsub(" ", "_", .x), 
      everything()
    ) %>%
    
    # Rename to match your expected output
    rename(
      year = active_years
    ) %>%
    
    # Rename to make column names R-friendly
    rename_with(
      ~ stringr::str_replace_all(., "-", "_"),
      everything()
    ) %>%
    
    # Group and aggregate to ensure no duplicates
    group_by(Local.authority, year) %>%
    summarise(across(where(is.numeric), sum)) %>%
    ungroup() %>%
    
    # Fill NA values with 0
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    
    # Sort the data
    arrange(Local.authority, year)
  
  return(panel_data)
}

# Apply the function to your data
LA_panel_data <- create_annual_panel(df%>%
                                       dplyr::distinct(URN, .keep_all = T))



LA_panel_data <- LA_panel_data %>%
  dplyr::full_join(., data%>%
                     dplyr::select(Local.authority,year, out_of_area_per, out_of_area, net_gain)%>%
                     dplyr::distinct(.keep_all = T),
                   by=c("Local.authority", "year"))%>%
  dplyr::filter(Local.authority!="LONDON",
                !is.na(year))

LA_panel_data <- LA_panel_data %>%
  dplyr::full_join(., workforce,
                   by=c("Local.authority", "year"))%>%
  dplyr::filter(Local.authority!="LONDON")

LA_panel_data <- LA_panel_data %>%
  dplyr::full_join(., pov,
                   by=c("Local.authority", "year"))%>%
  dplyr::filter(Local.authority!="LONDON")

LA_panel_data <- LA_panel_data %>%
  dplyr::left_join(., houseprice,
                   by=c("Local.authority", "year"))%>%
  dplyr::filter(Local.authority!="LONDON")

LA_panel_data <- LA_panel_data %>%
  dplyr::left_join(., house_price_two,
                   by=c("Local.authority"))%>%
  dplyr::filter(Local.authority!="LONDON")

LA_panel_data <- LA_panel_data %>%
  dplyr::left_join(., controls,
                   by=c("Local.authority", "year"))%>%
  dplyr::filter(Local.authority!="LONDON")




LA_panel_data$net_loss <- as.numeric(LA_panel_data$net_gain)*-1
LA_panel_data$occupancy_rate_fte <- 100-as.numeric(LA_panel_data$vacancy_rate_fte)
LA_panel_data$median_house_price_detached <- as.numeric(LA_panel_data$median_house_price_detached)/10000
LA_panel_data$children_in_care <- as.numeric((as.numeric(LA_panel_data$out_of_area)/as.numeric(LA_panel_data$out_of_area_per)*100))

LA_panel_data$net_loss_same_year <- as.numeric(scale(as.numeric(LA_panel_data$net_loss)))
LA_panel_data$All_homes <- LA_panel_data$Homes_Local_Authority+
  LA_panel_data$Homes_LA_owned_company+
  LA_panel_data$Homes_Investment_owned+
  LA_panel_data$Homes_Unidentified_for_profit+
  LA_panel_data$Homes_Individual_owned+
  LA_panel_data$Homes_Corporate_owned+
  LA_panel_data$Homes_Third_sector



LA_panel_data <- LA_panel_data %>%
  left_join(LA_panel_data %>% 
              mutate(year = (as.numeric(year) +1), net_loss_lagged = scale(as.numeric(net_loss))) %>% 
              dplyr::select(Local.authority, year,net_loss_lagged )%>%
              distinct(.keep_all = T), by = c("Local.authority", "year"))

LA_panel_data <- LA_panel_data %>%
  left_join(LA_panel_data %>% 
              mutate(year = (as.numeric(year) - 1), net_loss_lead = scale(as.numeric(net_loss))) %>% 
              dplyr::select(Local.authority, year,net_loss_lead )%>%
              distinct(.keep_all = T), by = c("Local.authority", "year"))

LA_panel_data <- LA_panel_data %>%
  left_join(er <- LA_panel_data %>% 
              mutate(year = (as.numeric(year) + 1), LA_lagged = as.numeric(Homes_Local_Authority)) %>% 
              dplyr::select(Local.authority, year,LA_lagged )%>%
              distinct(.keep_all = T), by = c("Local.authority", "year"))

LA_panel_data <- LA_panel_data %>%
  left_join(LA_panel_data %>% 
              mutate(year = (as.numeric(year) + 1), third_lagged = as.numeric(Homes_Third_sector)) %>% 
              dplyr::select(Local.authority, year,third_lagged )%>%
              distinct(.keep_all = T), by = c("Local.authority", "year"))


LA_panel_data <- LA_panel_data %>%
  left_join(LA_panel_data %>% 
              mutate(year = (as.numeric(year) + 1), invest_lagged = as.numeric(Homes_Investment_owned)) %>% 
              dplyr::select(Local.authority, year,invest_lagged )%>%
              distinct(.keep_all = T), by = c("Local.authority", "year"))


LA_panel_data <- LA_panel_data %>%
  left_join(LA_panel_data %>% 
              mutate(year = (as.numeric(year) + 1), all_lagged = as.numeric(All_homes)) %>% 
              dplyr::select(Local.authority, year,all_lagged )%>%
              distinct(.keep_all = T), by = c("Local.authority", "year"))


LA_panel_data <- LA_panel_data %>%
  left_join(LA_panel_data %>% 
              mutate(year = (as.numeric(year) + 1), ind_lagged = as.numeric(Homes_Individual_owned)) %>% 
              dplyr::select(Local.authority, year,ind_lagged )%>%
              distinct(.keep_all = T), by = c("Local.authority", "year"))

LA_panel_data <- LA_panel_data %>%
  left_join(LA_panel_data %>% 
              mutate(year = (as.numeric(year) + 1), corp_lagged = as.numeric(Homes_Corporate_owned)) %>% 
              dplyr::select(Local.authority, year,corp_lagged )%>%
              distinct(.keep_all = T), by = c("Local.authority", "year"))



LA_panel_data <- LA_panel_data %>%
  dplyr::left_join(.,read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="Expenditure", subcategory=="Total_expenditure",variable=="Residential care") %>%
                     dplyr::rename(Local.authority = LA_Name,
                                   residential_expenditure = number)%>%
                     dplyr::select(Local.authority, year, residential_expenditure), 
                   by=c("Local.authority", "year"))

LA_panel_data <- LA_panel_data %>%
  dplyr::left_join(.,read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(variable == "Unaccompanied asylum-seeking children"&subcategory=="Unaccompanied asylum-seeking children"&category=="child characteristic at 31st March") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   Asylum_per = percent,
                                   Asylum_no = number)%>%
                     dplyr::select(Local.authority, year, Asylum_per, Asylum_no), by=c("Local.authority", "year"))

LA_panel_data <- LA_panel_data %>%
  dplyr::mutate(all_change = All_homes-all_lagged,
                third_change = Homes_Third_sector-third_lagged,
                la_change = Homes_Local_Authority-LA_lagged,
                invest_change = Homes_Investment_owned-invest_lagged,
                corp_change = Homes_Corporate_owned-corp_lagged,
                ind_change = Homes_Individual_owned-ind_lagged)



LA_panel_data <- pdata.frame(LA_panel_data%>%
                               distinct(.keep_all = T), index = c("Local.authority", "year"))




#### Prepare Data for Multinomial BRMS Model ####


mlm <- df %>% 
  dplyr::mutate(joined = ifelse(is.na(Join) | Join <= "2016-01-01", 0, 1),
                left = ifelse(!is.na(Leave), 1, 0),
                age = as.integer(time_length(difftime( as.Date(Registration.date,  format =  "%d/%m/%Y"), as.Date("2024-01-01")), "months"))*-1)%>%
  left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/processed_profs.csv")),
    by = "Company.name"
  )%>%
  dplyr::left_join(., LA_panel_data %>%
                     dplyr::select(Local.authority,net_loss, claimant_count_rate,average_house_price_per_sq_m,rel_pov_per, Unemployment.rate...aged.16.64., occupancy_rate_fte, X..of.white...aged.16., median_house_price_detached, median_wage, residential_expenditure, out_of_area_per, out_of_area, Region.Country.name)%>%
                     dplyr::mutate(children_in_care = as.numeric((as.numeric(out_of_area)/as.numeric(out_of_area_per)*100)))%>%
                     dplyr::group_by(Local.authority, Region.Country.name)%>%
                     dplyr::summarise(net_loss = mean(as.numeric(net_loss), na.rm=T),
                                      median_house_price_detached = mean(as.numeric(median_house_price_detached), na.rm=T),
                                      median_wage = mean(as.numeric(median_wage), na.rm=T),
                                      average_house_price_per_sq_m = mean(as.numeric(average_house_price_per_sq_m), na.rm=T),
                                      children_in_care = mean(as.numeric(children_in_care), na.rm=T),
                                      residential_expenditure = mean(as.numeric(residential_expenditure), na.rm=T),
                                      Unemployment.rate...aged.16.64. = mean(as.numeric(Unemployment.rate...aged.16.64.), na.rm=T),
                                      occupancy_rate_fte = mean(as.numeric(occupancy_rate_fte), na.rm=T),
                                      X..of.white...aged.16. = mean(as.numeric(X..of.white...aged.16.), na.rm=T),
                                      claimant_count_rate = mean(as.numeric(claimant_count_rate), na.rm=T),
                                      rel_pov_per = mean(as.numeric(rel_pov_per), na.rm=T))%>%
                     dplyr::ungroup(),
                   by="Local.authority")%>%
  dplyr::filter(Sector_merge!="Unidentified for-profit",
                Sector_merge!="LA owned company")%>%
  dplyr::left_join(., inspection_data, by="URN")%>%
  dplyr::distinct(URN, .keep_all = T)


res_care_n <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
  dplyr::filter(subcategory=="Placement", category=="child characteristic at 31st March", 
                variable=="Secure units children's homes and semi-independent living accommodation"|variable=="Secure homes and children's homes")%>%
  dplyr::mutate(children_in_care = as.numeric(number))%>%
  dplyr::rename(Local.authority = LA_Name)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(children_in_care = mean(children_in_care, na.rm=T))%>%
  dplyr::ungroup()

mlm <- mlm %>%
  dplyr::select(-children_in_care)%>%
  dplyr::left_join(., res_care_n)

mlm$net_loss <- as.numeric(mlm$net_loss)
mlm$median_wage <- as.numeric(mlm$median_wage)
mlm$median_house_price_detached <- as.numeric(mlm$median_house_price_detached)
mlm$children_in_care <- as.numeric(mlm$children_in_care)
mlm$residential_expenditure <- as.numeric(mlm$residential_expenditure)
mlm$claimant_count_rate <- as.numeric(mlm$claimant_count_rate)
mlm$X..of.white...aged.16. <- as.numeric(mlm$X..of.white...aged.16.)
mlm$Unemployment.rate...aged.16.64. <- as.numeric(mlm$Unemployment.rate...aged.16.64.)
mlm$occupancy_rate_fte <- as.numeric(mlm$occupancy_rate_fte)
mlm$rel_pov_per <- as.numeric(mlm$rel_pov_per)


# Create a scaled version in the data frame
mlm$net_loss_s <- scale(mlm$net_loss)
mlm$median_wage_s <- scale(mlm$median_wage)
mlm$median_house_price_detached_s <- scale(mlm$median_house_price_detached)
mlm$average_house_price_per_sq_m_s <- scale(mlm$average_house_price_per_sq_m)
mlm$children_in_care_s <- scale(mlm$children_in_care)
mlm$residential_expenditure_s <- scale(mlm$residential_expenditure)
mlm$claimant_count_rate_s <- scale(mlm$claimant_count_rate)
mlm$X..of.white...aged.16._s <- scale(mlm$X..of.white...aged.16.)
mlm$Unemployment.rate...aged.16.64._s <- scale(mlm$Unemployment.rate...aged.16.64.)
mlm$occupancy_rate_fte_s <- scale(mlm$occupancy_rate_fte)
mlm$rel_pov_per_s <- scale(mlm$rel_pov_per)

mlm$net_loss_s <- as.numeric(mlm$net_loss_s)
mlm$median_wage_s <- as.numeric(mlm$median_wage_s)
mlm$median_house_price_detached <- as.numeric(mlm$median_house_price_detached)
mlm$average_house_price_per_sq_m_s <- as.numeric(mlm$average_house_price_per_sq_m_s)
mlm$children_in_care <- as.numeric(mlm$children_in_care)
mlm$residential_expenditure <- as.numeric(mlm$residential_expenditure)
mlm$claimant_count_rate <- as.numeric(mlm$claimant_count_rate)
mlm$X..of.white...aged.16. <- as.numeric(mlm$X..of.white...aged.16.)
mlm$Unemployment.rate...aged.16.64. <- as.numeric(mlm$Unemployment.rate...aged.16.64.)
mlm$occupancy_rate_fte <- as.numeric(mlm$occupancy_rate_fte)
mlm$rel_pov_per <- as.numeric(mlm$rel_pov_per)




mlm$Sector_merge <- as.factor(mlm$Sector_merge)

mlm$closed <- factor(mlm$left)

mlm$age_years <- mlm$age/12

####chain size from global ultimate owner plsssss####


mlm <- mlm %>%
  left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/chain_size.csv")), by= "URN")



mlm <- mlm %>%
  dplyr::mutate(binary_sector_private = ifelse(Sector_merge=="Local Authority", 0,
                                               ifelse(Sector_merge=="Third sector",0 ,1)),
                binary_sector_private = factor(binary_sector_private))


mlm <- mlm %>%
  dplyr::left_join(., 
                   read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/FOI%202024-0040813%20part%201.csv"), skip=13)%>%
                     dplyr::rename(Local.authority=la_name,
                                   year=time_period,
                                   net_gain = number)%>%
                     dplyr::filter(geographic_level=="Local authority",
                                   year==2014)%>%
                     dplyr::mutate(Local.authority = Local.authority %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::mutate(net_loss_2014 = as.numeric(net_gain)*-1)%>%
                     dplyr::select(Local.authority, net_loss_2014))


mlm <- mlm %>%
  left_join(., mlm %>%
              dplyr::select(Local.authority, net_loss) %>%
              dplyr::distinct(.keep_all = TRUE) %>%
              dplyr::mutate(needQuint = factor(ntile(net_loss, 5))) %>%
              dplyr::select(-net_loss)
  )

# mlm <- mlm %>%
#   dplyr::mutate(needQuint = ifelse(needQuint==1, "1 (Low need)",
#                                    ifelse(needQuint==2, "2",
#                                           ifelse(needQuint==3,"3",
#                                                  ifelse(needQuint==4, "4",
#                                                         ifelse(needQuint==5, "5 (High need)", NA))))),
#                 needQuint = factor(needQuint,
#                                    levels = c("5 (High need)", "4", "3", "2", "1 (Low need)")))
# 
mlm <- mlm %>%
  left_join(., mlm %>%
              dplyr::select(Local.authority, average_house_price_per_sq_m) %>%
              dplyr::distinct(.keep_all = TRUE) %>%
              dplyr::mutate(houseQuint = factor(ntile(average_house_price_per_sq_m, 5))) %>%
              dplyr::select(-average_house_price_per_sq_m)
  )

# mlm <- mlm %>%
#   dplyr::mutate(houseQuint = ifelse(houseQuint==1, "1 (Cheap property)",
#                                    ifelse(houseQuint==2, "2",
#                                           ifelse(houseQuint==3,"3",
#                                                  ifelse(houseQuint==4, "4",
#                                                         ifelse(houseQuint==5, "5 (Expensive property)", NA))))),
#                 houseQuint = factor(houseQuint,
#                                    levels = c("5 (Expensive property)", "4", "3", "2", "1 (Cheap property)")))




mlm$chain_size_0s <- mlm$chain_size/10



####reqs and recs####


mlm <- mlm %>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/reqs_per.csv")), by="URN")%>%
  dplyr::distinct(URN,.keep_all = T)


mlm <- mlm %>%
  dplyr::left_join(.,  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/priceperareadata.csv"), skip=2)%>%
                     dplyr::mutate(Local.authority =  LA.name %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::select(Local.authority, X2014)%>%
                     dplyr::rename(house_price_2014 = X2014)
                   
                   
  )






mlm <-  dplyr::left_join(mlm, read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/FOI%202024-0040813%20part%202.csv"), skip=13)%>%
                   dplyr::filter(geographic_level=="Local authority",
                                 time_period=="2023")%>%
                   dplyr::rename(Local.authority=la_name,
                                 year=time_period,
                                 out_of_area=number,
                                 out_of_area_per = percentage)%>%
                   dplyr::mutate(Local.authority = Local.authority %>%
                                   gsub('&', 'and', .) %>%
                                   gsub('[[:punct:] ]+', ' ', .) %>%
                                   gsub('[0-9]', '', .)%>%
                                   toupper() %>%
                                   gsub("CITY OF", "",.)%>%
                                   gsub("UA", "",.)%>%
                                   gsub("COUNTY OF", "",.)%>%
                                   gsub("ROYAL BOROUGH OF", "",.)%>%
                                   gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                   gsub("UA", "",.)%>%
                                   gsub("DARWIN", "DARWEN", .)%>%
                                   gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                   gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                   gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                   gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                   str_trim())%>%
                   dplyr::select(Local.authority,year, out_of_area, out_of_area_per)%>%
                   dplyr::mutate(children_in_care_23 = as.numeric(out_of_area)*(as.numeric(out_of_area_per)/100))%>%
                   dplyr::select(Local.authority, children_in_care_23),
                   by="Local.authority"
)%>%
  dplyr::distinct(URN, .keep_all = T) 














}




