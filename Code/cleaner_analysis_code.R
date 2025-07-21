



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

pre <- rbind(
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 3, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 4, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 5, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 6, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 7, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 8, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 9, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 10, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`)
)

all <- rbind(pre%>%
               dplyr::rename(Provision.type = `Provider type`,
                             Local.authority=`Local authority`)%>%
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
                               str_trim())%>% dplyr::select(URN,Local.authority,Sector,Places,Date,leave_join), 
             exits%>% dplyr::select(URN,Local.authority,Sector,Places,Date, leave_join)%>%
               dplyr::mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%  # Convert Date to Date format
               dplyr::filter(Date >= as.Date("2018-04-01"))  # Filter for dates after 1st April 2018
)%>%  
  dplyr::distinct(URN,leave_join, .keep_all = T)%>%
  tidyr::pivot_wider(id_cols = c("Local.authority", "Sector", "URN", "Places"), names_from = "leave_join", values_from = as.character("Date"))%>%
  dplyr::mutate(Join = substr(Join, 1, 10),
                Leave = substr(Leave, 1, 10),
                Sector= ifelse(Sector=="Health Authority", "Local Authority", Sector))


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
                     dplyr::filter(as.Date(Leave)>="2015-04-01"|
                                     is.na(Leave))%>%
                     dplyr::filter(as.Date(Join)<"2023-04-01"|
                                     is.na(Join)), by=c("URN"))%>%
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


checkforremove <- df %>%
  dplyr::distinct(URN, .keep_all = T) 

df_nola <- checkforremove %>% dplyr::filter(is.na(Local.authority)) 

checkforremove <- df %>%
  dplyr::distinct(URN, .keep_all = T) %>%
  dplyr::mutate(reason_drop = ifelse(is.na(Local.authority), "no LA",
                                     ))

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


lookup <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/lookup_fame_search_clean_manual.csv")%>%
  dplyr::mutate(Organisation_fame_search = Organisation_fame_search %>%
                  gsub("Selected", "", .),
                Company.name = Company.name %>%
                  gsub(" In liquidation", "", .)%>%
                  gsub(" Dissolved", "",.)%>%
                  str_trim())%>%
  dplyr::distinct(Organisation_fame_search, .keep_all = T)%>%
  dplyr::bind_rows(., data.frame(Organisation_fame_search = "BEACON CHILDCARE LTD", Company.name = "BEACON CHILDCARE LTD", stringsAsFactors = FALSE))%>%
  dplyr::mutate(Company.name = ifelse(Organisation_fame_search=="Kids", "KIDS", Company.name))



francois_clean <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_clean.csv")



df <- df %>%
  full_join(., lookup, by="Organisation_fame_search")%>%
  dplyr::left_join(., francois_clean, by="Company.name") %>%
  dplyr::mutate(Sector_merge = ifelse(Sector.x=="Local Authority", "Local Authority",
                                      ifelse(Sector.x=="Voluntary", "Third sector", Sector.y)),
                Sector_merge = ifelse(is.na(Sector_merge), "Unidentified for-profit", Sector_merge))

####Analysis Data####



chome_netgain <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/FOI%202024-0040813%20part%201.csv"), skip=13)%>%
  dplyr::rename(Local.authority=la_name,
                year=time_period,
                net_gain = number)%>%
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
  dplyr::full_join(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/profit_ratios.csv"), by="Company.name")

data <- data %>%
  full_join(
    rbind(read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/profits_over_time.csv") %>%
      mutate(across(starts_with("Profit.margin"), as.numeric),
             across(starts_with("Salaries..Turnover"), as.numeric),
             across(starts_with("EBITDA.margin."), as.numeric)) %>%
      mutate(
        profit_margin_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("Profit.margin")))) >= 3,
                                       rowMeans(dplyr::select(., starts_with("Profit.margin")), na.rm = TRUE), NA),
        ebitda_margin_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("EBITDA.margin.")))) >= 3,
                                       rowMeans(dplyr::select(., starts_with("EBITDA.margin.")), na.rm = TRUE), NA),
        salaries_turnover_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("Salaries..Turnover")))) >= 3,
                                           rowMeans(dplyr::select(., starts_with("Salaries..Turnover")), na.rm = TRUE), NA)
      ) %>%
      dplyr::select(Company.name, profit_margin_average, ebitda_margin_average, salaries_turnover_average),
      read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/kids_correction.csv")%>%
        dplyr::select(-GUO...Name)%>%
        mutate(across(starts_with("Profit.margin"), as.numeric),
               across(starts_with("Salaries..Turnover"), as.numeric),
               across(starts_with("EBITDA.margin."), as.numeric)) %>%
        mutate(
          profit_margin_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("Profit.margin")))) >= 3,
                                         rowMeans(dplyr::select(., starts_with("Profit.margin")), na.rm = TRUE), NA),
          ebitda_margin_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("EBITDA.margin.")))) >= 3,
                                         rowMeans(dplyr::select(., starts_with("EBITDA.margin.")), na.rm = TRUE), NA),
          salaries_turnover_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("Salaries..Turnover")))) >= 3,
                                             rowMeans(dplyr::select(., starts_with("Salaries..Turnover")), na.rm = TRUE), NA)
        ) %>%
        dplyr::select(Company.name, profit_margin_average, ebitda_margin_average, salaries_turnover_average)
        
      
    ),
    by = "Company.name"
  )

houseprice <- bind_rows(read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/detached_house_prices.csv", skip=2)%>%
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
                        read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/detached_house_prices_county.csv", skip=2)%>%
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

house_price_two <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/priceperareadata.csv", skip=2)%>%
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
  
  dplyr::left_join(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2016)_Lookup_in_EW.csv")%>%
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
  dplyr::bind_rows(.,  read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/priceperareadata.csv", skip=2)%>%
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

workforce <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/csww_indicators_2017_to_2024.csv")%>%
  dplyr::filter(geographic_level=="Local authority")%>%
  dplyr::select(time_period, la_name, vacancy_rate_fte)%>%
  dplyr::rename(year=time_period,
                Local.authority = la_name)%>%
  dplyr::mutate(vacancy_rate_fte = as.numeric(vacancy_rate_fte))%>%
  dplyr::bind_rows(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/SFR07_2016_UD.csv")%>%
                     dplyr::select(LA_name, A3_FTE_VacancyRate_2015)%>%
                     dplyr::mutate(year=2015)%>%
                     dplyr::rename(Local.authority= LA_name,
                                   vacancy_rate_fte = A3_FTE_VacancyRate_2015)%>%
                     dplyr::filter(Local.authority!="")%>%
                     dplyr::mutate(vacancy_rate_fte = as.numeric(vacancy_rate_fte)))%>%
  dplyr::bind_rows(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/SFR07_2016_UD.csv")%>%
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
  dplyr::filter(Local.authority!="DORSET",
                Local.authority!="LONDON",
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
  dplyr::filter(Local.authority!="LONDON",
                Local.authority!="DORSET")




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



LA_panel_data <- LA_panel_data %>%
  dplyr::left_join(.,
                   read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/FOI_2024-0014264_part_1.csv", skip=15) %>%
                     dplyr::rename(Local.authority = la_name,
                                   unregulated = number,
                                   year= time_period)%>%
                     dplyr::filter(new_la_code!="E10000009")%>%
                     dplyr::select(Local.authority,unregulated, year )%>%
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
                                     str_trim()),
                   by=c("Local.authority", "year"))



LA_panel_data <- pdata.frame(LA_panel_data%>%
                               distinct(.keep_all = T), index = c("Local.authority", "year"))




#### Prepare Data for Multinomial BRMS Model ####


mlm <- df %>% 
  dplyr::mutate(joined = ifelse(is.na(Join) | Join <= "2016-01-01", 0, 1),
                left = ifelse(!is.na(Leave), 1, 0),
                age = as.integer(time_length(difftime( as.Date(Registration.date,  format =  "%d/%m/%Y"), as.Date("2024-01-01")), "months"))*-1)%>%
  full_join(
    rbind(read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/profits_over_time.csv") %>%
            mutate(across(starts_with("Profit.margin"), as.numeric),
                   across(starts_with("Salaries..Turnover"), as.numeric),
                   across(starts_with("EBITDA.margin."), as.numeric)) %>%
            mutate(
              profit_margin_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("Profit.margin")))) >= 3,
                                             rowMeans(dplyr::select(., starts_with("Profit.margin")), na.rm = TRUE), NA),
              ebitda_margin_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("EBITDA.margin.")))) >= 3,
                                             rowMeans(dplyr::select(., starts_with("EBITDA.margin.")), na.rm = TRUE), NA),
              salaries_turnover_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("Salaries..Turnover")))) >= 3,
                                                 rowMeans(dplyr::select(., starts_with("Salaries..Turnover")), na.rm = TRUE), NA)
            ) %>%
            dplyr::select(Company.name, profit_margin_average, ebitda_margin_average, salaries_turnover_average),
          read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/kids_correction.csv")%>%
            dplyr::select(-GUO...Name)%>%
            mutate(across(starts_with("Profit.margin"), as.numeric),
                   across(starts_with("Salaries..Turnover"), as.numeric),
                   across(starts_with("EBITDA.margin."), as.numeric)) %>%
            mutate(
              profit_margin_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("Profit.margin")))) >= 3,
                                             rowMeans(dplyr::select(., starts_with("Profit.margin")), na.rm = TRUE), NA),
              ebitda_margin_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("EBITDA.margin.")))) >= 3,
                                             rowMeans(dplyr::select(., starts_with("EBITDA.margin.")), na.rm = TRUE), NA),
              salaries_turnover_average = ifelse(rowSums(!is.na(dplyr::select(., starts_with("Salaries..Turnover")))) >= 3,
                                                 rowMeans(dplyr::select(., starts_with("Salaries..Turnover")), na.rm = TRUE), NA)
            ) %>%
            dplyr::select(Company.name, profit_margin_average, ebitda_margin_average, salaries_turnover_average)
          
          
    ),
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
  left_join(., rbind(read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_1_basic_companies.csv")%>%
              dplyr::select(Company.name, GUO...Name)%>%
              dplyr::distinct(.keep_all = T)%>%
              dplyr::filter(Company.name!=""),
              read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/kids_correction.csv")%>%
                dplyr::select(Company.name, GUO...Name)%>%
                dplyr::distinct(.keep_all = T)%>%
                dplyr::filter(Company.name!="")
              ))


mlm <- mlm %>% dplyr::mutate(GUO...Name = ifelse(GUO...Name=="", Organisation, GUO...Name))


mlm <- mlm %>%
  mutate(Organisation = ifelse(Sector_merge=="Local Authority", Local.authority, Organisation))


mlm <- mlm %>%
  dplyr::group_by(GUO...Name) %>%
  dplyr::mutate(GUO_chain_size = dplyr::n())%>%
  dplyr::ungroup()%>%
  dplyr::mutate(company_filled = ifelse(is.na(Company.name), Organisation, Company.name))%>%
  dplyr::group_by(company_filled)%>%
  dplyr::mutate(comp_chain_size = dplyr::n())%>%
  dplyr::ungroup()%>%
  dplyr::mutate(chain_size = ifelse(is.na(GUO...Name)| GUO...Name=="",comp_chain_size, GUO_chain_size ))%>%
  dplyr::mutate(chain_size = ifelse(is.na(company_filled), NA, chain_size ))

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
####update provider data for 2024 and use that for inspections####
####work out reqs and recs per inspection during the period we have data, fold it into mlm####
CHrequirements <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/Requirements_ch_ifa.csv")%>%
  dplyr::mutate(Inspection.date = as.Date(Inspection.date, format="%d/%m/%Y"))%>%
  dplyr::mutate(number_of_requirements = 1)%>%
  dplyr::select(URN, number_of_requirements)%>%
  dplyr::group_by(URN)%>%
  dplyr::summarise(number_of_requirements = sum(number_of_requirements, na.rm=T))%>%
  dplyr::ungroup()

CHrecs <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/Recommendations_ch_ifa.csv")%>%
  dplyr::mutate(Inspection.date = as.Date(Inspection.date, format="%d/%m/%Y"))%>%
  dplyr::mutate(number_of_recomendations = 1)%>%
  dplyr::select(URN, number_of_recomendations)%>%
  dplyr::group_by(URN)%>%
  dplyr::summarise(number_of_recomendations = sum(number_of_recomendations, na.rm=T))%>%
  dplyr::ungroup()


source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/provider_cleaning_function.R")
ProviderData <- create_provider_data() %>%
  dplyr::filter(Event.type=="Full inspection")%>%
  dplyr::select(URN, Inspection.date) %>%
  dplyr::mutate(Inspection.date = as.Date(Inspection.date, format="%d/%m/%Y"))%>%
  dplyr::filter(Inspection.date>"2014-08-01",
                Inspection.date<"2021-05-05")%>%
  dplyr::mutate(number_of_requirement_inspections = 1)%>%
  dplyr::select(URN, number_of_requirement_inspections)%>%
  dplyr::group_by(URN)%>%
  dplyr::summarise(number_of_requirement_inspections = sum(number_of_requirement_inspections, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::full_join(., CHrecs, by="URN")%>%
  dplyr::full_join(., CHrequirements, by="URN")%>%
  dplyr::mutate(number_of_recomendations = ifelse(is.na(number_of_recomendations), 0, number_of_recomendations),
                number_of_requirements= ifelse(is.na(number_of_requirements), 0, number_of_requirements),
                recs_per = number_of_recomendations/ number_of_requirement_inspections,
                reqs_per = number_of_requirements/ number_of_requirement_inspections)%>%
  dplyr::select(URN, reqs_per, recs_per)



mlm <- mlm %>%
  dplyr::left_join(., ProviderData, by="URN")%>%
  dplyr::distinct(URN,.keep_all = T)


mlm <- mlm %>%
  dplyr::left_join(.,  read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Data/priceperareadata.csv", skip=2)%>%
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



####ANALYSIS####


#### figure1 ####
# Load necessary libraries
library(tidyverse)
library(cowplot)
library(viridis)
library(lubridate)


# Create a full sequence of times
all_times <- tibble(time = -238:0)

# Process the start dataset
carehomesstart <- df %>%
  mutate(
    Registration.date = as.Date(Registration.date, format = "%d/%m/%Y"),
    Join = as.Date(Join),
    location_start = coalesce(Registration.date, Join, as.Date("2014-03-15")),
    date = location_start,
    time = as.integer(time_length(difftime(date, as.Date("2023-12-01")), "months"))
  ) %>%
  distinct(URN, .keep_all = TRUE) %>%
  dplyr::select(time, Sector_merge, URN)

# Count observations by group
nobsByIdih <- carehomesstart %>%
  count(time, Sector_merge, name = "nobs")

# Ensure all time periods exist for each Sector_merge
nobsBySector <- nobsByIdih %>%
  complete(Sector_merge, time = all_times$time, fill = list(nobs = 0)) %>%
  group_by(Sector_merge) %>%
  mutate(cumulative = cumsum(nobs)) %>%
  ungroup()

# Process the end dataset
carehomesend <- df %>%
  filter(Leave != "") %>%
  mutate(
    date = as.Date(Leave),
    time = as.integer(time_length(difftime(date, as.Date("2023-12-01")), "months"))
  ) %>%
  distinct(URN, .keep_all = TRUE) %>%
  dplyr::select(time, Sector_merge, URN)

# Count observations by group
nobsEndByIdih <- carehomesend %>%
  count(time, Sector_merge, name = "nobs")

# Ensure all time periods exist for each Sector_merge
nobsEndBySector <- nobsEndByIdih %>%
  complete(Sector_merge, time = all_times$time, fill = list(nobs = 0)) %>%
  group_by(Sector_merge) %>%
  mutate(cumulative_end = cumsum(nobs)) %>%
  ungroup()

# Merge start and end datasets and compute the running total
nobser <- full_join(nobsBySector, nobsEndBySector, by = c("Sector_merge", "time")) %>%
  mutate(
    cumulative = replace_na(cumulative, 0),
    cumulative_end = replace_na(cumulative_end, 0),
    runningsum = cumulative - cumulative_end
  )

# Create the time-series plot
d <- ggplot(nobser %>% filter(time > -155,
                              Sector_merge != "LA owned company",
                              Sector_merge != "Unidentified for-profit"),
            aes(x = time, y = runningsum, group = Sector_merge,
                fill = Sector_merge, colour = Sector_merge, alpha = Sector_merge)) +
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("#008F5D", "#F0AB00", "#E4007C", "#FF5E5E", "#5B0000", "#1F77B4", "#9467BD")) +
  scale_alpha_manual(values = c(0.2, 0.2, 1, 1, 1, 0.2, 0.2)) +
  labs(x = "Year", y = "Number of children homes", 
       title = "Number of children's homes owned for-profit",
       fill = "Ownership", color = "Ownership", alpha = "Ownership") +
  scale_x_continuous(breaks = c(-12, -24, -36, -48, -60, -72, -84, -96, -108, -120, -132, -144, -156),
                     labels = c("2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11, face = "bold")  ) +
  coord_cartesian(xlim = c(-110, -22)) +
  theme(legend.position = "bottom")

# Reusable function for enhanced box/violin plots
create_plot_improved <- function(data, filter_col, filter_val, title, ylab_text, var_name, ylims = NULL) {
  ggplot(
    data %>% filter({{ filter_col }} == filter_val),
    aes(x = as.factor({{ filter_col }}), y = as.numeric(get(var_name)))
  ) +
    # Violin plot shows density distribution
    geom_violin(width = 0.8, fill = "#2A6EBB", alpha = 0.3, trim = TRUE) +
    # Box plot for median and quartiles
    geom_boxplot(width = 0.15, color = "#2A6EBB", fill = "white", outlier.shape = NA) +
    # Jittered points to show individual data points
#    geom_jitter(width = 0.1, size = 1.5, color = "black",fill="darkgrey", alpha = 0.1) +
    # Summary statistic: Mean (red diamond)
#    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_blank()
    ) +
    ggtitle(title) +
    xlab("") +
    ylab(ylab_text) +
    (if (!is.null(ylims)) coord_cartesian(ylim = ylims) else coord_cartesian())
}

# Generate improved plots for the "profit margin %" metric
fplow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Individual owned", 
  title = "\nIndividual owned",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

forlow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE),
  Sector_merge, "Corporate owned", 
  title = "\nCorporate owned",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

investlow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE),
  Sector_merge, "Investment owned", 
  title = "\nInvestment owned",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

lalow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE),
  Sector_merge, "Local Authority", 
  title = "Profit margins by sector\nLocal Authority",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

vollow_improved <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE),
  Sector_merge, "Third sector", 
  title = "\nThird Sector",
  ylab_text = "Profit margin %",
  var_name = "profit_margin_average",
  ylims = c(-25, 25)
)

# Combine improved profit margin plots into one composite row
yip_improved_profit <- cowplot::plot_grid(
  lalow_improved, vollow_improved, fplow_improved, forlow_improved, investlow_improved, 
  nrow = 1
)

# If needed, join inspection data (assuming 'inspection_data' exists)
data <- data %>%
  left_join(inspection_data, by = "URN")

# Generate improved plots for the "overall quality" metric (inspection scores)
lalow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Local Authority", 
  title = "Quality by sector\nLocal Authority",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

vollow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Third sector", 
  title = "\nThird Sector",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

fplow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Individual owned", 
  title = "\nIndividual owned",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

forlow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Corporate owned", 
  title = "\nCorporate owned",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

investlow_improved_overall <- create_plot_improved(
  data %>% distinct(URN, .keep_all = TRUE), 
  Sector_merge, "Investment owned", 
  title = "\nInvestment owned",
  ylab_text = "Quality (average inspection score)",
  var_name = "overall.average"
)

# Combine improved overall quality plots into one composite row
yip_improved_overall <- cowplot::plot_grid(
  lalow_improved_overall, vollow_improved_overall, fplow_improved_overall,
  forlow_improved_overall, investlow_improved_overall, 
  nrow = 1
)

# Combine both types of improved distribution plots with labels "D" and "E"
yip_both_improved <- cowplot::plot_grid(
  yip_improved_profit, yip_improved_overall, 
  ncol = 1, labels = c("B", "C")
)


final_plot <- cowplot::plot_grid(
  d, yip_both_improved, 
  ncol = 1, rel_heights = c(0.5, 0.5), labels = c("A", "")
)

# Display the final composite plot
print(final_plot)

ggsave(plot=final_plot, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_1.jpeg", width=12, height=14, dpi=600)







####Figure 2####







# Enhanced visualization of out-of-area placements
library(ggplot2)
library(dplyr)
library(scales)
library(ggtext)

# First, calculate some statistics to highlight in the visualization
stats <- data %>%
  distinct(Local.authority, year, .keep_all = TRUE) %>%
  group_by(year) %>%
  summarize(
    mean_pct = mean(as.numeric(out_of_area_per), na.rm = TRUE),
    median_pct = median(as.numeric(out_of_area_per), na.rm = TRUE),
    max_pct = max(as.numeric(out_of_area_per), na.rm = TRUE),
    q75_pct = quantile(as.numeric(out_of_area_per), 0.75, na.rm = TRUE),
    q25_pct = quantile(as.numeric(out_of_area_per), 0.25, na.rm = TRUE)
  )

# Calculate overall growth from first to last year
first_year <- min(stats$year, na.rm = TRUE)
last_year <- max(stats$year, na.rm = TRUE)
first_year_value <- stats$mean_pct[stats$year == first_year]
last_year_value <- stats$mean_pct[stats$year == last_year]
growth_pct <- round(((last_year_value - first_year_value) / first_year_value) * 100, 1)

# Create a more dramatic visualization
e <- ggplot() +
  # Add a filled area for the trend
  geom_ribbon(
    data = stats,
    aes(x = as.numeric(year), ymin = 0, ymax = mean_pct),
    fill = "#2A6EBB", alpha = 0.3
  ) +
  # Add individual points with size representing count
  geom_jitter(
    data = data %>% distinct(Local.authority, year, .keep_all = TRUE),
    aes(x = as.numeric(year), y = as.numeric(out_of_area_per)),
    color = "#2A6EBB", alpha = 0.4, width = 0.2, height = 0
  ) +
  # Add trend line
  geom_line(
    data = stats,
    aes(x = as.numeric(year), y = mean_pct),
    color = "#022a5e", size = 1.5
  ) +
  # Add points to trend line
  geom_point(
    data = stats,
    aes(x = as.numeric(year), y = mean_pct),
    color = "#022a5e", size = 3
  ) +
  # Add labels for first and last year
  geom_label(
    data = stats %>% filter(year %in% c(first_year, last_year)),
    aes(x = as.numeric(year), y = mean_pct, 
        label = paste0(round(mean_pct, 1), "%")),
    color = "#022a5e", fontface = "bold", vjust = -1.5, size = 4
  ) +
  # Add an arrow showing the growth
  
  # Add text for growth percentag
  # Formatting and labels
  labs(
    x = "",
    y = "Children placed in homes outside their area (%)",
    title = "Outside of area placements",
    subtitle = "More children are being placed outside their local area each year",
    caption = "Each point represents one local authority. Blue line shows average across all areas."
  ) +
  scale_x_continuous(breaks = 2014:2024) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(100, max(as.numeric(data$out_of_area_per), na.rm = TRUE) * 1.1))
  ) +
  # Improved theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"
  ) # Ensures custom color is used



















# Enhanced percentile visualization with dramatic dispersion
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

# Calculate percentiles for each year
percentile_data <- data %>%
  mutate(net_gain = as.numeric(net_gain))%>%
  distinct(Local.authority, year, .keep_all = TRUE) %>%
  group_by(year) %>%
  summarize(
    median = median(net_gain, na.rm = TRUE),
    p5 = quantile(net_gain, 0.05, na.rm = TRUE),   # More extreme low value
    p95 = quantile(net_gain, 0.95, na.rm = TRUE),  # More extreme high value
    p25 = quantile(net_gain, 0.25, na.rm = TRUE),
    p75 = quantile(net_gain, 0.75, na.rm = TRUE),
    extreme_range = p95 - p5,                      # Range between extremes
    extreme_range_abs = abs(p95) + abs(p5)         # Absolute magnitude of extremes
  )

# Create ribbon plot with filled areas
p1 <- ggplot(percentile_data, aes(x = as.numeric(year))) +
  # Add ribbon for 5th-95th percentile range (widest)
  geom_ribbon(aes(ymin = p5, ymax = p95), fill = "#F8766D", alpha = 0.3) +
  # Add ribbon for 25th-75th percentile range (narrower)
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F8766D", alpha = 0.5) +
  # Add lines for all percentiles
  geom_line(aes(y = p95, color = "95th percentile"), size = 1.2) +
  geom_line(aes(y = p5, color = "5th percentile"), size = 1.2) +
  geom_line(aes(y = p75, color = "75th percentile"), size = 1) +
  geom_line(aes(y = p25, color = "25th percentile"), size = 1) +
  geom_line(aes(y = median, color = "Median"), size = 1.5) +
  # Add zero reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  # Styling
  labs(
    x = "Year",
    y = "Net gain/loss of children",
    title = "Net gain/loss of children",
    subtitle = "Widening gap between gaining and losing local authorities",
    color = "Net gain percentile"
  ) +
  scale_x_continuous(breaks = 2014:2024) +
  # Define ordered factors for percentiles with specific order
  scale_color_manual(
    values = c(
      "5th percentile" = "#0072B2",
      "25th percentile" = "#56B4E9",
      "Median" = "#000000",
      "75th percentile" = "#E69F00", 
      "95th percentile" = "#D55E00"
    ),
    # The breaks parameter forces the legend to appear in this specific order
    breaks = c(
      "95th percentile",
      "75th percentile",
      "Median",
      "25th percentile",
      "5th percentile"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(size = 12, face = "bold")
  )


# Create a second plot showing the increasing range/dispersion
p2 <- ggplot(percentile_data, aes(x = as.numeric(year))) +
  geom_area(aes(y = extreme_range), fill = "#D55E00", alpha = 0.7) +
  geom_line(aes(y = extreme_range), color = "#D55E00", size = 1.5) +
  geom_point(aes(y = extreme_range), color = "#D55E00", size = 3) +
  labs(
    x = "Year",
    y = "Range (95th - 5th percentile)",
    title = "System Under Pressure: Growing Disparity",
    subtitle = "The gap between highest gainers and highest losers is widening dramatically"
  ) +
  scale_x_continuous(breaks = 2014:2024) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(size = 12, face = "bold")
  )

# Create a third plot showing the ratio of extreme range to median (how many times bigger the extremes are)
percentile_data <- percentile_data %>%
  mutate(
    median_abs = abs(median),
    # Protect against division by zero or very small medians
    range_to_median_ratio = ifelse(median_abs < 1, NA, extreme_range / median_abs)
  )

p3 <- ggplot(percentile_data, aes(x = as.numeric(year))) +
  geom_bar(aes(y = range_to_median_ratio), stat = "identity", fill = "#0072B2", alpha = 0.8) +
  geom_text(aes(y = range_to_median_ratio, label = round(range_to_median_ratio, 1)), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(
    x = "Year",
    y = "Ratio",
    title = "Extremes vs Typical: Ratio Analysis",
    subtitle = "How many times larger the extreme range is compared to median value"
  ) +
  scale_x_continuous(breaks = 2014:2024) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(size = 12, face = "bold")
  )

# Combine plots using patchwork
(p1 / p2 / p3) + 
  plot_layout(heights = c(3, 2, 2)) +
  plot_annotation(
    title = "THE GROWING CRISIS IN CHILDREN'S HOME PLACEMENTS",
    subtitle = "Local authorities experiencing increasingly extreme disparities in net gain/loss",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 14)
    )
  )








f <- ggplot(data%>%distinct(Local.authority, year, .keep_all = T), aes(x=as.numeric(year), y=sqrt(as.numeric(net_gain)^2)))+
  geom_point(aes(color = "#2A6EBB"), alpha = 0.2, size = 2) +  # Move color inside aes()
  geom_smooth(method = "loess", se = T)+
  labs(
    x = "Year",
    y = "Net gain of children's home places (sqrt n^2)",
    title = "Aboslute value of Net gain/loss of children home places")+
  theme_bw()+
  scale_x_continuous(breaks=c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024))+
  scale_color_identity()  # Ensures custom color is used

LA_panel_data$all_places <- LA_panel_data$Places_Individual_owned+
  
  LA_panel_data$Places_Corporate_owned+
  
  LA_panel_data$Places_Investment_owned+
  
  LA_panel_data$Places_Third_sector+
  
  LA_panel_data$Places_Local_Authority+
  
  LA_panel_data$Places_Unidentified_for_profit+
  
  LA_panel_data$Places_LA_owned_company

line <- data.frame(
  x = c(0, 200),
  y = c(0, 200)
)

library(ggrepel)


LA_panel_data <- LA_panel_data %>%
  dplyr::mutate(children_in_care = as.numeric(as.numeric(out_of_area)/as.numeric(out_of_area_per)*100))



# Prepare the data: calculate children_in_care and the ratio, then filter for 2023
LA_2023 <- as.data.frame(LA_panel_data)%>%
  mutate(Local.authority = as.character(Local.authority))%>%
  filter(year == 2023) %>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(variable=="Secure homes and children's homes", category=="child characteristic at 31st March", year==2023)%>%
                     dplyr::select(LA_Name, number)%>%
                     dplyr::rename(Local.authority= LA_Name)
  )%>%
  mutate(
    children_in_care = as.numeric(number),
    ratio = children_in_care / all_places
  ) 

# # Identify the local authorities with the highest and lowest ratios
# max_ratio_LA <- LA_2023 %>% filter(ratio == max(ratio, na.rm = TRUE))
# min_ratio_LA <- LA_2023 %>% filter(ratio == min(ratio, na.rm = TRUE))
# label_LAs <- bind_rows(max_ratio_LA, min_ratio_LA)

# Define thresholds for outliers (e.g., top 10% in either variable)
child_threshold  <- quantile(LA_2023$ratio, 0.92, na.rm = TRUE)
places_threshold <- quantile(LA_2023$ratio, 0.08, na.rm = TRUE)

# Flag outliers: either too high % of children in care or too many places
LA_2023 <- LA_2023 %>%
  mutate(outlier = ratio >= child_threshold | ratio <= places_threshold)


# Create a data frame for the 45° reference line
line <- data.frame(x = c(0, 200), y = c(0, 200))

library(ggplot2)
library(dplyr)
library(ggrepel)

# Convert the variables to numeric before plotting
LA_2023 <- LA_2023 %>%
  mutate(
    children_in_care = as.numeric(as.character(children_in_care)),
    all_places = as.numeric(as.character(all_places))
  ) %>%
  filter(!is.na(children_in_care) & !is.na(all_places))

# Define a diagonal reference line data frame
line <- data.frame(x = c(0, 200), y = c(0, 200))

LA_2023$outlier <- as.logical(LA_2023$outlier)



# Build the plot with an explicit grouping in geom_smooth
a <- ggplot(LA_2023, aes(x = children_in_care, y = all_places)) +
  # Points colored by whether they are flagged as an outlier
  geom_point(aes(color = outlier), size = 3, alpha = 0.7) +
  # Add a linear model smooth line using numeric variables with explicit group assignment
#  geom_smooth(data = LA_2023, aes(x = children_in_care, y = all_places), method = "lm", se = FALSE)+  # Draw a diagonal reference line (y = x)
  geom_line(data = line, aes(x = x, y = y), linetype = "dashed", color = "grey50") +
  # Add text labels for outliers only
  geom_text_repel(data = filter(LA_2023, outlier),
                  aes(label = Local.authority),
                  size = 2.5,
                  fontface = "bold",
                  color = "black",
                  box.padding = 0.15,
                  point.padding = 0.5,
                  segment.color = "grey50") +
  # Customize color scale for points (outliers vs. others)
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "firebrick")) +
  # Annotate the diagonal line with custom text
  annotate("text",
           x = 180, y = 145,
           label = "One bed available per child",
           angle = 17,
           vjust = -1,
           color = "grey20",
           fontface = "italic") +
  # Add informative labels and title
  labs(
    x = "Children in Residential Care Placements (n)",
    y = "Total Children Home Beds",
    title = "Children home beds and children in residential placements (2023)",
    subtitle = "Highlighting LAs with relatively too many places vs. not enough",
    caption = "Note: Beds do not equate to available capcity as many homes cannot operate with all beds filled."
  ) +
  # Use a clean theme and center titles
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"
  )


library(patchwork)

library(patchwork)

fig2 <- (a + (e / p1)) +
  plot_annotation(
    title = "The development of the sufficiency crisis in children's social care",
    tag_levels = "A",                    # will label A, B, C
    theme = theme(
      plot.title        = element_text(face = "bold", size = 18),
      plot.subtitle     = element_text(size = 14),
      plot.tag          = element_text(face = "bold", size = 16),   # bold labels
      plot.tag.position = c(0, 1)        # top-left of each panel
    )
  )

print(fig2)



ggsave(plot=fig2, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_2.jpeg", width=17, height=10, dpi=600)

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





#### Table 2####


# Install necessary packages if not installed
if (!require(stargazer)) install.packages("stargazer", dependencies = TRUE)
if (!require(htmltools)) install.packages("htmltools", dependencies = TRUE)

# Install necessary packages if not installed
# install.packages(c("modelsummary", "kableExtra", "estimatr"))

# Load required libraries
library(modelsummary)
library(kableExtra)
library(broom)
library(dplyr)
library(ggplot2)
library(estimatr)  # For clustering standard errors




# Fit models with clustered standard errors
reg1 <- lm_robust(overall.average ~ Sector_merge + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg2 <- lm_robust(overall.average ~ Sector_merge + Places + number_of_inspections + age_years + factor(left) + Local.authority, data = mlm, clusters = Organisation)
reg3 <- lm_robust(overall.average ~ profit_margin_average + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg4 <- lm_robust(overall.average ~ profit_margin_average + Places + number_of_inspections +factor(left)+ age_years  + Local.authority, data = mlm, clusters = Organisation)
reg5 <- lm_robust(ever_outstanding ~ Sector_merge + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg6 <- lm_robust(ever_outstanding ~ Sector_merge + Places + number_of_inspections + age_years +factor(left) + Local.authority, data = mlm, clusters = Organisation)
reg7 <- lm_robust(ever_outstanding ~ profit_margin_average + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg8 <- lm_robust(ever_outstanding ~ profit_margin_average + Places + number_of_inspections + age_years +factor(left) + Local.authority, data = mlm, clusters = Organisation)
reg11 <- lm_robust(reqs_per ~ Sector_merge + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg21 <- lm_robust(reqs_per ~ Sector_merge + Places + number_of_inspections + age_years + factor(left) + Local.authority, data = mlm, clusters = Organisation)
reg31 <- lm_robust(reqs_per ~ profit_margin_average + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg41 <- lm_robust(reqs_per ~ profit_margin_average + Places + number_of_inspections +factor(left)+ age_years  + Local.authority, data = mlm, clusters = Organisation)
reg51 <- lm_robust(recs_per ~ Sector_merge + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg61 <- lm_robust(recs_per ~ Sector_merge + Places + number_of_inspections + age_years +factor(left) + Local.authority, data = mlm, clusters = Organisation)
reg71 <- lm_robust(recs_per ~ profit_margin_average + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg81 <- lm_robust(recs_per ~ profit_margin_average + Places + number_of_inspections + age_years +factor(left) + Local.authority, data = mlm, clusters = Organisation)

library(modelsummary)
library(kableExtra)
library(dplyr)
library(gt)

# Your regression models (reg1 through reg8)
# First create the modelsummary table with estimates and std errors

library(modelsummary)
library(kableExtra)
library(gt)

# Try using modelsummary's built-in CI formatting
table_html <- modelsummary(
  list(reg1, reg2, reg3, reg4,
       reg11, reg21, reg31, reg41),
  stars = TRUE,
  coef_map = c(
    "Sector_mergeIndividual owned" = "Ownership [Individual-owned]", 
    "Sector_mergeCorporate owned" = "Ownership [Corporate-owned]",
    "Sector_mergeInvestment owned" = "Ownership [Investment-owned]",
    "Sector_mergeThird sector" = "Ownership [Third Sector]",
    "profit_margin_average" = "Profit Margin Average (%)",
    "Places" = "Places (n)",
    "age_years" = "Age (years)",
    "chain_size_0s" = "Chain Size (n, 10s)",
    "factor(left)1" = "De-registered home (closed or taken over dummy)"
  ),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
  fmt = "%.3f",
  estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
  statistic = NULL,  # Remove separate standard error row
  conf_level = 0.95, # 95% confidence intervals
  output = "gt"
) %>%
  tab_header(
    title = "Regression Models of Regulatory Outcomes"
  ) %>%
  tab_spanner(
    label = "Average Inspection Rating",
    columns = 2:5
  ) %>%
  tab_spanner(
    label = "Requirements per insepction",
    columns = 6:9
  )%>%
  tab_footnote(
    footnote = "Reference category for ownership is Local Authority",
    locations = cells_stub(rows = contains("Sector"))  # Apply to all ownership rows
  ) %>%
  tab_footnote(
    footnote = "* p < 0.1, ** p < 0.05, *** p < 0.01",
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "Number of inspections and Local Authority fixed effects are controlled for in all models.",
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "Standard errors are clustered at the chain level.",
    locations = cells_title()
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    source_notes.font.size = px(10),
    footnotes.font.size = px(10),
    table.width = pct(100)
  )

# Save the table as HTML
table_html %>%
  gtsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Table2_quality.html")

#### Table 3 new ####


head(df)


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


brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, net_loss_s, children_in_care_s,
                Local.authority, closed) %>%
  dplyr::mutate(
                Sector_merge = factor(Sector_merge),
                Local.authority = factor(Local.authority),
                closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 






### Fit the Hierarchical Multinomial Model ###
model_multilevel <- brm(
  formula   = Sector_merge ~ net_loss_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  prior     = priors,
  cores     = 4,
  iter      = 2000
)



brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, net_loss_s, children_in_care_s,
                Local.authority, closed, age_years) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed),
    age_s = scale(age_years))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 


### Fit the Hierarchical Multinomial Model ###
model_multilevel_int <- brm(
  formula   = Sector_merge ~ net_loss_s*age_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  prior     = priors,
  cores     = 4,
  iter      = 2000
)






brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,
                Local.authority, closed) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 

### Fit the Hierarchical Multinomial Model ###
model_multilevel_house <- brm(
  formula   = Sector_merge ~ average_house_price_per_sq_m_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  prior     = priors,
  cores     = 4,
  iter      = 2000
)

brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,
                Local.authority, closed, age_years) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed),
    age_s = scale(age_years))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 

### Fit the Hierarchical Multinomial Model ###
model_multilevel_house_int <- brm(
  formula   = Sector_merge ~ average_house_price_per_sq_m_s*age_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  prior     = priors,
  cores     = 4,
  iter      = 2000
)

get_prior(
  Sector_merge ~ net_loss_s + Places_s+ age_s+closed+chain_size_0s_s + children_in_care_s +
    residential_expenditure_s + average_house_price_per_sq_m_s + median_wage_s +rel_pov_per_s+
    Region.Country.name + (1 | Local.authority),
  family = categorical(),
  data   = brmdata_multinom
)



pp_check(model_multilevel)

pp_check(model_multilevel, type = "bars")           # bar plot comparison for categories
pp_check(model_multilevel, type = "stat", stat = "mean")  # compare means
pp_check(model_multilevel, type = "error_bars")     # interval checks

summary(model_multilevel)

library(dplyr)
library(kableExtra)
library(tidyr)

# Create extraction function if it doesn't already exist
if (!exists("extract_regression_table")) {
  extract_regression_table <- function(model, reference_level) {
    summary_model <- summary(model)
    results_table <- data.frame(
      Category = character(),
      Predictor = character(),
      Estimate = numeric(),
      SE = numeric(),
      CI_Lower = numeric(),
      CI_Upper = numeric(),
      P_value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Get outcome categories (excluding the specified reference)
    categories <- levels(model$data$Sector_merge)
    categories <- categories[categories != reference_level]
    
    for (cat in categories) {
      cat_clean <- gsub(" ", "", cat)
      fixed_effects <- summary_model$fixed[grep(paste0("^mu", cat_clean, "_"), rownames(summary_model$fixed)), ]
      
      for (i in 1:nrow(fixed_effects)) {
        param_name <- rownames(fixed_effects)[i]
        predictor <- gsub(paste0("^mu", cat_clean, "_"), "", param_name)
        
        results_table <- rbind(results_table, data.frame(
          Category = cat,
          Predictor = predictor,
          Estimate = fixed_effects[i, "Estimate"],
          SE = fixed_effects[i, "Est.Error"],
          CI_Lower = fixed_effects[i, "l-95% CI"],
          CI_Upper = fixed_effects[i, "u-95% CI"],
          P_value = 2 * (1 - pnorm(abs(fixed_effects[i, "Estimate"] / fixed_effects[i, "Est.Error"]))),
          stringsAsFactors = FALSE
        ))
      }
    }
    return(results_table)
  }
}

# Extract regression tables for all four models if they don't exist
if (!exists("reg_table_multinom_1") || !exists("reg_table_multinom_int") || 
    !exists("reg_table_multinom_2") || !exists("reg_table_multinom_house_int")) {
  
  # Model 1: Base net loss model
  reg_table_multinom_1 <- extract_regression_table(model_multilevel, "Local Authority")
  
  # Model 2: Net loss with age interaction
  reg_table_multinom_int <- extract_regression_table(model_multilevel_int, "Local Authority")
  
  # Model 3: House price model
  reg_table_multinom_2 <- extract_regression_table(model_multilevel_house, "Local Authority")
  
  # Model 4: House price with age interaction
  reg_table_multinom_house_int <- extract_regression_table(model_multilevel_house_int, "Local Authority")
  
  # Format all tables
  format_table <- function(table) {
    table %>%
      mutate(
        Significance = case_when(
          P_value < 0.001 ~ "***",
          P_value < 0.01  ~ "**",
          P_value < 0.05  ~ "*",
          P_value < 0.1   ~ ".",
          TRUE ~ ""
        ),
        Estimate_formatted = sprintf("%.3f%s", Estimate, Significance),
        CI_formatted = sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper)
      ) %>%
      dplyr::select(Category, Predictor, Estimate_formatted, CI_formatted, P_value)
  }
  
  reg_table_multinom_1 <- format_table(reg_table_multinom_1)
  reg_table_multinom_int <- format_table(reg_table_multinom_int)
  reg_table_multinom_2 <- format_table(reg_table_multinom_2)
  reg_table_multinom_house_int <- format_table(reg_table_multinom_house_int)
}

# First, let's combine the four regression tables
# Create copies with model identifiers
reg_table_1 <- reg_table_multinom_1 %>%
  mutate(Model = "Model 1: Net Loss")

reg_table_2 <- reg_table_multinom_int %>%
  mutate(Model = "Model 2: Net Loss × Age")

reg_table_3 <- reg_table_multinom_2 %>%
  mutate(Model = "Model 3: House Price")

reg_table_4 <- reg_table_multinom_house_int %>%
  mutate(Model = "Model 4: House Price × Age")

# Replace predictor names for clarity in all tables
format_predictors <- function(table) {
  table %>%
    mutate(Predictor = case_when(
      Predictor == "Intercept" ~ "Intercept",
      Predictor == "net_loss_s" ~ "Net Loss (std)",
      Predictor == "average_house_price_per_sq_m_s" ~ "House Price per sq.m (std)",
      Predictor == "closed1" ~ "Closed (Yes)",
      Predictor == "children_in_care_s" ~ "Children in Care (std)",
      Predictor == "age_s" ~ "Age (std)",
      Predictor == "net_loss_s:age_s" ~ "Net Loss × Age",
      Predictor == "average_house_price_per_sq_m_s:age_s" ~ "House Price × Age",
      TRUE ~ Predictor
    ))
}

reg_table_1 <- format_predictors(reg_table_1)
reg_table_2 <- format_predictors(reg_table_2)
reg_table_3 <- format_predictors(reg_table_3)
reg_table_4 <- format_predictors(reg_table_4)

# Combine the tables
combined_table <- bind_rows(reg_table_1, reg_table_2, reg_table_3, reg_table_4)

# Create a wide-format table with models as columns
wide_table <- combined_table %>%
  select(Category, Predictor, Model, Estimate_formatted, CI_formatted) %>%
  unite("Value", Estimate_formatted, CI_formatted, sep = " ") %>%
  pivot_wider(
    names_from = Model, 
    values_from = Value,
    values_fill = " "  # Use empty space for missing values
  ) %>%
  arrange(Category, match(Predictor, c(
    "Intercept", 
    "Net Loss (std)", 
    "House Price per sq.m (std)", 
    "Age (std)",
    "Net Loss × Age", 
    "House Price × Age",
    "Closed (Yes)", 
    "Children in Care (std)"
  )))%>%
  dplyr::filter(Predictor!="Intercept",
                Predictor!="Closed (Yes)",
                Predictor!="Children in Care (std)",
  )

# Create flextable
ft <- flextable(wide_table) %>%
  set_header_labels(
    Category = "Sector Type",
    Predictor = "Predictor",
    `Model 1: Net Loss` = "Model 1",
    `Model 2: Net Loss × Age` = "Model 2",
    `Model 3: House Price` = "Model 3",
    `Model 4: House Price × Age` = "Model 4"
  )

# Add header rows for grouping
ft <- add_header_row(
  ft,
  values = c("", "", "Base Models", "Interaction Models"),
  colwidths = c(1, 1, 2, 2)
)

ft <- add_header_row(
  ft,
  values = c("", "", "Coefficient Estimates [95% CI]"),
  colwidths = c(1, 1, 4)
)

# Style the table for Word
ft <- ft %>%
  theme_booktabs() %>%
  align(j = 1:2, align = "left") %>%
  align(j = 3:6, align = "center") %>%
  fontsize(size = 9) %>%  # Smaller font for better fit in Word
  width(j = 1, width = 1.5) %>%
  width(j = 2, width = 1.75) %>%
  width(j = 3:6, width = 1.6) %>%
  bold(j = 1) %>%
  italic(j = 2) %>%
  border(i = 1, border.top = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(i = 3, border.bottom = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(border.bottom = fp_border(color = "black", width = 1), part = "body") %>%
  bg(bg = "#f2f2f2", part = "header")

# Group rows by sector type
sector_types <- unique(wide_table$Category)
for (sector in sector_types) {
  indices <- which(wide_table$Category == sector)
  if (length(indices) > 1) {
    ft <- merge_at(ft, i = indices, j = 1)
  }
}

# Set caption and footnote
ft <- set_caption(ft, 
                  caption = "Multinomial Logistic Regression Models Comparing Care Home Ownership Types (Reference: Local Authority)")

ft <- add_footer_lines(ft, 
                       values = "Note: Standardized continuous predictors. *p<0.05; **p<0.01; ***p<0.001.")

# Save directly to Word
save_as_docx(ft, path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Table3_fin.docx")

# Return the flextable object
ft






####Figure 3####


#I had to remove chain size because you can't set it to a reasonable overlap between individual and investment owned.#

fig3data <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss, average_house_price_per_sq_m_s, Local.authority) %>%
  dplyr::select(Sector_merge, net_loss,average_house_price_per_sq_m, Local.authority) %>%
  dplyr::mutate(Sector_merge = factor(Sector_merge),
                Local.authority = factor(Local.authority))

### Fit the Hierarchical Multinomial Model ###
model_multilevel_house_price <- brm(
  formula = Sector_merge ~ average_house_price_per_sq_m^2 +
    (1 | Local.authority),
  data = fig3data,
  family = categorical(),
  cores = 4,
  iter = 2000
)

### Fit the Hierarchical Multinomial Model ###
model_multilevel_need <- brm(
  formula = Sector_merge ~ net_loss^2 +
    (1 | Local.authority),
  data = fig3data,
  family = categorical(),
  cores = 4,
  iter = 2000
)


summary(model_multilevel_house_price)

summary(model_multilevel_need)

# install.packages(c("broom.mixed","dplyr","knitr"))
library(broom.mixed)   # for broom::tidy.brmsfit
library(dplyr)
library(knitr)

# 1. extract the fixed-effect estimates
tbl_price <- tidy(model_multilevel_house_price, 
                  effects = "fixed", 
                  conf.int = FALSE) %>%
  filter(term != "(Intercept)") %>%            # drop intercept if you like
  rename(estimate = estimate, se = std.error) %>%
  mutate(
    z     = estimate / se,
    p     = 2 * pnorm(-abs(z)),
    model = "House-price²"
  )

tbl_need  <- tidy(model_multilevel_need, 
                  effects = "fixed", 
                  conf.int = FALSE) %>%
  filter(term != "(Intercept)") %>%
  rename(estimate = estimate, se = std.error) %>%
  mutate(
    z     = estimate / se,
    p     = 2 * pnorm(-abs(z)),
    model = "Need²"
  )

# 2. bind, format & print
tbl_all <- bind_rows(tbl_price, tbl_need) %>%
  mutate(
    star = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    coef = sprintf("%0.3f (%0.3f)%s", estimate, se, star)
  ) %>%
  select(model, term, coef, z = z, p = p)

kable(
  tbl_all,
  col.names = c("Model", "Term", "Estimate (SE)", "z-stat", "p-value"),
  digits = c(0, 0, 3, 2, 3),
  caption = "Frequentist-style table from brms models"
)

# Load required libraries
library(tidyverse)
library(brms)
library(patchwork)
library(viridis)
library(scales)
library(ggtext)

# Create a prediction function for continuous variables
predict_sectors <- function(model, var_name, var_range) {
  # Set up grid for predictions
  newdata <- expand_grid(
    Local.authority = "average" # For population-level predictions
  )
  
  # Add the appropriate variable based on model
  if(var_name == "average_house_price_per_sq_m") {
    newdata$average_house_price_per_sq_m <- var_range
  } else if(var_name == "net_loss") {
    newdata$net_loss <- var_range
  }
  
  # Get predictions
  predictions <- predict(model, newdata = newdata, allow_new_levels = TRUE)
  
  # Organize predictions into a tidy format
  pred_df <- bind_cols(
    newdata,
    as_tibble(predictions)
  ) %>%
    pivot_longer(
      cols = starts_with("P(Y"),
      names_to = "sector",
      values_to = "probability"
    ) %>%
    mutate(
      sector = case_when(
        grepl("Y = T", sector) ~ "Third Sector",
        grepl("Y = Ind", sector) ~ "Individual Owned",
        grepl("Y = C", sector) ~ "Corporate Owned",
        grepl("Y = Inv", sector) ~ "Investment Owned",
        grepl("Y = L", sector) ~ "Local Authority"
      ),
      sector = factor(sector, levels = c("Local Authority", "Third Sector", "Individual Owned", "Corporate Owned", "Investment Owned"))
    )
  
  return(pred_df)
}

# Create ranges for predictions
house_price_range <- seq(1000, 19000, by = 1000)
need_range <- seq(-230, 50, by = 10)

# Generate predictions for house price model
house_price_preds_list <- list()
for(hp in house_price_range) {
  temp_preds <- predict_sectors(
    model_multilevel_house_price, 
    "average_house_price_per_sq_m", 
    hp
  )
  temp_preds$house_price_value <- hp
  house_price_preds_list[[as.character(hp)]] <- temp_preds
}
house_price_preds <- bind_rows(house_price_preds_list)

# Generate predictions for need model
need_preds_list <- list()
for(nd in need_range) {
  temp_preds <- predict_sectors(
    model_multilevel_need, 
    "net_loss", 
    nd
  )
  temp_preds$need_value <- nd
  need_preds_list[[as.character(nd)]] <- temp_preds
}
need_preds <- bind_rows(need_preds_list)

# Create for-profit category
house_price_preds <- house_price_preds %>%
  mutate(
    investment_owned = sector == "Investment Owned"
  )
need_preds <- need_preds %>%
  mutate(
    investment_owned = sector == "Investment Owned"
  )

# Extract just the investment owned predictions
investment_house_price <- house_price_preds %>%
  filter(sector == "Investment Owned")
investment_need <- need_preds %>%
  filter(sector == "Investment Owned")


# # Color scheme
# sector_colors <- c(
#   "Local Authority" = "#022a5e",
#   "Third Sector" = "#3B9AB2",
#   "Individual Owned" = "#E1AF00",
#   "Corporate Owned" = "#EB783A",
#   "Investment Owned" = "#F21A00"
# )
# 
# # Figure 1: Investment owned probability by house price and age
# fig1 <- ggplot(investment_house_price, 
#                aes(x = age_years, y = probability, color = factor(house_price_value))) +
#   geom_line(size = 1.2) +
#   scale_color_viridis_d(
#     name = "House Price\n(£ per sq. m)",
#     option = "plasma",
#     end = 0.9,
#     direction = -1
#   ) +
#   scale_y_continuous(
#     labels = percent_format(),
#     limits = c(0, 1)
#   ) +
#   labs(
#     title = "Probability of Investment Ownership by House Price",
#     x = "Age of Children's Home (years)",
#     y = "Probability of Investment Ownership"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(face = "bold", size = 16),
#     panel.grid.minor = element_blank(),
#     legend.position = "right",
#     axis.title = element_text(face = "bold")
#   )
# 
# # Figure 2: Investment owned probability by need and age
# fig2 <- ggplot(investment_need, 
#                aes(x = age_years, y = probability, color = factor(need_value))) +
#   geom_line(size = 1.2) +
#   scale_color_viridis_d(
#     name = "Area Need\n(Net Loss)",
#     option = "viridis",
#     end = 0.9
#   ) +
#   scale_y_continuous(
#     labels = percent_format(),
#     limits = c(0, 1)
#   ) +
#   labs(
#     title = "Probability of Investment Ownership by Area Need",
#     x = "Age of Children's Home (years)",
#     y = "Probability of Investment Ownership"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(face = "bold", size = 16),
#     panel.grid.minor = element_blank(),
#     legend.position = "right",
#     axis.title = element_text(face = "bold")
#   )
# 
# # Figure 3: Heatmap of investment owned probability by house price and age
# fig3 <- ggplot(investment_house_price, 
#                aes(x = age_years, y = house_price_value, fill = probability)) +
#   geom_tile(interpolate = TRUE) +
#   scale_fill_viridis_c(
#     name = "Probability of\nInvestment Ownership",
#     option = "magma",
#     labels = percent_format()
#   ) +
#   scale_y_continuous(
#     name = "House Price (£ per sq. m)",
#     labels = scales::comma
#   ) +
#   labs(
#     title = "Investment Owned Children's Homes Concentration",
#     subtitle = "By house price and age of establishment",
#     x = "Age of Children's Home (years)"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(face = "bold", size = 16),
#     plot.subtitle = element_text(size = 12, color = "gray40"),
#     panel.grid = element_blank(),
#     legend.position = "right",
#     axis.title = element_text(face = "bold")
#   )
# 
# # Figure 4: Heatmap of investment owned probability by need and age
# fig4 <- ggplot(investment_need, 
#                aes(x = age_years, y = need_value, fill = probability)) +
#   geom_tile(interpolate = TRUE) +
#   scale_fill_viridis_c(
#     name = "Probability of\nInvestment Ownership",
#     option = "viridis",
#     labels = percent_format()
#   ) +
#   labs(
#     title = "Investment Owned Children's Homes Concentration",
#     subtitle = "By area need and age of establishment",
#     x = "Age of Children's Home (years)",
#     y = "Area Need (Net Loss)"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(face = "bold", size = 16),
#     plot.subtitle = element_text(size = 12, color = "gray40"),
#     panel.grid = element_blank(),
#     legend.position = "right",
#     axis.title = element_text(face = "bold")
#   )

# Figure 5: Newest homes (<=5 years) probability by house price
newest_homes_house <- investment_house_price %>%
  #filter(age_years <= 5) %>%
  group_by(house_price_value) %>%
  summarize(avg_prob = mean(probability), .groups = 'drop')

fig5 <- ggplot(newest_homes_house, 
               aes(x = house_price_value, y = avg_prob)) +
  geom_point(size = 3, color = "#F21A00") +
  geom_line(size = 1.2, color = "#F21A00") +
  geom_smooth(method = "loess", se = TRUE, color = "#022a5e", fill = "#022a5e", alpha = 0.2) +
  scale_x_continuous(
    name = "House Price (£ per sq. m)",
    labels = scales::comma
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1),
    name = "Probability of Investment Ownership"
  ) +
  labs(
    title = "Probability of investment ownership for children's homes",
    subtitle = "By house price"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# Figure 6: Newest homes (<=5 years) probability by need
newest_homes_need <- investment_need %>%
  #filter(age_years <= 5) %>%
  group_by(need_value) %>%
  summarize(avg_prob = mean(probability), .groups = 'drop')

fig6 <- ggplot(newest_homes_need, 
               aes(x = need_value, y = avg_prob)) +
  geom_point(size = 3, color = "#F21A00") +
  geom_line(size = 1.2, color = "#F21A00") +
  geom_smooth(method = "loess", se = TRUE, color = "#022a5e", fill = "#022a5e", alpha = 0.2) +
  scale_x_continuous(
    name = "Area Need (Net Loss)"
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1),
    name = "Probability of Investment Ownership"
  ) +
  labs(
    title = "Probability of investment ownership for children's homes",
    subtitle = "By area need"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# # Create combined figure for publications
# # Create a summary figure that emphasizes the key findings
# # Calculate contrasts for annotations (low vs high)
# house_price_lowest <- min(house_price_range)
# house_price_highest <- max(house_price_range)
# need_lowest <- min(need_range)
# need_highest <- max(need_range)
# 
# # Get probabilities for these extremes
# hp_low_prob <- newest_homes_house$avg_prob[newest_homes_house$house_price_value == house_price_lowest]
# hp_high_prob <- newest_homes_house$avg_prob[newest_homes_house$house_price_value == house_price_highest]
# need_low_prob <- newest_homes_need$avg_prob[newest_homes_need$need_value == need_lowest]
# need_high_prob <- newest_homes_need$avg_prob[newest_homes_need$need_value == need_highest]
# 
# # Calculate percentage differences
# hp_pct_diff <- (hp_low_prob / hp_high_prob - 1) * 100
# need_pct_diff <- (need_low_prob / need_high_prob - 1) * 100

# Main publication-ready figure
main_fig_title <- "**Investment-owned children's homes: Concentration in lower-priced areas with lower need**"



# # Alternative version with heatmaps
# fig_heatmaps <- (fig3 / fig4) + 
#   plot_annotation(
#     title = main_fig_title,
#     subtitle = main_fig_subtitle,
#     caption = "Data source: Multilevel multinomial regression model of children's home ownership patterns",
#     theme = theme(
#       plot.title = element_markdown(size = 18, face = "bold"),
#       plot.subtitle = element_text(size = 14, color = "gray40"),
#       plot.caption = element_text(size = 10, color = "gray40", hjust = 0)
#     )
#   )
# 
# # Create a list of all figures for export if needed
# figure_list <- list(
#   fig1 = fig1,
#   fig2 = fig2,
#   fig3 = fig3,
#   fig4 = fig4,
#   fig5 = fig5,
#   fig6 = fig6,
#   fig_publication = fig_publication,
#   fig_heatmaps = fig_heatmaps
# )
# 
# 





# Parse dates and filter for 2012 onwards
changeplot <- mlm %>%
  # left_join(., mlm %>%
  #             dplyr::select(Local.authority, net_loss) %>%
  #             dplyr::distinct(.keep_all = T)%>%
  #             dplyr::mutate(needQuint = ntile(net_loss, 5))%>%
  #             dplyr::select(-net_loss)
  #             )%>%
  mutate(location_start = as.Date(Registration.date, format = "%d/%m/%Y")) %>%
  filter(location_start > as.Date("2014-03-01"),
         Sector_merge=="Investment owned"
         ) %>%
  mutate(month = floor_date(location_start, "month")) %>%
  group_by(month, houseQuint) %>%
  summarise(new_care_homes = n(), .groups = "drop") %>%
  tidyr::complete(
    month = seq(min(month), max(month), by = "month"),
    houseQuint = unique(houseQuint),
    fill = list(new_care_homes = 0)
  )%>%
  group_by(houseQuint) %>%
  arrange(month) %>%
  mutate(cumulative_homes = cumsum(new_care_homes),
         houseQuint = ifelse(houseQuint==5, "5 (Most expensive)",
                              ifelse(houseQuint==4, "4",
                                     ifelse(houseQuint==3,"3",
                                            ifelse(houseQuint==2, "2",
                                                   ifelse(houseQuint==1, "1 (Cheapest)"))))))

# Add a zero baseline for each quintile
zero_rows <- changeplot %>%
  group_by(houseQuint) %>%
  summarise(month = min(month) - months(1), cumulative_homes = 0, .groups = "drop") %>%
  mutate(new_care_homes = 0)

changeplot <- bind_rows(changeplot, zero_rows) %>%
  arrange(houseQuint, month)


# Create the ggplot
plot2 <- ggplot(changeplot[!is.na(changeplot$houseQuint),], aes(x = month, y = cumulative_homes, color = factor(houseQuint))) +
  geom_line(size=1.5) +
  labs(
    title = "Cumulative number of new investment-owned children homes",
    subtitle = "By house price",
    x = "Year",
    y = "Investment-owned children homes opened\n(n, cumulative)",
    color = "House Price\n(per sq metre, Quintile)"
  ) +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )



# Parse dates and filter for 2012 onwards
changeplot <- mlm %>%
  # left_join(., mlm %>%
  #             dplyr::select(Local.authority, net_loss) %>%
  #             dplyr::distinct(.keep_all = T)%>%
  #             dplyr::mutate(needQuint = ntile(net_loss, 5))%>%
  #             dplyr::select(-net_loss)
  #             )%>%
  mutate(location_start = as.Date(Registration.date, format = "%d/%m/%Y")) %>%
  filter(location_start > as.Date("2014-03-01"),
         Sector_merge=="Investment owned"
  ) %>%
  mutate(month = floor_date(location_start, "month")) %>%
  group_by(month, needQuint) %>%
  summarise(new_care_homes = n(), .groups = "drop") %>%
  tidyr::complete(
    month = seq(min(month), max(month), by = "month"),
    needQuint = unique(needQuint),
    fill = list(new_care_homes = 0)
  )%>%
  group_by(needQuint) %>%
  arrange(month) %>%
  mutate(cumulative_homes = cumsum(new_care_homes),
         needQuint = ifelse(needQuint==5, "5 (High need)",
                            ifelse(needQuint==4, "4",
                                   ifelse(needQuint==3,"3",
                                          ifelse(needQuint==2, "2",
                                                 ifelse(needQuint==1, "1 (Low need)"))))))

# Add a zero baseline for each quintile
zero_rows <- changeplot %>%
  group_by(needQuint) %>%
  summarise(month = min(month) - months(1), cumulative_homes = 0, .groups = "drop") %>%
  mutate(new_care_homes = 0)

changeplot <- bind_rows(changeplot, zero_rows) %>%
  arrange(needQuint, month)


# Create the ggplot
plot1 <- ggplot(changeplot[!is.na(changeplot$needQuint),], aes(x = month, y = cumulative_homes, color = factor(needQuint))) +
  geom_line(size=1.5) +
  labs(
    title = "Cumulative number of new investment-owned children homes",
    subtitle = "By area need",
    x = "Year",
    y = "Investment-owned children homes opened\n(n, cumulative)",
    color = "Area Need\n(Net loss, Quintile)"
  ) +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )


# Combined publication-quality figure 
fig_publication <- (fig5 + plot2) / (fig6 + plot1) + 
  plot_annotation(
    title = main_fig_title,
   # subtitle = main_fig_subtitle,
    caption = "Data source: Probabilities calculated from bivariate multilevel multinomial regression model (see Supplementary Material A18)",
    theme = theme(
      plot.title = element_markdown(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray40", hjust = 0)
    )
  )


ggsave(plot=fig_publication, filename="Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/figures/figure_3_attempt2.png", width=16.5, height=10, dpi=600)


####number of homes####
length(unique(df$URN))
length(unique(mlm[mlm$Sector_merge=="Local Authority",]$URN))
length(unique(mlm[mlm$Sector_merge=="Third sector",]$URN))
length(unique(mlm[mlm$Sector_merge=="Investment owned",]$URN))
length(unique(mlm[mlm$Sector_merge=="Individual owned",]$URN))
length(unique(mlm[mlm$Sector_merge=="Corporate owned",]$URN))

length(unique(df$URN))-(
  length(unique(mlm[mlm$Sector_merge=="Local Authority",]$URN))+
  length(unique(mlm[mlm$Sector_merge=="Third sector",]$URN))+
  length(unique(mlm[mlm$Sector_merge=="Investment owned",]$URN))+
  length(unique(mlm[mlm$Sector_merge=="Individual owned",]$URN))+
  length(unique(mlm[mlm$Sector_merge=="Corporate owned",]$URN))
  

)



####APPENDIX####

#### ICC table ####
mlm
#–– Packages
if (!require(lme4))     install.packages("lme4");     library(lme4)
if (!require(broom.mixed)) install.packages("broom.mixed"); library(broom.mixed)
if (!require(dplyr))    install.packages("dplyr");    library(dplyr)
if (!require(knitr))    install.packages("knitr");    library(knitr)

#–– Outcomes and model formulas
outcomes <- list(
  overall_avg    = "overall.average",
  ever_outstanding = "ever_outstanding",
  reqs_per       = "reqs_per",
  recs_per       = "recs_per",
  profit_margin_average = "profit_margin_average",
  left = "left",
  age_years = "age_years"
)

#–– Function to fit null model and compute ICC
compute_icc <- function(varname) {
  is_binary <- varname == "ever_outstanding"
  formula_text <- if (is_binary) {
    paste0(varname, " ~ 1 + (1 | Organisation)")
  } else {
    paste0(varname, " ~ 1 + (1 | Organisation)")
  }
  # fit
  if (is_binary) {
    m0 <- glmer(
      as.formula(formula_text),
      data = mlm,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")
    )
    # variance on latent scale for logistic: π^2/3
    var_resid <- (pi^2) / 3
  } else {
    m0 <- lmer(
      as.formula(formula_text),
      data = mlm,
      REML = TRUE
    )
    var_resid <- sigma(m0)^2
  }
  vc <- as.data.frame(VarCorr(m0))  
  var_org <- vc %>% filter(grp == "Organisation") %>% pull(vcov)
  icc <- var_org / (var_org + var_resid)
  data.frame(
    Outcome = varname,
    Var_Organisation = var_org,
    Var_Residual     = var_resid,
    ICC              = icc
  )
}

#–– Compute ICCs for all outcomes
icc_table <- bind_rows(lapply(outcomes, compute_icc)) %>%
  mutate(
    ICC = round(ICC, 3)
  )

#–– Print as a nice table for your supplement
knitr::kable(
  icc_table,
  col.names = c("Outcome", "σ²<sub>Organisation</sub>", "σ²<sub>Residual</sub>", "ICC"),
  caption = "Intraclass correlation coefficients (ICC) for each outcome, Organisation‐level clustering"
)

####spatial analysis ####

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if(length(to_install)) install.packages(to_install)
}
install_if_missing(c("sf","lwgeom","spdep","brms","dplyr","tidyr","INLA","ggplot2","scales"))
# INLA has its own repo:
if (!requireNamespace("INLA", quietly=TRUE)) {
  install.packages("INLA", repos=c(INLA="https://inla.r-inla-download.org/R/stable"))
}

library(sf)
library(lwgeom)
library(spdep)
library(brms)
library(dplyr)
library(tidyr)
library(INLA)
library(ggplot2)
library(scales)


la_shp   <-  st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2022_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")%>%
  dplyr::rename(Local.authority = CTYUA22NM)%>%
  dplyr::filter(Local.authority!="Wales",
                Local.authority!="Scotland",
                grepl('^E', CTYUA22CD))%>%
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


#–– 2. Clean invalid geometries and build adjacency for INLA
la_shp_valid <- la_shp %>%
  st_make_valid() %>%    # fix geometry errors
  st_buffer(0)           # remove tiny gaps/overlaps

# build neighbours list and INLA graph
nb <- poly2nb(la_shp_valid, row.names = la_shp_valid$Local.authority, queen = TRUE)
nb2INLA("la_adj.graph", nb)
la_graph <- inla.read.graph("la_adj.graph")

#–– 3. Aggregate to LA × Year with covariates
la_year_data <-LA_panel_data %>%
  dplyr::mutate(O = Places_Individual_owned+Places_Local_Authority+ 
                  Places_Unidentified_for_profit+Places_Third_sector+
                  Places_Investment_owned+Places_Corporate_owned+
                  Places_LA_owned_company)%>%
  dplyr::select(Local.authority, year, O, net_loss, children_in_care, average_house_price_per_sq_m )# ensure zeros

# join region‑level covariates (time‑invariant)
region_cov <- mlm %>%
  group_by(Local.authority) %>%
  summarise(
    net_loss_s                    = mean(net_loss_s,                    na.rm=TRUE),
    children_in_care_s            = mean(children_in_care_s,            na.rm=TRUE),
    average_house_price_per_sq_m_s= mean(average_house_price_per_sq_m_s,na.rm=TRUE)  )

la_data <- la_year_data %>%
  left_join(region_cov, by = "Local.authority")

#–– 4. Fit Bayesian multilevel neg‑binomial model to estimate E
#    (one row per LA‑Year, random intercept on LA)
count_priors <- c(
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "sd")
)

model_nb <- brm(
  bf(O ~ net_loss_s 
     + children_in_care_s 
     + average_house_price_per_sq_m_s 
     + (1 | Local.authority)
  ),
  data    = la_data,
  family  = negbinomial(),
  prior   = count_priors,
  cores   = 4,
  iter    = 2000,
  control = list(adapt_delta = 0.95)
)

# extract expected counts E (marginalizing LA random effect)
fitted_E <- fitted(
  model_nb,
  newdata    = la_data,
  re_formula = NA,
  scale      = "response"
)
la_data <- la_data %>%
  mutate(
    E   = fitted_E[ , "Estimate"],
    OE  = O / E
  )



#–– 5b. Remove any rows where E is NA (so INLA won’t choke on the offset)
la_data <- la_data %>% 
  tidyr::drop_na(E)

#–– 6. (re)compute the indexing for INLA on the cleaned data
la_data <- la_data %>%
  arrange(Local.authority, year) %>%
  dplyr::mutate(year = as.numeric(as.character(year)))%>%
  mutate(
    spatial  = as.integer(factor(Local.authority)),
    temporal = year - min(year) + 1,
    st_idx   = (spatial - 1) * max(temporal) + temporal
  )

#–– Before fitting, ensure no missing index values
#    (you should have already done drop_na(E) and then recomputed temporal)
la_data <- la_data %>%
  filter(!is.na(spatial) & !is.na(temporal) & !is.na(st_idx))

# get the set of unique time‐levels
time_vals <- sort(unique(la_data$temporal))

# define a clean formula object
formula_inla <- formula(
  O ~ net_loss_s
  + children_in_care_s
  + average_house_price_per_sq_m_s
  + f(spatial,  model = "bym2", graph = la_graph)
  + f(temporal, model = "rw1", values = time_vals, scale.model = TRUE)
  + f(st_idx,   model = "iid")
)

# now call inla() by naming the formula argument
res_inla <- inla(
  formula       = formula_inla,
  family        = "poisson",
  data          = la_data,
  E             = la_data$E,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE)
)




#–– 7. Summaries
# fixed effects
print(res_inla$summary.fixed, digits = 3)
# spatial hyperpars
print(res_inla$summary.hyperpar, digits = 3)

library(dplyr)
library(ggplot2)

#–– 1. Extract just the total BYM2 effect for each spatial ID
n_la <- length(unique(la_data$spatial))           # number of LAs in your INLA data
spat_summary <- res_inla$summary.random$spatial   # this has 2*n_la rows for BYM2

# the first n_la rows are the *total* effect
total_spat <- spat_summary[1:n_la, c("ID", "mean")]
colnames(total_spat) <- c("spatial", "spat_mean")

#–– 2. Ensure your shapefile has the same 'spatial' codes
la_shp_plot <- la_shp_valid %>%
  mutate(
    spatial = as.integer(factor(Local.authority)) 
    # this must match how you constructed la_data$spatial
  )

#–– 3. Left‐join the effect values
la_shp_plot <- la_shp_plot %>%
  left_join(total_spat, by = "spatial")

#–– 4. Plot (NAs will simply be blank)
ggplot(la_shp_plot) +
  geom_sf(aes(fill = spat_mean.y), colour = "grey80") +
  scale_fill_viridis_c(
    na.value = "white", 
    option   = "viridis",
    name     = "Spatial effect"
  ) +
  labs(
    title = "Posterior mean total spatial effect (BYM2)"
  ) +
  theme_minimal()


#–– 9. (Optional) plot temporal effect
temp <- res_inla$summary.random$temporal
ggplot(temp, aes(x = ID, y = mean)) +
  geom_line() +
  labs(x="Year index", y="Posterior mean", title="Temporal RW1 effect")

#–– 10. Save E and OE for reporting
write.csv(
  la_data %>% select(Local.authority, year, O, E, OE),
  "LA_yearly_OE.csv",
  row.names = FALSE
)



####O:E ration####


#–– 0. Packages  
library(dplyr)        # data wrangling  
library(tidyr)        # data wrangling  
library(brms)         # Bayesian multilevel modeling  
library(sf)           # spatial data  
library(ggplot2)      # plotting  
library(scales)       # axis formatting  


 la_shp   <-  st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2022_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")%>%
   dplyr::rename(Local.authority = CTYUA22NM)%>%
   dplyr::filter(Local.authority!="Wales",
                 Local.authority!="Scotland",
                 grepl('^E', CTYUA22CD))%>%
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

#–– 2. Aggregate to Region‑Level & Build covariates  
obs_la <- mlm %>% 
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(O = sum(Places, na.rm=T))%>%
  dplyr::ungroup()


region_data <- obs_la %>%
  left_join(
    mlm %>%
      group_by(Local.authority) %>%
      summarise(
        children_in_care_s            = mean(children_in_care_s,            na.rm=TRUE)      ),
    by = "Local.authority"
  )

#–– 3. Specify Weakly Informative Priors  
count_priors <- c(
  prior(normal(0, 1), class = "b"),            # slopes
  prior(student_t(3, 0, 2.5), class = "Intercept"),  # intercept
  prior(student_t(3, 0, 2.5), class = "sd")         # random‐effect SD
)

#–– 4. Fit Multilevel Count Models  
# 4a. Poisson  
model_poisson <- brm(
  bf(O ~ children_in_care_s 
     + (1 | Local.authority)
  ),
  data    = region_data,
  family  = poisson(),
  prior   = count_priors,
  cores   = 4,
  iter    = 2000,
  control = list(adapt_delta = 0.95)
)



# Choose best (e.g. model_negbin) for E

#–– 6. Extract Expected Counts (E) & Compute O:E  
fitted_E <- fitted(
  model_poisson,        # or model_poisson
  newdata    = region_data,
  re_formula = NA,     # marginalise over LA random effects
  scale      = "response"
)

region_data <- region_data %>%
  mutate(
    E   = fitted_E[, "Estimate"],
    O_E = O / E
  )

#–– 7. Summary Statistics of O:E  
summary_stats <- region_data %>%
  summarise(
    median_OE = median(O_E, na.rm=T),
    IQR_OE    = IQR(O_E, na.rm=T),
    min_OE    = min(O_E, na.rm=T),
    max_OE    = max(O_E, na.rm=T)
  )
print(summary_stats)

#–– 8. Choropleth Map of O:E  
map_df <- la_shp %>%
  left_join(region_data, by = "Local.authority")

p_map <- ggplot(map_df) +
  geom_sf(aes(fill = O_E), colour = "grey30", size = 0.1) +
  scale_fill_viridis_c(
    name   = "O / E ratio",
    option = "magma",
    limits = c(0, 3),
    oob    = scales::squish
  ) +
  labs(
    title   = "Observed to Expected Ratio of Children’s Home Places by LA",
    caption = "Expected from Bayesian Poisson model"
  ) +
  theme_minimal()

# print or save
print(p_map)
# ggsave("Figure2_choropleth_OE.png", p_map, width=8, height=6)

#–– 9. Funnel Plot of O:E  
region_data <- region_data %>%
  mutate(
    z95     = qnorm(0.975),
    z998    = qnorm(0.999),
    lower95  = 1 - z95  / sqrt(E),
    upper95  = 1 + z95  / sqrt(E),
    lower998 = 1 - z998 / sqrt(E),
    upper998 = 1 + z998 / sqrt(E),
    outlier  = case_when(
      O_E > upper998 ~ "High",
      O_E < lower998 ~ "Low",
      TRUE           ~ "Normal"
    )
  )

# install.packages("ggrepel")  # if not already installed
library(ggrepel)

p_funnel_labeled <- ggplot(region_data, aes(x = E, y = O_E)) +
  # funnel limits
  geom_line(aes(y = upper95),  linetype = "dashed") +
  geom_line(aes(y = lower95),  linetype = "dashed") +
  geom_line(aes(y = upper998), linetype = "solid") +
  geom_line(aes(y = lower998), linetype = "solid") +
  # points colored by outlier status
  geom_point(aes(col = outlier), size = 2) +
  # labels only for true outliers
  geom_text_repel(
    data = subset(region_data, O_E > 3 | O_E < 0.3),
    aes(label = Local.authority),
    size         = 3,
    max.overlaps = Inf,
    box.padding  = 0.3,
    point.padding= 0.2
  ) +
  # manual colours
  scale_colour_manual(
    values = c("High"   = "red",
               "Low"    = "blue",
               "Normal" = "black"),
    guide  = guide_legend(title = NULL)
  ) +
  scale_x_continuous("Expected count of homes (E)", labels = comma) +
  scale_y_continuous("Observed / Expected (O / E)", limits = c(0, NA)) +
  labs(
    title    = "Funnel Plot of LA‑level O:E Ratios",
    subtitle = "dashed = 95 % limits; solid = 99.8 % limits"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_funnel_labeled)


# print or save
print(p_funnel)
# ggsave("SupplFig_A8_funnel_OE.png", p_funnel, width=6, height=4)

#–– 10. Export Results  
# write.csv(
#   region_data %>% select(Local.authority, O, E, O_E, outlier),
#   "LA_OE_ratios.csv",
#   row.names = FALSE
# )











####prior diagnostics####



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


brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, net_loss_s, children_in_care_s,
                Local.authority, closed) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 






### Fit the Hierarchical Multinomial Model ###
model_multilevel <- brm(
  formula   = Sector_merge ~ net_loss_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  prior     = priors,
  cores     = 4,
  iter      = 2000
)

pp_check(model_multilevel, type = "bars")           # bar plot comparison for categories



####Simple MLM####

mlm$sector_third <- ifelse(mlm$Sector_merge=="Local Authority", 0,
                           ifelse(mlm$Sector_merge=="Third sector", 1, NA
                                  ))

mlm$sector_invest <- ifelse(mlm$Sector_merge=="Local Authority", 0,
                           ifelse(mlm$Sector_merge=="Investment owned", 1, NA
                           ))

mlm$sector_corp <- ifelse(mlm$Sector_merge=="Local Authority", 0,
                           ifelse(mlm$Sector_merge=="Corporate owned", 1, NA
                           ))

mlm$sector_indiv <- ifelse(mlm$Sector_merge=="Local Authority", 0,
                           ifelse(mlm$Sector_merge=="Individual owned", 1, NA
                           ))


reg1 <- lmerTest::lmer(sector_third ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg2 <- lmerTest::lmer(sector_indiv ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm)
reg3 <- lmerTest::lmer(sector_corp ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg4 <- lmerTest::lmer(sector_invest ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm)

reg5 <- lmerTest::lmer(sector_third ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg6 <- lmerTest::lmer(sector_indiv ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm)
reg7 <- lmerTest::lmer(sector_corp ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg8 <- lmerTest::lmer(sector_invest ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm)





modelsummary(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
             stars = TRUE,
             coef_omit = "Intercept",
             output = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/simple_mlm.docx")


####changing all results to net_loss in 2014####



# Function to create plot for each ownership type
create_ownership_plot <- function(data, ownership_type, title_suffix) {
  # Parse dates and filter for the specified ownership type
  changeplot <- data %>%
    dplyr::select(Registration.date, Sector_merge, Local.authority)%>%
    left_join(., data %>%
                dplyr::select(Local.authority, net_loss_2014) %>%
                dplyr::distinct(.keep_all = TRUE) %>%
                dplyr::mutate(needQuint = factor(ntile(net_loss_2014, 5))) %>%
                dplyr::select(-net_loss_2014)
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

# Combine all plots in a grid with the legend
combined_plot <- cowplot::plot_grid(
  plotlist = plots_list,
  ncol = 3,
  nrow = 2,
  labels = "AUTO"
)

# Create the final plot with the legend
final_plot <- cowplot::plot_grid(
  combined_plot,
  legend,
  ncol = 2,
  rel_widths = c(0.9, 0.1)
)

# Display the final plot
final_plot

# Save the plot if needed
ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/Figure3_2014_need.png", final_plot, width = 15, height = 10, dpi = 600)




# Function to create plot for each ownership type
create_ownership_plot <- function(data, ownership_type, title_suffix) {
  # Parse dates and filter for the specified ownership type
  changeplot <- data %>%
    left_join(., data %>%
                dplyr::select(Local.authority, house_price_2014) %>%
                dplyr::distinct(.keep_all = TRUE) %>%
                dplyr::mutate(houseQuint = factor(ntile(house_price_2014, 5))) %>%
                dplyr::select(-house_price_2014)
    ) %>%
    mutate(location_start = dmy(Registration.date)) %>%
    filter(location_start >= as.Date("2014-03-01"),
           Sector_merge == ownership_type,
           !is.na(houseQuint)
    ) %>%
    mutate(month = floor_date(location_start, "month")) %>%
    group_by(month, houseQuint) %>%
    summarise(new_care_homes = n(), .groups = "drop") %>%
    complete(
      month = seq(min(month), max(month), by = "month"),
      houseQuint = unique(houseQuint),
      fill = list(new_care_homes = 0)
    ) %>%
    group_by(houseQuint) %>%
    arrange(month) %>%
    mutate(cumulative_homes = cumsum(new_care_homes),
           houseQuint = case_when(
             houseQuint == 5 ~ "5 (High price)",
             houseQuint == 4 ~ "4",
             houseQuint == 3 ~ "3",
             houseQuint == 2 ~ "2",
             houseQuint == 1 ~ "1 (Low price)"
           ))
  
  # Add a zero baseline for each quintile
  zero_rows <- changeplot %>%
    group_by(houseQuint) %>%
    summarise(month = min(month) - months(1), cumulative_homes = 0, .groups = "drop") %>%
    mutate(new_care_homes = 0)
  
  changeplot <- bind_rows(changeplot, zero_rows) %>%
    arrange(houseQuint, month)
  
  # Create the ggplot
  plot <- ggplot(changeplot, aes(x = month, y = cumulative_homes, color = factor(houseQuint))) +
    geom_line(size = 1.5) +
    labs(
      title = paste0("Cumulative number of new children homes\n(", title_suffix, ")"),
      x = "Year",
      y = paste0(title_suffix, " children homes opened (n, cumulative)"),
      color = "House price\n(Quintile)"
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
legend_plot <- ggplot(plots_list[[1]]$data, aes(x = month, y = cumulative_homes, color = factor(houseQuint))) +
  geom_line(size = 1.5) +
  labs(color = "House price\n(Quintile)") +
  theme_bw() +
  scale_color_viridis_d(option = "viridis")

legend <- cowplot::get_legend(legend_plot)

# Combine all plots in a grid with the legend
combined_plot <- cowplot::plot_grid(
  plotlist = plots_list,
  ncol = 3,
  nrow = 2,
  labels = "AUTO"
)

# Create the final plot with the legend
final_plot <- cowplot::plot_grid(
  combined_plot,
  legend,
  ncol = 2,
  rel_widths = c(0.9, 0.1)
)

# Display the final plot
final_plot

# Save the plot if needed
ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/Figure3_2014_house.png", final_plot, width = 15, height = 10, dpi = 600)






####Map of area need and house price####





map <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2022_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")%>%
  dplyr::rename(Local.authority = CTYUA22NM)%>%
  dplyr::filter(Local.authority!="Wales",
                Local.authority!="Scotland",
                grepl('^E', CTYUA22CD))%>%
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
  dplyr::full_join(., mlm %>%
                     dplyr::select(Local.authority, net_loss_s),
                   by="Local.authority")%>%
  st_as_sf(.)




no_classes <- 6


quantiles <- quantile(as.double(map$net_loss_s), 
                      probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " - ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]




a <- map %>%  dplyr::mutate(unreg_quantiles =cut(net_loss_s, 
                                                 breaks = quantiles, 
                                                 labels = labels, 
                                                 include.lowest = T) )%>%
  ggplot(.) +
  geom_sf(aes(fill = unreg_quantiles), color = "black") +
  theme_map()+
  labs(x = NULL, 
       y = NULL)+
  scale_fill_brewer(
    palette = "OrRd",
    name = "Area need\n(net loss, average, 2014-23)", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))


ggsave(plot=a, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/map_need.jpeg", width=8, height=6, dpi=600)






map <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2022_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")%>%
  dplyr::rename(Local.authority = CTYUA22NM)%>%
  dplyr::filter(Local.authority!="Wales",
                Local.authority!="Scotland",
                grepl('^E', CTYUA22CD))%>%
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
  dplyr::full_join(., mlm %>%
                     dplyr::select(Local.authority, average_house_price_per_sq_m_s),
                   by="Local.authority")%>%
  st_as_sf(.)




no_classes <- 6


quantiles <- quantile(as.double(map$average_house_price_per_sq_m_s), 
                      probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " - ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]




a <- map %>%  dplyr::mutate(unreg_quantiles =cut(average_house_price_per_sq_m_s, 
                                                 breaks = quantiles, 
                                                 labels = labels, 
                                                 include.lowest = T) )%>%
  ggplot(.) +
  geom_sf(aes(fill = unreg_quantiles), color = "black") +
  theme_map()+
  labs(x = NULL, 
       y = NULL)+
  scale_fill_brewer(
    palette = "OrRd",
    name = "House price\n(per sq m, average, 2014-23)", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))


ggsave(plot=a, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/map_house.jpeg", width=8, height=6, dpi=600)







####care home falsification####


updata <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_Materials/Data/complete inspection and location data_ben_feb2025v2.csv")%>%
  filter(closed_complete!=1)%>%
  dplyr::distinct(locationid, .keep_all = T)%>%
  dplyr::select(locationlocalauthority, ownership)%>%
  dplyr::mutate(
                ownership = ifelse(ownership==0, "For-profit",
                                   ifelse(ownership==1, "Local Authority",
                                          ifelse(ownership==2, "Third sector", NA))))%>%
  dplyr::filter(!ownership=="")%>%
  dplyr::mutate(Local.authority = locationlocalauthority %>%
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
  left_join(., mlm %>%
              dplyr::select(Local.authority,children_in_care_s, net_loss_s, median_house_price_detached_s, Unemployment.rate...aged.16.64._s, residential_expenditure_s, claimant_count_rate_s, median_wage_s, Region.Country.name)%>%
              dplyr::distinct(.keep_all = T))%>%
  mutate(ownership = factor(ownership, levels = c("Local Authority","Third sector","For-profit")))%>%
  drop_na(ownership,Local.authority,children_in_care_s, net_loss_s, median_house_price_detached_s, Unemployment.rate...aged.16.64._s, )%>%
  dplyr::mutate(third  = ifelse(ownership=="Local Authority", 0,
                                ifelse(ownership=="Third sector", 1, NA)),
                profit  = ifelse(ownership=="Local Authority", 0,
                                ifelse(ownership=="For-profit", 1, NA)))


model_multilevel <- brm(
  formula = ownership ~ net_loss_s +  children_in_care_s +
    residential_expenditure_s + median_house_price_detached_s + median_wage_s +
    Region.Country.name + (1 | Local.authority),
  data = updata,
  family = categorical(),
  cores = 4,
  iter = 2000
)

summary(model_multilevel)


### Function to Extract Multinomial Regression Table ###
extract_regression_table <- function(model, reference_level) {
  summary_model <- summary(model)
  results_table <- data.frame(
    Category = character(),
    Predictor = character(),
    Estimate = numeric(),
    SE = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Get outcome categories (excluding the specified reference)
  categories <- levels(model$data$ownership)
  categories <- categories[categories != reference_level]
  
  for (cat in categories) {
    # Remove both spaces and hyphens from category names.
    cat_clean <- gsub("[ -]", "", cat)
    pattern <- paste0("^mu", cat_clean, "_")
    
    # Debug: print the pattern and available rownames
    message("Processing category: ", cat, " with pattern: ", pattern)
    print(rownames(summary_model$fixed))
    
    fixed_effects <- summary_model$fixed[grep(pattern, rownames(summary_model$fixed)), ]
    
    # Check if any fixed effects were found for this category
    if(nrow(fixed_effects) == 0){
      warning(paste("No fixed effects found for category:", cat))
      next
    }
    
    for (i in 1:nrow(fixed_effects)) {
      param_name <- rownames(fixed_effects)[i]
      predictor <- gsub(pattern, "", param_name)
      
      results_table <- rbind(results_table, data.frame(
        Category = cat,
        Predictor = predictor,
        Estimate = fixed_effects[i, "Estimate"],
        SE = fixed_effects[i, "Est.Error"],
        CI_Lower = fixed_effects[i, "l-95% CI"],
        CI_Upper = fixed_effects[i, "u-95% CI"],
        P_value = 2 * (1 - pnorm(abs(fixed_effects[i, "Estimate"] / fixed_effects[i, "Est.Error"]))),
        stringsAsFactors = FALSE
      ))
    }
  }
  return(results_table)
}


# Adjust the reference level as needed (here we assume "Local Authority" is the reference)
reg_table_multinom <- extract_regression_table(model_multilevel, "Local Authority")

formatted_multinom <- reg_table_multinom %>%
  mutate(
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01  ~ "**",
      P_value < 0.05  ~ "*",
      P_value < 0.1   ~ ".",
      TRUE ~ ""
    ),
    Estimate_formatted = sprintf("%.3f%s", Estimate, Significance),
    CI_formatted = sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper)
  ) %>%
  dplyr::select(Category, Predictor, Estimate_formatted, CI_formatted, P_value)




# Pivot the multinomial table so that each category becomes a column
multinom_wide <- formatted_multinom %>%
  pivot_wider(names_from = Category, values_from = c(Estimate_formatted, CI_formatted, P_value))

# Rename Predictor to term to merge by predictor name
multinom_wide <- multinom_wide %>% rename(term = Predictor)

library(dplyr)
library(stringr)
library(gt)

# Merge overall and profit margin estimates by predictor ("term")
final_table <- multinom_wide %>%
  filter(!str_detect(term, "Region"),
         term != "Intercept") %>%
  select(term, 
         `Estimate_formatted_Third sector`, `CI_formatted_Third sector`, 
         `Estimate_formatted_For-profit`, `CI_formatted_For-profit`)

# Create a prettier version of the table with clear column names and cleaned predictors
pretty_table <- final_table %>%
  rename(
    Predictor = term,
    `Third Sector (Estimate)` = `Estimate_formatted_Third sector`,
    `Third Sector (95% CI)` = `CI_formatted_Third sector`,
    `For-profit (Estimate)` = `Estimate_formatted_For-profit`,
    `For-profit (95% CI)` = `CI_formatted_For-profit`
  ) %>%
  mutate(
    Predictor = case_when(
      Predictor == "net_loss_s" ~ "Area Need (Net Loss)",
      Predictor == "children_in_care_s" ~ "Children in Care",
      Predictor == "residential_expenditure_s" ~ "Local Authority Expenditure (Residential care)",
      Predictor == "median_house_price_detached_s" ~ "Median House Price (detached)",
      Predictor == "median_wage_s" ~ "Median Wage (Hourly)",
      TRUE ~ Predictor
    )
  )

# # Calculate sample sizes for each model
# n_multinom <- nrow(brmdata_multinom)
# n_overall  <- nrow(brmdata_overall)
# n_profit   <- nrow(brmdata_profit)
# 
# # Add rows for fixed/random effects and sample sizes
# # Using NA for cells that are not applicable provides more consistent formatting.
# pretty_table <- pretty_table %>%
#   add_row(Predictor = "Regional Fixed Effects", 
#           `Third Sector (Estimate)` = "Included", 
#           `Third Sector (95% CI)` = NA,
#           `Individual Owned (Estimate)` = "Included",
#           `Individual Owned (95% CI)` = NA,
#           `Corporate Owned (Estimate)` = "Included",
#           `Corporate Owned (95% CI)` = NA,
#           `Investment Owned (Estimate)` = "Included",
#           `Investment Owned (95% CI)` = NA,
#           `Overall Rating (Estimate)` = "Included",
#           `Overall Rating (95% CI)` = NA,
#           `Profit Margin (Estimate)` = "Included",
#           `Profit Margin (95% CI)` = NA
#   ) %>%
#   add_row(Predictor = "Local Authority Random Effects", 
#           `Third Sector (Estimate)` = "Included", 
#           `Third Sector (95% CI)` = NA,
#           `Individual Owned (Estimate)` = "Included",
#           `Individual Owned (95% CI)` = NA,
#           `Corporate Owned (Estimate)` = "Included",
#           `Corporate Owned (95% CI)` = NA,
#           `Investment Owned (Estimate)` = "Included",
#           `Investment Owned (95% CI)` = NA,
#           `Overall Rating (Estimate)` = "Included",
#           `Overall Rating (95% CI)` = NA,
#           `Profit Margin (Estimate)` = "Included",
#           `Profit Margin (95% CI)` = NA
#   ) %>%
#   add_row(Predictor = "Observations (n)", 
#           `Third Sector (Estimate)` = as.character(n_multinom), 
#           `Third Sector (95% CI)` = NA,
#           `Individual Owned (Estimate)` = NA, 
#           `Individual Owned (95% CI)` = NA,
#           `Corporate Owned (Estimate)` = NA, 
#           `Corporate Owned (95% CI)` = NA,
#           `Investment Owned (Estimate)` = NA, 
#           `Investment Owned (95% CI)` = NA,
#           `Overall Rating (Estimate)` = as.character(n_overall), 
#           `Overall Rating (95% CI)` = NA,
#           `Profit Margin (Estimate)` = as.character(n_profit), 
#           `Profit Margin (95% CI)` = NA
#   )
# 
# # Combine each estimate with its 95% CI into one column per model/metric
# pretty_table <- pretty_table %>%
#   mutate(
#     `Third Sector (Estimate) [95% CI]` = paste0(`Third Sector (Estimate)`, " ", `Third Sector (95% CI)`, ""),
#     `Individual Owned (Estimate) [95% CI]` = paste0(`Individual Owned (Estimate)`, " ", `Individual Owned (95% CI)`, ""),
#     `Corporate Owned (Estimate) [95% CI]` = paste0(`Corporate Owned (Estimate)`, " ", `Corporate Owned (95% CI)`, ""),
#     `Investment Owned (Estimate) [95% CI]` = paste0(`Investment Owned (Estimate)`, " ", `Investment Owned (95% CI)`, ""),
#     `Quality (Estimate) [95% CI]` = paste0(`Overall Rating (Estimate)`, " ", `Overall Rating (95% CI)`, ""),
#     `Profit Margin (Estimate) [95% CI]` = paste0(`Profit Margin (Estimate)`, " ", `Profit Margin (95% CI)`, "")
#   ) %>%
#   dplyr::select(Predictor,
#                 `Third Sector (Estimate) [95% CI]`,
#                 `Individual Owned (Estimate) [95% CI]`,
#                 `Corporate Owned (Estimate) [95% CI]`,
#                 `Investment Owned (Estimate) [95% CI]`,
#                 `Quality (Estimate) [95% CI]`,
#                 `Profit Margin (Estimate) [95% CI]`)

# Build the gt table
regression_table <- pretty_table %>%
  gt() %>%
  tab_header(
    title = "Hierarchical Regression Models Examining Predictors of Children Home Ownership and Performance"
  ) %>%
  tab_spanner(
    label = c("Reference category for ownership type: Local Authority"),
    columns = matches("Third Sector|For-profit")
  )  %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    source_notes.font.size = px(10),
    footnotes.font.size = px(10),
    table.width = pct(100)
  )

# Save the table as HTML
regression_table %>%
  gtsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/care_home")




####other quality reg table ####
# Fit models with clustered standard errors
reg5 <- lm_robust(ever_outstanding ~ Sector_merge + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg6 <- lm_robust(ever_outstanding ~ Sector_merge + Places + number_of_inspections + age_years +factor(left)+ chain_size_0s + Local.authority, data = mlm, clusters = Organisation)
reg7 <- lm_robust(ever_outstanding ~ profit_margin_average + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg8 <- lm_robust(ever_outstanding ~ profit_margin_average + Places + number_of_inspections + age_years +factor(left)+ chain_size_0s + Local.authority, data = mlm, clusters = Organisation)
reg51 <- lm_robust(recs_per ~ Sector_merge + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg61 <- lm_robust(recs_per ~ Sector_merge + Places + number_of_inspections + age_years +factor(left)+ chain_size_0s + Local.authority, data = mlm, clusters = Organisation)
reg71 <- lm_robust(recs_per ~ profit_margin_average + number_of_inspections + Local.authority, data = mlm, clusters = Organisation)
reg81 <- lm_robust(recs_per ~ profit_margin_average + Places + number_of_inspections + age_years +factor(left)+ chain_size_0s + Local.authority, data = mlm, clusters = Organisation)

library(modelsummary)
library(kableExtra)
library(gt)

# Try using modelsummary's built-in CI formatting
table_html <- modelsummary(
  list( reg5, reg6, reg7, reg8,
        reg51, reg61, reg71, reg81),
  stars = TRUE,
  coef_map = c(
    "Sector_mergeIndividual owned" = "Ownership [Individual-owned]", 
    "Sector_mergeCorporate owned" = "Ownership [Corporate-owned]",
    "Sector_mergeInvestment owned" = "Ownership [Investment-owned]",
    "Sector_mergeThird sector" = "Ownership [Third Sector]",
    "profit_margin_average" = "Profit Margin Average (%)",
    "Places" = "Places (n)",
    "age_years" = "Age (years)",
    "chain_size_0s" = "Chain Size (n, 10s)",
    "factor(left)1" = "De-registered home (closed or taken over dummy)"
  ),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
  fmt = "%.3f",
  estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
  statistic = NULL,  # Remove separate standard error row
  conf_level = 0.95, # 95% confidence intervals
  output = "gt"
) %>%
  tab_header(
    title = "Regression Models of Inspection Ratings and Outstanding Status"
  ) %>%
  tab_spanner(
    label = "Ever Rated Outstanding",
    columns = 2:5
  )%>%
  tab_spanner(
    label = "Recommendations per inspection",
    columns = 6:9
  ) %>%
  tab_footnote(
    footnote = "Reference category for ownership is Local Authority",
    locations = cells_stub(rows = contains("Sector"))  # Apply to all ownership rows
  ) %>%
  tab_footnote(
    footnote = "* p < 0.1, ** p < 0.05, *** p < 0.01",
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "Number of inspections and Local Authority fixed effects are controlled for in all models.",
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "Standard errors are clustered at the chain level.",
    locations = cells_title()
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    source_notes.font.size = px(10),
    footnotes.font.size = px(10),
    table.width = pct(100)
  )

# Save the table as HTML
table_html %>%
  gtsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table2_quality_others.html")

####percent of beds in top 5 LAs####
LA_panel_data$all_places <- LA_panel_data$Places_Individual_owned+
  
  LA_panel_data$Places_Corporate_owned+
  
  LA_panel_data$Places_Investment_owned+
  
  LA_panel_data$Places_Third_sector+
  
  LA_panel_data$Places_Local_Authority+
  
  LA_panel_data$Places_Unidentified_for_profit+
  
  LA_panel_data$Places_LA_owned_company

LA_panel_data %>%
  dplyr::filter(year == 2023) %>%
  dplyr::distinct(Local.authority, .keep_all = TRUE) %>%
  dplyr::filter(all_places > 300) %>%
  dplyr::summarise(total_all_places = sum(all_places, na.rm = TRUE)) %>%
  dplyr::pull(total_all_places)

LA_panel_data %>%
  dplyr::filter(year == 2023) %>%
  dplyr::distinct(Local.authority, .keep_all = TRUE) %>%
  dplyr::filter(all_places < 300) %>%
  dplyr::summarise(total_all_places = sum(all_places, na.rm = TRUE)) %>%
  dplyr::pull(total_all_places)

2308/10127

LA_panel_data %>%
  dplyr::filter(year == 2023) %>%
  dplyr::distinct(Local.authority, .keep_all = TRUE) %>%
  dplyr::filter(all_places > 300) %>%
  dplyr::summarise(children_in_care = sum(children_in_care, na.rm = TRUE)) %>%
  dplyr::pull(children_in_care)

LA_panel_data %>%
  dplyr::filter(year == 2023) %>%
  dplyr::distinct(Local.authority, .keep_all = TRUE) %>%
  dplyr::filter(all_places < 300) %>%
  dplyr::summarise(children_in_care = sum(children_in_care, na.rm = TRUE)) %>%
  dplyr::pull(children_in_care)

605/6042




as.data.frame(LA_panel_data)%>%
  mutate(Local.authority = as.character(Local.authority))%>%
  filter(year == 2023) %>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(variable=="Secure homes and children's homes", category=="child characteristic at 31st March", year==2023)%>%
                     dplyr::select(LA_Name, number)%>%
                     dplyr::rename(Local.authority= LA_Name)
  )%>%
  mutate(
    children_in_care = as.numeric(number),
    ratio = children_in_care / all_places
  ) %>%
  dplyr::filter(ratio>1)

####Outcomes of profit and quality ####


### Prepare Data for the Numerical BRMS Models ###
brmdata_overall <- mlm %>%
  tidyr::drop_na(overall.average,number_of_inspections, age_s, net_loss_s, Places_s,rel_pov_per_s, children_in_care_s,
                 residential_expenditure_s, average_house_price_per_sq_m_s, median_wage_s,
                 Region.Country.name, Local.authority) %>%
  dplyr::mutate(Region.Country.name = factor(Region.Country.name),
                Local.authority = factor(Local.authority))

brmdata_profit <- mlm %>%
  tidyr::drop_na(profit_margin_average, age, net_loss_s, Places, children_in_care_s,rel_pov_per_s,
                 residential_expenditure_s, average_house_price_per_sq_m_s, median_wage_s,
                 Region.Country.name, Local.authority) %>%
  dplyr::mutate(Region.Country.name = factor(Region.Country.name),
                Local.authority = factor(Local.authority))



### Fit Hierarchical Gaussian Models for the Numerical Outcomes ###

# Model for overall.average
model_overall <- brm(
  formula = overall.average ~ net_loss_s +  children_in_care_s +closed+ average_house_price_per_sq_m_s  + (1 | Local.authority),
  data = brmdata_overall,
  family = gaussian(),
  cores = 4,
  iter = 2000
)

# Model for profit_margin_average
model_profit <- brm(
  formula = profit_margin_average ~ net_loss_s +  children_in_care_s +closed+ average_house_price_per_sq_m_s  + (1 | Local.authority),
  data = brmdata_profit,
  family = gaussian(),
  cores = 4,
  iter = 2000
)

# required packages
library(dplyr)
library(tidyr)
library(flextable)
library(broom.mixed)

# 1. Function to extract and tidy a single brms model
tidy_brm <- function(model, model_name) {
  # broom.mixed::tidy() gives Estimate, std.error, conf.low, conf.high
  tbl <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    filter(term != "sigma") %>%     # drop the residual sd
    mutate(
      # compute p-value (approximate z-test)
      p.value = 2 * (1 - pnorm(abs(estimate / std.error))),
      # significance stars
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      ),
      # formatted strings
      Estimate_fmt = sprintf("%.2f%s", estimate, sig),
      CI_fmt       = sprintf("[%.2f, %.2f]", conf.low, conf.high),
      Model        = model_name
    ) %>%
    select(Model, Predictor = term, Estimate_fmt, CI_fmt, p.value)
  return(tbl)
}

# 2. Extract tables for both models
tbl_overall <- tidy_brm(model_overall, model_name = "Overall Average")
tbl_profit  <- tidy_brm(model_profit,  model_name = "Profit Margin")

# 3. Combine and reshape to wide
combined <- bind_rows(tbl_overall, tbl_profit) %>%
  # rename predictors for clarity
  mutate(Predictor = case_when(
    Predictor == "(Intercept)"                ~ "Intercept",
    Predictor == "net_loss_s"                 ~ "Net Loss (std)",
    Predictor == "children_in_care_s"         ~ "Children in Care (std)",
    Predictor == "closed1"                    ~ "Closed (Yes)",
    Predictor == "average_house_price_per_sq_m_s" ~ "House Price (std)",
    TRUE ~ Predictor
  )) %>%
  unite("Coef [95% CI]", Estimate_fmt, CI_fmt, sep = " ") %>%
  pivot_wider(
    names_from  = Model,
    values_from = `Coef [95% CI]`,
    values_fill = "-"
  ) %>%
  # reorder predictors how you like
  mutate(order = case_when(
    Predictor == "Intercept"           ~ 1,
    Predictor == "Net Loss (std)"      ~ 2,
    Predictor == "House Price (std)"   ~ 3,
    Predictor == "Closed (Yes)"        ~ 4,
    Predictor == "Children in Care (std)" ~ 5,
    TRUE ~ 6
  )) %>%
  arrange(order) %>%
  select(-order)

# 4. Build flextable
ft <- flextable(combined) %>%
  set_header_labels(
    Predictor          = "Predictor",
    `Overall Average`  = "Overall Avg model",
    `Profit Margin`    = "Profit Margin model"
  )%>%
  theme_booktabs() %>%
  align(j = 2:3, align = "center") %>%
  bold(part = "header") %>%
  fontsize(size = 9) %>%
  width(j = 1, width = 2.5) %>%
  width(j = 2:3, width = 1.5) %>%
  set_caption("Table: Fixed‐effect estimates (with 95% CIs) from two brms models") %>%
  add_footer_lines("Note: Standardized continuous predictors; *p<0.05; **p<0.01; ***p<0.001.")

# 5. Save to Word (uncomment & adjust path)
save_as_docx(ft, path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table3_qual_prof.docx")

# print to Viewer
ft




####changing the comparison to invest firm####

mlm$sector_third <- ifelse(mlm$Sector_merge=="Investment owned", 0,
                           ifelse(mlm$Sector_merge=="Third sector", 1, NA
                           ))

mlm$sector_la <- ifelse(mlm$Sector_merge=="Local Authority", 1,
                            ifelse(mlm$Sector_merge=="Investment owned", 0, NA
                            ))

mlm$sector_corp <- ifelse(mlm$Sector_merge=="Investment owned", 0,
                          ifelse(mlm$Sector_merge=="Corporate owned", 1, NA
                          ))

mlm$sector_indiv <- ifelse(mlm$Sector_merge=="Investment owned", 0,
                           ifelse(mlm$Sector_merge=="Individual owned", 1, NA
                           ))



reg1 <- lmerTest::lmer(sector_third ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg2 <- lmerTest::lmer(sector_indiv ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm)
reg3 <- lmerTest::lmer(sector_corp ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg4 <- lmerTest::lmer(sector_la ~ net_loss_s +  children_in_care_s +  (1 | Local.authority),data = mlm)

reg5 <- lmerTest::lmer(sector_third ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg6 <- lmerTest::lmer(sector_indiv ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm)
reg7 <- lmerTest::lmer(sector_corp ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm )
reg8 <- lmerTest::lmer(sector_la ~ average_house_price_per_sq_m_s +  children_in_care_s +  (1 | Local.authority),data = mlm)





modelsummary(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
             stars = TRUE,
             coef_omit = "Intercept",
             output = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/simple_mlm_invest_as_ref.docx")



####council politics####

mlm <- mlm %>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/refs/heads/main/Data/history2016-2025.csv"))%>%
                     dplyr::mutate(Local.authority = authority %>%
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
                     dplyr::filter(year==2023)%>%
                     dplyr::select(Local.authority, majority)
  
  
  )




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


brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, net_loss_s, children_in_care_s,
                Local.authority, closed, majority) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 






### Fit the Hierarchical Multinomial Model ###
model_multilevel <- brm(
  formula   = Sector_merge ~ net_loss_s  + closed + majority+
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  prior     = priors,
  cores     = 4,
  iter      = 2000
)





brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,majority,
                Local.authority, closed) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 

### Fit the Hierarchical Multinomial Model ###
model_multilevel_house <- brm(
  formula   = Sector_merge ~ average_house_price_per_sq_m_s  + closed + majority+
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  prior     = priors,
  cores     = 4,
  iter      = 2000
)


# Create extraction function if it doesn't already exist
extract_regression_table <- function(model, reference_level) {
  summary_model <- summary(model)
  results_table <- data.frame(
    Category = character(),
    Predictor = character(),
    Estimate = numeric(),
    SE = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Get outcome categories (excluding the specified reference)
  categories <- levels(model$data$Sector_merge)
  categories <- categories[categories != reference_level]
  
  for (cat in categories) {
    cat_clean <- gsub(" ", "", cat)
    fixed_effects <- summary_model$fixed[grep(paste0("^mu", cat_clean, "_"), rownames(summary_model$fixed)), ]
    
    for (i in 1:nrow(fixed_effects)) {
      param_name <- rownames(fixed_effects)[i]
      predictor <- gsub(paste0("^mu", cat_clean, "_"), "", param_name)
      
      results_table <- rbind(results_table, data.frame(
        Category = cat,
        Predictor = predictor,
        Estimate = fixed_effects[i, "Estimate"],
        SE = fixed_effects[i, "Est.Error"],
        CI_Lower = fixed_effects[i, "l-95% CI"],
        CI_Upper = fixed_effects[i, "u-95% CI"],
        P_value = 2 * (1 - pnorm(abs(fixed_effects[i, "Estimate"] / fixed_effects[i, "Est.Error"]))),
        stringsAsFactors = FALSE
      ))
    }
  }
  return(results_table)
}

# Extract regression tables for both models
reg_table_netLoss <- extract_regression_table(model_multilevel, "Local Authority")
reg_table_housePrice <- extract_regression_table(model_multilevel_house, "Local Authority")

# Format tables function
format_table <- function(table) {
  table %>%
    mutate(
      Significance = case_when(
        P_value < 0.001 ~ "***",
        P_value < 0.01  ~ "**",
        P_value < 0.05  ~ "*",
        P_value < 0.1   ~ ".",
        TRUE ~ ""
      ),
      Estimate_formatted = sprintf("%.3f%s", Estimate, Significance),
      CI_formatted = sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper)
    ) %>%
    dplyr::select(Category, Predictor, Estimate_formatted, CI_formatted, P_value)
}

# Apply formatting
reg_table_netLoss <- format_table(reg_table_netLoss)
reg_table_housePrice <- format_table(reg_table_housePrice)

# Add model identifiers
reg_table_1 <- reg_table_netLoss %>%
  mutate(Model = "Model 1: Net Loss")

reg_table_2 <- reg_table_housePrice %>%
  mutate(Model = "Model 2: House Price")

# Format predictor names for clarity
format_predictors <- function(table) {
  table %>%
    mutate(Predictor = case_when(
      Predictor == "Intercept" ~ "Intercept",
      Predictor == "net_loss_s" ~ "Net Loss (std)",
      Predictor == "average_house_price_per_sq_m_s" ~ "House Price per sq.m (std)",
      Predictor == "closed1" ~ "Closed (Yes)",
      Predictor == "children_in_care_s" ~ "Children in Care (std)",
      Predictor == "majorityConplurality" ~ "Political Control: Conservative plurality",
      Predictor == "majorityLab" ~ "Political Control: Labour majority",
      Predictor == "majorityLabplurality" ~ "Political Control: Labour plurality",
      Predictor == "majorityLD" ~ "Political Control: Liberal Democrat majority",
      Predictor == "majorityLDplurality" ~ "Political Control: Liberal Democrat plurality",
      Predictor == "majorityOther" ~ "Political Control: Other majority",
      Predictor == "majorityOtherplurality" ~ "Political Control: Other plurality",
      TRUE ~ Predictor
    ))
}

reg_table_1 <- format_predictors(reg_table_1)
reg_table_2 <- format_predictors(reg_table_2)

# Combine the tables
combined_table <- bind_rows(reg_table_1, reg_table_2)

# Create a wide-format table with models as columns
wide_table <- combined_table %>%
  select(Category, Predictor, Model, Estimate_formatted, CI_formatted) %>%
  unite("Value", Estimate_formatted, CI_formatted, sep = " ") %>%
  pivot_wider(
    names_from = Model, 
    values_from = Value,
    values_fill = " "  # Use empty space for missing values
  ) %>%
  arrange(Category, case_when(
    grepl("^Intercept", Predictor) ~ 1,
    grepl("^Net Loss", Predictor) ~ 2,
    grepl("^House Price", Predictor) ~ 3,
    grepl("^Closed", Predictor) ~ 4,
    grepl("^Children in Care", Predictor) ~ 5,
    grepl("^Political Control:", Predictor) ~ 6,
    TRUE ~ 7
  ), Predictor)

# Create flextable
ft <- flextable(wide_table) %>%
  set_header_labels(
    Category = "Sector Type",
    Predictor = "Predictor",
    `Model 1: Net Loss` = "Model 1: Net Loss",
    `Model 2: House Price` = "Model 2: House Price"
  )

# Add header rows for grouping
ft <- add_header_row(
  ft,
  values = c("", "", "Coefficient Estimates [95% CI]"),
  colwidths = c(1, 1, 2)
)

# Filter out political control rows if they don't have significant results
# This is optional - you might want to keep all variables regardless of significance
# wide_table_filtered <- wide_table %>%
#   filter(!(grepl("^Political Control:", Predictor) & 
#            !grepl("\\*", `Model 1: Net Loss`) & 
#            !grepl("\\*", `Model 2: House Price`)))

# Style the table for Word
ft <- ft %>%
  theme_booktabs() %>%
  align(j = 1:2, align = "left") %>%
  align(j = 3:4, align = "center") %>%
  fontsize(size = 9) %>%  # Smaller font for better fit in Word
  width(j = 1, width = 1.5) %>%
  width(j = 2, width = 2.5) %>%  # Wider to accommodate longer political control names
  width(j = 3:4, width = 1.6) %>%
  bold(j = 1) %>%
  italic(j = 2) %>%
  border(i = 1, border.top = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(i = 2, border.bottom = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(border.bottom = fp_border(color = "black", width = 1), part = "body") %>%
  bg(bg = "#f2f2f2", part = "header") %>%
  # Conditional formatting for rows containing "Political Control:" to be slightly indented
  padding(i = which(grepl("^Political Control:", wide_table$Predictor)), padding.left = 15, j = 2)


# Set caption and footnote
ft <- set_caption(ft, 
                  caption = "Multinomial Logistic Regression Models Comparing Care Home Ownership Types (Reference: Local Authority)")

ft <- add_footer_lines(ft, 
                       values = "Note: Standardized continuous predictors. Political control is included as control variable. *p<0.05; **p<0.01; ***p<0.001.")

# Save directly to Word
save_as_docx(ft, path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table3_council_cont.docx")


####non informative priors####



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


brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, net_loss_s, children_in_care_s,
                Local.authority, closed) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 






### Fit the Hierarchical Multinomial Model ###
model_multilevel <- brm(
  formula   = Sector_merge ~ net_loss_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  cores     = 4,
  iter      = 2000
)



brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, net_loss_s, children_in_care_s,
                Local.authority, closed, age_years) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed),
    age_s = scale(age_years))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 


### Fit the Hierarchical Multinomial Model ###
model_multilevel_int <- brm(
  formula   = Sector_merge ~ net_loss_s*age_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  cores     = 4,
  iter      = 2000
)






brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,
                Local.authority, closed) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 

### Fit the Hierarchical Multinomial Model ###
model_multilevel_house <- brm(
  formula   = Sector_merge ~ average_house_price_per_sq_m_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  cores     = 4,
  iter      = 2000
)

brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,  Local.authority) %>%
  dplyr::select(Sector_merge, average_house_price_per_sq_m_s, children_in_care_s,
                Local.authority, closed, age_years) %>%
  dplyr::mutate(
    Sector_merge = factor(Sector_merge),
    Local.authority = factor(Local.authority),
    closed = factor(closed),
    age_s = scale(age_years))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 

### Fit the Hierarchical Multinomial Model ###
model_multilevel_house_int <- brm(
  formula   = Sector_merge ~ average_house_price_per_sq_m_s*age_s  + closed + 
    children_in_care_s   +
    (1 | Local.authority),
  data      = brmdata_multinom,
  family    = categorical(),
  cores     = 4,
  iter      = 2000
)





library(dplyr)
library(kableExtra)
library(tidyr)

# Create extraction function if it doesn't already exist
if (!exists("extract_regression_table")) {
  extract_regression_table <- function(model, reference_level) {
    summary_model <- summary(model)
    results_table <- data.frame(
      Category = character(),
      Predictor = character(),
      Estimate = numeric(),
      SE = numeric(),
      CI_Lower = numeric(),
      CI_Upper = numeric(),
      P_value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Get outcome categories (excluding the specified reference)
    categories <- levels(model$data$Sector_merge)
    categories <- categories[categories != reference_level]
    
    for (cat in categories) {
      cat_clean <- gsub(" ", "", cat)
      fixed_effects <- summary_model$fixed[grep(paste0("^mu", cat_clean, "_"), rownames(summary_model$fixed)), ]
      
      for (i in 1:nrow(fixed_effects)) {
        param_name <- rownames(fixed_effects)[i]
        predictor <- gsub(paste0("^mu", cat_clean, "_"), "", param_name)
        
        results_table <- rbind(results_table, data.frame(
          Category = cat,
          Predictor = predictor,
          Estimate = fixed_effects[i, "Estimate"],
          SE = fixed_effects[i, "Est.Error"],
          CI_Lower = fixed_effects[i, "l-95% CI"],
          CI_Upper = fixed_effects[i, "u-95% CI"],
          P_value = 2 * (1 - pnorm(abs(fixed_effects[i, "Estimate"] / fixed_effects[i, "Est.Error"]))),
          stringsAsFactors = FALSE
        ))
      }
    }
    return(results_table)
  }
}

# Extract regression tables for all four models if they don't exist
if (!exists("reg_table_multinom_1") || !exists("reg_table_multinom_int") || 
    !exists("reg_table_multinom_2") || !exists("reg_table_multinom_house_int")) {
  
  # Model 1: Base net loss model
  reg_table_multinom_1 <- extract_regression_table(model_multilevel, "Local Authority")
  
  # Model 2: Net loss with age interaction
  reg_table_multinom_int <- extract_regression_table(model_multilevel_int, "Local Authority")
  
  # Model 3: House price model
  reg_table_multinom_2 <- extract_regression_table(model_multilevel_house, "Local Authority")
  
  # Model 4: House price with age interaction
  reg_table_multinom_house_int <- extract_regression_table(model_multilevel_house_int, "Local Authority")
  
  # Format all tables
  format_table <- function(table) {
    table %>%
      mutate(
        Significance = case_when(
          P_value < 0.001 ~ "***",
          P_value < 0.01  ~ "**",
          P_value < 0.05  ~ "*",
          P_value < 0.1   ~ ".",
          TRUE ~ ""
        ),
        Estimate_formatted = sprintf("%.3f%s", Estimate, Significance),
        CI_formatted = sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper)
      ) %>%
      dplyr::select(Category, Predictor, Estimate_formatted, CI_formatted, P_value)
  }
  
  reg_table_multinom_1 <- format_table(reg_table_multinom_1)
  reg_table_multinom_int <- format_table(reg_table_multinom_int)
  reg_table_multinom_2 <- format_table(reg_table_multinom_2)
  reg_table_multinom_house_int <- format_table(reg_table_multinom_house_int)
}

# First, let's combine the four regression tables
# Create copies with model identifiers
reg_table_1 <- reg_table_multinom_1 %>%
  mutate(Model = "Model 1: Net Loss")

reg_table_2 <- reg_table_multinom_int %>%
  mutate(Model = "Model 2: Net Loss × Age")

reg_table_3 <- reg_table_multinom_2 %>%
  mutate(Model = "Model 3: House Price")

reg_table_4 <- reg_table_multinom_house_int %>%
  mutate(Model = "Model 4: House Price × Age")

# Replace predictor names for clarity in all tables
format_predictors <- function(table) {
  table %>%
    mutate(Predictor = case_when(
      Predictor == "Intercept" ~ "Intercept",
      Predictor == "net_loss_s" ~ "Net Loss (std)",
      Predictor == "average_house_price_per_sq_m_s" ~ "House Price per sq.m (std)",
      Predictor == "closed1" ~ "Closed (Yes)",
      Predictor == "children_in_care_s" ~ "Children in Care (std)",
      Predictor == "age_s" ~ "Age (std)",
      Predictor == "net_loss_s:age_s" ~ "Net Loss × Age",
      Predictor == "average_house_price_per_sq_m_s:age_s" ~ "House Price × Age",
      TRUE ~ Predictor
    ))
}

reg_table_1 <- format_predictors(reg_table_1)
reg_table_2 <- format_predictors(reg_table_2)
reg_table_3 <- format_predictors(reg_table_3)
reg_table_4 <- format_predictors(reg_table_4)

# Combine the tables
combined_table <- bind_rows(reg_table_1, reg_table_2, reg_table_3, reg_table_4)

# Create a wide-format table with models as columns
wide_table <- combined_table %>%
  select(Category, Predictor, Model, Estimate_formatted, CI_formatted) %>%
  unite("Value", Estimate_formatted, CI_formatted, sep = " ") %>%
  pivot_wider(
    names_from = Model, 
    values_from = Value,
    values_fill = " "  # Use empty space for missing values
  ) %>%
  arrange(Category, match(Predictor, c(
    "Intercept", 
    "Net Loss (std)", 
    "House Price per sq.m (std)", 
    "Age (std)",
    "Net Loss × Age", 
    "House Price × Age",
    "Closed (Yes)", 
    "Children in Care (std)"
  )))%>%
  dplyr::filter(Predictor!="Intercept",
                Predictor!="Closed (Yes)",
                Predictor!="Children in Care (std)",
  )

# Create flextable
ft <- flextable(wide_table) %>%
  set_header_labels(
    Category = "Sector Type",
    Predictor = "Predictor",
    `Model 1: Net Loss` = "Model 1",
    `Model 2: Net Loss × Age` = "Model 2",
    `Model 3: House Price` = "Model 3",
    `Model 4: House Price × Age` = "Model 4"
  )

# Add header rows for grouping
ft <- add_header_row(
  ft,
  values = c("", "", "Base Models", "Interaction Models"),
  colwidths = c(1, 1, 2, 2)
)

ft <- add_header_row(
  ft,
  values = c("", "", "Coefficient Estimates [95% CI]"),
  colwidths = c(1, 1, 4)
)

# Style the table for Word
ft <- ft %>%
  theme_booktabs() %>%
  align(j = 1:2, align = "left") %>%
  align(j = 3:6, align = "center") %>%
  fontsize(size = 9) %>%  # Smaller font for better fit in Word
  width(j = 1, width = 1.5) %>%
  width(j = 2, width = 1.75) %>%
  width(j = 3:6, width = 1.6) %>%
  bold(j = 1) %>%
  italic(j = 2) %>%
  border(i = 1, border.top = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(i = 3, border.bottom = fp_border(color = "black", width = 1.5), part = "header") %>%
  border(border.bottom = fp_border(color = "black", width = 1), part = "body") %>%
  bg(bg = "#f2f2f2", part = "header")

# Group rows by sector type
sector_types <- unique(wide_table$Category)
for (sector in sector_types) {
  indices <- which(wide_table$Category == sector)
  if (length(indices) > 1) {
    ft <- merge_at(ft, i = indices, j = 1)
  }
}

# Set caption and footnote
ft <- set_caption(ft, 
                  caption = "Multinomial Logistic Regression Models Comparing Care Home Ownership Types (Reference: Local Authority)")

ft <- add_footer_lines(ft, 
                       values = "Note: Standardized continuous predictors. *p<0.05; **p<0.01; ***p<0.001.")

# Save directly to Word
save_as_docx(ft, path = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Appendix/Table3_no_priors.docx")

# Return the flextable object
ft

####all owenership cats####



# Create a full sequence of times
all_times <- tibble(time = -238:0)

# Process the start dataset
carehomesstart <- df %>%
  mutate(
    Registration.date = as.Date(Registration.date, format = "%d/%m/%Y"),
    Join = as.Date(Join),
    location_start = coalesce(Registration.date, Join, as.Date("2014-03-15")),
    date = location_start,
    time = as.integer(time_length(difftime(date, as.Date("2023-12-01")), "months"))
  ) %>%
  distinct(URN, .keep_all = TRUE) %>%
  dplyr::select(time, Sector_merge, URN)

# Count observations by group
nobsByIdih <- carehomesstart %>%
  count(time, Sector_merge, name = "nobs")

# Ensure all time periods exist for each Sector_merge
nobsBySector <- nobsByIdih %>%
  complete(Sector_merge, time = all_times$time, fill = list(nobs = 0)) %>%
  group_by(Sector_merge) %>%
  mutate(cumulative = cumsum(nobs)) %>%
  ungroup()

# Process the end dataset
carehomesend <- df %>%
  filter(Leave != "") %>%
  mutate(
    date = as.Date(Leave),
    time = as.integer(time_length(difftime(date, as.Date("2023-12-01")), "months"))
  ) %>%
  distinct(URN, .keep_all = TRUE) %>%
  dplyr::select(time, Sector_merge, URN)

# Count observations by group
nobsEndByIdih <- carehomesend %>%
  count(time, Sector_merge, name = "nobs")

# Ensure all time periods exist for each Sector_merge
nobsEndBySector <- nobsEndByIdih %>%
  complete(Sector_merge, time = all_times$time, fill = list(nobs = 0)) %>%
  group_by(Sector_merge) %>%
  mutate(cumulative_end = cumsum(nobs)) %>%
  ungroup()

# Merge start and end datasets and compute the running total
nobser <- full_join(nobsBySector, nobsEndBySector, by = c("Sector_merge", "time")) %>%
  mutate(
    cumulative = replace_na(cumulative, 0),
    cumulative_end = replace_na(cumulative_end, 0),
    runningsum = cumulative - cumulative_end
  )

# Create the time-series plot
d <- ggplot(nobser %>% filter(time > -155),
            aes(x = time, y = runningsum, group = Sector_merge,
                fill = Sector_merge, colour = Sector_merge, alpha = Sector_merge)) +
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("#008F5D", "#F0AB00", "#E4007C", "#FF5E5E", "#5B0000", "#1F77B4", "#9467BD")) +
  scale_alpha_manual(values = c(0.2, 0.2, 1, 1, 1, 0.2, 0.2)) +
  labs(x = "Year", y = "Number of children homes", 
       title = "Number of children's homes owned for-profit",
       fill = "Ownership", color = "Ownership", alpha = "Ownership") +
  scale_x_continuous(breaks = c(-12, -24, -36, -48, -60, -72, -84, -96, -108, -120, -132, -144, -156),
                     labels = c("2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11, face = "bold")  ) +
  coord_cartesian(xlim = c(-110, -22)) +
  theme(legend.position = "bottom")


  ggsave(plot=d, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new//Care-Markets/figures/Appendix/all_cats.png", width=8, height=6, dpi=600)

  
####missing data analysis####
  # Required libraries
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  
  # Calculate missing counts and proportions
  missing_df <- mlm %>%
    select(Sector_merge, Places, age_years, overall.average, net_loss_s, chain_size, closed,
           average_house_price_per_sq_m_s, reqs_per, profit_margin_average, children_in_care_s) %>%
    summarise(across(everything(), ~ sum(is.na(.)), .names = "miss_{col}")) %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "n_missing") %>%
    mutate(
      variable = sub("^miss_", "", variable),
      total_n = nrow(mlm),
      p_missing = n_missing / total_n
    ) %>%
    arrange(desc(n_missing)) %>%
    mutate(variable = fct_inorder(variable))
  
  # Plot
  ggplot(missing_df, aes(x = n_missing, y = variable)) +
    geom_col(fill = "grey50", width = 0.6) +
    geom_text(aes(label = n_missing),
              hjust = -0.1, size = 4, family = "Helvetica") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(
      x = "Number of Missing Values",
      y = NULL,
      title = "Missing Data per Variable",
      subtitle = paste0("N = ", nrow(mlm), " observations"),
      caption = "Data source: mlm dataset"
    ) +
    theme_classic(base_size = 14, base_family = "Helvetica") +
    theme(
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 13),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 9),
      panel.grid.major.x = element_line(color = "grey80", linetype = "dashed")
    )
  
  

  ####tables for figu 3####
  
  
  # Load required packages
  library(tidyverse)
  library(broom.mixed)
  library(knitr)
  library(kableExtra)
  
  # Extract coefficient tables from the Bayesian models
  # House price model coefficients
  house_price_coefs <- fixef(model_multilevel_house_price) %>%
    as.data.frame() %>%
    rownames_to_column("parameter") %>%
    # Ensure nice formatting for parameter names
    mutate(parameter = gsub("mu", "", parameter)) %>%
    mutate(parameter = gsub("_", " ", parameter)) %>%
    # Add significance stars
    mutate(sig = case_when(
      `Q2.5` > 0 | `Q97.5` < 0 ~ "*",
      TRUE ~ ""
    ))
  
  # Need (net loss) model coefficients
  need_coefs <- fixef(model_multilevel_need) %>%
    as.data.frame() %>%
    rownames_to_column("parameter") %>%
    # Ensure nice formatting for parameter names
    mutate(parameter = gsub("mu", "", parameter)) %>%
    mutate(parameter = gsub("_", " ", parameter)) %>%
    # Add significance stars
    mutate(sig = case_when(
      `Q2.5` > 0 | `Q97.5` < 0 ~ "*",
      TRUE ~ ""
    ))
  
  # Create formatted table for House Price model
  house_price_table <- house_price_coefs %>%
    mutate(
      estimate_fmt = sprintf("%.4f%s", Estimate, sig),
      std_error = sprintf("(%.4f)", Est.Error)
    ) %>%
    select(parameter, estimate_fmt, std_error) %>%
    pivot_wider(
      names_from = parameter,
      values_from = c(estimate_fmt, std_error),
      names_sep = "_"
    )
  
  # Create formatted table for Need model
  need_table <- need_coefs %>%
    mutate(
      estimate_fmt = sprintf("%.4f%s", Estimate, sig),
      std_error = sprintf("(%.4f)", Est.Error)
    ) %>%
    select(parameter, estimate_fmt, std_error) %>%
    pivot_wider(
      names_from = parameter,
      values_from = c(estimate_fmt, std_error),
      names_sep = "_"
    )
  
  # Print nicely formatted tables
  cat("Table 1: House Price Model Results\n")
  house_price_coefs %>%
    mutate(
      Estimate = sprintf("%.4f%s", Estimate, sig),
      `Std. Error` = sprintf("%.4f", Est.Error),
      `Lower 95% CI` = sprintf("%.4f", Q2.5),
      `Upper 95% CI` = sprintf("%.4f", Q97.5)
    ) %>%
    select(parameter, Estimate, `Std. Error`, `Lower 95% CI`, `Upper 95% CI`) %>%
    kable(format = "simple", align = "lrrrr")
  
  cat("\nTable 2: Need (Net Loss) Model Results\n")
  need_coefs %>%
    mutate(
      Estimate = sprintf("%.4f%s", Estimate, sig),
      `Std. Error` = sprintf("%.4f", Est.Error),
      `Lower 95% CI` = sprintf("%.4f", Q2.5),
      `Upper 95% CI` = sprintf("%.4f", Q97.5)
    ) %>%
    select(parameter, Estimate, `Std. Error`, `Lower 95% CI`, `Upper 95% CI`) %>%
    kable(format = "simple", align = "lrrrr")
  
  # Alternative visualization: sector-specific effects
  # Extract just the coefficients for the predictors (not intercepts)
  house_price_effects <- house_price_coefs %>%
    filter(str_detect(parameter, "average house price per sq m")) %>%
    mutate(sector = str_extract(parameter, "^[^_]+"))
  
  need_effects <- need_coefs %>%
    filter(str_detect(parameter, "net loss")) %>%
    mutate(sector = str_extract(parameter, "^[^_]+"))
  
  # Create a simple dot plot of coefficients with error bars
  ggplot(house_price_effects, aes(x = sector, y = Estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = "Effect of House Price on Sector",
      x = "Sector",
      y = "Coefficient Estimate"
    ) +
    theme_minimal() +
    coord_flip()
  
  ggplot(need_effects, aes(x = sector, y = Estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = "Effect of Net Loss on Sector",
      x = "Sector",
      y = "Coefficient Estimate"
    ) +
    theme_minimal() +
    coord_flip()
  
  # If you want more traditional regression tables with standard frequentist logic
  # We can create simplified tables that mimic lm() output
  # Extract and format key coefficients
  
  # Function to format coefficient table in a traditional way
  format_coef_table <- function(model_summary, model_name) {
    coefs <- model_summary %>%
      filter(!str_detect(parameter, "Intercept")) %>%
      mutate(
        significance = case_when(
          abs(Q2.5) > 0 & Q2.5 * Q97.5 > 0 ~ "*",
          TRUE ~ ""
        ),
        formatted_estimate = sprintf("%.4f%s", Estimate, significance),
        formatted_se = sprintf("(%.4f)", Est.Error)
      ) %>%
      select(parameter, formatted_estimate, formatted_se)
    
    # Add intercepts as separate rows
    intercepts <- model_summary %>%
      filter(str_detect(parameter, "Intercept")) %>%
      mutate(
        significance = case_when(
          abs(Q2.5) > 0 & Q2.5 * Q97.5 > 0 ~ "*",
          TRUE ~ ""
        ),
        formatted_estimate = sprintf("%.4f%s", Estimate, significance),
        formatted_se = sprintf("(%.4f)", Est.Error)
      ) %>%
      select(parameter, formatted_estimate, formatted_se)
    
    # Combine for final table
    data.frame(
      Model = model_name,
      Parameter = c("Intercepts", intercepts$parameter, "Coefficients", coefs$parameter),
      Estimate = c("", intercepts$formatted_estimate, "", coefs$formatted_estimate),
      `Std.Error` = c("", intercepts$formatted_se, "", coefs$formatted_se)
    )
  }
  
  # Create traditional-style regression tables
  house_price_table_trad <- format_coef_table(house_price_coefs, "House Price Model")
  need_table_trad <- format_coef_table(need_coefs, "Net Loss Model")
  
  # Print the tables
  cat("\nTraditional Style Regression Tables\n")
  cat("\nHouse Price Model\n")
  house_price_table_trad %>%
    kable(format = "simple", align = "lllr")
  
  cat("\nNet Loss Model\n")
  need_table_trad %>%
    kable(format = "simple", align = "lllr")
  
  # Note: * indicates significance at 95% confidence level
  cat("\nNote: * indicates significance at 95% confidence level (CI doesn't cross zero)")
  
  
  # For the House Price Model
  house_price_table_html <- house_price_coefs %>%
    mutate(
      Estimate = sprintf("%.4f%s", Estimate, sig),
      `Std. Error` = sprintf("%.4f", Est.Error),
      `Lower 95% CI` = sprintf("%.4f", Q2.5),
      `Upper 95% CI` = sprintf("%.4f", Q97.5)
    ) %>%
    select(parameter, Estimate, `Std. Error`, `Lower 95% CI`, `Upper 95% CI`) %>%
    kable(format = "html", align = "lrrrr", caption = "House Price Model Results") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                  full_width = FALSE)
  
  # Save to HTML file
  writeLines(house_price_table_html, "house_price_model_table.html")
  
  # For the Net Loss Model
  need_table_html <- need_coefs %>%
    mutate(
      Estimate = sprintf("%.4f%s", Estimate, sig),
      `Std. Error` = sprintf("%.4f", Est.Error),
      `Lower 95% CI` = sprintf("%.4f", Q2.5),
      `Upper 95% CI` = sprintf("%.4f", Q97.5)
    ) %>%
    select(parameter, Estimate, `Std. Error`, `Lower 95% CI`, `Upper 95% CI`) %>%
    kable(format = "html", align = "lrrrr", caption = "Net Loss Model Results") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                  full_width = FALSE)
  
  # Save to HTML file
  writeLines(need_table_html, "net_loss_model_table.html")
  
  # For traditional style tables (combined in one file)
  combined_tables_html <- bind_rows(
    house_price_table_trad %>% mutate(Model = "House Price Model"),
    data.frame(Model = "", Parameter = "", Estimate = "", Std.Error = ""),  # Empty row as separator
    need_table_trad %>% mutate(Model = "Net Loss Model")
  ) %>%
    kable(format = "html", align = "llrr", caption = "Combined Regression Tables") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                  full_width = FALSE) %>%
    add_footnote("* indicates significance at 95% confidence level (CI doesn't cross zero)")
  
  # Save to HTML file
  writeLines(combined_tables_html, "combined_regression_tables.html") 
  

####to delete no evidence#####



# Load necessary libraries
library(lmerTest)
library(dplyr)
library(ggplot2)
library(purrr)
library(broom.mixed)  # For tidying mixed-effects models

# Define the outcome variables
outcomes <- c("sector_third", "sector_indiv", "sector_corp", 
              "sector_invest", "overall.average", "profit_margin_average")

# Define the potential control variables (to be varied)
controls <- c("Places", "age_years", "closed", "chain_size_0s", 
              "children_in_care_s", "residential_expenditure_s", 
               "median_wage_s", 
              "Region.Country.name")

# A list to collect results from all specifications
results <- list()

# Loop over each outcome variable
for (outcome in outcomes) {
  
  # For each possible number of controls (from 0 to all available)
  for (k in 0:length(controls)) {
    
    # Get all combinations of controls of size k 
    ctrl_combinations <- if(k == 0) list(character(0)) else combn(controls, k, simplify = FALSE)
    
    # Loop over each specific combination of controls
    for (ctrl_set in ctrl_combinations) {
      
      # Always include 'net_loss_s' and add the current set of controls
      fixed_effects <- if(length(ctrl_set) > 0) {
        paste(c("median_house_price_detached_s", ctrl_set), collapse = " + ")
      } else {
        "median_house_price_detached_s"
      }
      
      # Construct the model formula: outcome ~ net_loss_s + ... + (1|Local.authority)
      formula_str <- paste(outcome, "~", fixed_effects, "+ (1 | Local.authority)")
      formula_model <- as.formula(formula_str)
      
      # Fit the model using lmer (wrap in try() in case of convergence or data issues)
      fit <- try(lmer(formula_model, data = mlm), silent = TRUE)
      if (inherits(fit, "try-error")) next
      
      # Extract the net_loss_s coefficient details using broom.mixed::tidy with confidence intervals
      tidy_fit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
      net_loss_coef <- tidy_fit %>% filter(term == "median_house_price_detached_s")
      
      # If confidence intervals are missing, compute them using a normal approximation
      if(nrow(net_loss_coef) > 0) {
        if (!("conf.low" %in% names(net_loss_coef)) ||
            !("conf.high" %in% names(net_loss_coef))) {
          net_loss_coef <- net_loss_coef %>%
            mutate(conf.low = estimate - 1.96 * std.error,
                   conf.high = estimate + 1.96 * std.error)
        }
        results[[length(results) + 1]] <- data.frame(
          outcome      = outcome,
          controls     = if(length(ctrl_set) == 0) "None" else paste(ctrl_set, collapse = ", "),
          num_controls = length(ctrl_set),
          estimate     = net_loss_coef$estimate,
          std.error    = net_loss_coef$std.error,
          conf.low     = net_loss_coef$conf.low,
          conf.high    = net_loss_coef$conf.high
        )
      }
    }
  }
}

# Combine the list into one data frame if at least one model was successful
if(length(results) > 0) {
  results_df <- do.call(rbind, results)
} else {
  stop("No model specifications were successfully fit.")
}

results_df$outcome <- factor(ifelse(results_df$outcome=="sector_third", "Third sector",
                                    ifelse(results_df$outcome=="sector_indiv", "Individual-owned",
                                           ifelse(results_df$outcome=="sector_corp", "Corporate-owned",
                                                  ifelse(results_df$outcome=="sector_invest", "Investment-owned",
                                                         ifelse(results_df$outcome=="overall.average", "Quality score",
                                                                ifelse(results_df$outcome=="profit_margin_average", "Profit Margin (%)", NA)))))))

# Create a specification curve plot:
y <- ggplot(results_df, aes(x = factor(num_controls), y = estimate)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, alpha = 0.3) +
  facet_wrap(~ outcome, scales = "free_y") +
  geom_hline(yintercept = 0)+
  labs(x = "Number of Controls Included",
       y = "Coefficient Estimate for Area Need (net loss)",
       title = "Specification Curve: Effect of Area Need, varying controls") +
  theme_bw()
ggsave(plot=y, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/specification_curve.jpeg", width=8, height=8, dpi=600)








# Load necessary libraries
library(lmerTest)
library(dplyr)
library(ggplot2)
library(purrr)
library(broom.mixed)  # For tidying mixed-effects models

# Define the outcome variables
outcomes <- c("sector_third", "sector_indiv", "sector_corp", 
              "sector_invest", "overall.average", "profit_margin_average")

# Define the potential control variables (to be varied)
controls <- c("Places", "age_years", "closed", "chain_size_0s", 
              "children_in_care_s", "residential_expenditure_s", 
              "average_house_price_per_sq_m_s", "median_wage_s", 
              "Region.Country.name")

# A list to collect results from all specifications
results <- list()

# Loop over each outcome variable
for (outcome in outcomes) {
  
  # For each possible number of controls (from 0 to all available)
  for (k in 0:length(controls)) {
    
    # Get all combinations of controls of size k 
    ctrl_combinations <- if(k == 0) list(character(0)) else combn(controls, k, simplify = FALSE)
    
    # Loop over each specific combination of controls
    for (ctrl_set in ctrl_combinations) {
      
      # Always include 'net_loss_s' and add the current set of controls
      fixed_effects <- if(length(ctrl_set) > 0) {
        paste(c("net_loss_s", ctrl_set), collapse = " + ")
      } else {
        "net_loss_s"
      }
      
      # Construct the model formula: outcome ~ net_loss_s + ... + (1|Local.authority)
      formula_str <- paste(outcome, "~", fixed_effects, "+ (1 | Local.authority)")
      formula_model <- as.formula(formula_str)
      
      # Fit the model using lmer (wrap in try() in case of convergence or data issues)
      fit <- try(lmer(formula_model, data = mlm), silent = TRUE)
      if (inherits(fit, "try-error")) next
      
      # Extract the net_loss_s coefficient details using broom.mixed::tidy with confidence intervals
      tidy_fit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
      net_loss_coef <- tidy_fit %>% filter(term == "net_loss_s")
      
      # If confidence intervals are missing, compute them using a normal approximation
      if(nrow(net_loss_coef) > 0) {
        if (!("conf.low" %in% names(net_loss_coef)) ||
            !("conf.high" %in% names(net_loss_coef))) {
          net_loss_coef <- net_loss_coef %>%
            mutate(conf.low = estimate - 1.96 * std.error,
                   conf.high = estimate + 1.96 * std.error)
        }
        results[[length(results) + 1]] <- data.frame(
          outcome      = outcome,
          controls     = if(length(ctrl_set) == 0) "None" else paste(ctrl_set, collapse = ", "),
          num_controls = length(ctrl_set),
          estimate     = net_loss_coef$estimate,
          std.error    = net_loss_coef$std.error,
          conf.low     = net_loss_coef$conf.low,
          conf.high    = net_loss_coef$conf.high
        )
      }
    }
  }
}

# Combine the list into one data frame if at least one model was successful
if(length(results) > 0) {
  results_df <- do.call(rbind, results)
} else {
  stop("No model specifications were successfully fit.")
}

results_df$outcome <- factor(ifelse(results_df$outcome=="sector_third", "Third sector",
                                    ifelse(results_df$outcome=="sector_indiv", "Individual-owned",
                                           ifelse(results_df$outcome=="sector_corp", "Corporate-owned",
                                                  ifelse(results_df$outcome=="sector_invest", "Investment-owned",
                                                         ifelse(results_df$outcome=="overall.average", "Quality score",
                                                                ifelse(results_df$outcome=="profit_margin_average", "Profit Margin (%)", NA)))))))

# Create a specification curve plot:
y <- ggplot(results_df, aes(x = factor(num_controls), y = estimate)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, alpha = 0.3) +
  facet_wrap(~ outcome, scales = "free_y") +
  geom_hline(yintercept = 0)+
  labs(x = "Number of Controls Included",
       y = "Coefficient Estimate for Area Need (net loss)",
       title = "Specification Curve: Effect of Area Need, varying controls") +
  theme_bw()
ggsave(plot=y, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/Appendix/specification_curve.jpeg", width=8, height=8, dpi=600)







nbm <- as.data.frame(LA_panel_data) %>%
  dplyr::mutate(needQuint = factor(ntile(net_loss, 5),
                                   levels=c(5,4,3,2,1)),
                needQuint = ifelse(needQuint==1, "1 (Low need)",
                                   ifelse(needQuint==2, "2",
                                          ifelse(needQuint==3,"3",
                                                 ifelse(needQuint==4, "4",
                                                        ifelse(needQuint==5, "5 (High need)", NA))))),
                needQuint = factor(needQuint,
                                   levels = c("5 (High need)", "4", "3", "2", "1 (Low need)")))



nbm$net_loss <- as.numeric(nbm$net_loss_lagged)
nbm$median_wage <- as.numeric(nbm$median_wage)
nbm$median_house_price_detached <- as.numeric(nbm$median_house_price_detached)
nbm$occupancy_rate_fte <- as.numeric(nbm$occupancy_rate_fte)
nbm$Unemployment.rate...aged.16.64. <- as.numeric(nbm$Unemployment.rate...aged.16.64.)
nbm$children_in_care <- as.numeric(as.numeric(nbm$out_of_area_per)*as.numeric(nbm$out_of_area))
nbm$claimant_count_rate <- as.numeric(nbm$claimant_count_rate)
nbm$white <- as.numeric(nbm$X..of.white...aged.16.)
nbm$residential_expenditure <- as.numeric(nbm$residential_expenditure)


# Create a scaled version in the data frame
nbm$net_loss_s <- scale(nbm$net_loss)
nbm$median_wage_s <- scale(nbm$median_wage)
nbm$median_house_price_detached_s <- scale(nbm$median_house_price_detached)
nbm$occupancy_rate_fte_s <- scale(nbm$occupancy_rate_fte)
nbm$Unemployment.rate...aged.16.64._s <- scale(nbm$Unemployment.rate...aged.16.64.)
nbm$claimant_count_rate_s <- scale(nbm$claimant_count_rate)
nbm$children_in_care_s <- scale(nbm$children_in_care)
nbm$white_s <- scale(nbm$white)
nbm$residential_expenditure_s <- scale(nbm$residential_expenditure)


nbm$non_invest = scale(as.numeric(nbm$All_homes-nbm$Homes_Investment_owned))
nbm$non_corp = scale(as.numeric(nbm$All_homes-nbm$Homes_Corporate_owned))
nbm$non_indiv = scale(as.numeric(nbm$All_homes-nbm$Homes_Individual_owned))
nbm$non_la = scale(as.numeric(nbm$All_homes-nbm$Homes_Local_Authority))
nbm$non_3rd = scale(as.numeric(nbm$All_homes-nbm$Homes_Third_sector))

reg1 <- plm(residential_expenditure_s ~  net_loss_s  + median_house_price_detached_s +median_wage_s +Unemployment.rate...aged.16.64._s + white_s+ occupancy_rate_fte_s+claimant_count_rate_s + children_in_care_s  +  Region.Country.name, 
            index = c("Local.authority", "year"), data = nbm%>%
              dplyr::filter(Region.Country.name!="London"), model = "pooling")
coef_test(reg1, vcov = "CR2", cluster = nbm%>%
            dplyr::filter(Region.Country.name!="London")$Local.authority, test = "Satterthwaite")

nbm_filtered <- nbm %>% dplyr::filter(Region.Country.name != "London")

# Run the PLM model
reg1 <- plm(residential_expenditure_s ~ net_loss_s + median_house_price_detached_s + 
              median_wage_s + Unemployment.rate...aged.16.64._s + white_s + 
              occupancy_rate_fte_s + claimant_count_rate_s + children_in_care_s + 
              Region.Country.name, 
            index = c("Local.authority", "year"), 
            data = nbm_filtered, 
            model = "pooling")

# Run coefficient test with clustering
coef_test(reg1, vcov = "CR2", cluster = nbm_filtered$Local.authority, test = "Satterthwaite")

reg1 <- plm(as.numeric(unregulated) ~  net_loss_s+ as.numeric(Asylum_per)  + median_house_price_detached_s +median_wage_s +Unemployment.rate...aged.16.64._s + white_s+ occupancy_rate_fte_s+claimant_count_rate_s + children_in_care_s  +  Region.Country.name, 
            index = c("Local.authority", "year"), data = nbm, model = "pooling")
coef_test(reg1, vcov = "CR2", cluster = nbm$Local.authority, test = "Satterthwaite")






# Check its length
length(nbm$net_loss_s)  # Should match nrow(nbm)

rownames(nbm) <- NULL


library(dplyr)
library(plm)
library(sjPlot)
library(clubSandwich)
library(cowplot)

# Prepare the panel data
nbm <- nbm %>% distinct(.keep_all = TRUE)

# Load required libraries if not already loaded
# library(plm)
# library(dplyr)
# library(sjPlot)
# library(lmtest)
# library(sandwich)
# library(clubSandwich)  # For coef_test function

# Create a named vector for variable labels
var_labels <- c(
  "needQuint" = "Area need\n(net loss)",
  "median_wage_s" = "Median Wage\n(£, hourly)",
  "median_house_price_detached_s" = "Median House Price\n(Detached)",
  "Unemployment.rate...aged.16.64._s" = "Unemployment Rate\n(16-64, %)",
  "occupancy_rate_fte_s" = "Social Worker Occupancy Rate\n(FTE, %)",
  "white_s" = "White Population\n(%)",
  "claimant_count_rate_s" = "Claimant Count Rate\n(%)",
  "children_in_care_s" = "Children in Residential Care"
)

# List of variables to include in models
vars_to_include <- c("needQuint", "median_wage_s", "median_house_price_detached_s", 
                     "Unemployment.rate...aged.16.64._s", "occupancy_rate_fte_s", 
                     "white_s", "claimant_count_rate_s", "children_in_care_s")

# Set up the panel data
nbm <- pdata.frame(nbm %>% distinct(.keep_all = TRUE), 
                   index = c("Local.authority", "year"))

add_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  if (p_value < 0.1) return(".")
  return("")
}

# Function to create model and plot with clustered standard errors
create_model_plot <- function(dependent_var, title) {
  # Run the model
  model_formula <- as.formula(paste(
    dependent_var, "~", 
    paste(vars_to_include, collapse = " + "), 
    "+ factor(Region.Country.name)"
  ))
  
  model <- lm(model_formula, data = nbm)
  
  # Compute clustered standard errors
  clustered <- coef_test(model, vcov = "CR2", 
                         cluster = nbm$Local.authority, 
                         test = "Satterthwaite")
  
  # Extract coefficients and SEs from clustered results
  # Convert the clustered results to a data frame
  coefs_data <- data.frame(
    term = clustered$Coef,
    estimate = clustered$beta,
    std.error = clustered$SE,
    satt.freedom = clustered$df_Satt,
    statistic = clustered$tstat,
    p.value = clustered$p_Satt,
    stringsAsFactors = FALSE
  )
  
  
  # Set significance level (for a 95% confidence interval)
  alpha <- 0.05
  
  # Compute the lower and upper bounds of the confidence interval for each coefficient
  coefs_data$conf.low <- coefs_data$estimate - qt(1 - alpha/2, df = coefs_data$satt.freedom) * coefs_data$std.error
  coefs_data$conf.high <- coefs_data$estimate + qt(1 - alpha/2, df = coefs_data$satt.freedom) * coefs_data$std.error
  
  # Add significance stars
  coefs_data$stars <- sapply(coefs_data$p.value, add_significance_stars)
  
  # Format estimates for display (2 decimal places)
  coefs_data$est_label <- paste0(format(round(coefs_data$estimate, 2), nsmall = 2), coefs_data$stars)
  
  # Filter to only include the variables we want to plot
  coefs_filtered <- coefs_data[coefs_data$term %in% vars_to_include, ]
  
  # Create factor with levels in desired order for plotting
  coefs_filtered$term <- factor(coefs_filtered$term, levels = rev(vars_to_include))
  
  plot <- ggplot(coefs_filtered, aes(x = term, y = estimate, 
                                     ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    coord_flip() +
    scale_x_discrete(labels = var_labels[as.character(coefs_filtered$term)]) +
    # Add coefficient values with stars
    geom_text(aes(label = est_label), vjust = -0.7, hjust = ifelse(coefs_filtered$estimate > 0, -0.1, 1.1), size = 3) +
    labs(
      title = title,
      x = "",
      y = "Coefficient"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  
  return(list(model = model, clustered = clustered, plot = plot))
}


# Create all models and plots
models <- list(
  all = create_model_plot("all_change", "All Homes"),
  ind = create_model_plot("ind_change", "Individual Owned"),
  corp = create_model_plot("corp_change", "Corporate Owned"),
  invest = create_model_plot("invest_change", "Investment Owned"),
  la = create_model_plot("la_change", "Local Authority"),
  third = create_model_plot("third_change", "Third Sector")
)

# Extract just the plots for the grid
plots <- list(
  models$all$plot,
  models$la$plot,
  models$third$plot,
  models$ind$plot,
  models$corp$plot,
  models$invest$plot
)

# Create the combined plot grid
combined_plot <- cowplot::plot_grid(plotlist = plots, ncol = 3)

# Display the combined plot
combined_plot

# To access the clustered standard errors results for any model:
# models$all$clustered
# models$ind$clustered
# etc.


ggsave(plot=combined_plot, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_2.jpeg", width=14, height=7, dpi=600)









corr <- LA_panel_data %>% dplyr::select(Homes_Individual_owned, Homes_Corporate_owned, Homes_Investment_owned, Homes_Third_sector, Homes_Local_Authority,
                                        median_wage, out_of_area_per,  net_loss, vacancy_rate_fte, average_house_price_per_sq_m, children_in_care, residential_expenditure, 
                                        Asylum_per, unregulated, claimant_count_rate, Unemployment.rate...aged.16.64.)%>%
  dplyr::distinct(.keep_all = T)%>%
  mutate(across(everything(), as.numeric))%>%
  dplyr::mutate(unregulated_per = unregulated/children_in_care)%>%
  dplyr::select(-unregulated)



# Calculate the correlation matrix
cor_matrix <- cor(corr, use = "complete.obs")

# Create a more attractive visualization using corrplot
par(mar = c(1, 1, 2, 1))  # Adjust margins


# Set up a high-resolution image file
png("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/correlation_matrix_full.png", width = 5000, height = 5000, res = 500)

# Set the plotting parameters for a full-size plot
par(mar = c(2, 2, 2, 2), # Minimal margins
    oma = c(0, 0, 0, 0), # No outer margins
    mfrow = c(1, 1))     # Single plot

# Generate the correlation plot with enhanced visual features
corrplot(cor_matrix,
         method = "color",      # Color-coded squares
         type = "upper",        # Upper triangular matrix
         order = "hclust",      # Hierarchical clustering to group similar variables
         tl.col = "black",      # Text label color
         tl.srt = 45,           # Text label rotation
         addCoef.col = "black", # Add correlation coefficients to the plot
         number.cex = 0.7,      # Size of correlation coefficients
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), # Custom color palette
         diag = FALSE,          # Hide the diagonal
         mar = c(0, 0, 1, 0),   # Minimal margins
         tl.cex = 1.2)          # Text label size increased

# Close the device to save the image
dev.off()







corr <- mlm %>% dplyr::select(age_years, Places, left, profit_margin_average, ebitda_margin_average, salaries_turnover_average, overall.average, ever_inadequate, ever_outstanding, chain_size_0s)%>%
  dplyr::distinct(.keep_all = T)%>%
  mutate(across(everything(), as.numeric))



# Calculate the correlation matrix
cor_matrix <- cor(corr, use = "complete.obs")

# Create a more attractive visualization using corrplot
par(mar = c(1, 1, 2, 1))  # Adjust margins


# Set up a high-resolution image file
png("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/figures/correlation_matrix_full_home_level.png", width = 5000, height = 5000, res = 500)

# Set the plotting parameters for a full-size plot
par(mar = c(2, 2, 2, 2), # Minimal margins
    oma = c(0, 0, 0, 0), # No outer margins
    mfrow = c(1, 1))     # Single plot

# Generate the correlation plot with enhanced visual features
corrplot(cor_matrix,
         method = "color",      # Color-coded squares
         type = "upper",        # Upper triangular matrix
         order = "hclust",      # Hierarchical clustering to group similar variables
         tl.col = "black",      # Text label color
         tl.srt = 45,           # Text label rotation
         addCoef.col = "black", # Add correlation coefficients to the plot
         number.cex = 0.7,      # Size of correlation coefficients
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), # Custom color palette
         diag = FALSE,          # Hide the diagonal
         mar = c(0, 0, 1, 0),   # Minimal margins
         tl.cex = 1.2)          # Text label size increased

# Close the device to save the image
dev.off()
