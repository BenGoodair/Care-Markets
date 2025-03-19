



if (!require("pacman")) install.packages("pacman")

pacman::p_load(MCMCglmm,ordinal,devtools,MASS,  sandwich,  lmtest,  clubSandwich,  
               modelsummary, cowplot,ggmap,googleway,hrbrthemes,viridis,jsonlite,survival, 
               httr, purrr, dplyr,gt, gtsummary, tidyverse,rattle,ggeffects, glmnet,caret, 
               rpart.plot,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, 
               stringr, patchwork,knitr,  sf, clubSandwich, modelsummary, sjPlot,
               lmerTest, lme4, brms, glmmTMB, ggeffects, MCMCglmm, tidybayes, bayesplot,
               gridExtra, nnet, clubSandwich, fixest, patchwork, gtExtras)


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
                              str_detect(Provision.type, "(?i)day"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2016.csv"), skip=1)%>%
              dplyr::mutate(year=2016,
                            Organisation = NA,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2017.csv"), skip=1)%>%
              dplyr::mutate(year=2017,
                            Organisation = NA,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018.csv"), skip=1)%>%
              dplyr::mutate(year=2018,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018_part2.csv"))%>%
              dplyr::mutate(year=2018,
                            Registration.date = NA,
                            Overall.experiences.and.progress.of.children.and.young.people = NA,
                            Latest.full.inspection.date = NA)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.name)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2019.csv"), skip=1)%>%
              dplyr::mutate(year=2019)%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2020.csv"), skip=1)%>%
              dplyr::mutate(year=2020)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2021.csv"), skip=1)%>%
              dplyr::mutate(year=2021)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2022_yep.csv"), skip=4)%>%
              dplyr::mutate(year=2022)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Overall.experiences.and.progress.of.children.and.young.people =    Latest.full.inspection.overall.experiences.and.progress.of.children.and.young.people)%>%
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2023.csv"), skip=3)%>%
              dplyr::mutate(year=2023)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
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
  dplyr::bind_rows(., data.frame(Organisation_fame_search = "BEACON CHILDCARE LTD", Company.name = "BEACON CHILDCARE LTD", stringsAsFactors = FALSE))

#### François categories ####

francois <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_1_basic_companies.csv")%>%
#francois <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_1_basic_companies_fin.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name),
         Registered.number = ifelse(Registered.number == "", NA, Registered.number),
         Latest.accounts.date = ifelse(Latest.accounts.date == "", NA, Latest.accounts.date),
         Entity.type = ifelse(Entity.type == "", NA, Entity.type),
         No.of.companies.in.corporate.group = ifelse(No.of.companies.in.corporate.group == "", NA, No.of.companies.in.corporate.group),
         Operating.revenue..Turnover..th.GBP.Last.avail..yr = ifelse(Operating.revenue..Turnover..th.GBP.Last.avail..yr == "", NA, Operating.revenue..Turnover..th.GBP.Last.avail..yr),
         Number.of.employees.Last.avail..yr = ifelse(Number.of.employees.Last.avail..yr == "", NA, Number.of.employees.Last.avail..yr)) %>%
  fill(Company.name, .direction = "down")%>%
  group_by(Company.name)%>%
  fill(Registered.number, .direction = "down")%>%
  fill(Latest.accounts.date, .direction = "down")%>%
  fill(Entity.type, .direction = "down")%>%
  fill(No.of.companies.in.corporate.group, .direction = "down")%>%
  fill(Operating.revenue..Turnover..th.GBP.Last.avail..yr, .direction = "down")%>%
  fill(Number.of.employees.Last.avail..yr, .direction = "down")%>%
  fill(Primary.UK.SIC..2007..code, .direction = "down")%>%
  ungroup()
  
PSC <- francois %>%
  dplyr::select(Registered.number, PSC...Address, PSC...Nature.of.control, PSC...Name, PSC...Type)%>%
  mutate(PSC...Address = ifelse(PSC...Address == "", NA, PSC...Address),
         PSC...Name = ifelse(PSC...Name == "", NA, PSC...Name),
         PSC...Type = ifelse(PSC...Type == "", NA, PSC...Type))%>%
  dplyr::group_by(Registered.number)%>%
  fill(PSC...Address, .direction = "down")%>%
  fill(PSC...Name, .direction = "down")%>%
  fill(PSC...Type, .direction = "down")

francois <- francois %>%
  dplyr::select(-X, -PSC...Address, -PSC...Nature.of.control, -PSC...Name, -PSC...Type, -PSC...Country.of.residence)%>%
  distinct(.keep_all = T)%>%
  dplyr::full_join(., PSC ,by="Registered.number")%>%
  distinct(.keep_all = T)



francois_2 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_2_shareholders_fixed.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name),
         Registered.number = ifelse(Registered.number == "", NA, Registered.number),
         BvD.ID.number = ifelse(BvD.ID.number == "", NA, BvD.ID.number)) %>%
  fill(Company.name, .direction = "down")%>%
  group_by(Company.name)%>%
  fill(Registered.number, .direction = "down")%>%
  fill(BvD.ID.number, .direction = "down")%>%
  ungroup()

PSC <- francois_2 %>%
  dplyr::select(Registered.number, PSC...Nature.of.control, PSC...Name, PSC...Type)%>%
  mutate(PSC...Name = ifelse(PSC...Name == "", NA, PSC...Name),
         PSC...Type = ifelse(PSC...Type == "", NA, PSC...Type))%>%
  dplyr::group_by(Registered.number)%>%
  fill(PSC...Name, .direction = "down")%>%
  fill(PSC...Type, .direction = "down")%>%
  ungroup()

francois_2 <- francois_2 %>%
  dplyr::select(-X, -PSC...Nature.of.control, -PSC...Name, -PSC...Type, -PSC...Country.of.residence)%>%
  distinct(.keep_all = T)%>%
  dplyr::full_join(., PSC ,by="Registered.number")%>%
  distinct(.keep_all = T)

francois_2 <- francois_2 %>%
  dplyr::rename(shareholder_name = Company.name,
                SH...BvD.ID.number = BvD.ID.number,
                shareholder_Registered.number = Registered.number,
                shareholder_SH...Name = SH...Name,
                shareholder_SH...BvD.ID.number = SH...BvD.ID.number,
                shareholder_SH...Country.ISO.code = SH...Country.ISO.code,
                shareholder_SH...City = SH...City,
                shareholder_SH...Type = SH...Type,
                shareholder_SH...NACE.Core.code = SH...NACE.Core.code,
                shareholder_SH...NACE.text.description = SH...NACE.text.description,
                shareholder_SH...Total.. = SH...Total..,
                shareholder_SH...Direct.. = SH...Direct..,
                shareholder_GUO...Name = GUO...Name,
                shareholder_GUO...BvD.ID.number = GUO...BvD.ID.number,
                shareholder_GUO...Country.ISO.code = GUO...Country.ISO.code,
                shareholder_GUO...City = GUO...City,
                shareholder_GUO...Type = GUO...Type,
                shareholder_GUO...NACE.Core.code = GUO...NACE.Core.code,
                shareholder_GUO...NACE.text.description = GUO...NACE.text.description,
                shareholder_GUO...Direct.. = GUO...Direct..,
                shareholder_GUO...Total.. = GUO...Total..,
                shareholder_DUO...Name = DUO...Name,
                shareholder_DUO...BvD.ID.number = DUO...BvD.ID.number,
                shareholder_DUO...Country.ISO.code = DUO...Country.ISO.code,
                shareholder_DUO...City = DUO...City,
                shareholder_DUO...Type = DUO...Type,
                shareholder_DUO...NACE.Core.code = DUO...NACE.Core.code,
                shareholder_DUO...NACE.text.description = DUO...NACE.text.description,
                shareholder_DUO...Direct.. = DUO...Direct..,
                shareholder_DUO...Total.. = DUO...Total..,
                shareholder_PSC...Nature.of.control = PSC...Nature.of.control,
                shareholder_PSC...Name = PSC...Name,
                shareholder_PSC...Type = PSC...Type,
                shareholder_CSH...Name = CSH...Name,
                shareholder_CSH...BvD.ID.number = CSH...BvD.ID.number,
                shareholder_CSH...Country.ISO.code = CSH...Country.ISO.code,
                shareholder_CSH...City = CSH...City,
                shareholder_CSH...Type = CSH...Type,
                shareholder_CSH...NACE.Core.code = CSH...NACE.Core.code,
                shareholder_CSH...NACE.Core.code = CSH...NACE.Core.code
  )

francois <- left_join(francois, francois_2, by="SH...BvD.ID.number")



####


francois_3 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_3_controlling_shareholders_fixed.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name),
         Registered.number = ifelse(Registered.number == "", NA, Registered.number),
         BvD.ID.number = ifelse(BvD.ID.number == "", NA, BvD.ID.number)) %>%
  fill(Company.name, .direction = "down")%>%
  group_by(Company.name)%>%
  fill(Registered.number, .direction = "down")%>%
  fill(BvD.ID.number, .direction = "down")%>%
  ungroup()

PSC <- francois_3 %>%
  dplyr::select(Registered.number, PSC...Nature.of.control, PSC...Name, PSC...Type)%>%
  mutate(PSC...Name = ifelse(PSC...Name == "", NA, PSC...Name),
         PSC...Type = ifelse(PSC...Type == "", NA, PSC...Type))%>%
  dplyr::group_by(Registered.number)%>%
  fill(PSC...Name, .direction = "down")%>%
  fill(PSC...Type, .direction = "down")%>%
  ungroup()

francois_3 <- francois_3 %>%
  dplyr::select(-X, -PSC...Nature.of.control, -PSC...Name, -PSC...Type, -PSC...Country.of.residence)%>%
  distinct(.keep_all = T)%>%
  dplyr::full_join(., PSC ,by="Registered.number")%>%
  distinct(.keep_all = T)

francois_3 <- francois_3 %>%
  dplyr::rename(controlling_shareholder_name = Company.name,
                CSH...BvD.ID.number = BvD.ID.number,
                controlling_shareholder_Registered.number = Registered.number,
                controlling_shareholder_SH...Name = SH...Name,
                controlling_shareholder_SH...BvD.ID.number = SH...BvD.ID.number,
                controlling_shareholder_SH...Country.ISO.code = SH...Country.ISO.code,
                controlling_shareholder_SH...City = SH...City,
                controlling_shareholder_SH...Type = SH...Type,
                controlling_shareholder_SH...NACE.Core.code = SH...NACE.Core.code,
                controlling_shareholder_SH...NACE.text.description = SH...NACE.text.description,
                controlling_shareholder_SH...Total.. = SH...Total..,
                controlling_shareholder_SH...Direct.. = SH...Direct..,
                controlling_shareholder_GUO...Name = GUO...Name,
                controlling_shareholder_GUO...BvD.ID.number = GUO...BvD.ID.number,
                controlling_shareholder_GUO...Country.ISO.code = GUO...Country.ISO.code,
                controlling_shareholder_GUO...City = GUO...City,
                controlling_shareholder_GUO...Type = GUO...Type,
                controlling_shareholder_GUO...NACE.Core.code = GUO...NACE.Core.code,
                controlling_shareholder_GUO...NACE.text.description = GUO...NACE.text.description,
                controlling_shareholder_GUO...Direct.. = GUO...Direct..,
                controlling_shareholder_GUO...Total.. = GUO...Total..,
                controlling_shareholder_DUO...Name = DUO...Name,
                controlling_shareholder_DUO...BvD.ID.number = DUO...BvD.ID.number,
                controlling_shareholder_DUO...Country.ISO.code = DUO...Country.ISO.code,
                controlling_shareholder_DUO...City = DUO...City,
                controlling_shareholder_DUO...Type = DUO...Type,
                controlling_shareholder_DUO...NACE.Core.code = DUO...NACE.Core.code,
                controlling_shareholder_DUO...NACE.text.description = DUO...NACE.text.description,
                controlling_shareholder_DUO...Direct.. = DUO...Direct..,
                controlling_shareholder_DUO...Total.. = DUO...Total..,
                controlling_shareholder_PSC...Nature.of.control = PSC...Nature.of.control,
                controlling_shareholder_PSC...Name = PSC...Name,
                controlling_shareholder_PSC...Type = PSC...Type,
                controlling_shareholder_CSH...Name = CSH...Name,
                controlling_shareholder_CSH...BvD.ID.number = CSH...BvD.ID.number,
                controlling_shareholder_CSH...Country.ISO.code = CSH...Country.ISO.code,
                controlling_shareholder_CSH...City = CSH...City,
                controlling_shareholder_CSH...Type = CSH...Type,
                controlling_shareholder_CSH...NACE.Core.code = CSH...NACE.Core.code,
                controlling_shareholder_CSH...NACE.Core.code = CSH...NACE.Core.code
  )

francois <- left_join(francois, francois_3, by="CSH...BvD.ID.number")



######


gc()



francois_4 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_4_persons_significant_control.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name),
         Registered.number = ifelse(Registered.number == "", NA, Registered.number),
         BvD.ID.number = ifelse(BvD.ID.number == "", NA, BvD.ID.number)) %>%
  fill(Company.name, .direction = "down")%>%
  group_by(Company.name)%>%
  fill(Registered.number, .direction = "down")%>%
  fill(BvD.ID.number, .direction = "down")%>%
  ungroup()%>%
  dplyr::left_join(.,read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/psc_nace_lookup.csv")%>%
                     dplyr::rename(Company.name = PSC_search_term)%>%
                     dplyr::mutate(Company.name = toupper(gsub("Selected", "", Company.name))),
                   by="Company.name")%>%
  dplyr::left_join(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_9_PSC_nace.csv")%>%
                     dplyr::rename(PSC_result = Company.name)%>%
                     dplyr::select(PSC_result, Primary.NACE.Rev..2.code),
                   by="PSC_result"
                     )

PSC <- francois_4 %>%
  dplyr::select(Registered.number, PSC...Nature.of.control, PSC...Name, PSC...Type)%>%
  mutate(PSC...Name = ifelse(PSC...Name == "", NA, PSC...Name),
         PSC...Type = ifelse(PSC...Type == "", NA, PSC...Type))%>%
  dplyr::group_by(Registered.number)%>%
  fill(PSC...Name, .direction = "down")%>%
  fill(PSC...Type, .direction = "down")%>%
  ungroup()

francois_4 <- francois_4 %>%
  dplyr::select(-X, -PSC...Nature.of.control, -PSC...Name, -PSC...Type, -PSC...Country.of.residence)%>%
  distinct(.keep_all = T)%>%
  dplyr::full_join(., PSC ,by="Registered.number")%>%
  distinct(.keep_all = T)

francois_4 <- francois_4 %>%
  dplyr::rename(PSC...Name = Company.name,
                pers_sig_control_BvD.ID.number = BvD.ID.number,
                pers_sig_control_Registered.number = Registered.number,
                pers_sig_control_NACE = Primary.NACE.Rev..2.code,
                pers_sig_control_SH...Name = SH...Name,
                pers_sig_control_SH...BvD.ID.number = SH...BvD.ID.number,
                pers_sig_control_SH...Country.ISO.code = SH...Country.ISO.code,
                pers_sig_control_SH...City = SH...City,
                pers_sig_control_SH...Type = SH...Type,
                pers_sig_control_SH...NACE.Core.code = SH...NACE.Core.code,
                pers_sig_control_SH...NACE.text.description = SH...NACE.text.description,
                pers_sig_control_SH...Total.. = SH...Total..,
                pers_sig_control_SH...Direct.. = SH...Direct..,
                pers_sig_control_GUO...Name = GUO...Name,
                pers_sig_control_GUO...BvD.ID.number = GUO...BvD.ID.number,
                pers_sig_control_GUO...Country.ISO.code = GUO...Country.ISO.code,
                pers_sig_control_GUO...City = GUO...City,
                pers_sig_control_GUO...Type = GUO...Type,
                pers_sig_control_GUO...NACE.Core.code = GUO...NACE.Core.code,
                pers_sig_control_GUO...NACE.text.description = GUO...NACE.text.description,
                pers_sig_control_GUO...Direct.. = GUO...Direct..,
                pers_sig_control_GUO...Total.. = GUO...Total..,
                pers_sig_control_DUO...Name = DUO...Name,
                pers_sig_control_DUO...BvD.ID.number = DUO...BvD.ID.number,
                pers_sig_control_DUO...Country.ISO.code = DUO...Country.ISO.code,
                pers_sig_control_DUO...City = DUO...City,
                pers_sig_control_DUO...Type = DUO...Type,
                pers_sig_control_DUO...NACE.Core.code = DUO...NACE.Core.code,
                pers_sig_control_DUO...NACE.text.description = DUO...NACE.text.description,
                pers_sig_control_DUO...Direct.. = DUO...Direct..,
                pers_sig_control_DUO...Total.. = DUO...Total..,
                pers_sig_control_PSC...Nature.of.control = PSC...Nature.of.control,
                pers_sig_control_PSC...Name = PSC...Name,
                pers_sig_control_PSC...Type = PSC...Type,
                pers_sig_control_CSH...Name = CSH...Name,
                pers_sig_control_CSH...BvD.ID.number = CSH...BvD.ID.number,
                pers_sig_control_CSH...Country.ISO.code = CSH...Country.ISO.code,
                pers_sig_control_CSH...City = CSH...City,
                pers_sig_control_CSH...Type = CSH...Type,
                pers_sig_control_CSH...NACE.Core.code = CSH...NACE.Core.code,
                pers_sig_control_CSH...Total = CSH...Total..,
                pers_sig_control_CSH...Direct = CSH...Direct..,
                pers_sig_control_CSH...NACE.text.description = CSH...NACE.text.description,
                pers_sig_control_PSC...Address = PSC...Address)


gc()

francois <- left_join(francois%>%
                    dplyr::mutate(PSC...Name = toupper(PSC...Name)), francois_4, by="PSC...Name")




#######


francois_5 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_5_guo.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name),
         Registered.number = ifelse(Registered.number == "", NA, Registered.number),
         BvD.ID.number = ifelse(BvD.ID.number == "", NA, BvD.ID.number)) %>%
  fill(Company.name, .direction = "down")%>%
  group_by(Company.name)%>%
  fill(Registered.number, .direction = "down")%>%
  fill(BvD.ID.number, .direction = "down")%>%
  ungroup()%>%
  dplyr::left_join(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_10_GUO_Subs.csv"),
                   by="Company.name")%>%
  dplyr::rename(GUO_Subsidiaries = No.of.subsidiaries)

PSC <- francois_5 %>%
  dplyr::select(Registered.number, PSC...Nature.of.control, PSC...Name, PSC...Type)%>%
  mutate(PSC...Name = ifelse(PSC...Name == "", NA, PSC...Name),
         PSC...Type = ifelse(PSC...Type == "", NA, PSC...Type))%>%
  dplyr::group_by(Registered.number)%>%
  fill(PSC...Name, .direction = "down")%>%
  fill(PSC...Type, .direction = "down")%>%
  ungroup()

francois_5 <- francois_5 %>%
  dplyr::select( -PSC...Nature.of.control, -PSC...Name, -PSC...Type, -PSC...Country.of.residence)%>%
  distinct(.keep_all = T)%>%
  dplyr::full_join(., PSC ,by="Registered.number")%>%
  distinct(.keep_all = T)

francois_5 <- francois_5 %>%
  dplyr::mutate(PSC...Name = toupper(PSC...Name))%>%
  dplyr::left_join(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_7_GUO_PSC_NACE.csv")%>%
              dplyr::rename(PSC...Name= Company.name,
                            psc_comp_house = Registered.number,
                            guo_psc_nace = Primary.NACE.Rev..2.code),
            by= "PSC...Name")
  

francois_5 <- francois_5 %>%
  dplyr::select(BvD.ID.number, guo_psc_nace, GUO_Subsidiaries)%>%
  dplyr::rename(GUO...BvD.ID.number = BvD.ID.number )%>%
  dplyr::distinct(.keep_all = T)

gc()

francois <- francois %>% dplyr::select(-SH...UCI, -SH...Salutation, -GUO...Salutation,-DUO...Salutation, -GUO...UCI,
                                      -DUO...UCI,-SH...UCI, -CSH...Information.source, -CSH...Information.date)

francois <- left_join(francois, francois_5, by="GUO...BvD.ID.number")



#######


francois_6 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_6_DUO_BVD_ID.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name),
         Registered.number = ifelse(Registered.number == "", NA, Registered.number),
         BvD.ID.number = ifelse(BvD.ID.number == "", NA, BvD.ID.number)) %>%
  fill(Company.name, .direction = "down")%>%
  group_by(Company.name)%>%
  fill(Registered.number, .direction = "down")%>%
  fill(BvD.ID.number, .direction = "down")%>%
  ungroup()

PSC <- francois_6 %>%
  dplyr::select(Registered.number, PSC...Nature.of.control, PSC...Name, PSC...Type)%>%
  mutate(PSC...Name = ifelse(PSC...Name == "", NA, PSC...Name),
         PSC...Type = ifelse(PSC...Type == "", NA, PSC...Type))%>%
  dplyr::group_by(Registered.number)%>%
  fill(PSC...Name, .direction = "down")%>%
  fill(PSC...Type, .direction = "down")%>%
  ungroup()

francois_6 <- francois_6 %>%
  dplyr::select(-X, -PSC...Nature.of.control, -PSC...Name, -PSC...Type, -PSC...Country.of.residence)%>%
  distinct(.keep_all = T)%>%
  dplyr::full_join(., PSC ,by="Registered.number")%>%
  distinct(.keep_all = T)

francois_6 <- francois_6 %>%
  dplyr::mutate(PSC...Name = toupper(PSC...Name))%>%
  dplyr::left_join(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_8_DUO_PSC_NACE.csv")%>%
                     dplyr::rename(PSC...Name= Company.name,
                                   psc_comp_house = Registered.number,
                                   duo_psc_nace = Primary.NACE.Rev..2.code),
                   by= "PSC...Name")

gc()

francois_6 <- francois_6 %>%
  dplyr::select(BvD.ID.number, duo_psc_nace)%>%
  dplyr::rename(DUO...BvD.ID.number = BvD.ID.number )%>%
  dplyr::distinct(.keep_all = T)

francois <- left_join(francois, francois_6, by="DUO...BvD.ID.number")



####Sector####



keywords <- c("topco", "seniorco", "newco", "midco", "bidco", "opco",
              "equity", "partner", "capital")

# Precompute the regex pattern (case-insensitive)
pattern <- paste0("(?i)", paste(keywords, collapse = "|"))

# Define a helper function for checking NACE code conditions:
# Returns TRUE if the code starts with "64", "65", or "66" but not "64.2".
qualifies_nace <- function(code) {
  # Use coalesce() to convert NA to FALSE
  starts <- coalesce(str_starts(code, "64") | str_starts(code, "65") | str_starts(code, "66"), FALSE)
  exclusion <- coalesce(str_starts(code, "642"), FALSE)
  starts & !exclusion
}

francois <- francois %>%
  # First, flag rows where any name column contains one of the keywords.
  mutate(Invest_keyword = case_when(
    str_detect(SH...Name, pattern) ~ 1,
    str_detect(PSC...Name, pattern) ~ 1,
    str_detect(CSH...Name, pattern) ~ 1,
    str_detect(DUO...Name, pattern) ~ 1,
    str_detect(GUO...Name, pattern) ~ 1,
    str_detect(shareholder_SH...Name, pattern) ~ 1,
    str_detect(shareholder_GUO...Name, pattern) ~ 1,
    str_detect(shareholder_DUO...Name, pattern) ~ 1,
    str_detect(shareholder_CSH...Name, pattern) ~ 1,
    str_detect(shareholder_PSC...Name, pattern) ~ 1,
    str_detect(controlling_shareholder_SH...Name, pattern) ~ 1,
    str_detect(controlling_shareholder_GUO...Name, pattern) ~ 1,
    str_detect(controlling_shareholder_DUO...Name, pattern) ~ 1,
    str_detect(controlling_shareholder_CSH...Name, pattern) ~ 1,
    str_detect(controlling_shareholder_PSC...Name, pattern) ~ 1,
    str_detect(pers_sig_control_SH...Name, pattern) ~ 1,
    str_detect(pers_sig_control_GUO...Name, pattern) ~ 1,
    str_detect(pers_sig_control_DUO...Name, pattern) ~ 1,
    str_detect(pers_sig_control_CSH...Name, pattern) ~ 1,
    str_detect(pers_sig_control_PSC...Name, pattern) ~ 1,
    TRUE ~ 0
  )) %>%
  # Next, flag rows based on NACE code conditions.
  mutate(Invest_industry = case_when(
    qualifies_nace(SH...NACE.Core.code) ~ 1,
    qualifies_nace(CSH...NACE.Core.code) ~ 1,
    qualifies_nace(DUO...NACE.Core.code) ~ 1,
    qualifies_nace(GUO...NACE.Core.code) ~ 1,
    qualifies_nace(pers_sig_control_CSH...NACE.Core.code) ~ 1,
    qualifies_nace(pers_sig_control_SH...NACE.Core.code) ~ 1,
    qualifies_nace(pers_sig_control_DUO...NACE.Core.code) ~ 1,
    qualifies_nace(pers_sig_control_GUO...NACE.Core.code) ~ 1,
    qualifies_nace(controlling_shareholder_SH...NACE.Core.code) ~ 1,
    qualifies_nace(controlling_shareholder_GUO...NACE.Core.code) ~ 1,
    qualifies_nace(controlling_shareholder_DUO...NACE.Core.code) ~ 1,
    qualifies_nace(controlling_shareholder_CSH...NACE.Core.code) ~ 1,
    qualifies_nace(shareholder_CSH...NACE.Core.code) ~ 1,
    qualifies_nace(shareholder_SH...NACE.Core.code) ~ 1,
    qualifies_nace(shareholder_GUO...NACE.Core.code) ~ 1,
    qualifies_nace(shareholder_DUO...NACE.Core.code) ~ 1,
    qualifies_nace(duo_psc_nace) ~ 1,
    qualifies_nace(guo_psc_nace) ~ 1,
    qualifies_nace(pers_sig_control_NACE) ~ 1,
    TRUE ~ 0
  )) %>%
  # Combine both conditions. If either is 1, set Invest to 1; otherwise 0.
   mutate(Invest = if_else((Invest_keyword == 1 | Invest_industry == 1), 1, 0)) #%>%
  # # Optionally remove intermediate columns if they are no longer needed.
  # select(-Invest_keyword, -Invest_industry)

# Run garbage collection if needed.


gc()

# Helper function to convert SH...Direct.. values
convert_direct <- function(x) {
  case_when(
    x == ">75.00" ~ "75.1",
    x == ">50.00" ~ "50.1",
    x == "<0.50" ~ "0.51",
    x %in% c("WO", "MO", "FC") ~ "100",
    x %in% c("NG", "-") ~ "0",
    TRUE ~ x
  )
}

### STEP 1: Process the base data to compute total_individual ###
left_data1 <- francois %>%
  mutate(SH...Direct.. = convert_direct(SH...Direct..)) %>%
  distinct(Company.name, SH...Name, .keep_all = TRUE) %>%
  group_by(Company.name) %>%
  summarise(total_individual = sum(
    if_else(SH...Type == "One or more named individuals or families",
            as.numeric(SH...Direct..), 0, missing = 0),
    na.rm = TRUE
  )) %>%
  ungroup()

# Join total_individual back to the base dataframe
francois <- francois %>%
  left_join(left_data1, by = "Company.name")

### STEP 2: Process the CSV data to compute GUO_Indiv ###
csv_data <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_5_guo.csv",
                     stringsAsFactors = FALSE) %>%
  mutate(
    Company.name = if_else(Company.name == "", NA_character_, Company.name),
    Registered.number = if_else(Registered.number == "", NA_character_, Registered.number),
    BvD.ID.number = if_else(BvD.ID.number == "", NA_character_, BvD.ID.number),
    SH...Direct.. = convert_direct(SH...Direct..)
  ) %>%
  fill(Company.name, .direction = "down") %>%
  group_by(Company.name) %>%
  fill(Registered.number, .direction = "down") %>%
  fill(BvD.ID.number, .direction = "down") %>%
  ungroup() %>%
  distinct(Company.name, SH...Name, .keep_all = TRUE) %>%
  group_by(Company.name) %>%
  summarise(total_individual_csv = sum(
    if_else(SH...Type == "One or more named individuals or families",
            as.numeric(SH...Direct..), 0, missing = 0),
    na.rm = TRUE
  )) %>%
  mutate(GUO_Indiv = if_else(total_individual_csv > 50, 1, 0)) %>%
  ungroup() %>%
  select(Company.name, GUO_Indiv) %>%
  distinct() %>%
  rename(`GUO...Name` = Company.name)

# Left join CSV data onto francois using the key "GUO...Name"
francois <- francois %>%
  left_join(csv_data, by = "GUO...Name")

### STEP 3: Compute final Indiv in a single step ###
# Here, Indiv is set to 1 if either:
# - the base total_individual is greater than 50, or
# - the additional GUO conditions hold (matching BvD.ID numbers, GUO_Indiv == 1, and GUO_Subsidiaries == 1)
francois <- francois %>%
  group_by(Company.name) %>%
  mutate(Indiv = if_else(
    total_individual > 50 |
      (first(SH...BvD.ID.number) == first(GUO...BvD.ID.number) &
         GUO_Indiv == 1 & GUO_Subsidiaries == 1)|
      (first(SH...Name)==""&
         first(DUO...Name)==""&
         first(GUO...Name)==""&
         all(PSC...Type=="Individual")),
    1, 0
  )) %>%
  ungroup()


gc()



LA_keywords <- c("council", "borough", "mayor", "authority")


francois <- francois %>%
  dplyr::mutate(LA_owned_companies = ifelse(str_detect(SH...Name, paste0("(?i)", paste(LA_keywords, collapse = "|"))), 1, 
                                ifelse(str_detect(PSC...Name, paste0("(?i)", paste(LA_keywords, collapse = "|"))), 1, 
                                       ifelse(str_detect(CSH...Name, paste0("(?i)", paste(LA_keywords, collapse = "|"))), 1, 
                                              ifelse(str_detect(DUO...Name, paste0("(?i)", paste(LA_keywords, collapse = "|"))), 1, 
                                                     ifelse(str_detect(GUO...Name, paste0("(?i)", paste(LA_keywords, collapse = "|"))), 1, 
                                                            ifelse(str_detect(shareholder_SH...Name, paste0("(?i)", paste(LA_keywords, collapse = "|"))), 1, 
                                                                   ifelse(str_detect(shareholder_GUO...Name, paste0("(?i)", paste(LA_keywords, collapse = "|"))), 1, 0 ))))))))


francois_clean <- francois %>%
  dplyr::select(Company.name, Indiv, Invest, LA_owned_companies, Registered.number, Invest_industry, Invest_keyword )%>%
  dplyr::group_by(Company.name)%>%
  dplyr::summarise(Indiv = sum(Indiv, na.rm=T),
                   Invest = sum(Invest, na.rm=T),
                   Invest_industry = sum(Invest_industry, na.rm=T),
                   Invest_keyword = sum(Invest_keyword, na.rm=T),
                   LA_owned_companies = sum(LA_owned_companies, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector = ifelse(Invest>0, "Investment owned", 
                                ifelse(Indiv>0, "Individual owned",
                                       ifelse(LA_owned_companies>0, "LA owned company",
                                              "Corporate owned")))) %>%
  dplyr::distinct(.keep_all = T)

write.csv(francois_clean, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_clean.csv")



francois_clean <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_clean.csv")



df <- df %>%
  full_join(., lookup, by="Organisation_fame_search")%>%
  dplyr::left_join(., francois_clean, by="Company.name") %>%
  dplyr::mutate(Sector_merge = ifelse(Sector.x=="Local Authority", "Local Authority",
                                      ifelse(Sector.x=="Voluntary", "Third sector", Sector.y)),
                Sector_merge = ifelse(is.na(Sector_merge), "Unidentified for-profit", Sector_merge))


####Compare with francois####

fran <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/FAME_coded_25022025_17h51min28sec.csv")
me <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_clean.csv")

check <- fran %>%
  dplyr::full_join(., me, by="Company.name")%>%
  #dplyr::select(Company.name, Ownership_Type, Sector)%>%
  dplyr::mutate(Sector = ifelse(Sector=="Investment owned", "investment-owned",
                                ifelse(Sector=="Corporate owned", "corporate-owned",
                                       ifelse(Sector=="LA owned company", "local-authority-owned",
                                              ifelse(Sector=="Individual owned", "individuals-owned", NA)))),
                match = ifelse(Sector==Ownership_Type, 0,1))%>%
  dplyr::distinct(.keep_all = T)


compare_plot <- ggplot(check, aes(x=Ownership_Type))+
  geom_bar()+
  theme_bw()+
  ggtitle("François")


compare_plot2 <- ggplot(check, aes(x=Sector))+
  geom_bar()+
  theme_bw()+
  ggtitle("Ben")


matchplot <- ggplot(check%>%
                      dplyr::mutate(Matches = ifelse(match==1,"Failed match", "Match")), aes(x=Matches))+
  geom_bar()+
  theme_bw()+
  ggtitle("Matches")

one <- cowplot::plot_grid(compare_plot, compare_plot2,ncol=2)
two <- cowplot::plot_grid(one, matchplot, ncol=1)

check <- check %>%
  group_by(Company.name) %>%
  dplyr::mutate(mismatch_reason = 
                  ifelse(all(SH...Name == "")&
                           all(GUO...Name=="")&
                           match==1, "Empty Shareholder Names",
                         ifelse(Sector=="investment-owned"&
                                  Ownership_Type!="investment-owned", "Check PSC and upper tier names",
                                ifelse(match==1, "Individual condition 2", "Perfect match"))))

filtered_df <- check %>%
  group_by(Company.name) %>%
  filter(all(SH...Name == "")&
           all(GUO...Name=="")&
           match==1) %>%
  ungroup()%>%
  dplyr::mutate(PSC...Name= toupper(PSC...Name))%>%
  dplyr::left_join(., francois%>%
                     dplyr::select(PSC...Name, PSC...Nature.of.control, PSC...Type)%>%
                     dplyr::distinct(.keep_all = T),
                   by="PSC...Name")

write.csv(check %>%
            dplyr::select(Company.name, mismatch_reason)%>%
            dplyr::distinct(.keep_all = T), "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/mismatches_2.csv")


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
    read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/profits_over_time.csv") %>%
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




inspection_data <-rbind( read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2015.csv"))%>%
              dplyr::mutate(year=2015,
                            Places= NA,
                            Organisation = NA, 
                            Registration.date = NA)%>%
              dplyr::rename(Registration.status=Reg.Status,
                            Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.Date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home")|
                              str_detect(Provision.type, "(?i)day"))%>%
              dplyr::select( URN, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2016.csv"), skip=1)%>%
              dplyr::mutate(year=2016,
                            Organisation = NA,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select( URN,  Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2017.csv"), skip=1)%>%
              dplyr::mutate(year=2017,
                            Organisation = NA,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(URN,  Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018.csv"), skip=1)%>%
              dplyr::mutate(year=2018,
                            Registration.date = NA)%>%
              dplyr::rename(Overall.experiences.and.progress.of.children.and.young.people = Overall.effectiveness,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(URN, Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018_part2.csv"))%>%
              dplyr::mutate(year=2018,
                            Registration.date = NA,
                            Overall.experiences.and.progress.of.children.and.young.people = NA,
                            Latest.full.inspection.date = NA)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.name)%>%
              dplyr::select( URN,Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2019.csv"), skip=1)%>%
              dplyr::mutate(year=2019)%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(URN,  Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2020.csv"), skip=1)%>%
              dplyr::mutate(year=2020)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::select(URN,Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2021.csv"), skip=1)%>%
              dplyr::mutate(year=2021)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Latest.full.inspection.date = Inspection.date)%>%
              dplyr::select(URN,  Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2022_yep.csv"), skip=4)%>%
              dplyr::mutate(year=2022)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider,
                            Overall.experiences.and.progress.of.children.and.young.people =    Latest.full.inspection.overall.experiences.and.progress.of.children.and.young.people)%>%
              dplyr::select(URN,  Overall.experiences.and.progress.of.children.and.young.people, Latest.full.inspection.date),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2023.csv"), skip=3)%>%
              dplyr::mutate(year=2023)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider)%>%
              dplyr::select( URN,  Overall.experiences.and.progress.of.children.and.young.people,Latest.full.inspection.date),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2024.csv"), skip=3)%>%
              dplyr::mutate(year=2024)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::rename(Organisation = Organisation.which.owns.the.provider)%>%
              dplyr::select( URN,  Overall.experiences.and.progress.of.children.and.young.people,Latest.full.inspection.date))%>%
  dplyr::distinct(.keep_all = T)%>%
  dplyr::mutate(overall.average = ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Outstanding", 4,
                                         ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Good", 3,
                                                ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Requires improvement to be good", 2,
                                                       ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Inadequate", 1,NA
                                                       )))))%>%
  dplyr::mutate(number_of_inspections = 1)%>%
  dplyr::group_by(URN)%>%
  dplyr::summarise(overall.average = mean(overall.average, na.rm=T),
                   number_of_inspections = sum(number_of_inspections, na.rm=T))%>%
  dplyr::ungroup()
  

####ANALYSIS####

df$Sector_merge <- factor(df$Sector_merge, levels = c("Local Authority", "LA owned company", "Third sector", "Individual owned", "Corporate owned", "Investment owned", "Unidentified for-profit"))
df$Overall.experiences.and.progress.of.children.and.young.people<- factor(df$Overall.experiences.and.progress.of.children.and.young.people, levels = c("Inadequate", "Requires improvement to be good", "Good", "Outstanding"))
df$year = lubridate::year(df$Latest.full.inspection.date)

randommodel = clmm(Overall.experiences.and.progress.of.children.and.young.people ~  Sector_merge + (1|URN),
                   data = df)

#### figure1 ####
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

# **Ensure all time periods exist for each Sector2**
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

# **Ensure all time periods exist for each Sector2**
nobsEndBySector <- nobsEndByIdih %>%
  complete(Sector_merge, time = all_times$time, fill = list(nobs = 0)) %>%
  group_by(Sector_merge) %>%
  mutate(cumulative_end = cumsum(nobs)) %>%
  ungroup()

# Merge start and end datasets
nobser <- full_join(nobsBySector, nobsEndBySector, by = c("Sector_merge", "time")) %>%
  mutate(
    cumulative = replace_na(cumulative, 0),
    cumulative_end = replace_na(cumulative_end, 0),
    runningsum = cumulative - cumulative_end
  )


d <- ggplot(nobser %>%
              filter(time>-155,
                     Sector_merge!="LA owned company",
                     Sector_merge!="Unidentified for-profit"), 
            aes(x=time, y=runningsum, group=Sector_merge,fill=Sector_merge,  colour = Sector_merge, alpha=Sector_merge))+
  geom_point()+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_color_manual(values = c("#008F5D", "#F0AB00", "#E4007C","#FF5E5E", "#5B0000" , "#1F77B4", "#9467BD")) +
  scale_alpha_manual(values=c( 0.2,0.2,1,1,1, 0.2, 0.2))+
  # theme(legend.position="left")+
  labs(x="Year", y="Number of children homes", title = "Number of profit-motivated children's homes", fill="Ownership", color="Ownership", alpha = "Ownership")+
  scale_x_continuous(breaks=c(-12,-24,-36,-48,-60,-72,-84,-96,-108,-120,-132,-144,-156),
                     labels=c("2023","2022","2021","2020","2019","2018",  "2017", "2016", "2015","2014", "2013", "2012", "2011"))+
  theme_bw()+
  coord_cartesian(xlim=c(-110,-22))+
  theme(legend.position = "bottom")


e <- ggplot(data%>%distinct(Local.authority, year, .keep_all = T), aes(x=as.numeric(year), y=as.numeric(out_of_area_per)))+
  geom_point(aes(color = "#2A6EBB"), alpha = 0.2, size = 2) +  # Move color inside aes()
  geom_smooth(method = "loess", se = T)+
  labs(
    x = "Year",
    y = "Children placed in homes outside their area (%)",
    title = "Out of area children home places")+
  theme_bw()+
  scale_x_continuous(breaks=c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024))+
  scale_color_identity()  # Ensures custom color is used



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


yes <- cowplot::plot_grid(e,f, ncol=2,  labels = c("B", "C"))

yes <- cowplot::plot_grid(d,yes, ncol=1, labels = c("A", ""))



# Define a reusable function for ggplot generation
create_plot <- function(data, filter_col, filter_val, title) {
  ggplot(
    data %>% filter({{ filter_col }} == filter_val), 
    aes(x = as.factor({{ filter_col }}), y = as.numeric(profit_margin_average))
  ) +
    #geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
    geom_boxplot(width = 0.1, color = "#2A6EBB") +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11),
      axis.text.x = element_blank()
    ) +
    ggtitle(title) +
    xlab("") +
    ylab("Profit margin %")+
    coord_cartesian(ylim = c(-50,50))
}

# Generate plots using the reusable function
fplow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge, "Individual owned", "\nIndividual owned")
forlow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge ,  "Corporate owned", "\nCorporate owned")
investlow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge, "Investment owned", "\nInvestment owned")
lalow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge, "Local Authority", "Profit margins by sector\nLocal Authority")
vollow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge, "Third sector", "\nThird Sector")

# # Combine plots
yip <- cowplot::plot_grid(lalow, vollow, fplow,forlow, investlow, nrow = 1)
# 



data <- data %>%
  dplyr::left_join(., inspection_data, by="URN")


# Define a reusable function for ggplot generation
create_plot <- function(data, filter_col, filter_val, title) {
  ggplot(
    data %>% filter({{ filter_col }} == filter_val), 
    aes(x = as.factor({{ filter_col }}), y = as.numeric(overall.average))
  ) +
    #geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
    geom_boxplot(width = 0.1, color = "#2A6EBB") +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11),
      axis.text.x = element_blank()
    ) +
    ggtitle(title) +
    xlab("") +
    ylab("Quality (average inspection score)")#+
  #coord_cartesian(ylim = c(-50,50))
}

# Generate plots using the reusable function
fplow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge, "Individual owned", "\nIndividual owned")
forlow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge ,  "Corporate owned", "\nCorporate owned")
investlow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge, "Investment owned", "\nInvestment owned")
lalow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge, "Local Authority", "Quality by sector\nLocal Authority")
vollow <- create_plot(data%>%distinct(URN, .keep_all = T), Sector_merge, "Third sector", "\nThird Sector")

# # Combine plots
yip_inspect <- cowplot::plot_grid(lalow, vollow, fplow,forlow, investlow, nrow = 1)
# 
yip_both <- cowplot::plot_grid(yip, yip_inspect, ncol=1,  labels = c("D", "E"))


yes <- cowplot::plot_grid(yes, yip_both, ncol=1, rel_heights = c(0.6,0.4))

ggsave(plot=yes, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_1.jpeg", width=12, height=16, dpi=600)

####check on search reasons####
data$Invest_industry

data <- data %>%
  mutate(investreason = ifelse(Invest_industry>0,"NACE",
                               ifelse(Invest_keyword>0, "Keyword", NA)))


lalow <- create_plot(data%>%distinct(URN, .keep_all = T), investreason, "Keyword", "Keyword")
vollow <- create_plot(data%>%distinct(URN, .keep_all = T), investreason, "NACE", "NACE")


yip <- cowplot::plot_grid(lalow, vollow, nrow = 1)




 
####Regressions####


####Table 2####


# Function to create annual panel dataset
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
  dplyr::left_join(., houseprice,
                   by=c("Local.authority", "year"))%>%
  dplyr::filter(Local.authority!="LONDON")

LA_panel_data <- LA_panel_data %>%
  dplyr::left_join(., controls,
                   by=c("Local.authority", "year"))%>%
  dplyr::filter(Local.authority!="LONDON",
                Local.authority!="DORSET")


LA_panel_data$net_loss <- as.numeric(LA_panel_data$net_gain)*-1
LA_panel_data$occupancy_rate_fte <- 100-as.numeric(LA_panel_data$vacancy_rate_fte)
LA_panel_data$median_house_price_detached <- as.numeric(LA_panel_data$median_house_price_detached)/10000
LA_panel_data$children_in_care <- as.numeric(LA_panel_data$children_in_care)
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
  dplyr::mutate(all_change = All_homes-all_lagged,
                third_change = Homes_Third_sector-third_lagged,
                la_change = Homes_Local_Authority-LA_lagged,
                invest_change = Homes_Investment_owned-invest_lagged,
                corp_change = Homes_Corporate_owned-corp_lagged,
                ind_change = Homes_Individual_owned-ind_lagged)


LA_panel_data <- pdata.frame(LA_panel_data%>%
                               distinct(.keep_all = T), index = c("Local.authority", "year"))



####Negative Binomial ####
nbm <- as.data.frame(LA_panel_data) 


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
  "net_loss_s" = "Area need\n(net loss)",
  "median_wage_s" = "Median Wage\n(£, hourly)",
  "median_house_price_detached_s" = "Median House Price\n(Detached)",
  "Unemployment.rate...aged.16.64._s" = "Unemployment Rate\n(16-64, %)",
  "occupancy_rate_fte_s" = "Social Worker Occupancy Rate\n(FTE, %)",
  "white_s" = "White Population\n(%)",
  "claimant_count_rate_s" = "Claimant Count Rate\n(%)",
  "children_in_care_s" = "Children in Residential Care"
)

# List of variables to include in models
vars_to_include <- c("net_loss_s", "median_wage_s", "median_house_price_detached_s", 
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
    "+ factor(Region.Country.name) + year"
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





models$ind$clustered$beta - qt(1 - 0.05/2, df = models$ind$clustered$df_Satt) * models$ind$clustered$SE
models$ind$clustered$beta + qt(1 - 0.05/2, df = models$ind$clustered$df_Satt) * models$ind$clustered$SE

models$ind$clustered









#### multi-level model ####
mlm <- df %>% 
  dplyr::mutate(joined = ifelse(is.na(Join) | Join <= "2016-01-01", 0, 1),
                left = ifelse(!is.na(Leave), 1, 0),
                age = as.integer(time_length(difftime( as.Date(Registration.date,  format =  "%d/%m/%Y"), as.Date("2024-01-01")), "months"))*-1)%>%
  full_join(
    read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/profits_over_time.csv") %>%
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
    by = "Company.name"
  )%>%
  dplyr::left_join(., LA_panel_data %>%
                     dplyr::select(Local.authority,net_loss, claimant_count_rate, Unemployment.rate...aged.16.64., occupancy_rate_fte, X..of.white...aged.16., median_house_price_detached, median_wage, residential_expenditure, out_of_area_per, out_of_area, Region.Country.name)%>%
                     dplyr::mutate(children_in_care = as.numeric(as.numeric(out_of_area_per)*as.numeric(out_of_area)))%>%
                     dplyr::group_by(Local.authority, Region.Country.name)%>%
                     dplyr::summarise(net_loss = mean(as.numeric(net_loss), na.rm=T),
                                      median_house_price_detached = mean(as.numeric(median_house_price_detached), na.rm=T),
                                      median_wage = mean(as.numeric(median_wage), na.rm=T),
                                      children_in_care = mean(as.numeric(children_in_care), na.rm=T),
                                      residential_expenditure = mean(as.numeric(residential_expenditure), na.rm=T),
                                      Unemployment.rate...aged.16.64. = mean(as.numeric(Unemployment.rate...aged.16.64.), na.rm=T),
                                      occupancy_rate_fte = mean(as.numeric(occupancy_rate_fte), na.rm=T),
                                      X..of.white...aged.16. = mean(as.numeric(X..of.white...aged.16.), na.rm=T),
                                      claimant_count_rate = mean(as.numeric(claimant_count_rate), na.rm=T))%>%
                     dplyr::ungroup(),
                   by="Local.authority")%>%
  dplyr::filter(Sector_merge!="Unidentified for-profit",
                Sector_merge!="LA owned company")%>%
  dplyr::left_join(., inspection_data, by="URN")%>%
  dplyr::distinct(URN, .keep_all = T)




mlm$net_loss <- as.numeric(mlm$net_loss)
mlm$median_wage <- as.numeric(mlm$median_wage)
mlm$median_house_price_detached <- as.numeric(mlm$median_house_price_detached)
mlm$children_in_care <- as.numeric(mlm$children_in_care)
mlm$residential_expenditure <- as.numeric(mlm$residential_expenditure)
mlm$claimant_count_rate <- as.numeric(mlm$claimant_count_rate)
mlm$X..of.white...aged.16. <- as.numeric(mlm$X..of.white...aged.16.)
mlm$Unemployment.rate...aged.16.64. <- as.numeric(mlm$Unemployment.rate...aged.16.64.)
mlm$occupancy_rate_fte <- as.numeric(mlm$occupancy_rate_fte)


# Create a scaled version in the data frame
mlm$net_loss_s <- scale(mlm$net_loss)
mlm$median_wage_s <- scale(mlm$median_wage)
mlm$median_house_price_detached_s <- scale(mlm$median_house_price_detached)
mlm$children_in_care_s <- scale(mlm$children_in_care)
mlm$residential_expenditure_s <- scale(mlm$residential_expenditure)
mlm$claimant_count_rate_s <- scale(mlm$claimant_count_rate)
mlm$X..of.white...aged.16._s <- scale(mlm$X..of.white...aged.16.)
mlm$Unemployment.rate...aged.16.64._s <- scale(mlm$Unemployment.rate...aged.16.64.)
mlm$occupancy_rate_fte_s <- scale(mlm$occupancy_rate_fte)




summary(lmerTest::lmer (profit_margin_average~net_loss_s + Places+ age + children_in_care_s +
                          residential_expenditure_s + median_house_price_detached_s + median_wage_s +
                          Region.Country.name +(1|Local.authority), data = brmdata_profit))



#### BRMS ####

### Prepare Data for Multinomial BRMS Model ###
mlm$Sector_merge <- as.factor(mlm$Sector_merge)

brmdata_multinom <- mlm %>%
  tidyr::drop_na(Sector_merge, net_loss_s,age, Places, children_in_care_s,
                 residential_expenditure_s, median_house_price_detached_s, median_wage_s,
                 Region.Country.name, Local.authority) %>%
  dplyr::select(Sector_merge, net_loss_s,age, Places, children_in_care_s,
                residential_expenditure_s, median_house_price_detached_s, median_wage_s,
                Region.Country.name, Local.authority) %>%
  dplyr::mutate(Region.Country.name = factor(Region.Country.name),
                Sector_merge = factor(Sector_merge),
                Local.authority = factor(Local.authority))

brmdata_multinom$Sector_merge <- droplevels(brmdata_multinom$Sector_merge)
levels(brmdata_multinom$Sector_merge) 

### Fit the Hierarchical Multinomial Model ###
model_multilevel <- brm(
  formula = Sector_merge ~ net_loss_s + Places+ age + children_in_care_s +
    residential_expenditure_s + median_house_price_detached_s + median_wage_s +
    Region.Country.name + (1 | Local.authority),
  data = brmdata_multinom,
  family = categorical(),
  cores = 4,
  iter = 2000
)

summary(model_multilevel)

ce_netloss <- conditional_effects(model_multilevel, effects = "net_loss_s", categorical = TRUE)
ugly_plot <- plot(ce_netloss, plot = FALSE)[[1]] + 
  labs(x = "Area need (net loss)", color = "Ownership", fill = "Ownership") +
  theme_bw()

ggsave(plot=ugly_plot, filename="Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/figures/figure_3.png", width=6, height=6, dpi=600)

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

### Prepare Data for the Numerical BRMS Models ###
brmdata_overall <- mlm %>%
  tidyr::drop_na(overall.average,number_of_inspections, age, net_loss_s, Places, children_in_care_s,
                 residential_expenditure_s, median_house_price_detached_s, median_wage_s,
                 Region.Country.name, Local.authority) %>%
  dplyr::mutate(Region.Country.name = factor(Region.Country.name),
                Local.authority = factor(Local.authority))

brmdata_profit <- mlm %>%
  tidyr::drop_na(profit_margin_average, age, net_loss_s, Places, children_in_care_s,
                 residential_expenditure_s, median_house_price_detached_s, median_wage_s,
                 Region.Country.name, Local.authority) %>%
  dplyr::mutate(Region.Country.name = factor(Region.Country.name),
                Local.authority = factor(Local.authority))



### Fit Hierarchical Gaussian Models for the Numerical Outcomes ###

# Model for overall.average
model_overall <- brm(
  formula = overall.average ~ net_loss_s + Places +number_of_inspections+ age+ children_in_care_s +
    residential_expenditure_s + median_house_price_detached_s + median_wage_s +
    Region.Country.name + (1 | Local.authority),
  data = brmdata_overall,
  family = gaussian(),
  cores = 4,
  iter = 2000
)

# Model for profit_margin_average
model_profit <- brm(
  formula = profit_margin_average ~ net_loss_s + Places + age+children_in_care_s +
    residential_expenditure_s + median_house_price_detached_s + median_wage_s +
    Region.Country.name + (1 | Local.authority),
  data = brmdata_profit,
  family = gaussian(),
  cores = 4,
  iter = 2000
)

### Function to Extract Fixed Effects from a Gaussian BRMS Model ###
extract_brms_linear_table <- function(model) {
  summ <- summary(model)$fixed
  results <- data.frame(
    term = rownames(summ),
    Estimate = summ[,"Estimate"],
    SE = summ[,"Est.Error"],
    CI_Lower = summ[,"l-95% CI"],
    CI_Upper = summ[,"u-95% CI"],
    stringsAsFactors = FALSE
  )
  # Exclude the intercept
  results <- results %>% filter(term != "Intercept")
  results <- results %>% mutate(
    P_value = 2 * (1 - pnorm(abs(Estimate / SE))),
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01  ~ "**",
      P_value < 0.05  ~ "*",
      P_value < 0.1   ~ ".",
      TRUE ~ ""
    ),
    Estimate_formatted = sprintf("%.3f%s", Estimate, Significance),
    CI_formatted = sprintf("[%.3f, %.3f]", CI_Lower, CI_Upper)
  )
  results <- results %>% dplyr::select(term, Estimate_formatted, CI_formatted, P_value)
  return(results)
}

overall_table <- extract_brms_linear_table(model_overall)
profit_table  <- extract_brms_linear_table(model_profit)

### Reshape Multinomial Table and Merge with Numerical Outcome Tables ###

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
  full_join(overall_table %>% 
              rename(overall_average = Estimate_formatted,
                     overall_average_CI = CI_formatted,
                     overall_average_P_value = P_value),
            by = "term") %>%
  full_join(profit_table  %>% 
              rename(profit_margin_average = Estimate_formatted,
                     profit_margin_CI = CI_formatted,
                     profit_margin_P_value = P_value),
            by = "term") %>%
  filter(!str_detect(term, "Region"),
         term != "Intercept") %>%
  select(term, 
         `Estimate_formatted_Third sector`, `CI_formatted_Third sector`, 
         `Estimate_formatted_Individual owned`, `CI_formatted_Individual owned`, 
         `Estimate_formatted_Corporate owned`, `CI_formatted_Corporate owned`,
         `Estimate_formatted_Investment owned`, `CI_formatted_Investment owned`, 
         overall_average, overall_average_CI, 
         profit_margin_average, profit_margin_CI)

# Create a prettier version of the table with clear column names and cleaned predictors
pretty_table <- final_table %>%
  rename(
    Predictor = term,
    `Third Sector (Estimate)` = `Estimate_formatted_Third sector`,
    `Third Sector (95% CI)` = `CI_formatted_Third sector`,
    `Individual Owned (Estimate)` = `Estimate_formatted_Individual owned`,
    `Individual Owned (95% CI)` = `CI_formatted_Individual owned`,
    `Corporate Owned (Estimate)` = `Estimate_formatted_Corporate owned`,
    `Corporate Owned (95% CI)` = `CI_formatted_Corporate owned`,
    `Investment Owned (Estimate)` = `Estimate_formatted_Investment owned`,
    `Investment Owned (95% CI)` = `CI_formatted_Investment owned`,
    `Overall Rating (Estimate)` = overall_average,
    `Overall Rating (95% CI)` = overall_average_CI,
    `Profit Margin (Estimate)` = profit_margin_average,
    `Profit Margin (95% CI)` = profit_margin_CI
  ) %>%
  mutate(
    Predictor = case_when(
      Predictor == "net_loss_s" ~ "Area Need (Net Loss)",
      Predictor == "Places" ~ "Places",
      Predictor == "age" ~ "Age of Home",
      Predictor == "children_in_care_s" ~ "Children in Care",
      Predictor == "residential_expenditure_s" ~ "Local Authority Expenditure (Residential care)",
      Predictor == "median_house_price_detached_s" ~ "Median House Price (detached)",
      Predictor == "median_wage_s" ~ "Median Wage (Hourly)",
      Predictor == "number_of_inspections" ~ "Number of Previous Inspections",
      TRUE ~ Predictor
    )
  )

# Calculate sample sizes for each model
n_multinom <- nrow(brmdata_multinom)
n_overall  <- nrow(brmdata_overall)
n_profit   <- nrow(brmdata_profit)

# Add rows for fixed/random effects and sample sizes
# Using NA for cells that are not applicable provides more consistent formatting.
pretty_table <- pretty_table %>%
  add_row(Predictor = "Regional Fixed Effects", 
          `Third Sector (Estimate)` = "Included", 
          `Third Sector (95% CI)` = NA,
          `Individual Owned (Estimate)` = "Included",
          `Individual Owned (95% CI)` = NA,
          `Corporate Owned (Estimate)` = "Included",
          `Corporate Owned (95% CI)` = NA,
          `Investment Owned (Estimate)` = "Included",
          `Investment Owned (95% CI)` = NA,
          `Overall Rating (Estimate)` = "Included",
          `Overall Rating (95% CI)` = NA,
          `Profit Margin (Estimate)` = "Included",
          `Profit Margin (95% CI)` = NA
  ) %>%
  add_row(Predictor = "Local Authority Random Effects", 
          `Third Sector (Estimate)` = "Included", 
          `Third Sector (95% CI)` = NA,
          `Individual Owned (Estimate)` = "Included",
          `Individual Owned (95% CI)` = NA,
          `Corporate Owned (Estimate)` = "Included",
          `Corporate Owned (95% CI)` = NA,
          `Investment Owned (Estimate)` = "Included",
          `Investment Owned (95% CI)` = NA,
          `Overall Rating (Estimate)` = "Included",
          `Overall Rating (95% CI)` = NA,
          `Profit Margin (Estimate)` = "Included",
          `Profit Margin (95% CI)` = NA
  ) %>%
  add_row(Predictor = "Observations (n)", 
          `Third Sector (Estimate)` = as.character(n_multinom), 
          `Third Sector (95% CI)` = NA,
          `Individual Owned (Estimate)` = NA, 
          `Individual Owned (95% CI)` = NA,
          `Corporate Owned (Estimate)` = NA, 
          `Corporate Owned (95% CI)` = NA,
          `Investment Owned (Estimate)` = NA, 
          `Investment Owned (95% CI)` = NA,
          `Overall Rating (Estimate)` = as.character(n_overall), 
          `Overall Rating (95% CI)` = NA,
          `Profit Margin (Estimate)` = as.character(n_profit), 
          `Profit Margin (95% CI)` = NA
  )

# Combine each estimate with its 95% CI into one column per model/metric
pretty_table <- pretty_table %>%
  mutate(
    `Third Sector (Estimate) [95% CI]` = paste0(`Third Sector (Estimate)`, " ", `Third Sector (95% CI)`, ""),
    `Individual Owned (Estimate) [95% CI]` = paste0(`Individual Owned (Estimate)`, " ", `Individual Owned (95% CI)`, ""),
    `Corporate Owned (Estimate) [95% CI]` = paste0(`Corporate Owned (Estimate)`, " ", `Corporate Owned (95% CI)`, ""),
    `Investment Owned (Estimate) [95% CI]` = paste0(`Investment Owned (Estimate)`, " ", `Investment Owned (95% CI)`, ""),
    `Quality (Estimate) [95% CI]` = paste0(`Overall Rating (Estimate)`, " ", `Overall Rating (95% CI)`, ""),
    `Profit Margin (Estimate) [95% CI]` = paste0(`Profit Margin (Estimate)`, " ", `Profit Margin (95% CI)`, "")
  ) %>%
  dplyr::select(Predictor,
         `Third Sector (Estimate) [95% CI]`,
         `Individual Owned (Estimate) [95% CI]`,
         `Corporate Owned (Estimate) [95% CI]`,
         `Investment Owned (Estimate) [95% CI]`,
         `Quality (Estimate) [95% CI]`,
         `Profit Margin (Estimate) [95% CI]`)

# Build the gt table
regression_table <- pretty_table %>%
  gt() %>%
  tab_header(
    title = "Hierarchical Regression Models Examining Predictors of Children Home Ownership and Performance"
  ) %>%
  tab_spanner(
    label = c("Reference category for ownership type: Local Authority"),
    columns = matches("Third Sector|Individual Owned|Corporate Owned|Investment Owned")
  ) %>%
  tab_spanner(
    label = c("Ownership Type (Multinomial Model)"),
    columns = matches("Third Sector|Individual Owned|Corporate Owned|Investment Owned")
  ) %>%
  tab_spanner(
    label = "Performance Metrics",
    columns = matches("Quality|Profit Margin")
  ) %>%
  cols_align(
    align = "left",
    columns = "Predictor"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Third Sector (Estimate) [95% CI]",
                "Individual Owned (Estimate) [95% CI]",
                "Corporate Owned (Estimate) [95% CI]",
                "Investment Owned (Estimate) [95% CI]",
                "Quality (Estimate) [95% CI]",
                "Profit Margin (Estimate) [95% CI]")
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
regression_table %>%
  gtsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Table2.html")




ce_netloss <- conditional_effects(model_multilevel, effects = "net_loss_s", categorical = T)

ugly_plot <- plot(ce_netloss, plot = FALSE)[[1]] + 
  labs(x="Area need (net loss)", color = "Ownership", fill="Ownership")+
  theme_bw()









####Table 1####


library(dplyr)
library(gtsummary)
library(gt)

# Create the summary table with renamed variables
summary_table <- mlm %>%
  dplyr::mutate(residential_expenditure = residential_expenditure/1000000)%>%
  mutate(Sector_merge = droplevels(Sector_merge)) %>%
  select(Sector_merge,
         overall.average, age, Places, left, profit_margin_average,
         children_in_care, net_loss, residential_expenditure,
         median_house_price_detached, median_wage) %>%
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
      age ~ "Age of Home (months)",
      Places ~ "Places (n)",
      left ~ "Closed",
      profit_margin_average ~ "Profit margin (%)",
      children_in_care ~ "Children in Care (n)",
      net_loss ~ "Area need (Net loss)",
      residential_expenditure ~ "Local Authority expenditure (Residential care, £ms)",
      median_house_price_detached ~ "Median House Price (Detached, £10ks)",
      median_wage ~ "Median Wage (Hourly)"
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
  tab_row_group(
    label = "Area Characteristics",
    rows = 13:28
  ) %>%
  # Add row groups for subheaders. Adjust row numbers if your table structure changes.
  tab_row_group(
    label = "Children Home Characteristics",
    rows = 1:13
  ) %>%

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
  gtsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Tables/Table1.html")



























# Ensure that the outcome variable is a factor
mlm$Sector_merge <- as.factor(mlm$Sector_merge)

# Fit the hierarchical multinomial model using glmmTMB
# (Here, 'categorical()' specifies a multinomial logit model.)
model_multilevel <- glmmTMB(
  Sector_merge ~ net_loss_s + Places + children_in_care_s +
    residential_expenditure_s + median_house_price_detached_s + median_wage_s +
    Region.Country.name + (1 | Local.authority),   # Random intercept for LA-level clustering
  data = mlm,
  family = categorical()
)

simple_model <- glmmTMB(
  Sector_merge ~ net_loss_s + (1 | Local.authority),
  data = mlm,
  family = categorical(),
  control = glmmTMBControl(optimizer = optim, 
                           optArgs = list(method = "BFGS"))
)
summary(simple_model)



mcmc_data <- mlm%>%
  tidyr::drop_na(Sector_merge, net_loss_s , Places , children_in_care_s ,
                 residential_expenditure_s , median_house_price_detached_s , median_wage_s ,
                 Region.Country.name, Local.authority)%>%
  dplyr::select(Sector_merge, net_loss_s , Places , children_in_care_s ,
                residential_expenditure_s , median_house_price_detached_s , median_wage_s ,
                Region.Country.name, Local.authority)%>%
  dplyr::mutate(Region.Country.name = factor(Region.Country.name),
                Sector_merge = factor(Sector_merge),
                Local.authority = factor(Local.authority))

mcmc_data$Sector_merge <- droplevels(mcmc_data$Sector_merge)

levels(mcmc_data$Sector_merge) 

mcmc_model <- MCMCglmm(
  Sector_merge ~ net_loss_s + Places + children_in_care_s +
    residential_expenditure_s + median_house_price_detached_s + median_wage_s +
    Region.Country.name,
  random = ~Local.authority,
  rcov = ~ idh(trait):units,  # Correct error structure for categorical data
  family = "categorical",
  data = mcmc_data,
  nitt = 50000,       
  thin = 25,          
  burnin = 5000       
)




# (b) Alternatively, using broom.mixed for a tidy table:
if (!require("broom.mixed")) install.packages("broom.mixed")
library(broom.mixed)
tidy_table <- tidy(mcmc_model, conf.int = TRUE)
print(tidy_table)

tab_model(mcmc_model, title = "Multinomial Multilevel Model Coefficients")


library(MCMCplots)
MCMCplot(mcmc_model, 
         main = "Posterior Distributions of Fixed Effects",
         xlab = "Coefficient Value")


# Print the model summary to see fixed effects, random effects, and HPD intervals
summary(mcmc_model)

# Extract fixed effects table
fixef_table <- summary(mcmc_model)$solutions
print(fixef_table)


# Load necessary package
library(coda)

# Assuming 'mcmc_model' is your MCMCglmm model:
summary_model <- summary(mcmc_model)

# Extract fixed effects (coefficients) for all categories of Sector_merge
fixed_effects <- summary(mcmc_model)$solutions

# Print out the fixed effects summary
print(fixed_effects)

# Get the fixed effects matrix
fixed_effects <- summary_model$solutions

# Create a list to store the regression tables for each category
regression_table_list <- list()

# Loop over each category (excluding the reference category) and extract the coefficients
for (category in 2:ncol(fixed_effects)) {  # Start from 2 because column 1 is for the intercept
  category_name <- colnames(fixed_effects)[category]
  
  # Extract coefficients for this category
  category_effects <- data.frame(
    Variable = rownames(fixed_effects),
    Estimate = fixed_effects[, category],
    `Lower 95% CI` = fixed_effects[, paste0("l-95% CI")],
    `Upper 95% CI` = fixed_effects[, paste0("u-95% CI")],
    `pMCMC` = fixed_effects[, paste0("pMCMC")],
    Category = category_name
  )
  
  # Append the result for this category
  regression_table_list[[category_name]] <- category_effects
}

# Combine all the results into a single data frame
regression_table <- do.call(rbind, regression_table_list)

# View the regression table
print(regression_table)


# Print out the regression table
print(regression_table)


summary(mcmc_model)



# Assuming you've already fit the MCMCglmm model:
# mcmc_model <- MCMCglmm(...)



# ---- 1. Trace plots to check MCMC convergence ----
# Plot trace for fixed effects
plot(mcmc_model$Sol)

# Plot trace for random effects variance components
plot(mcmc_model$VCV)

# ---- 2. Posterior distributions of fixed effects ----
# Extract posterior samples
mcmc_samples <- as.mcmc(mcmc_model$Sol)
mcmc_df <- as.data.frame(mcmc_samples)

# Reshape for ggplot
fixed_effects_long <- pivot_longer(mcmc_df, 
                                   cols = everything(), 
                                   names_to = "parameter", 
                                   values_to = "estimate")

# Create posterior distribution plot
ggplot(fixed_effects_long, aes(x = estimate)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  facet_wrap(~ parameter, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Posterior distributions of fixed effects",
       x = "Parameter estimate",
       y = "Density")

# ---- 3. Forest plot of fixed effects with credible intervals ----
# Calculate summary statistics
fixed_effects_summary <- fixed_effects_long %>%
  group_by(parameter) %>%
  summarize(
    mean = mean(estimate),
    lower = quantile(estimate, 0.025),
    upper = quantile(estimate, 0.975)
  )

# Create forest plot
ggplot(fixed_effects_summary, aes(x = mean, y = parameter)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Fixed effects with 95% credible intervals",
       x = "Parameter estimate",
       y = "")

# ---- 4. Random effects variance components ----
# Extract variance components
vcv_samples <- as.mcmc(mcmc_model$VCV)
vcv_df <- as.data.frame(vcv_samples)

# Reshape for ggplot
vcv_long <- pivot_longer(vcv_df, 
                         cols = everything(), 
                         names_to = "variance_component", 
                         values_to = "estimate")

# Create posterior distribution plot for variance components
ggplot(vcv_long, aes(x = estimate)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  facet_wrap(~ variance_component, scales = "free") +
  theme_minimal() +
  labs(title = "Posterior distributions of variance components",
       x = "Variance estimate",
       y = "Density")

# ---- 5. Predicted probabilities across net_loss_s values ----
# Create a sequence of net_loss values
net_loss_range <- seq(min(mcmc_data$net_loss_s, na.rm = TRUE),
                      max(mcmc_data$net_loss_s, na.rm = TRUE),
                      length.out = 100)

newdata <- data.frame(
  net_loss_s = net_loss_range,
  Places = mean(mcmc_data$Places, na.rm = TRUE),
  children_in_care_s = mean(mcmc_data$children_in_care_s, na.rm = TRUE),
  residential_expenditure_s = mean(mcmc_data$residential_expenditure_s, na.rm = TRUE),
  median_house_price_detached_s = mean(mcmc_data$median_house_price_detached_s, na.rm = TRUE),
  median_wage_s = mean(mcmc_data$median_wage_s, na.rm = TRUE),
  Region.Country.name = names(sort(table(mcmc_data$Region.Country.name), decreasing = TRUE))[1]
)

# Ensure factor levels match
newdata$Local.authority <- as.character(names(sort(table(mcmc_data$Local.authority), decreasing = TRUE))[1], 
                                  levels = levels(factor(mcmc_data$Local.authority)))
newdata$Region.Country.name <- as.character(newdata$Region.Country.name, levels = levels(factor(mcmc_data$Region.Country.name)))
newdata$Sector_merge <- factor(names(sort(table(mcmc_data$Sector_merge), decreasing = TRUE))[1], 
                               levels = levels(mcmc_data$Sector_merge))



newdata$Region.Country.name <- as.character(sample(levels(factor(mcmc_data$Region.Country.name)), 2, replace = TRUE),
                                      levels = levels(factor(mcmc_data$Region.Country.name)))

newdata$Local.authority <- as.character(sample(levels(factor(mcmc_data$Local.authority)), 2, replace = TRUE),
                                  levels = levels(factor(mcmc_data$Local.authority)))


# Check the structure again
str(newdata)
str(mcmc_data)

# Proceed with predictions
predictions <- predict(mcmc_model, newdata = newdata, type = "response")


predictions <- predict.MCMCglmm(mcmc_model)



# Convert predictions to data frame
pred_df <- data.frame(
  net_loss_s = rep(net_loss_range, ncol(predictions)),
  sector = rep(colnames(predictions), each = length(net_loss_range)),
  probability = as.vector(predictions)
)

# Plot predicted probabilities
ggplot(pred_df, aes(x = net_loss_s, y = probability, color = sector)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = probability - 1.96 * sd(probability),
                  ymax = probability + 1.96 * sd(probability),
                  fill = sector), alpha = 0.2, color = NA) +
  theme_minimal() +
  labs(title = "Predicted probabilities by sector across net_loss_s values",
       x = "Net Loss (standardized)",
       y = "Probability",
       color = "Sector",
       fill = "Sector")

# ---- 6. Caterpillar plot of random effects ----
# Extract random effects
ranef_samples <- mcmc_model$Sol[, grep("Local.authority", colnames(mcmc_model$Sol))]

# Calculate summary statistics
ranef_summary <- apply(ranef_samples, 2, function(x) {
  c(mean = mean(x), 
    lower = quantile(x, 0.025), 
    upper = quantile(x, 0.975))
})

# Convert to data frame
ranef_df <- data.frame(
  Local_authority = gsub("Local.authority", "", colnames(ranef_samples)),
  mean = ranef_summary["mean", ],
  lower = ranef_summary["lower", ],
  upper = ranef_summary["upper", ]
)

# Sort by mean
ranef_df <- ranef_df[order(ranef_df$mean), ]

# Create caterpillar plot
ggplot(ranef_df, aes(x = mean, y = reorder(Local_authority, mean))) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Random effects for Local Authorities with 95% credible intervals",
       x = "Random effect estimate",
       y = "Local Authority")

# ---- 7. Posterior predictive checks ----
# Function to simulate data from the model
simulate_data <- function(mcmc_model) {
  # Extract one random set of parameters
  iteration <- sample(1:nrow(mcmc_model$Sol), 1)
  fixed_effects <- mcmc_model$Sol[iteration, ]
  random_effects <- mcmc_model$VCV[iteration, ]
  
  # Simulate data (simplified example)
  # In practice, you'd need to implement the full data generation process
  # for your specific categorical model
  X <- model.matrix(~ net_loss_s + Places + children_in_care_s +
                      residential_expenditure_s + median_house_price_detached_s + 
                      median_wage_s + Region.Country.name, data = mlm)
  
  linear_pred <- X %*% fixed_effects[1:ncol(X)]
  
  # Add random effects (simplified)
  # In practice, this would be more complex for categorical models
  
  # Convert to probabilities (simplified)
  # For categorical models, you'd need to implement the appropriate link function
  
  # Return simulated data
  # This is a placeholder for demonstration
  return(data.frame(simulated_value = as.vector(linear_pred)))
}

# Generate simulated datasets
n_sims <- 100
simulated_data <- replicate(n_sims, simulate_data(mcmc_model), simplify = FALSE)

# Plot comparison of simulated vs observed data
# This is a placeholder - you'd need to adapt this for your specific model
# For categorical data, you might compare frequency distributions

# ---- 8. Model comparison ----
# If you have multiple models, you can compare them using DIC
# Lower DIC indicates better fit

# Extract DIC
dic_value <- mcmc_model$DIC

# Print DIC
cat("DIC:", dic_value, "\n")

# If you have multiple models:
# model_list <- list(model1 = mcmc_model1, model2 = mcmc_model2)
# dic_values <- sapply(model_list, function(x) x$DIC)
# dic_df <- data.frame(model = names(dic_values), DIC = dic_values)
# print(dic_df[order(dic_df$DIC), ])

# If the above still doesn't work, try an alternative approach using nnet package



mlm <- mlm %>%
  mutate(across(c(net_loss_s, children_in_care_s, residential_expenditure_s,
                  median_house_price_detached_s, median_wage_s, occupancy_rate_fte_s , Unemployment.rate...aged.16.64._s,
                  claimant_count_rate_s , X..of.white...aged.16._s), as.numeric))


mlm$Sector_merge <- factor(mlm$Sector_merge)
# Fit a multinomial model first to get good starting values
basic_multinomial <- multinom(
  Sector_merge ~ net_loss_s + Places + children_in_care_s +
    residential_expenditure_s + median_house_price_detached_s + median_wage_s +
    Region.Country.name,
  data = mlm
)
cluster_se <- coef_test(basic_multinomial, 
                        vcov = "CR2", 
                        cluster = mlm$Local.authority)



cluster_vcov <- vcovCR(basic_multinomial, cluster = mlm$Local.authority, type = "CR2")


# View the clustered standard errors
cluster_vcov_alt
# Calculate predicted probabilities
pred <- ggpredict(basic_multinomial, terms = "net_loss_s", ci_level = "0.95", vcov = cluster(mlm$Local.authority))



####event study####

# Load required libraries


# Clean and prepare the initial dataset
event_study <- LA_panel_data %>%
  dplyr::select(
    Local.authority, year, net_loss, All_homes, children_in_care, 
    vacancy_rate_fte, Unemployment.rate...aged.16.64., 
    Homes_Investment_owned, Homes_LA_owned_company, Homes_Third_sector, 
    Homes_Individual_owned, Homes_Corporate_owned
  ) %>%
  dplyr::mutate(year = as.numeric(as.character(year)))

# Define home types
home_types <- c(
  "Homes_Investment_owned", "Homes_LA_owned_company", "Homes_Third_sector", 
  "Homes_Individual_owned", "Homes_Corporate_owned", "All_homes"
)

# Store plots
plot_list <- list()

# Loop through each home type
for (home_type in home_types) {
  
  # Select relevant columns for the current home type
  event_study_subset <- LA_panel_data %>%
    dplyr::select(
      Local.authority, year, net_loss, all_of(home_type), children_in_care, 
      vacancy_rate_fte, Unemployment.rate...aged.16.64.
    ) %>%
    dplyr::mutate(year = as.numeric(as.character(year)))
  
  # Calculate the annual change in the selected home type
  event_study_subset <- event_study_subset %>%
    group_by(Local.authority) %>%
    mutate(change = c(NA, diff(.data[[home_type]]))) %>%
    ungroup()
  
  # Identify event years (when there's a positive change)
  events <- event_study_subset %>%
    filter(change > 0) %>%
    dplyr::select(Local.authority, year) %>%
    rename(event_year = year)
  
  # Create stacked event study dataset with a 3-year window before and after
  stacked_data <- events %>%
    group_by(Local.authority, event_year) %>%
    do({
      la <- .$Local.authority[1]
      ev <- .$event_year[1]
      window <- event_study_subset %>% 
        filter(Local.authority == la, year >= (ev - 3), year <= (ev + 3)) %>%
        mutate(event_time = year - ev)
      window
    }) %>%
    ungroup()
  
  # Convert event_time to factor for proper reference in regression
  stacked_data$event_time <- factor(stacked_data$event_time)
  
  # Run event-study regression
  mod_stacked <- feols(
    net_loss ~ i(event_time, ref = "-1") | Local.authority + event_year, 
    data = stacked_data, 
    cluster = ~Local.authority
  )
  
  # Extract coefficients and create dataframe for plotting
  coef_data <- data.frame(
    estimate = coef(mod_stacked),
    event_time = names(coef(mod_stacked)),
    stringsAsFactors = FALSE
  )
  
  # Clean up event_time names by removing prefix
  coef_data$event_time <- gsub("event_time::", "", coef_data$event_time)
  
  # Convert to numeric for proper ordering in plot
  coef_data$event_time_num <- as.numeric(coef_data$event_time)
  
  # Extract standard errors
  coef_data$se <- sqrt(diag(vcov(mod_stacked)))
  
  # Calculate 95% confidence intervals
  coef_data <- coef_data %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    ) %>%
    arrange(event_time_num)
  
  # Generate plot with confidence intervals
  p <- ggplot(coef_data, aes(x = event_time_num, y = estimate, group = 1)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "blue", alpha = 0.2) +
    geom_point(size = 3, color = "blue") +
    geom_line(color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Event Study:", gsub("_", " ", home_type)),
      x = "Years from Event",
      y = "Effect on Net Loss"
    ) +
    scale_x_continuous(breaks = -3:3) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9)
    )
  
  # Add to plot list
  plot_list[[home_type]] <- p
}

# Arrange plots in a 2x3 grid
combined_plot <- (plot_list[["Homes_Investment_owned"]] | plot_list[["Homes_LA_owned_company"]] | plot_list[["Homes_Third_sector"]]) /
  (plot_list[["Homes_Individual_owned"]] | plot_list[["Homes_Corporate_owned"]] | plot_list[["All_homes"]])

# Display the combined plot
combined_plot











# Assume your dataset is in a data frame called df.
# Your columns are:
#   local_authority: the local authority name (e.g., "BARKING AND DAGENHAM")
#   year: the year (numeric)
#   net_loss: our measure of need (net loss)
#   All_homes: the number of children's homes in the area

# Step 1. Sort the data by local authority and year and calculate the annual change in All_homes.

# Arrange data by Local.authority and year, then calculate the change.
event_study <- event_study %>%
  group_by(Local.authority) %>%
  mutate(change = c(NA, diff(Homes_Corporate_owned))) %>%  # Difference within each group
  ungroup()

events <- event_study %>%
  filter(change > 0) %>%
  select(Local.authority, year) %>%
  rename(event_year = year)

# Create the stacked event study dataset.
# For each event, keep observations in a window of years from (event_year - 3) to (event_year + 3)
stacked_data <- events %>%
  group_by(Local.authority, event_year) %>%
  do({
    la <- .$Local.authority[1]
    ev <- .$event_year[1]
    # Select rows for the given local authority in the event window
    window <- event_study %>% filter(Local.authority == la,
                            year >= (ev - 3),
                            year <= (ev + 3))
    # Create an event time variable relative to the event year
    window <- window %>% mutate(event_time = year - ev)
    window
  }) %>%
  ungroup()

# Check the stacked data
head(stacked_data)

# Run an event-study regression.
# Here we regress net_loss (our measure of need) on event-time dummies, 
# controlling for Local.authority fixed effects and event fixed effects (to absorb event-specific factors).
# We use event_time = -1 as the reference period.
mod_stacked <- feols(net_loss ~ i(event_time, ref = -1) | Local.authority + event_year, 
                     data = stacked_data, 
                     cluster = ~Local.authority)

# Display the regression summary
summary(mod_stacked)

# Plot the event study coefficients using fixest's built-in iplot function
iplot(mod_stacked,
      xlab = "Years from Event", 
      main = "Stacked Event Study: Net Loss (Need) Around Children's Home Openings")



LA_panel_data

# Define models
reg1 <- plm(All_homes ~  net_loss_lagged + occupancy_rate_fte + median_house_price_detached +median_wage +Unemployment.rate...aged.16.64. + X..of.white...aged.16.+ claimant_count_rate + children_in_care  +  Region.Country.name, 
            index = c("Local.authority", "year"), data = LA_panel_data, model = "pooling")

reg2 <- plm(Homes_Local_Authority ~  net_loss_lagged + occupancy_rate_fte + median_house_price_detached+median_wage +Unemployment.rate...aged.16.64. + X..of.white...aged.16.+ claimant_count_rate + children_in_care  + Homes_Corporate_owned + Homes_Investment_owned + Homes_Third_sector + Homes_Individual_owned+ Region.Country.name, 
            index = c("Local.authority", "year"), data = LA_panel_data, model = "pooling")

reg3 <- plm(Homes_Third_sector ~  net_loss_lagged + occupancy_rate_fte+ median_house_price_detached+median_wage  +Unemployment.rate...aged.16.64. + X..of.white...aged.16.+ claimant_count_rate + children_in_care  + Homes_Corporate_owned + Homes_Investment_owned + Homes_Individual_owned + Homes_Local_Authority+ Region.Country.name, 
            index = c("Local.authority", "year"), data = LA_panel_data,  model = "pooling")

reg4 <- plm(Homes_Individual_owned ~  net_loss_lagged+ occupancy_rate_fte + median_house_price_detached+median_wage +Unemployment.rate...aged.16.64. + X..of.white...aged.16.+ claimant_count_rate + children_in_care  + Homes_Corporate_owned + Homes_Investment_owned + Homes_Third_sector + Homes_Local_Authority+ Region.Country.name, 
            index = c("Local.authority", "year"), data = LA_panel_data,  model = "pooling")

reg5 <- plm(Homes_Corporate_owned ~  net_loss_lagged + occupancy_rate_fte+ median_house_price_detached+median_wage +Unemployment.rate...aged.16.64. + X..of.white...aged.16.+ claimant_count_rate + children_in_care  + Homes_Individual_owned + Homes_Investment_owned + Homes_Third_sector + Homes_Local_Authority+ Region.Country.name, 
            index = c("Local.authority", "year"), data = LA_panel_data,  model = "pooling")

reg6 <- plm(Homes_Investment_owned ~  net_loss_lagged + occupancy_rate_fte+ median_house_price_detached+median_wage +Unemployment.rate...aged.16.64. + X..of.white...aged.16.+ claimant_count_rate + children_in_care  + Homes_Corporate_owned + Homes_Individual_owned + Homes_Third_sector + Homes_Local_Authority+ Region.Country.name, 
            index = c("Local.authority", "year"), data = LA_panel_data,  model = "pooling")

# Function to process model results
get_summary <- function(reg_model) {
  reg_sum <- as.list(modelsummary(reg_model, output = "modelsummary_list", statistic = c("conf.int", "p={p.value}")))
  
  coef_stats <- coef_test(reg_model, vcov = "CR2", cluster = LA_panel_data$Local.authority, test = "Satterthwaite")
  
  reg_sum$tidy$p.value <- coef_stats$p
  reg_sum$tidy$std.error <- coef_stats$SE
  reg_sum$tidy$conf.low <- reg_sum$tidy$estimate - (1.96 * coef_stats$SE)
  reg_sum$tidy$conf.high <- reg_sum$tidy$estimate + (1.96 * coef_stats$SE)
  
  # Create two separate lists: one for estimates and another for p-values
  estimates <- reg_sum
  estimates$tidy$estimate <- paste0(round(estimates$tidy$estimate, 4), 
                                    " [", round(estimates$tidy$conf.low, 4), ", ", 
                                    round(estimates$tidy$conf.high, 4), "]")
  
  pvalues <- reg_sum
  pvalues$tidy$estimate <- paste0("p = ", round(pvalues$tidy$p.value, 4))
  
  return(list(estimates = estimates, pvalues = pvalues))
}

# Generate model summaries with two columns for each regression
reg1_results <- get_summary(reg1)
reg2_results <- get_summary(reg2)
reg3_results <- get_summary(reg3)
reg4_results <- get_summary(reg4)
reg5_results <- get_summary(reg5)
reg6_results <- get_summary(reg6)

# Custom coefficient names
cm <- c("net_loss_lagged" = "Area need (net loss of children, standardised, lagged)",
        "children_in_care" = "Children in care (n)",
        "median_wage" = "Median wage (full-time, hourly gross, £)")

# Add summary rows
rows <- tribble(
  ~term, ~`Local Authority Homes [% CI]`, ~`p-value`, ~`Third Sector Homes [% CI]`, ~`p-value`, 
  ~`Individual Owned Homes [% CI]`, ~`p-value`, ~`Corporate Owned Homes [% CI]`, ~`p-value`, 
  ~`Investment Owned Homes [% CI]`, ~`p-value`,~`Investment Owned Homes [% CI]`, ~`p-value`,
  "Clustered Standard Errors", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes",
  "Control Variables", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes",
  "Region Fixed effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"
)

# Create table with estimates and p-values as separate columns
table <- modelsummary(
  list(
    "All Homes [% CI]" = reg1_results$estimates, "p-value" = reg1_results$pvalues,
    "Local Authority Homes [% CI]" = reg2_results$estimates, "p-value" = reg2_results$pvalues,
    "Third Sector Homes [% CI]" = reg3_results$estimates, "p-value" = reg3_results$pvalues,
    "Individual Owned Homes [% CI]" = reg4_results$estimates, "p-value" = reg4_results$pvalues,
    "Corporate Owned Homes [% CI]" = reg5_results$estimates, "p-value" = reg5_results$pvalues,
    "Investment Owned Homes [% CI]" = reg6_results$estimates, "p-value" = reg6_results$pvalues
  ),
  coef_omit = "Intercept|dept|year",
  add_rows = rows,
  coef_map = cm,
  fmt = 4,
  estimate = "{estimate}",  # This now only contains the formatted estimate from get_summary()
  statistic = NULL,  # No additional statistics needed
  notes = list(
    "Table reports results from multivariate pooled longitudinal regression models.",
    "Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2).",
    "All regressions control for: children in care, deprivation rate, ethnic minority (%), unemployment rate (%), and claimant rate (%)."
  ),
  output = "gt"
)

table

# Define models
reg1 <- plm(Homes_Local_Authority ~ vacancy_rate_fte + Homes_Corporate_owned + Homes_Investment_owned + Homes_Third_sector + Homes_Individual_owned, 
            index = c("Local.authority", "year"), data = LA_panel_data, model = "pooling")

reg2 <- plm(Homes_Third_sector ~ vacancy_rate_fte + Homes_Corporate_owned + Homes_Investment_owned + Homes_Individual_owned + Homes_Local_Authority, 
            index = c("Local.authority", "year"), data = LA_panel_data, model = "pooling")

reg3 <- plm(Homes_Individual_owned ~ vacancy_rate_fte + Homes_Corporate_owned + Homes_Investment_owned + Homes_Third_sector + Homes_Local_Authority, 
            index = c("Local.authority", "year"), data = LA_panel_data, model = "pooling")

reg4 <- plm(Homes_Corporate_owned ~ vacancy_rate_fte + Homes_Individual_owned + Homes_Investment_owned + Homes_Third_sector + Homes_Local_Authority, 
            index = c("Local.authority", "year"), data = LA_panel_data, model = "pooling")

reg5 <- plm(Homes_Investment_owned ~ vacancy_rate_fte + Homes_Corporate_owned + Homes_Individual_owned + Homes_Third_sector + Homes_Local_Authority, 
            index = c("Local.authority", "year"), data = LA_panel_data, model = "pooling")

# Function to process model results
get_summary <- function(reg_model) {
  reg_sum <- as.list(modelsummary(reg_model, output = "modelsummary_list", statistic = c("conf.int", "p={p.value}")))
  
  coef_stats <- coef_test(reg_model, vcov = "CR2", cluster = LA_panel_data$Local.authority, test = "Satterthwaite")
  
  reg_sum$tidy$p.value <- coef_stats$p
  reg_sum$tidy$std.error <- coef_stats$SE
  reg_sum$tidy$conf.low <- reg_sum$tidy$estimate - (1.96 * coef_stats$SE)
  reg_sum$tidy$conf.high <- reg_sum$tidy$estimate + (1.96 * coef_stats$SE)
  
  # Create two separate lists: one for estimates and another for p-values
  estimates <- reg_sum
  estimates$tidy$estimate <- paste0(round(estimates$tidy$estimate, 4), 
                                    " [", round(estimates$tidy$conf.low, 4), ", ", 
                                    round(estimates$tidy$conf.high, 4), "]")
  
  pvalues <- reg_sum
  pvalues$tidy$estimate <- paste0("p = ", round(pvalues$tidy$p.value, 4))
  
  return(list(estimates = estimates, pvalues = pvalues))
}

# Generate model summaries with two columns for each regression
reg1_results <- get_summary(reg1)
reg2_results <- get_summary(reg2)
reg3_results <- get_summary(reg3)
reg4_results <- get_summary(reg4)
reg5_results <- get_summary(reg5)

# Custom coefficient names
cm <- c("scale(as.numeric(net_gain))" = "Area need (net loss of children)")

# Add summary rows
rows <- tribble(
  ~term, ~`Local Authority Homes [% CI]`, ~`p-value`, ~`Third Sector Homes [% CI]`, ~`p-value`, 
  ~`Individual Owned Homes [% CI]`, ~`p-value`, ~`Corporate Owned Homes [% CI]`, ~`p-value`, 
  ~`Investment Owned Homes [% CI]`, ~`p-value`,
  "Clustered Standard Errors", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Control Variables", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"
)

# Create table with estimates and p-values as separate columns
table <- modelsummary(
  list(
    "Local Authority Homes [% CI]" = reg1_results$estimates, "p-value" = reg1_results$pvalues,
    "Third Sector Homes [% CI]" = reg2_results$estimates, "p-value" = reg2_results$pvalues,
    "Individual Owned Homes [% CI]" = reg3_results$estimates, "p-value" = reg3_results$pvalues,
    "Corporate Owned Homes [% CI]" = reg4_results$estimates, "p-value" = reg4_results$pvalues,
    "Investment Owned Homes [% CI]" = reg5_results$estimates, "p-value" = reg5_results$pvalues
  ),
  coef_omit = "Intercept|dept|year",
  add_rows = rows,
  coef_map = cm,
  fmt = 4,
  estimate = "{estimate}",  # This now only contains the formatted estimate from get_summary()
  statistic = NULL,  # No additional statistics needed
  notes = list(
    "Table reports results from multivariate pooled longitudinal regression models.",
    "Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2).",
    "All regressions control for: LA population, LA population under 18 (%), degree of education (%), managerial or professional occupation (%), ethnic minority (%), unemployment rate (%), and claimant rate (%)."
  ),
  output = "gt"
)

table







summary(plm(scale(as.numeric(net_gain))~Places_Individual_owned+Places_Corporate_owned+Places_Investment_owned+Places_Third_sector+Places_Local_Authority, index = c("Local.authority", "year"),data= LA_panel_data, model = "pooling"))  
summary(plm(scale(as.numeric(net_gain))~Homes_Individual_owned+Homes_Corporate_owned+Homes_Investment_owned+Homes_Third_sector+Homes_Local_Authority, index = c("Local.authority", "year"),data= LA_panel_data, model = "pooling"))  

summary(plm(scale(as.numeric(IMD.2019...Extent))~Homes_Individual_owned+Homes_Corporate_owned+Homes_Investment_owned+Homes_Third_sector+Homes_Local_Authority, index = c("Local.authority", "year"),data= LA_panel_data, model = "pooling"))  
summary(plm(scale(as.numeric(Average_house_price))~Homes_Individual_owned+Homes_Corporate_owned+Homes_Investment_owned+Homes_Third_sector+Homes_Local_Authority, index = c("Local.authority", "year"),data= LA_panel_data, model = "pooling"))  


yes <- (plm(scale(as.numeric(net_gain))~Homes_Individual_owned+Homes_Corporate_owned+Homes_Investment_owned+Homes_Third_sector+Homes_Local_Authority, index = c("Local.authority", "year"),data= LA_panel_data, model = "pooling"))  
yes <- (plm(scale(as.numeric(net_gain))~Places_Individual_owned+Places_Corporate_owned+Places_Investment_owned+Places_Third_sector+Places_Local_Authority, index = c("Local.authority", "year"),data= LA_panel_data, model = "pooling"))  

yes <- (plm(scale(as.numeric(IMD.2019...Extent))~Homes_Individual_owned+Homes_Corporate_owned+Homes_Investment_owned+Homes_Third_sector+Homes_Local_Authority, index = c("Local.authority", "year"),data= LA_panel_data, model = "pooling"))  
yes <- (plm(scale(as.numeric(Average_house_price))~Homes_Individual_owned+Homes_Corporate_owned+Homes_Investment_owned+Homes_Third_sector+Homes_Local_Authority, index = c("Local.authority", "year"),data= LA_panel_data, model = "pooling"))  


coef_test(reg1, vcov = "CR2", cluster = data$Local.authority, test = "Satterthwaite")
coef_test(yes, vcov = "CR2", cluster = data$Local.authority, test = "Satterthwaite")




####table 3####

data$Sector_merge <- factor(data$Sector_merge, levels = c("Local Authority", "LA owned company", "Third sector", "Individual owned", "Corporate owned", "Investment owned", "Unidentified for-profit"))

data <- data %>%
  dplyr::mutate(age = as.integer(time_length(difftime( as.Date(Registration.date,  format =  "%d/%m/%Y"), as.Date("2024-01-01")), "months"))*-1,
                closed = ifelse(is.na(Leave), 0,1),
                Homes = 1)

data$Sector_merge <- factor(data$Sector_merge)

levels(data$Sector_merge) <- make.names(levels(data$Sector_merge))
data$net_gain_num <- as.numeric(as.character(data$net_gain))
data$joined <- factor(data$joined)
data$closed <- factor(data$closed)
data <- data %>% mutate(churn = ifelse(!is.na(Join)&!is.na(Leave), "Churn", "Stable"))
data$Sector_merge

data$age_deciles <- factor(cut(data$age, breaks = quantile(data$age, probs = seq(0, 1, by = 0.2), na.rm = TRUE), include.lowest = TRUE, labels = FALSE))


summary(lm((net_gain_num)~age+Places+Sector_merge+profit_margin_average+salaries_turnover_average, data = data%>%
             dplyr::filter(Sector_merge!="Unidentified.for.profit",
                           Sector_merge!="LA.owned.company",
                           year=="2023",
                           net_gain_num!=0
             )))


yes <- (lm((net_gain_num)~Sector_merge*age, data = data%>%
             dplyr::filter(Sector_merge!="Unidentified.for.profit",
                           Sector_merge!="LA.owned.company",
                           year=="2023",
                           net_gain_num!=0
             )))
predicted <- ggpredict(yes, terms = c("profit_margin_average", "Sector_merge")) 



ggplot(predicted, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.5)) +  # Aligns error bars properly
  geom_point(position = position_dodge(width = 0.5), size = 2) +  # Matches dodge width for alignment
  theme_minimal() +
  labs(x = "Age Deciles", y = "Predicted Net Gain", 
       title = "Interaction Effects of Age Deciles and Sector",
       color = "Sector", fill = "Sector")














####sensitivity check on joined outcome####

all <- lm(net_loss_s ~  Sector_merge + Places + children_in_care_s + 
            residential_expenditure_s + median_house_price_detached_s + 
            median_wage_s +Region.Country.name,
          data = mlm)

all <- lmer(net_loss_s ~  Sector_merge + Places + children_in_care_s + 
              residential_expenditure_s + median_house_price_detached_s + 
              median_wage_s +Region.Country.name + (1  | Local.authority), 
            data = mlm)


all <- glmer(joined ~ net_loss_s +Sector_merge + Places + children_in_care_s + 
               residential_expenditure_s + median_house_price_detached_s + 
               median_wage_s +Region.Country.name + (1 + net_loss_s | Local.authority), 
             family = binomial, data = mlm)

corp <- glmer(joined ~ net_loss_s  + Places + children_in_care_s + 
                residential_expenditure_s + median_house_price_detached_s + 
                median_wage_s +Region.Country.name + (1 + net_loss_s | Local.authority), 
              family = binomial, data = mlm%>%
                dplyr::filter(Sector_merge=="Corporate owned"))

invest <- glmer(joined ~ net_loss_s  + Places + children_in_care_s + 
                  residential_expenditure_s + median_house_price_detached_s + 
                  median_wage_s +Region.Country.name + (1 + net_loss_s | Local.authority), 
                family = binomial, data = mlm%>%
                  dplyr::filter(Sector_merge=="Investment owned"))

indiv <- glmer(joined ~ net_loss_s  + Places + children_in_care_s + 
                 residential_expenditure_s + median_house_price_detached_s + 
                 median_wage_s +Region.Country.name + (1 + net_loss_s | Local.authority), 
               family = binomial, data = mlm%>%
                 dplyr::filter(Sector_merge=="Individual owned"))

third <- glmer(joined ~ net_loss_s  + Places + children_in_care_s + 
                 residential_expenditure_s + median_house_price_detached_s + 
                 median_wage_s +Region.Country.name + (1 + net_loss_s | Local.authority), 
               family = binomial, data = mlm%>%
                 dplyr::filter(Sector_merge=="Third sector"))

la <- glmer(joined ~ net_loss_s  + Places + children_in_care_s + 
              residential_expenditure_s + median_house_price_detached_s + 
              median_wage_s +Region.Country.name + (1 + net_loss_s | Local.authority), 
            family = binomial, data = mlm%>%
              dplyr::filter(Sector_merge=="Local Authority"))





y <- plot_model(all, "est")+
  ggtitle("All")


yy <- plot_model(indiv, "est")+
  ggtitle("Individual")
yyy <- plot_model(corp, "est")+
  ggtitle("Corporate")
yyyy <- plot_model(la, "est")+
  ggtitle("LA")
yyyyy <- plot_model(third, "est")+
  ggtitle("Third")
yyyyyy <- plot_model(invest, "est")+
  ggtitle("Invest")


cowplot::plot_grid(y, yy, yyy, yyyy, yyyyy, yyyyyy)




# 1. Simple cross-tabulation by quartiles
# First, create quartiles of net_loss
mlm$net_loss_quartile <- cut(mlm$net_loss, 
                             breaks = quantile(mlm$net_loss, 
                                               probs = c(0, 0.25, 0.5, 0.75, 1), 
                                               na.rm = TRUE),
                             labels = c("Q1 (Low net loss)", 
                                        "Q2", 
                                        "Q3", 
                                        "Q4 (High net loss)"),
                             include.lowest = TRUE)

# Cross-tabulation of joined by net_loss quartile
joined_by_loss <- table(mlm$joined, mlm$net_loss_quartile)
prop_joined_by_loss <- prop.table(joined_by_loss, margin = 2) * 100  # Percentage within each quartile

# Print the tables
print("Count of homes by joined status and net loss quartile:")
print(joined_by_loss)
print("Percentage of homes by joined status within each net loss quartile:")
print(round(prop_joined_by_loss, 2))

# 2. Cross-tabulation by sector and net loss
joined_by_sector_loss <- table(mlm$joined, mlm$Sector_merge, mlm$net_loss_quartile)
print("Count of homes by joined status, sector, and net loss quartile:")
print(joined_by_sector_loss)

# 3. Visualizations

# a. Simple proportion plot
ggplot(mlm, aes(x = net_loss_quartile, fill = factor(joined))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Homes Opened Since 2014 by Net Loss Quartile",
       x = "Net Loss Quartile",
       y = "Proportion",
       fill = "Opened Since 2014") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme_minimal()

# b. Mean joined status by net loss quartile - shows relationship direction
ggplot(mlm, aes(x = net_loss_quartile, y = joined)) +
  stat_summary(fun = mean, geom = "bar", fill = "steelblue") +
  labs(title = "Mean Proportion of Homes Opened Since 2014 by Net Loss Quartile",
       x = "Net Loss Quartile",
       y = "Proportion Opened Since 2014") +
  theme_minimal()

# c. Relationship by sector
ggplot(mlm, aes(x = net_loss_quartile, y = joined, fill = Sector_merge)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "Proportion of Homes Opened Since 2014 by Net Loss Quartile and Sector",
       x = "Net Loss Quartile",
       y = "Proportion Opened Since 2014",
       fill = "Sector") +
  theme_minimal()

# d. Scatterplot with regression line to show raw relationship
ggplot(mlm, aes(x = net_loss, y = joined)) +
  geom_jitter(alpha = 0.1, height = 0.05) +  # Jittered points (since joined is binary)
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +
  labs(title = "Relationship Between Net Loss and Probability of Opening Since 2014",
       x = "Net Loss",
       y = "Probability of Opening Since 2014") +
  theme_minimal()

# e. Faceted by sector to show interaction
ggplot(mlm, aes(x = net_loss_s, y = joined)) +
  geom_jitter(alpha = 0.1, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +
  facet_wrap(~ Sector_merge) +
  labs(title = "Relationship Between Net Loss and Probability of Opening by Sector",
       x = "Net Loss",
       y = "Probability of Opening Since 2014") +
  theme_minimal()



mlm <- mlm %>%
  mutate(across(c(net_loss_s, children_in_care_s, residential_expenditure_s,
                  median_house_price_detached_s, median_wage_s, occupancy_rate_fte_s , Unemployment.rate...aged.16.64._s,
                  claimant_count_rate_s , X..of.white...aged.16._s), as.numeric))

simple_model <- glm(joined ~ net_loss_s * Sector_merge + Places +
                      children_in_care_s + residential_expenditure_s +
                      median_house_price_detached_s + median_wage_s +
                      occupancy_rate_fte_s + Unemployment.rate...aged.16.64._s+
                      claimant_count_rate_s + X..of.white...aged.16._s +
                      Region.Country.name, 
                    family = binomial, data = mlm)

simple_model <- glm(left ~ net_loss_s * salaries_turnover_average + Places +
                      children_in_care_s + residential_expenditure_s +
                      median_house_price_detached_s + median_wage_s +
                      occupancy_rate_fte_s + Unemployment.rate...aged.16.64._s+
                      claimant_count_rate_s + X..of.white...aged.16._s +
                      Region.Country.name, 
                    family = binomial, data = mlm)

simple_model <- glm(joined ~ net_loss_s * salaries_turnover_average + Places +
                      children_in_care_s + residential_expenditure_s +
                      median_house_price_detached_s + median_wage_s +
                      occupancy_rate_fte_s + Unemployment.rate...aged.16.64._s+
                      claimant_count_rate_s + X..of.white...aged.16._s +
                      Region.Country.name, 
                    family = binomial, data = mlm)

summary(simple_model)

plot_model(simple_model, "int")+
  theme_minimal()

glmer_model <- glmer(joined ~ net_loss_s  +Region.Country.name + (1 + net_loss_s | Local.authority), 
                     family = binomial, data = mlm)

summary(glmer_model)


between_LA_model <- glmer(
  joined ~ net_loss_s * Sector_merge + 
    Places + 
    children_in_care_s + 
    residential_expenditure_s + 
    median_house_price_detached_s + 
    median_wage_s + 
    Region.Country.name + 
    (1 | Local.authority),  # Random intercept only, no random slope
  family = binomial, 
  data = mlm
)

plot_model(between_LA_model, "int")






####DELETED panel shit####



# Filter out entities with fewer than 8 time periods
grainger_panel_data <- LA_panel_data %>%
  group_by(Local.authority) %>%
  filter(n() >= 10) %>%
  ungroup()

grainger_panel_data <- grainger_panel_data %>%
  filter(!is.na(All_homes) & !is.na(net_loss_same_year))

# Ensure data is in panel format
grainger_panel_data <- pdata.frame(grainger_panel_data, index = c("Local.authority", "year"))

balanced_panel <- make.pbalanced(grainger_panel_data, balance.type = "fill.omit")


# Perform Granger causality test with 2 lags on the filtered dataset
pgrangertest(All_homes ~net_loss_same_year, data = grainger_panel_data)

# Check panel dimensions
dim_info <- pdim(grainger_panel_data)
print(dim_info)

# Create a properly balanced panel
balanced_panel <- make.pbalanced(grainger_panel_data, balance.type = "fill")

# Check if balanced now
is.pbalanced(balanced_panel)

# Try Granger test with explicitly setting order=1
pgrangertest(All_homes ~ net_loss_same_year, data = balanced_panel, order = 1)



# Create a balanced panel with only complete cases
balanced_panel2 <- make.pbalanced(grainger_panel_data, balance.type = "shared.individuals")

# Check if balanced
is.pbalanced(balanced_panel2)

# Check for NAs
sum(is.na(balanced_panel2$All_homes))
sum(is.na(balanced_panel2$net_loss_same_year))

# Try the test again
pgrangertest(All_homes ~ net_loss_same_year, data = balanced_panel2, order = 1)


# Install if needed
# install.packages("plm")
# install.packages("panelvar")

library(panelvar)

# Try a panel VAR model
model <- pvargmm(dependent_vars = c("All_homes", "net_loss_same_year"),
                 lags = 1,
                 transformation = "fd",  # First difference
                 data = balanced_panel2,
                 panel_identifier = c("Local.authority", "year"),
                 steps = c("twostep"),
                 system_instruments = FALSE)

summary(model)


gmm_model <- pgmm(Homes_Third_sector ~ lag(Homes_Third_sector, 1) + net_loss_same_year +  occupancy_rate_fte + median_house_price_detached+Unemployment.rate...aged.16.64. + X..of.white...aged.16.+ claimant_count_rate + children_in_care   | 
                    lag(Homes_Third_sector, 1:3) + lag(net_loss_same_year, 0:3),
                  data = LA_panel_data,
                  effect = "individual",
                  model = "onestep")
summary(gmm_model)


















# Print hypothesis test results
print(hypothesis_test_1)
print(hypothesis_test_2)
print(hypothesis_test_3)
print(hypothesis_test_4)

# Combine fixed effects and hypothesis test results in a regression table format
regression_table <- cbind(fixed_effects, 
                          hypothesis_test_1$hypothesis, 
                          hypothesis_test_2$hypothesis, 
                          hypothesis_test_3$hypothesis, 
                          hypothesis_test_4$hypothesis)

# Add column names
colnames(regression_table) <- c("Estimate", "Est.Error", "l-95% CI", "u-95% CI", 
                                "Hypothesis_Test_Individualowned", 
                                "Hypothesis_Test_Investmentowned", 
                                "Hypothesis_Test_Thirdsector", 
                                "Hypothesis_Test_Corporateowned")

# View the regression table
print(regression_table)









####DELETED.   survival analysis####

survival_data <- df %>%
  mutate(year = lubridate::year(Join))%>%
  dplyr::filter(Join>"2014-01-01")%>%
  dplyr::left_join(., data%>%
                     dplyr::select(Local.authority,year,out_of_area,out_of_area_per,net_gain,)%>%
                     dplyr::full_join(., workforce,
                                      by=c("Local.authority", "year"))%>%
                     dplyr::left_join(., houseprice,
                                      by=c("Local.authority", "year"))%>%
                     dplyr::left_join(., controls,
                                      by=c("Local.authority", "year"))%>%
                     dplyr::mutate(year = as.numeric(year)+1), by= c("Local.authority", "year"))%>%
  dplyr::arrange(desc(Latest.full.inspection.date))%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::mutate(open_date = as.Date("2014-01-01"),
                time = as.numeric(as.Date(Join) - open_date),
                event=1,
                net_loss = (as.numeric(net_gain)*-1),
                occupancy_rate_fte = 100-as.numeric(vacancy_rate_fte),
                median_house_price_detached = as.numeric(median_house_price_detached),
                children_in_care = as.numeric(children_in_care),
                unemployment = `Unemployment rate - aged 16-64 `,
                white = `% of white - aged 16+`,
                median_wage = median_wage*100,
                claimant_count_rate = claimant_count_rate*100,
                
  )


melanoma <- boot::melanoma #F1 here for help page with data dictionary

cox <- coxph(Surv(time, event) ~scale(net_loss)   +scale(median_wage) +scale(median_house_price_detached)+scale(unemployment)+scale(occupancy_rate_fte) +scale(Places)+ scale(white)+ scale(claimant_count_rate) + scale(children_in_care) +strata(Region.Country.name),
             data = survival_data%>%
               dplyr::filter(Sector_merge!="LA owned company",
                             Sector_merge!="Unidentified for-profit"))

plot_model(cox, "int")+
  ggtitle("Investment owned")

cox <- coxph(Surv(time, event) ~scale(net_loss)   +scale(median_wage) +scale(median_house_price_detached)+scale(unemployment)+scale(occupancy_rate_fte) +scale(Places)+ scale(white)+ scale(claimant_count_rate) + scale(children_in_care) +strata(Region.Country.name),
             data = survival_data%>%
               dplyr::filter(Sector_merge=="Investment owned"))

one <- plot_model(cox, "est")+
  ggtitle("Investment owned")+
  theme_bw()+
  geom_hline(yintercept = 1)+
  coord_cartesian(ylim = c(0.1,20))+
  coord_flip()

cox <- coxph(Surv(time, event) ~scale(net_loss)   +scale(median_wage) +scale(median_house_price_detached)+scale(unemployment)+scale(occupancy_rate_fte) +scale(Places)+ scale(white)+ scale(claimant_count_rate) + scale(children_in_care) +strata(Region.Country.name),
             data = survival_data%>%
               dplyr::filter(Sector_merge=="Corporate owned"))

two <- plot_model(cox, "est")+
  ggtitle("Corporate owned")+
  theme_bw()+
  geom_hline(yintercept = 1)+
  coord_cartesian(ylim = c(0.1,20))+
  coord_flip()

cox <- coxph(Surv(time, event) ~scale(net_loss)   +scale(median_wage) +scale(median_house_price_detached)+scale(unemployment)+scale(occupancy_rate_fte) +scale(Places)+ scale(white)+ scale(claimant_count_rate) + scale(children_in_care) +strata(Region.Country.name),
             data = survival_data%>%
               dplyr::filter(Sector_merge=="Individual owned"))

three <- plot_model(cox, "est")+
  ggtitle("Individual owned")+
  theme_bw()+
  geom_hline(yintercept = 1)+
  coord_cartesian(ylim = c(0.1,20))+
  coord_flip()

cox <- coxph(Surv(time, event) ~scale(net_loss)   +scale(median_wage) +scale(median_house_price_detached)+scale(unemployment)+scale(occupancy_rate_fte) +scale(Places)+ scale(white)+ scale(claimant_count_rate) + scale(children_in_care) +strata(Region.Country.name),
             data = survival_data%>%
               dplyr::filter(Sector_merge=="Local Authority"))

four <- plot_model(cox, "est")+
  ggtitle("Local Authority")+
  theme_bw()+
  geom_hline(yintercept = 1)+
  coord_cartesian(ylim = c(0.1,20))+
  coord_flip()

cox <- coxph(Surv(time, event) ~scale(net_loss)   +scale(median_wage) +scale(median_house_price_detached)+scale(unemployment)+scale(occupancy_rate_fte) +scale(Places)+ scale(white)+ scale(claimant_count_rate) + scale(children_in_care) +strata(Region.Country.name),
             data = survival_data%>%
               dplyr::filter(Sector_merge=="Third sector"))

five <- plot_model(cox, "est")+
  ggtitle("Third sector")+
  theme_bw()+
  geom_hline(yintercept = 1)+
  coord_cartesian(ylim = c(0.1,20))+
  coord_flip()


cowplot::plot_grid(  three, two, one, four, five)




####DELETED negative binomial models####




# Function to compute clustered standard errors
get_summary <- function(reg_model, data, cluster_var) {
  vcov_cluster <- vcovCR(reg_model, cluster = data[[cluster_var]], type = "CR2")
  coef_stats <- coef_test(reg_model, vcov = vcov_cluster, test = "Satterthwaite")
  
  summary_df <- data.frame(
    term = rownames(coef_stats),
    estimate = coef_stats$beta,
    std.error = coef_stats$SE,
    p.value = coef_stats$p,
    conf.low = coef_stats$beta - 1.96 * coef_stats$SE,
    conf.high = coef_stats$beta + 1.96 * coef_stats$SE
  )
  
  return(summary_df)
}

# List of dependent variables
outcomes <- c("Homes_Individual_owned", "Homes_Corporate_owned", 
              "Homes_Investment_owned", "Homes_Local_Authority", "Homes_Third_sector",
              "All_homes")

conts <- c("non_indiv", "non_corp", "non_invest", "non_la", "non_3rd", "")

# Ensure `conts` and `outcomes` have the same length
if (length(outcomes) != length(conts)) {
  stop("Mismatch between length of outcomes and conts")
}

# Empty list to store summary results
summary_results_list <- list()

# Loop through each outcome variable
for (i in seq_along(outcomes)) {
  # Only add control variable if it's non-empty
  additional_control <- if (conts[i] != "") paste("+", conts[i]) else ""
  
  # Construct the formula
  formula_str <- paste(outcomes[i], "~ net_loss_s + median_wage_s +",
                       "median_house_price_detached_s + Unemployment.rate...aged.16.64._s +",
                       "occupancy_rate_fte_s + white_s +",
                       "claimant_count_rate_s + children_in_care_s + residential_expenditure_s +",
                       "factor(year) + factor(Region.Country.name)", additional_control)
  
  # Fit negative binomial regression
  model_nb <- glm.nb(as.formula(formula_str), data = nbm)
  
  # Store clustered SE summary
  summary_results_list[[outcomes[i]]] <- get_summary(model_nb, nbm, "Local.authority")
}


# Combine all summary results into one data frame
summary_results_df <- bind_rows(summary_results_list, .id = "Outcome")

# Filter only relevant terms for plotting
plot_data <- summary_results_df %>%
  filter(term %in% c("net_loss_s", "median_wage_s", "median_house_price_detached_s", "children_in_care_s",
                     "non_la", "non_3rd", "non_corp", "non_indiv", "non_invest", "residential_expenditure_s"))%>%
  dplyr::mutate(Outcome = ifelse(Outcome=="Homes_Individual_owned", "Individual owned",
                                 ifelse(Outcome=="Homes_Corporate_owned", "Corporate owned",
                                        ifelse(Outcome=="Homes_Investment_owned", "Investment owned",
                                               ifelse(Outcome=="Homes_Local_Authority", "Local Authority",
                                                      ifelse(Outcome=="Homes_Third_sector", "Third sector",
                                                             ifelse(Outcome=="All_homes", "All homes",
                                                                    Outcome)))))),
                Outcome = factor(Outcome, levels = c("Individual owned", "Corporate owned", 
                                                     "Investment owned", "Local Authority", "Third sector",
                                                     "All homes")),
                term = ifelse(term=="children_in_care_s", "Children in residential care",
                              ifelse(term=="claimant_count_rate_s", "Claimant count (%)",
                                     ifelse(term=="median_house_price_detached_s", "House price (dettached)",
                                            ifelse(term=="median_wage_s", "Median hourly wage",
                                                   ifelse(term=="net_loss_s", "Area need (Net loss)",
                                                          ifelse(term=="occupancy_rate_fte_s", "Occupancy rate (%, FTE)",
                                                                 ifelse(term=="Unemployment.rate...aged.16.64._s", "Unemployment rate (%)",
                                                                        ifelse(term=="white_s", "White ethnicity (%)",
                                                                               ifelse(term=="non_la", "Other Homes (n)",
                                                                                      ifelse(term=="non_indiv", "Other Homes (n)",
                                                                                             ifelse(term=="non_3rd", "Other Homes (n)",
                                                                                                    ifelse(term=="non_corp", "Other Homes (n)",
                                                                                                           ifelse(term=="non_invest", "Other Homes (n)",
                                                                                                                  ifelse(term=="residential_expenditure_s", "LA expenditure (£, residential care)",
                                                                                                                         
                                                                                                                         term
                                                                                                                  )))))))))))))))


# Plot the clustered standard error estimates
figure_2 <- ggplot(plot_data, aes(x = factor(term, levels = rev(levels(factor(term)))), 
                                  y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Effect of area characteristics on children home numbers",
       x = "Predictor Variables",
       y = "Coefficient Estimate (with 95% CI)",
       color = "Outcome Type") +
  theme(legend.position = "bottom")+
  facet_wrap(~ Outcome, nrow=2)+
  theme(
    strip.text = element_text(face = "bold", size = 14, color = "black"),  # Bold and blue facet titles
    strip.background = element_rect(fill = "lightgray", color = "black")    # Pretty background for facet titles
  )


ggsave(plot=figure_2, filename="~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care-Markets/Figures/figure_2.jpeg", width=8, height=7, dpi=600)



