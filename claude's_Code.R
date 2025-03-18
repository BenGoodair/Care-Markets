



if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,ggmap,googleway,hrbrthemes,viridis,jsonlite, httr, purrr, dplyr,gt, gtsummary, tidyverse,rattle,glmnet,caret, rpart.plot,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)


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
              dplyr::select(Sector, URN, Places, Local.authority, Organisation, Registration.date, Overall.experiences.and.progress.of.children.and.young.people,Latest.full.inspection.date))%>%
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
  ungroup()


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



####francois categories####
# Load libraries first
library(dplyr)
library(tidyr)
library(stringr)
library(data.table) # For more efficient data manipulation

# Helper function to clean and prepare dataframes - with column existence check
clean_dataframe <- function(df, prefix = NULL) {
  # Convert to data.table for efficiency
  df <- as.data.table(df)
  
  # Replace empty strings with NA
  for (col in names(df)[sapply(df, is.character)]) {
    set(df, which(df[[col]] == ""), col, NA)
  }
  
  # Fill company name and reg number downward
  if("Company.name" %in% names(df)) {
    df[, Company.name := nafill(Company.name, "locf"), by = .(seq_len(nrow(df)))]
    
    if("Registered.number" %in% names(df)) {
      df[, Registered.number := nafill(Registered.number, "locf"), by = .(seq_len(nrow(df)))]
    }
    
    if("BvD.ID.number" %in% names(df)) {
      df[, BvD.ID.number := nafill(BvD.ID.number, "locf"), by = .(seq_len(nrow(df)))]
    }
  }
  
  # Handle PSC fields if they exist
  psc_cols <- grep("PSC...", names(df), value = TRUE)
  if(length(psc_cols) > 0) {
    psc_name_col <- grep("PSC...Name", psc_cols, value = TRUE)
    psc_type_col <- grep("PSC...Type", psc_cols, value = TRUE)
    
    if(length(psc_name_col) > 0 && length(psc_type_col) > 0 && "Registered.number" %in% names(df)) {
      PSC <- df[, c("Registered.number", psc_cols), with = FALSE]
      for (col in c(psc_name_col, psc_type_col)) {
        PSC[, (col) := nafill(get(col), "locf"), by = Registered.number]
      }
      
      # Remove PSC columns from main df and join later
      df <- df[, !..psc_cols]
      df <- unique(df)
      df <- merge(df, PSC, by = "Registered.number", all.x = TRUE)
      df <- unique(df)
    }
  }
  
  # Add prefix to column names if specified
  if(!is.null(prefix)) {
    exclude_cols <- c("BvD.ID.number", "Registered.number")
    exclude_cols <- exclude_cols[exclude_cols %in% names(df)]  # Only use columns that exist
    rename_cols <- setdiff(names(df), exclude_cols)
    setnames(df, rename_cols, paste0(prefix, rename_cols))
  }
  
  return(df)
}

# Read and process the first dataframe
francois <- fread("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_1_basic_companies.csv")
francois <- clean_dataframe(francois)

# Additional fills specific to this dataframe
cols_to_fill <- c("Latest.accounts.date", "Entity.type", "No.of.companies.in.corporate.group", 
                  "Operating.revenue..Turnover..th.GBP.Last.avail..yr", 
                  "Number.of.employees.Last.avail..yr", "Primary.UK.SIC..2007..code")

# Only fill columns that exist
cols_to_fill <- cols_to_fill[cols_to_fill %in% names(francois)]
for (col in cols_to_fill) {
  francois[, (col) := nafill(get(col), "locf"), by = Company.name]
}

# Remove X column which appears in multiple dataframes
if("X" %in% names(francois)) {
  francois[, X := NULL]
}

# Process shareholders dataframe
francois_2 <- fread("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_2_shareholders_fixed.csv")
francois_2 <- clean_dataframe(francois_2, "shareholder_")

# Check which columns exist before renaming
cols_to_rename <- c("SH...BvD.ID.number", "CSH...BvD.ID.number", "BvD.ID.number", 
                    "GUO...BvD.ID.number", "DUO...BvD.ID.number")
new_names <- c("shareholder_SH...BvD.ID.number", "shareholder_CSH...BvD.ID.number", 
               "SH...BvD.ID.number", "shareholder_GUO...BvD.ID.number", "shareholder_DUO...BvD.ID.number")

# Only rename columns that exist
for (i in seq_along(cols_to_rename)) {
  if (cols_to_rename[i] %in% names(francois_2)) {
    setnames(francois_2, cols_to_rename[i], new_names[i])
  }
}

# Process controlling shareholders dataframe
francois_3 <- fread("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_3_controlling_shareholders_fixed.csv")
francois_3 <- clean_dataframe(francois_3, "controlling_shareholder_")

# Check which columns exist before renaming
cols_to_rename <- c("CSH...BvD.ID.number", "BvD.ID.number", "GUO...BvD.ID.number", "DUO...BvD.ID.number")
new_names <- c("controlling_hareholder_CSH...BvD.ID.number", "CSH...BvD.ID.number", 
               "controlling_hareholder_GUO...BvD.ID.number", "controlling_hareholder_DUO...BvD.ID.number")

# Only rename columns that exist
for (i in seq_along(cols_to_rename)) {
  if (cols_to_rename[i] %in% names(francois_3)) {
    setnames(francois_3, cols_to_rename[i], new_names[i])
  }
}

# Process persons of significant control
francois_4 <- fread("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_4_persons_significant_control.csv")
francois_4 <- clean_dataframe(francois_4, "pers_sig_control_")

# Check which columns exist before renaming
cols_to_rename <- c("pers_sig_control_Company.name", "GUO...BvD.ID.number", "DUO...BvD.ID.number")
new_names <- c("PSC...Name", "pers_sig_control_shareholder_GUO...BvD.ID.number", "pers_sig_control_shareholder_DUO...BvD.ID.number")

# Only rename columns that exist
for (i in seq_along(cols_to_rename)) {
  if (cols_to_rename[i] %in% names(francois_4)) {
    setnames(francois_4, cols_to_rename[i], new_names[i])
  }
}

# Convert PSC names to uppercase for consistent matching if the column exists
if ("PSC...Name" %in% names(francois)) {
  francois[, PSC...Name := toupper(PSC...Name)]
}

# Join dataframes efficiently using data.table - with error checking
# First join francois_2 by SH...BvD.ID.number
if ("SH...BvD.ID.number" %in% names(francois) && "SH...BvD.ID.number" %in% names(francois_2)) {
  cols_to_use_2 <- setdiff(names(francois_2), "SH...BvD.ID.number")
  francois <- merge(francois, francois_2[, c("SH...BvD.ID.number", ..cols_to_use_2)], 
                    by = "SH...BvD.ID.number", all.x = TRUE)
}

# Force garbage collection
rm(francois_2)
gc()

# Then join francois_3 by CSH...BvD.ID.number
if ("CSH...BvD.ID.number" %in% names(francois) && "CSH...BvD.ID.number" %in% names(francois_3)) {
  cols_to_use_3 <- setdiff(names(francois_3), "CSH...BvD.ID.number")
  francois <- merge(francois, francois_3[, c("CSH...BvD.ID.number", ..cols_to_use_3)], 
                    by = "CSH...BvD.ID.number", all.x = TRUE)
}

# Force garbage collection
rm(francois_3)
gc()

# Then join francois_4 by PSC...Name
if ("PSC...Name" %in% names(francois) && "PSC...Name" %in% names(francois_4)) {
  cols_to_use_4 <- setdiff(names(francois_4), "PSC...Name")
  francois <- merge(francois, francois_4[, c("PSC...Name", ..cols_to_use_4)], 
                    by = "PSC...Name", all.x = TRUE)
}

# Force garbage collection
rm(francois_4)
gc()

# Process GUO dataframe - THIS IS THE PROBLEMATIC PART, PROCESS DIFFERENTLY
guo_data <- fread("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_5_guo.csv")
guo_data <- clean_dataframe(guo_data)

# Read the GUO PSC NACE data
guo_psc_nace <- fread("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_7_GUO_PSC_NACE.csv")

# Check which columns exist before renaming
if (all(c("Company.name", "Primary.NACE.Rev..2.code") %in% names(guo_psc_nace))) {
  cols_to_rename <- c("Company.name", "Primary.NACE.Rev..2.code")
  new_names <- c("PSC...Name", "guo_psc_nace")
  
  if ("Registered.number" %in% names(guo_psc_nace)) {
    cols_to_rename <- c(cols_to_rename, "Registered.number")
    new_names <- c(new_names, "psc_comp_house")
  }
  
  setnames(guo_psc_nace, cols_to_rename, new_names)
  
  if ("PSC...Name" %in% names(guo_psc_nace)) {
    guo_psc_nace[, PSC...Name := toupper(PSC...Name)]
  }
}

# Minimize before merge - only select columns that exist
guo_cols <- c("BvD.ID.number", "PSC...Name")
guo_cols <- guo_cols[guo_cols %in% names(guo_data)]
guo_data <- guo_data[, ..guo_cols]

guo_psc_cols <- c("PSC...Name", "guo_psc_nace")
guo_psc_cols <- guo_psc_cols[guo_psc_cols %in% names(guo_psc_nace)]
guo_psc_nace <- guo_psc_nace[, ..guo_psc_cols]

# Merge efficiently if both dataframes have required columns
francois_5 <- NULL
if ("PSC...Name" %in% guo_cols && "PSC...Name" %in% guo_psc_cols) {
  francois_5 <- merge(guo_data, guo_psc_nace, by = "PSC...Name", all.x = TRUE)
  if ("BvD.ID.number" %in% names(francois_5)) {
    francois_5 <- unique(francois_5[, .(BvD.ID.number, guo_psc_nace)])
    setnames(francois_5, "BvD.ID.number", "GUO...BvD.ID.number")
  }
}

# Clean up to free memory
rm(guo_data, guo_psc_nace)
gc()

# Process DUO dataframe with similar memory-efficient approach
duo_data <- fread("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/Francois_6_DUO_BVD_ID.csv")
duo_data <- clean_dataframe(duo_data)

# Read the DUO PSC NACE data
duo_psc_nace <- fread("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's care homes project/Data/francois_8_DUO_PSC_NACE.csv")

# Check which columns exist before renaming
if (all(c("Company.name", "Primary.NACE.Rev..2.code") %in% names(duo_psc_nace))) {
  cols_to_rename <- c("Company.name", "Primary.NACE.Rev..2.code")
  new_names <- c("PSC...Name", "duo_psc_nace")
  
  if ("Registered.number" %in% names(duo_psc_nace)) {
    cols_to_rename <- c(cols_to_rename, "Registered.number")
    new_names <- c(new_names, "psc_comp_house")
  }
  
  setnames(duo_psc_nace, cols_to_rename, new_names)
  
  if ("PSC...Name" %in% names(duo_psc_nace)) {
    duo_psc_nace[, PSC...Name := toupper(PSC...Name)]
  }
}

# Minimize before merge - only select columns that exist
duo_cols <- c("BvD.ID.number", "PSC...Name")
duo_cols <- duo_cols[duo_cols %in% names(duo_data)]
duo_data <- duo_data[, ..duo_cols]

duo_psc_cols <- c("PSC...Name", "duo_psc_nace")
duo_psc_cols <- duo_psc_cols[duo_psc_cols %in% names(duo_psc_nace)]
duo_psc_nace <- duo_psc_nace[, ..duo_psc_cols]

# Merge efficiently if both dataframes have required columns
francois_6 <- NULL
if ("PSC...Name" %in% duo_cols && "PSC...Name" %in% duo_psc_cols) {
  francois_6 <- merge(duo_data, duo_psc_nace, by = "PSC...Name", all.x = TRUE)
  if ("BvD.ID.number" %in% names(francois_6)) {
    francois_6 <- unique(francois_6[, .(BvD.ID.number, duo_psc_nace)])
    setnames(francois_6, "BvD.ID.number", "DUO...BvD.ID.number")
  }
}

# Clean up to free memory
rm(duo_data, duo_psc_nace)
gc()

# Add GUO and DUO NACE codes to main dataframe if they exist
if (!is.null(francois_5) && "GUO...BvD.ID.number" %in% names(francois)) {
  francois <- merge(francois, francois_5, by = "GUO...BvD.ID.number", all.x = TRUE)
}
rm(francois_5)
gc()

if (!is.null(francois_6) && "DUO...BvD.ID.number" %in% names(francois)) {
  francois <- merge(francois, francois_6, by = "DUO...BvD.ID.number", all.x = TRUE)
}
rm(francois_6)
gc()

# Remove unnecessary columns - check if they exist first
remove_pattern <- "(SH|GUO|DUO|CSH)\\.\\.\\.(UCI|Salutation)|CSH\\.\\.\\.(Information.source|Information.date)"
remove_cols <- grep(remove_pattern, names(francois), value = TRUE)
if(length(remove_cols) > 0) {
  francois[, (remove_cols) := NULL]
}

# Define keywords once
investment_keywords <- c("topco", "seniorco", "newco", "midco", "bidco", "opco",
                         "equity", "partner", "capital", "investment")
la_keywords <- c("council", "borough", "mayor", "authority")

# Create a function to check for keywords in multiple columns - with safety checks
check_keywords_in_columns <- function(dt, cols, keywords) {
  result <- rep(FALSE, nrow(dt))
  for (col in cols) {
    if (col %in% names(dt)) {
      col_values <- dt[[col]]
      if (is.factor(col_values)) {
        col_values <- as.character(col_values)
      }
      if (is.character(col_values)) {
        col_match <- grepl(paste(keywords, collapse = "|"), tolower(col_values))
        result <- result | col_match
      }
    }
  }
  return(result)
}

# Get all name columns that exist
name_cols <- grep("Name$", names(francois), value = TRUE)

# Create investment sector classification
francois[, Sector := NA_character_]

# First pass: Set sector based on keywords in names
investment_match <- check_keywords_in_columns(francois, name_cols, investment_keywords)
la_match <- check_keywords_in_columns(francois, name_cols, la_keywords)

francois[investment_match, Sector := "Investment owned"]
francois[!investment_match & la_match, Sector := "LA owned"]

# Second pass: Set sector based on NACE codes - only check columns that exist
nace_cols <- c("SH...NACE.Core.code", "CSH...NACE.Core.code", "DUO...NACE.Core.code", 
               "GUO...NACE.Core.code", "guo_psc_nace", "duo_psc_nace")
nace_cols <- nace_cols[nace_cols %in% names(francois)]

check_financial_nace <- function(code) {
  if (is.na(code)) return(FALSE)
  starts_with_financial <- grepl("^64|^65|^66", code)
  not_holding <- !grepl("^64\\.2", code)
  return(starts_with_financial & not_holding)
}

for (col in nace_cols) {
  if (!is.null(francois[[col]])) { # Extra safety check
    financial_matches <- sapply(francois[[col]], check_financial_nace)
    francois[is.na(Sector) & financial_matches, Sector := "Investment owned"]
  }
}

# Final pass: Check for individual ownership if the required columns exist
if (all(c("SH...Type", "SH...Direct..", "Company.name") %in% names(francois))) {
  francois[, total_individual := sum(
    ifelse(`SH...Type` == "One or more named individuals or families" & !is.na(`SH...Direct..`), 
           as.numeric(`SH...Direct..`), 0),
    na.rm = TRUE
  ), by = Company.name]
  
  francois[total_individual > 50, Sector := "individual"]
}

# Convert character columns to factors to save memory
for (col in names(francois)[sapply(francois, is.character)]) {
  francois[, (col) := as.factor(get(col))]
}

# Final cleaned dataset
setDF(francois)  # Convert back to data.frame if needed