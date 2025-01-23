
#######JOBSSS#########



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



greatermanchester <- df %>%
  dplyr::filter(Local.authority=="MANCHESTER"|
                  Local.authority=="WIGAN"|
                  Local.authority=="SALFORD"|
                  Local.authority=="STOCKPORT"|
                  Local.authority=="BURY"|
                  Local.authority=="BOLTON"|
                  Local.authority=="ROCHDALE"|
                  Local.authority=="OLDHAM"|
                  Local.authority=="TRAFFORD"|
                  Local.authority=="TAMESIDE")

save_df <- df %>% dplyr::filter(Sector!="Local Authority")%>%
  dplyr::select(Organisation_fame_search)%>%
  dplyr::filter(!is.na(Organisation_fame_search))%>%
  dplyr::distinct(.keep_all = T)%>%
  split(., (seq(nrow(.)) - 1) %/% 500)


#write.csv(save_df[1], "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/orgs1.csv")
#write.csv(save_df[2], "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/orgs2.csv")
#write.csv(save_df[3], "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/orgs3.csv")

####can we connect fame to ofsted?#####

test <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/co_house_nos_fame.csv")

lookup_test <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/lookup_fame_search_clean_manual.csv")%>%
  dplyr::mutate(Organisation_fame_search = Organisation_fame_search %>%
                  gsub("Selected", "", .),
                Company.name = Company.name %>%
                  gsub(" In liquidation", "", .)%>%
                  gsub(" Dissolved", "",.)%>%
                  str_trim())%>%
  dplyr::distinct(Organisation_fame_search, .keep_all = T)%>%
  dplyr::bind_rows(., data.frame(Organisation_fame_search = "BEACON CHILDCARE LTD", Company.name = "BEACON CHILDCARE LTD", stringsAsFactors = FALSE))



df_bind <- df %>% dplyr::full_join(., lookup_test, by="Organisation_fame_search")

df_bind <- df_bind %>% dplyr::full_join(., test, by="Company.name")

missing_obs <- df_bind[!is.na(df_bind$Organisation)&is.na(df_bind$Company.name) & df_bind$Sector!="Local Authority",]%>% 
  dplyr::distinct(URN, .keep_all=T)



full_look <- df_bind %>% dplyr::filter(Sector != "Local Authority")%>%
  dplyr::select(Organisation, Organisation_fame_search,Company.name, Registered.number) %>%
  dplyr::distinct()%>%
  dplyr::filter(Organisation!="")%>%
  dplyr::mutate(Registered.number = as.character(Registered.number))

#write.csv(full_look, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/lookup_fame_search_final.csv")


####foreign owned####

fame <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/foreign_investment.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>%
  fill(Company.name, .direction = "down") 

lookup <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/lookup_fame_search_clean_manual.csv")%>%
  dplyr::mutate(Organisation_fame_search = Organisation_fame_search %>%
                  gsub("Selected", "", .),
                Company.name = Company.name %>%
                  gsub(" In liquidation", "", .)%>%
                  gsub(" Dissolved", "",.)%>%
                  str_trim())%>%
  dplyr::distinct(Organisation_fame_search, .keep_all = T)%>%
  dplyr::bind_rows(., data.frame(Organisation_fame_search = "BEACON CHILDCARE LTD", Company.name = "BEACON CHILDCARE LTD", stringsAsFactors = FALSE))

shareholders_foreign <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/shareholders.csv") %>%
  dplyr::select(Company.name,SH...Name, SH...Country.ISO.code, SH...Direct.., SH...Type,SH...City)%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
  fill(Company.name, .direction = "down") %>%
  dplyr::filter(SH...Type!="One or more named individuals or families",
                SH...Type!="Other unnamed shareholders, aggregated",
                SH...Country.ISO.code!="GB"|str_detect(SH...City, regex("helier", ignore_case = TRUE)),
                SH...Country.ISO.code!="n.a."|str_detect(SH...City, regex("helier", ignore_case = TRUE)),
                SH...Country.ISO.code!=""|str_detect(SH...City, regex("helier", ignore_case = TRUE)),
                SH...Country.ISO.code!="-"|str_detect(SH...City, regex("helier", ignore_case = TRUE)))%>%
  dplyr::mutate(foreign_share=1)%>%
  dplyr::select(Company.name, foreign_share)



GUO_foreign <- fame %>%
  dplyr::select(Company.name,GUO...Name, GUO...City, GUO...Country.ISO.code, GUO...Type,GUO...NAICS..text.description)%>%
  dplyr::filter(Company.name!="",
                GUO...Type!="One or more named individuals or families",
                GUO...Type!="",
                GUO...Country.ISO.code!="GB"|str_detect(GUO...City, regex("helier", ignore_case = TRUE)),
                GUO...Country.ISO.code!="n.a."|str_detect(GUO...City, regex("helier", ignore_case = TRUE)),
                GUO...Country.ISO.code!=""|str_detect(GUO...City, regex("helier", ignore_case = TRUE)),
                GUO...Country.ISO.code!="-"|str_detect(GUO...City, regex("helier", ignore_case = TRUE))
  )%>%
  dplyr::mutate(foreign=1)%>%
  dplyr::select(GUO...Name, foreign)

GUO_holding_foreign <- fame %>%
  dplyr::select(Company.name,GUO...Name, GUO...City, GUO...Country.ISO.code, GUO...Type,GUO...NAICS..text.description, GUO...NACE.text.description)%>%
  dplyr::filter(Company.name!="",
                GUO...NACE.text.description=="Activities of holding companies",
                GUO...Country.ISO.code=="GB")

#write.csv(GUO_holding_foreign, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/holding_cos.csv")

fame_holdings <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/shareholders_GUOs.csv")%>%
  dplyr::select(Company.name,SH...Name, SH...Country.ISO.code, SH...Type,SH...City)%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
  fill(Company.name, .direction = "down") %>%
  dplyr::filter(SH...Type!="One or more named individuals or families",
                SH...Type!="Other unnamed shareholders, aggregated",
                SH...Country.ISO.code!="GB"|str_detect(SH...City, regex("helier", ignore_case = TRUE))|str_detect(SH...City, regex("Jersey", ignore_case = TRUE)),
                SH...Country.ISO.code!="n.a.",
                SH...Country.ISO.code!="",
                SH...Country.ISO.code!="-")%>%
  dplyr::mutate(foreign_holds=1)%>%
  dplyr::select(Company.name, foreign_holds)%>%
  dplyr::rename(GUO...Name = Company.name)





fame_duo <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/domestic_ultimate_owners.csv")

# write.csv(fame_duo %>%
#             dplyr::filter(DUO...Name!="") %>%
#             dplyr::select(DUO...Name, DUO...BvD.ID.number)%>%
#             dplyr::distinct(), 
#           "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/duo_list.csv")

fame_duo <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/domestic_ultimate_owners.csv")


# # Set up your Google API Key
# register_google(key = "")
# 
# 
# 
# 
# duo_pscs <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/duo_pscs_address.csv")%>%
#   mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
#   fill(Company.name, .direction = "down")%>%
#   dplyr::filter(PSC...Type!="Individual",
#                 PSC...Type!="",
#                 PSC...Type!="Legal person")%>%
#   rowwise() %>%
#   mutate(
#     geocode_result = list(geocode(PSC...Address, output = "all", source = "google")),
#     City = if (!is.null(geocode_result$results)) {
#       geocode_result$results[[1]]$address_components %>%
#         map_chr(~ if ("locality" %in% .x$types) .x$long_name else NA_character_) %>%
#         na.omit() %>%
#         unique() %>%
#         .[1] # First valid result
#     } else {
#       NA
#     },
#     Country = if (!is.null(geocode_result$results)) {
#       geocode_result$results[[1]]$address_components %>%
#         map_chr(~ if ("country" %in% .x$types) .x$long_name else NA_character_) %>%
#         na.omit() %>%
#         unique() %>%
#         .[1] # First valid result
#     } else {
#       NA
#     }
#   ) %>%
#   ungroup() %>%
#   select(-geocode_result) # Remove intermediate column





  
#write.csv(duo_pscs, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/duo_corporate_pscs_locations.csv")

duo_pscs <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/duo_corporate_pscs_locations.csv")%>%
  dplyr::mutate(foreign_psc = ifelse(Country == "United Kingdom", 0,1))%>%
  dplyr::select(Company.name, foreign_psc)%>%
  dplyr::rename(DUO...Name= Company.name)

fame_bind <- merge(fame, GUO_foreign, by="GUO...Name", all=T)
fame_bind <- merge(fame_bind, fame_holdings, by="GUO...Name", all=T)
fame_bind <- merge(fame_bind, shareholders_foreign, by= "Company.name", all=T)
fame_bind <- merge(fame_bind, duo_pscs, by= "DUO...Name", all=T)

fame_bind <- fame_bind %>%
  dplyr::mutate(
    foreign_any = ifelse(foreign == 1 | foreign_holds == 1 | foreign_share == 1 | foreign_psc == 1, 1, 0)
  )%>%
  dplyr::select(Company.name, foreign_any)%>%
  dplyr::group_by(Company.name)%>%
  dplyr::summarise(foreign_any = sum(foreign_any, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(
    foreign_any = ifelse(foreign_any>0, 1,0 ))%>%
  dplyr::filter(!is.na(Company.name))


df_bind <- df %>% dplyr::full_join(., lookup, by="Organisation_fame_search")

df_bind <- df_bind %>% dplyr::full_join(., fame_bind, by="Company.name")
df_bind <- df_bind%>%
  dplyr::distinct(URN, .keep_all=T)


missing_obs <- df_bind[!is.na(df_bind$Organisation)&is.na(df_bind$Company.name) & df_bind$Sector!="Local Authority",]%>% 
  dplyr::distinct(URN, .keep_all=T)

wuut <- df_bind[!is.na(df_bind$Organisation),]%>% 
  dplyr::distinct(URN, .keep_all=T)



df_bind <- df_bind %>%
  dplyr::mutate(foreign_any = ifelse(is.na(foreign_any),0,foreign_any),
  left = ifelse(is.na(Leave),0,1))


#### investment owned ####

fame <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/foreign_investment.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>%
  fill(Company.name, .direction = "down") 

lookup <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/lookup_fame_search_clean_manual.csv")%>%
  dplyr::mutate(Organisation_fame_search = Organisation_fame_search %>%
                  gsub("Selected", "", .),
                Company.name = Company.name %>%
                  gsub(" In liquidation", "", .)%>%
                  gsub(" Dissolved", "",.)%>%
                  str_trim())%>%
  dplyr::distinct(Organisation_fame_search, .keep_all = T)%>%
  dplyr::bind_rows(., data.frame(Organisation_fame_search = "BEACON CHILDCARE LTD", Company.name = "BEACON CHILDCARE LTD", stringsAsFactors = FALSE))


duo_sector <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/DUO_sectors.csv") %>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
  fill(Company.name, .direction = "down") %>%
  mutate(All.NACE.Rev..2.codes = as.character(All.NACE.Rev..2.codes)) %>%
  dplyr::filter(
    str_starts(All.NACE.Rev..2.codes, "64") |
      str_starts(All.NACE.Rev..2.codes, "65") |
      str_starts(All.NACE.Rev..2.codes, "66") |
      (is.na(All.UK.SIC..2003..codes) & is.na(All.NACE.Rev..2.codes) & is.na(All.NAICS.2017.codes))
  ) %>%
  filter(All.NACE.Rev..2.codes != "6420"|
  (is.na(All.UK.SIC..2003..codes) & is.na(All.NACE.Rev..2.codes) & is.na(All.NAICS.2017.codes)))%>%
  dplyr::filter(Company.status!="Active (dormant)")%>%
  dplyr::mutate(invest_duo=1)%>%
  dplyr::select(Company.name, invest_duo)%>%
  dplyr::rename(DUO...Name= Company.name)



shareholders_list <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/shareholders.csv") %>%
  dplyr::select(Company.name,SH...Name, SH...Country.ISO.code, SH...Direct.., SH...Type,SH...City)%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
  fill(Company.name, .direction = "down") %>%
  dplyr::filter(SH...Type!="One or more named individuals or families",
                SH...Type!="Other unnamed shareholders, aggregated",
                SH...Type!="")


#write.csv(shareholders_list, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/shareholderss_list.csv")


shareholders_sector <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/shareholders_sectors.csv") %>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
  fill(Company.name, .direction = "down") %>%
  dplyr::mutate(All.NACE.Rev..2.codes = as.character(All.NACE.Rev..2.codes))%>%
  dplyr::filter(
    str_starts(All.NACE.Rev..2.codes, "64") |
      str_starts(All.NACE.Rev..2.codes, "65") |
      str_starts(All.NACE.Rev..2.codes, "66") |
      (is.na(All.UK.SIC..2003..codes) & is.na(All.NACE.Rev..2.codes) & is.na(All.NAICS.2017.codes))
  ) %>%
  filter(All.NACE.Rev..2.codes != "6420"|
           (is.na(All.UK.SIC..2003..codes) & is.na(All.NACE.Rev..2.codes) & is.na(All.NAICS.2017.codes)))%>%
  dplyr::filter(Company.status!="Active (dormant)")%>%
  dplyr::mutate(invest_SH=1)%>%
  dplyr::select(Company.name, invest_SH)%>%
  dplyr::rename(SH...Name= Company.name)%>%
  dplyr::full_join(., shareholders_list%>%
                     dplyr::mutate(SH...Name = toupper(SH...Name)), by="SH...Name" )%>%####ERRORS here matching SH names #####
  dplyr::select(Company.name, invest_SH)%>%
  dplyr::filter(invest_SH==1)

# shareholders_core_sector <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/shareholders_core_sectors.csv") %>%
#   mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
#   fill(Company.name, .direction = "down") %>%
#   dplyr::mutate(SH...NACE.Core.code = as.character(SH...NACE.Core.code))%>%
#   dplyr::filter(SH...Name!="")%>%
#   dplyr::filter(
#     str_starts(SH...NACE.Core.code, "64") |
#       str_starts(SH...NACE.Core.code, "65") |
#       str_starts(SH...NACE.Core.code, "66") |
#       (SH...NACE.Core.code=="")
#   ) %>%
#   filter(SH...NACE.Core.code != "6420"|
#            (SH...NACE.Core.code==""))%>%
#   dplyr::mutate(invest_SH_core=1)%>%
#   dplyr::select(Company.name,SH...Name, invest_SH_core)
#   



duo_pscs_invest_link <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/duo_pscs_address.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
     fill(Company.name, .direction = "down")

psc_sector <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/psc_sectors.csv") %>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
  fill(Company.name, .direction = "down") %>%
  dplyr::mutate(All.NACE.Rev..2.codes = as.character(All.NACE.Rev..2.codes)) %>%
  dplyr::filter(
    str_starts(All.NACE.Rev..2.codes, "64") |
      str_starts(All.NACE.Rev..2.codes, "65") |
      str_starts(All.NACE.Rev..2.codes, "66") |
      (is.na(All.UK.SIC..2003..codes) & is.na(All.NACE.Rev..2.codes) & is.na(All.NAICS.2017.codes))
  ) %>%
  filter(All.NACE.Rev..2.codes != "6420"|
  (is.na(All.UK.SIC..2003..codes) & is.na(All.NACE.Rev..2.codes) & is.na(All.NAICS.2017.codes)))%>%
  dplyr::filter(Company.status!="Active (dormant)")%>%
  dplyr::mutate(invest_psc=1)%>%
  dplyr::rename(PSC...Name = Company.name)%>%
  dplyr::select(PSC...Name, invest_psc)%>%
  dplyr::full_join(., duo_pscs_invest_link%>%
                     dplyr::mutate(PSC...Name = toupper(PSC...Name)), by="PSC...Name")%>%
  dplyr::select(Company.name, invest_psc)%>%
  dplyr::rename(DUO...Name= Company.name)

  


GUO_sector <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/GUO_sectors.csv") %>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
  fill(Company.name, .direction = "down") %>%
  dplyr::mutate(All.NACE.Rev..2.codes = as.character(All.NACE.Rev..2.codes))%>%
  dplyr::filter(
    str_starts(All.NACE.Rev..2.codes, "64") |
      str_starts(All.NACE.Rev..2.codes, "65") |
      str_starts(All.NACE.Rev..2.codes, "66") |
      (is.na(All.UK.SIC..2003..codes) & is.na(All.NACE.Rev..2.codes) & is.na(All.NAICS.2017.codes))
  ) %>%
  filter(All.NACE.Rev..2.codes != "6420"|
           (is.na(All.UK.SIC..2003..codes) & is.na(All.NACE.Rev..2.codes) & is.na(All.NAICS.2017.codes)))%>%
  dplyr::filter(Company.status!="Active (dormant)")%>%
  dplyr::mutate(invest_guo = 1)%>%
  dplyr::select(Company.name, invest_guo)%>%
  dplyr::rename(GUO...Name = Company.name)



# GUO_shareholders_list <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/shareholders_GUOs.csv") %>%
#   dplyr::select(Company.name,SH...Name, SH...Country.ISO.code, SH...Type,SH...City)%>%
#   mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
#   fill(Company.name, .direction = "down") %>%
#   dplyr::filter(SH...Type!="One or more named individuals or families",
#                 SH...Type!="Other unnamed shareholders, aggregated",
#                 SH...Type!="")
# 


# write.csv(GUO_shareholders_list, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/GUO_shareholderss_list.csv")

GUO_shareholders <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/shareholders_GUOs.csv") %>%
   mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
   fill(Company.name, .direction = "down") %>%
   dplyr::filter(SH...Type!="One or more named individuals or families",
                 SH...Type!="Other unnamed shareholders, aggregated",
                 SH...Type!="")%>%
  dplyr::mutate(SH...NACE.Core.code = as.character(SH...NACE.Core.code))%>%
  dplyr::filter(
    str_starts(SH...NACE.Core.code, "64") |
      str_starts(SH...NACE.Core.code, "65") |
      str_starts(SH...NACE.Core.code, "66") |
      ((SH...NACE.Core.code==""))
  ) %>%
  filter(SH...NACE.Core.code != "6420"|
           ((SH...NACE.Core.code=="")))%>%
  dplyr::mutate(invest_guo_sh = 1)%>%
  dplyr::select(Company.name, invest_guo_sh)%>%
  dplyr::rename(GUO...Name = Company.name)
  


####okay come back here, I think we re-run searches for shareholders with ID's not names pls benny boi####
GUO_shareholders_two <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/shareholders_GUOs_sector.csv")%>%
  mutate(Company.name = ifelse(Company.name == "", NA, Company.name)) %>% # Replace blanks with NA
  fill(Company.name, .direction = "down") %>%
  dplyr::filter(SH...Type!="One or more named individuals or families",
                SH...Type!="Other unnamed shareholders, aggregated",
                SH...Type!="")%>%
  dplyr::filter()





invest_fame_bind <- merge(fame, duo_sector, by="DUO...Name", all=T)
invest_fame_bind <- merge(invest_fame_bind, shareholders_sector, by="Company.name", all=T)
invest_fame_bind <- merge(invest_fame_bind, psc_sector, by= "DUO...Name", all=T)
invest_fame_bind <- merge(invest_fame_bind, GUO_sector, by= "GUO...Name", all=T)
invest_fame_bind <- merge(invest_fame_bind, GUO_shareholders, by= "GUO...Name", all=T)

invest_fame_bind <- invest_fame_bind %>%
  dplyr::mutate(
    invest_any = ifelse(invest_guo_sh == 1 | invest_guo == 1 | invest_psc == 1 | invest_duo == 1 | invest_SH == 1, 1, 0)
  )%>%
  dplyr::select(Company.name, invest_any)%>%
  dplyr::group_by(Company.name)%>%
  dplyr::summarise(invest_any = sum(invest_any, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(
    invest_any = ifelse(invest_any>0, 1,0 ))%>%
  dplyr::filter(!is.na(Company.name))



df_bind <- df_bind %>% dplyr::full_join(., invest_fame_bind, by="Company.name")
df_bind <- df_bind%>%
  dplyr::distinct(URN, .keep_all=T)


missing_obs <- df_bind[!is.na(df_bind$Organisation)&is.na(df_bind$Company.name) & df_bind$Sector!="Local Authority",]%>% 
  dplyr::distinct(URN, .keep_all=T)

wuut <- df_bind[!is.na(df_bind$Organisation),]%>% 
  dplyr::distinct(URN, .keep_all=T)



df_bind <- df_bind %>%
  dplyr::mutate(invest_any = ifelse(is.na(invest_any),0,invest_any))



obvious_misses <- df_bind %>%
  dplyr::filter(invest_any==0,
                foreign_any==1)

obvious_success <- df_bind %>%
  dplyr::filter(invest_any==1,
                foreign_any==0)


df_bind <- df_bind %>%
  dplyr::full_join(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/corporate_group_no.csv"), by="Company.name")
  
df_bind <- df_bind %>%
  dplyr::full_join(., read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/profit_ratios.csv"), by="Company.name")



#### Analysis####

#### Descriptive rate of openings, closures, churn, by ownership ####

#### likelihood of being in low need area ####

low_need <- df_bind %>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
                     # dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")%>%
                     dplyr::filter(variable=="Net gain of children by responsible LA"&year==2023)%>%
                     dplyr::rename(Local.authority = LA_Name),
                   by="Local.authority")%>%
  dplyr::left_join(.,read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(variable=="Total" &subcategory=="Placement"&year==2023) %>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care = number)%>%
                     dplyr::select(Local.authority, year, children_in_care), 
                   by="Local.authority")%>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
                     # dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")%>%
                     dplyr::filter(variable=="Placed outside the local authority boundary"&year==2023)%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   outside=percent,
                                   outsideno=number),
                   by="Local.authority")%>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
                     # dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")%>%
                     dplyr::filter(variable=="1. Children who are the responsibility of other LAs placed within this LA boundary"&year==2023)%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   others_inside=number),
                   by="Local.authority")%>%
  dplyr::left_join(.,  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))%>%
                     dplyr::select(Local.authority, IMD.2019...Extent, Average_house_price)%>%
                     dplyr::distinct(.keep_all = T), by="Local.authority")%>%
  dplyr::distinct(URN, .keep_all=T)%>%
  dplyr::mutate(net_gain = as.numeric(number),
                high_net_gain = ifelse(net_gain>(as.numeric(children_in_care)/2),1,0),
                high_need_areas = ifelse(outside>50,1,0),
                low_cost_areas = ifelse(Average_house_price<150000,1,0))%>%
  dplyr::mutate(  joined = ifelse(is.na(Join),0,1))%>%
  dplyr::mutate(
    age = as.Date(Registration.date, format = "%d/%m/%Y"),
    month = format(age, "%m/%y"),
    year = format(age, "%Y"),
    age_months = time_length(difftime(as.Date("2021-06-01"), age), "months"),
    age_years = time_length(difftime(as.Date("2021-06-01"), age), "years")
  )%>%
  dplyr::mutate(rating_numeric = ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Outstanding", 4,
                                        ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Good", 3,
                                               ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Requires improvement to be good", 2,
                                                      ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Inadequate", 1,NA
                                                      )))))
  



forlow <- ggplot(
  low_need %>% filter(foreign_any == 1), 
  aes(x = as.factor(foreign_any), y = as.numeric(outside))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Foreign Owned") +
  xlab("") +
  ylab("Outside of area placements %")+
  coord_cartesian(ylim = c(0,85))

investlow <- ggplot(
  low_need %>% filter(invest_any == 1), 
  aes(x = as.factor(invest_any), y = as.numeric(outside))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Investment Owned") +
  xlab("") +
  ylab("Outside of area placements %")+
  coord_cartesian(ylim = c(0,85))


lalow <- ggplot(
  low_need %>% filter(Sector=="Local Authority"), 
  aes(x = as.factor(Sector), y = as.numeric(outside))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Local Authority") +
  xlab("") +
  ylab("Outside of area placements %")+
  coord_cartesian(ylim = c(0,85))

vollow <- ggplot(
  low_need %>% filter(Sector=="Voluntary"), 
  aes(x = as.factor(Sector), y = as.numeric(outside))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Third Sector") +
  xlab("") +
  ylab("Outside of area placements %")+
  coord_cartesian(ylim = c(0,85))

fplow <- ggplot(
  low_need %>% filter(Sector=="Private"), 
  aes(x = as.factor(Sector), y = as.numeric(outside))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("All for-profit") +
  xlab("") +
  ylab("Outside of area placements %")+
  coord_cartesian(ylim = c(0,85))



cowplot::plot_grid(lalow, vollow, fplow, investlow, forlow, nrow=1)



forlow <- ggplot(
  low_need %>% filter(foreign_any == 1), 
  aes(x = as.factor(foreign_any), y = as.numeric(Profit.margin.Last.avail..yr))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Foreign Owned") +
  xlab("") +
  ylab("Profit margin %")+
  coord_cartesian(ylim = c(0,85))

investlow <- ggplot(
  low_need %>% filter(invest_any == 1), 
  aes(x = as.factor(invest_any), y = as.numeric(Profit.margin.Last.avail..yr))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Investment Owned") +
  xlab("") +
  ylab("Profit margin %")+
  coord_cartesian(ylim = c(0,85))


lalow <- ggplot(
  low_need %>% filter(Sector=="Local Authority"), 
  aes(x = as.factor(Sector), y = as.numeric(Profit.margin.Last.avail..yr))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Local Authority") +
  xlab("") +
  ylab("Profit margin %")+
  coord_cartesian(ylim = c(0,85))

vollow <- ggplot(
  low_need %>% filter(Sector=="Voluntary"), 
  aes(x = as.factor(Sector), y = as.numeric(Profit.margin.Last.avail..yr))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Third Sector") +
  xlab("") +
  ylab("Profit margin %")+
  coord_cartesian(ylim = c(0,85))

fplow <- ggplot(
  low_need %>% filter(Sector=="Private",
                      foreign_any==0,
                      invest_any==0), 
  aes(x = as.factor(Sector), y = as.numeric(Profit.margin.Last.avail..yr))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Other for-profit") +
  xlab("") +
  ylab("Profit margin %")+
  coord_cartesian(ylim = c(0,85))



cowplot::plot_grid(lalow, vollow,fplow, investlow, forlow, nrow=1)




forlow <- ggplot(
  low_need %>% filter(foreign_any == 1), 
  aes(x = as.factor(foreign_any), y = as.numeric(log(Average_house_price)))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Foreign Owned") +
  xlab("") +
  ylab("Average house price (logged)")+
  coord_cartesian(ylim = c(11.5,14))

investlow <- ggplot(
  low_need %>% filter(invest_any == 1), 
  aes(x = as.factor(invest_any), y = as.numeric(log(Average_house_price)))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Investment Owned") +
  xlab("") +
  ylab("Average house price (logged)")+
  coord_cartesian(ylim = c(11.5,14))


lalow <- ggplot(
  low_need %>% filter(Sector=="Local Authority"), 
  aes(x = as.factor(Sector), y = as.numeric(log(Average_house_price)))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Local Authority") +
  xlab("") +
  ylab("Average house price (logged)")+
  coord_cartesian(ylim = c(11.5,14))

vollow <- ggplot(
  low_need %>% filter(Sector=="Voluntary"), 
  aes(x = as.factor(Sector), y = as.numeric(log(Average_house_price)))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("Third Sector") +
  xlab("") +
  ylab("Average house price (logged)")+
  coord_cartesian(ylim = c(11.5,14))

fplow <- ggplot(
  low_need %>% filter(Sector=="Private"), 
  aes(x = as.factor(Sector), y = as.numeric(log(Average_house_price)))
) +
  geom_violin(width = 0.8, fill = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    axis.text.x = element_blank()
  ) +
  ggtitle("All for-profit") +
  xlab("") +
  ylab("Average house price (logged)")+
  coord_cartesian(ylim = c(11.5,14))



cowplot::plot_grid(lalow, vollow, fplow, investlow, forlow, nrow=1)






cowplot::plot_grid(lalow, vollow, fplow, investlow, forlow, nrow=1)



#ERROR HERE INVESTMENT AND FOREIGN EXCLUSIve
low_need_plot <- low_need %>%
  mutate(
    Ownership = case_when(
      Sector == "Local Authority" ~ "Local Authority",
      Sector == "Voluntary" ~ "Third Sector",
      foreign_any == 1 ~ "Foreign Owned",
      invest_any == 1 ~ "Investment Owned",
      TRUE ~ "Other private"
    )
  ) %>%
  mutate(Ownership = factor(Ownership, levels = c("Local Authority", "Third Sector", "Other private", "Investment Owned", "Foreign Owned")))%>%
  group_by(Ownership) %>%
  summarise(
    Total = n(),  # Total number of homes
    HighNeed = sum(high_need_areas, na.rm=T),
    lowcost= sum(low_cost_areas, na.rm=T)  # Homes in high-need areas
  ) %>%
  mutate(Percentage = (HighNeed / Total) * 100,
         Percentage_low_cose = (lowcost / Total) * 100)  # Percentage in high-need areas

# Create a bar plot
ggplot(low_need_plot, aes(x = Ownership, y = Percentage, fill = Ownership)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Percentage of Each Ownership Type Located in High-Need Areas",
    x = "Ownership Type",
    y = "Percentage of homes located in areas >50% sent out of area (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

# Create a bar plot
ggplot(low_need_plot, aes(x = Ownership, y = Percentage_low_cose, fill = Ownership)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Percentage of Each Ownership Type Located in Low-Cost Areas",
    x = "Ownership Type",
    y = "Percentage of homes located in areas with low housing cost (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

####regressions####

summary(lm(as.numeric(outside)~Places+factor(left)+foreign_any+rating_numeric+age_months, data=low_need))
summary(lm(as.numeric(high_need_areas)~Sector, data=low_need))


summary(lm(as.numeric(others_inside)~Sector+Places+factor(left)+foreign_any+rating_numeric+age_months, data=low_need))



ggplot(unique(low_need[c("others_inside", "outside")]), aes(x=as.numeric(others_inside), y=as.numeric(outside)))+
  geom_point()


summary(lm(as.numeric(net_gain)~as.numeric(outside) +IMD.2019...Extent+Average_house_price, data=unique(low_need[c("net_gain", "outside", "IMD.2019...Extent", "Average_house_price")])))
summary(lm(as.numeric(net_gain)~as.numeric(outside) , data=unique(low_need[c("net_gain", "outside", "IMD.2019...Extent", "Average_house_price")])))

summary(lm(as.numeric(high_net_gain)~Sector+Places+factor(left)+foreign_any+rating_numeric+age_months, data=low_need))

v







































ggplot(low_need, aes(x=factor(invest_any), y=as.numeric(number)))+
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of Children's Homes by LA Net Gain") +
  xlab("Investment-Backed")+
  ylab("Net Gain of LA")

ggplot(low_need%>%dplyr::filter(!is.na(Sector)), aes(x=factor(Sector), y=as.numeric(number)))+
  geom_violin(width=1.4)+
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of Children's Homes by LA Net Gain") +
  xlab("")+
  ylab("Net Gain of LA")

ggplot(low_need, aes(x=No.of.companies.in.corporate.group, y=log(as.numeric(number))))+
  geom_point()+
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  geom_smooth(method="lm")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of Children's Homes by LA Net Gain") +
  xlab("Size of Corporate Group")+
  ylab("Net Gain of LA")


ggplot(low_need, aes(x=factor(invest_any), y=as.numeric(Salaries..Turnover.Last.avail..yr)))+
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of Children's Homes by LA Net Gain") +
  xlab("Investment-Backed")+
  ylab("Salaries per Turnover")

ggplot(low_need, aes(x=factor(invest_any), y=as.numeric(EBITDA.margin.Last.avail..yr)))+
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of Children's Homes by LA Net Gain") +
  xlab("Investment-Backed")+
  ylab("EBITDA Margin")

ggplot(low_need, aes(x=factor(invest_any), y=as.numeric(Profit.margin.Last.avail..yr)))+
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of Children's Homes by LA Net Gain") +
  xlab("Investment-Backed")+
  ylab("Profit Margin")

ggplot(low_need, aes(x=factor(foreign_any), y=as.numeric(Salaries..Turnover.Last.avail..yr)))+
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of Children's Homes by LA Net Gain") +
  xlab("Foreign-Backed")+
  ylab("Salaries per turnover")

summary(lm((as.numeric(number))~factor(foreign_any)+factor(invest_any)+factor(Sector), data=low_need%>%dplyr::filter(is.na(Leave))))

summary(lm(as.numeric(number)~factor(foreign_any)+factor(invest_any), data=low_need%>%dplyr::filter(Sector=="Private")))


summary(glm((factor(joined))~factor(foreign_any)+factor(invest_any)+factor(Sector), data=low_need%>%dplyr::filter(is.na(Leave)), family="binomial"))
summary(glm((factor(joined))~factor(foreign_any)+factor(invest_any)+factor(Sector), data=low_need%>%dplyr::filter(is.na(Leave)), family="binomial"))
summary(glm((factor(left))~factor(foreign_any)+factor(invest_any)+factor(Sector), data=low_need%>%dplyr::filter(!is.na(Organisation)), family="binomial"))








inspections <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/refs/heads/main/Final_Data/outputs/Provider_data.csv"))

inspections <-unique(inspections %>% full_join(., df_bind, by="URN"))

#inspections <- inspections %>% distinct(Event.number, .keep_all = T)

inspections <- inspections %>%
  dplyr::mutate(rating_numeric = ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Outstanding", 4,
                                        ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Good", 3,
                                               ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Requires improvement to be good", 2,
                                                      ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Inadequate", 1,NA
                                                      )))))

summary(lm(rating_numeric~factor(foreign_any)+factor(invest_any)+factor(Sector.x)+factor(left), data=inspections))
summary(lm(rating_numeric~No.of.companies.in.corporate.group+factor(foreign_any)+factor(invest_any)+Places.x, data=inspections))


check_LA = df_bind %>%
  dplyr::filter(Sector=="Local Authority")%>%
  dplyr::select(Local.authority,Organisation, URN)%>%
  dplyr::mutate(ifelse(Local.authority!= Organisation, 1,0))










#















fame <- merge(read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/Export 16_10_2024 17_55.csv"),
  read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/just_duo_type (1).csv"),
  by="Company.name", all=T)

lookup <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/lookup_fame_search_clean_manual.csv")%>%
  dplyr::mutate(Organisation_fame_search = Organisation_fame_search %>%
                  gsub("Selected", "", .),
                Company.name = Company.name %>%
                  gsub(" In liquidation", "", .)%>%
                  gsub(" Dissolved", "",.)%>%
                  str_trim())%>%
  dplyr::distinct(Organisation_fame_search, .keep_all = T)%>%
  dplyr::bind_rows(., data.frame(Organisation_fame_search = "BEACON CHILDCARE LTD", Company.name = "BEACON CHILDCARE LTD", stringsAsFactors = FALSE))


#write.csv(lookup_test, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/lookup_fame_search_clean.csv")

df_bind <- df %>% dplyr::full_join(., lookup, by="Organisation_fame_search")

df_bind <- df_bind %>% dplyr::full_join(., fame, by="Company.name")

missing_obs <- df_bind[!is.na(df_bind$Organisation)&is.na(df_bind$Company.name) & df_bind$Sector!="Local Authority",]%>% 
  dplyr::distinct(URN, .keep_all=T)

# global_owners <- df_bind %>%
#   dplyr::select(GUO...Name)%>%
#   dplyr::distinct()%>%
#   dplyr::filter(!is.na(GUO...Name),
#                 GUO...Name!="")%>%
#   write.csv(., "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/global_owners.csv")

df_bind <- df_bind %>% 
  dplyr::mutate(joined = ifelse(is.na(Join),0,1),
                left = ifelse(is.na(Leave),0,1),
                foreign = ifelse(GUO...Country.ISO.code=="GB", 0,
                                 ifelse(GUO...Country.ISO.code=="", 0,
                                        ifelse(GUO...Country.ISO.code=="n.a.", 0,
                                               ifelse(GUO...Country.ISO.code=="-", 0,
                                                      ifelse(is.na(GUO...Country.ISO.code), 0,1
                                                      ))))))



summary(lm(left~(foreign), data=df_bind))


greatermanchester <- greatermanchester%>% dplyr::left_join(., lookup_test, by="Organisation_fame_search")
greatermanchester <- greatermanchester%>% dplyr::left_join(., fame, by="Company.name")

write_csv(greatermanchester%>%
            dplyr::select(Sector,URN,Places,Local.authority,Organisation,Organisation_fame_search,Company.name,Join,Leave,Registered.number)%>%
            dplyr::mutate(Registered.number = as.character(Registered.number))%>%
            dplyr::filter(is.na(Leave)), "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/Data/GM_children_homes.csv")

greatermanchester%>%
  dplyr::filter(is.na(Leave))%>%
  dplyr::select(Sector,Places,Local.authority,No.of.companies.in.corporate.group,Entity.type,Salaries..Turnover.Last.avail..yr,EBITDA.margin.Last.avail..yr,Profit.margin.Last.avail..yr,GUO...Type,GUO...Country.ISO.code, GUO...NAICS..text.description)%>%
  dplyr::mutate(Salaries..Turnover.Last.avail..yr = as.numeric(Salaries..Turnover.Last.avail..yr),
                EBITDA.margin.Last.avail..yr = as.numeric(EBITDA.margin.Last.avail..yr),
                Profit.margin.Last.avail..yr = as.numeric(Profit.margin.Last.avail..yr),
  )%>%
  gtsummary::tbl_summary(
    by = c(Local.authority), # split table by group
    missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{N_nonmiss}",
                                     "{mean} ({median})", 
                                     "{min}, {max} ({sd})")# don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  #add_overall() %>%
  modify_caption("**Table. Summary of care home companies by GM LA**") %>%
  modify_header(label = "**Variable**")%>%
  bold_labels()%>%
  as_gt()%>%
  gtsave(., "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Care-Markets/figures/GM_ch_homes.html")

####do scumbags enter high net gain areas?####


joiners <- df_bind %>%
  dplyr::filter(joined==1)%>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
                    # dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")%>%
                     dplyr::filter(variable=="Net gain of children by responsible LA"&year==2023)%>%
                     dplyr::rename(Local.authority = LA_Name),
                   by="Local.authority")%>%
  dplyr::distinct(URN, .keep_all=T)


summary(lm(as.numeric(number)~(No.of), data=joiners))






inspections <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/refs/heads/main/Final_Data/outputs/Provider_data.csv"))

hahaha <-unique(inspections %>% full_join(., df_bind, by="URN"))

#hahaha <- hahaha %>% distinct(Event.number, .keep_all = T)

hahaha <- hahaha %>%
  dplyr::mutate(rating_numeric = ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Outstanding", 1,
                                        ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Good", 2,
                                               ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Requires improvement to be good", 3,
                                                      ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Inadequate", 4,NA
                                                      )))))



####dunnnnnnooooooo fuck ja , make start and end times, then count them baby####





wtf <- df %>%
  dplyr::arrange(Places)%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::mutate(
    Homes_2014 = ifelse(is.na(Join), 1,0),
    Places_2014 =ifelse(is.na(Join), Places, 0),
    Homes= ifelse(!is.na(Join)&is.na(Leave), 1,
                              ifelse(!is.na(Leave)&is.na(Join), -1,
                                     0)),
    Places= ifelse(!is.na(Join)&is.na(Leave), Places,
                              ifelse(!is.na(Leave)&is.na(Join), Places*-1,
                                     0)))%>%
  dplyr::select(Local.authority, Homes, Places,Sector, Homes_2014, Places_2014)%>%
  dplyr::group_by(Local.authority, Sector)%>%
  dplyr::summarise(Homes = sum(Homes),
                   Places = sum(Places),
                   Homes_2014 = sum(Homes_2014),
                   Places_2014 = sum(Places_2014))%>%
  dplyr::ungroup()%>%
  tidyr::complete(Local.authority, Sector = c("Private", "Local Authority", "Voluntary"))%>%
  dplyr::mutate(Homes=ifelse(is.na(Homes), 0, Homes),
                Places=ifelse(is.na(Places), 0, Places),
                Homes_2014=ifelse(is.na(Homes_2014), 0, Homes_2014),
                Places_2014=ifelse(is.na(Places_2014), 0, Places_2014))

all_sector <- wtf %>%
  group_by(Local.authority) %>%
  summarize(
    Sector = "All",
    Homes = sum(Homes, na.rm = TRUE),
    Places = sum(Places, na.rm = TRUE),
    Homes_2014 = sum(Homes_2014, na.rm = TRUE),
    Places_2014 = sum(Places_2014, na.rm = TRUE)
  )

# Combine the original data with the new rows
yeskid <- bind_rows(wtf, all_sector) %>%
  arrange(Local.authority, Sector)%>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))%>%
                                      dplyr::select(Local.authority, IMD.2019...Extent, Average_house_price)%>%
                                      dplyr::distinct(.keep_all = T), by="Local.authority")%>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
                     dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")%>%
                     dplyr::filter(variable=="Net gain of children by responsible LA"&year==2023)%>%
                     dplyr::rename(Local.authority = LA_Name),
                   by="Local.authority")%>%
  dplyr::full_join(.,read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(variable=="Total" &subcategory=="Placement") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care = number)%>%
                     dplyr::select(Local.authority, year, children_in_care), by=c("Local.authority", "year"))%>%
  dplyr::mutate(per = Homes/as.numeric(number),
                places_change = Places/ Places_2014,
                homes_change = Homes/Homes_2014)%>%
  dplyr::filter(!str_detect(Local.authority, "(?i)northampton"))%>%
  dplyr::left_join(., read.csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/FOI_2024-0014264_part_1.csv", skip=15) %>%
  dplyr::rename(Local.authority = la_name,
                unregulated = number,
                year= time_period)%>%
  dplyr::filter(new_la_code!="E10000009",
                year==2023)%>%
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
                  str_trim()), by="Local.authority")
  
  
  ggplot(yeskid[!is.na(yeskid$Sector),], aes(y=Homes/Homes_2014*100, x=as.numeric(Average_house_price)))+
    geom_smooth(method = "lm")+
    geom_point(color = "black", alpha = 0.5)+
    facet_wrap(~Sector)+
    geom_hline(yintercept = 0, linetype="dashed")+
    theme_bw()+
  facet_grid(~Sector)+theme_bw()+
  labs(x = "Net gain (children, 2023)", y= "Percentage change in care homes 2014-23\n(%)")
  
  yeskid <- yeskid[!is.na(yeskid$Sector),]
  
  
  
  
  ####map####
  
  
  theme_map <- function(...) {
    theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #    panel.background = element_rect(fill = "transparent"), # bg of the panel
        #     plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        # legend.background = element_rect(fill = "transparent", color=NA), # get rid of legend bg
        #  legend.box.background = element_rect(fill = "transparent", color=NA),
        # panel.border = element_blank(),legend.title=element_text(size=8), 
        #  legend.text=element_text(size=7),legend.key.size = unit(0.3, "cm"),
        ...
      )
  }
  
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
    dplyr::full_join(., wtf %>%
                       dplyr::filter(Sector=="Private")%>%
                       dplyr::select(Local.authority, Places),
                     by="Local.authority")%>%
    st_as_sf(.)
  
  
  
  
  no_classes <- 6
  
  
  quantiles <- quantile(as.double(map$Places), 
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
  
  
  
  
  a <- unique(map) %>%  dplyr::mutate(unreg_quantiles =cut(Places, 
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
      name = "Change in for-profit ch. home places\n(2014-2023)", na.value="grey")+
    theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))
  
  
  
  
  
  
  
llll <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
  dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")%>%
  dplyr::filter(variable=="Total"&subcategory=="Age group"&year==2023)


####Names cleaning code ####
# Load necessary packages
library(tidyverse)
library(curl)

options(scipen = 999) # Disable scientific notation
# Load necessary packages
library(tidyverse)
library(curl)

options(scipen = 999) # Disable scientific notation

# List of URLs for datasets
urls <- c(
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_2122_fin.csv",
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1920.csv",
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1819.csv",
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1718.csv",
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1617.csv",
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1516.csv",
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1415.csv",
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_exccarehomes_inspec_1314.csv",
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/Carehomes_inspec_14_1.csv",
  "https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/Carehomes_inspec_14_2.csv"
)

# Function to rename columns before merging
rename_columns <- function(df) {
  df %>%
    rename(
      Outcomes.in.education.and.related.learning.activities = `Outcomes.in.education.and.related.learning.activities`,
      Organisation.which.owns.the.provider = `Organisation.which.owns.the.provider`,
      Quality.of.care = `Quality.of.care`
      # Add any other necessary column renaming here
    )
}

# Read, rename, and combine datasets while converting to character type
ProviderData <- urls %>%
  map_dfr(~ {
    read.csv(curl(.x), stringsAsFactors = FALSE) %>%
      rename_columns() %>%                # Rename columns
      mutate(across(everything(), as.character))  # Convert all columns to character
  }, .id = "source")

# Check structure to confirm all columns are characters
str(ProviderData)

# Final clean up
rm(list=setdiff(ls(), c("ProviderData")))








####test difference to old option####

exits <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))


leaves <- exits %>%
  dplyr::arrange(Places)%>%
  dplyr::filter(leave_join=="Leave")%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::select(leave_join, imd_decile, Sector,IMD.2019...Extent, Places)%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(imd_decile,IMD.2019...Extent, Sector )%>%
  dplyr::summarise(homes = sum(yes),
                   places = sum(Places, na.rm=T))%>%
  dplyr::ungroup()

leaves <- leaves %>%
  dplyr::full_join(., tidyr:: expand_grid(leaves %>% 
                                            dplyr::select(IMD.2019...Extent)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("IMD.2019...Extent", "Sector"))


joins <- exits %>%
  dplyr::arrange(Places)%>%
  dplyr::filter(leave_join=="Join")%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::select(leave_join, imd_decile, Sector,IMD.2019...Extent, Places)%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(imd_decile,IMD.2019...Extent, Sector )%>%
  dplyr::summarise(homes_j = sum(yes),
                   places_j = sum(Places, na.rm=T))%>%
  dplyr::ungroup()

joins <- joins %>%
  dplyr::full_join(., tidyr:: expand_grid(joins %>% 
                                            dplyr::select(IMD.2019...Extent)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("IMD.2019...Extent", "Sector"))


plot <- dplyr::full_join(leaves, joins, by=c("IMD.2019...Extent", "Sector"))%>%
  dplyr::mutate(homes_j = ifelse(is.na(homes_j), 0, homes_j),
                places_j = ifelse(is.na(places_j), 0, homes_j),
                homes = ifelse(is.na(homes), 0, homes),
                places = ifelse(is.na(places), 0, places),
                net = homes_j-homes,
                Sector = ifelse(Sector=="Private", "For-profit",
                                ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))




plot <- merge(plot, exits[c("IMD.2019...Extent", "Local.authority")], by="IMD.2019...Extent")

yes <- unique(plot[c("Local.authority", "Sector", "net")])
yes2 <- unique(wtf[c("Local.authority", "Sector", "Homes")])%>%dplyr::mutate(Sector = ifelse(Sector=="Private", "For-profit",
                                                                                             ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))


what <- merge(yes, yes2, by=c("Local.authority", "Sector"), all=T)

ggplot(what, aes(x=Homes, y=net, colour=Sector))+
  geom_point(alpha=0.5)+
  labs(y="Net change in care homes 2016-23\n(gives us association with dep)",
       x = "Net change in care homes 2014-23\n(gives us zero association with dep)")+
  theme_bw()
  #geom_label(label = what$Local.authority, position = "identity")









####earlier version####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)

la_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
  dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")
exits <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))
#ProviderData = read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/Provider_data.csv"))
#carehomes <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/Children's Care Homes Project/Data/Ben report dates.csv")

####ope close pre 2018####

pre <- rbind(
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 3, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 4, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 5, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 6, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 7, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 8, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 9, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 10, skip=3) %>%
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
               dplyr::filter(as.Date(Date)>="2018-04-01")
)%>%  
  dplyr::distinct(URN, .keep_all = T)#%>%
#   tidyr::pivot_wider(id_cols = c("Local.authority", "Sector", "URN", "Places"), names_from = "leave_join", values_from = as.character("Date"))%>%
#   dplyr::mutate(Join = substr(Join, 1, 10),
#                 Leave = substr(Leave, 1, 10)
#                 
# )



####provider_data####

df <-rbind( read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2015.csv"))%>%
              dplyr::mutate(year=2015,
                            Places= NA)%>%
              dplyr::rename(Registration.status=Reg.Status)%>%
              filter(Provision.type=="Children's Home",
                     Registration.status=="Active")%>%
              dplyr::select(Sector, year, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2016.csv"), skip=1)%>%
              dplyr::mutate(year=2016)%>%
              filter(Provision.type=="Children's home",
                     Registration.status=="Active")%>%
              dplyr::select(Sector, year, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2017.csv"), skip=1)%>%
              dplyr::mutate(year=2017)%>%
              filter(Provision.type=="Children's home",
                     Registration.status=="Active")%>%
              dplyr::select(Sector, year, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018.csv"), skip=1)%>%
              dplyr::mutate(year=2018)%>%
              filter(Provision.type=="Children's home",
                     Registration.status=="Active")%>%
              dplyr::select(Sector, year, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2019.csv"), skip=1)%>%
              dplyr::mutate(year=2019)%>%
              filter(Provision.type=="Children's home",
                     Registration.status=="Active")%>%
              dplyr::select(Sector, year, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2020.csv"), skip=1)%>%
              dplyr::mutate(year=2020)%>%
              filter(Provision.type=="Children's home",
                     Registration.status=="Active")%>%
              dplyr::select(Sector, year, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2021.csv"), skip=1)%>%
              dplyr::mutate(year=2021)%>%
              filter(Provision.type=="Children's home",
                     Registration.status=="Active")%>%
              dplyr::select(Sector, year, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2022_yep.csv"), skip=4)%>%
              dplyr::mutate(year=2022)%>%
              filter(Provision.type=="Children's home",
                     Registration.status=="Active")%>%
              dplyr::select(Sector, year, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2023.csv"), skip=3)%>%
              dplyr::mutate(year=2023)%>%
              filter(Provision.type=="Children's home",
                     Registration.status=="Active")%>%
              dplyr::select(Sector, year, Places, Local.authority))%>%
  dplyr::mutate(Sector= ifelse(Sector=="Health Authority", "Local Authority", Sector),
                Homes=1)%>%
  dplyr::group_by(Sector, year,  Local.authority)%>%
  dplyr::summarise(Places = sum(as.numeric(Places), na.rm=T),
                   Homes = sum(as.numeric(Homes), na.rm=T))%>%
  dplyr::ungroup() %>%
  tidyr::complete(Local.authority, Sector = c("Private", "Local Authority", "Voluntary"), year = c(2015,2016,2017,2018,2019,2020,2021,2022,2023)) %>%
  # Replace NA Places with 0
  replace_na(list(Places = 0,
                  Homes = 0))%>%
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
####

####panel lags####
panel <- all %>% 
  dplyr::mutate(year = as.character(ifelse(as.Date(Date)>="2022-04-01"&as.Date(Date)<"2023-04-01", 2023,
                              ifelse(as.Date(Date)>="2021-04-01"&as.Date(Date)<"2022-04-01", 2022,
                                     ifelse(as.Date(Date)>="2020-04-01"&as.Date(Date)<"2021-04-01", 2021,
                                            ifelse(as.Date(Date)>="2019-04-01"&as.Date(Date)<"2020-04-01", 2020,
                                                   ifelse(as.Date(Date)>="2018-04-01"&as.Date(Date)<"2019-04-01", 2019,
                                                          ifelse(as.Date(Date)>="2017-04-01"&as.Date(Date)<"2018-04-01", 2018,
                                                                 ifelse(as.Date(Date)>="2016-04-01"&as.Date(Date)<"2017-04-01", 2017,
                                                                        ifelse(as.Date(Date)>="2015-04-01"&as.Date(Date)<"2016-04-01", 2016,
                                                                               ifelse(as.Date(Date)>="2014-04-01"&as.Date(Date)<"2015-04-01", 2015,
                                                                                      ifelse(as.Date(Date)>="2013-04-01"&as.Date(Date)<"2014-04-01", 2014,NA))))))))))))%>%
  dplyr::select(Local.authority, Places,leave_join, year, Sector)%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority, leave_join, year, Sector)%>%
  dplyr::summarise(homes = sum(yes),
                   places = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()%>%
  tidyr::pivot_wider(id_cols = c("Local.authority", "Sector", "year"), names_from = "leave_join", values_from = c("homes", "places"))

panel <- panel %>%  dplyr::full_join(., tidyr:: expand_grid(panel %>% 
                                            dplyr::select(Local.authority)%>%
                                            dplyr::distinct(), 
                                          c("Private", "Local Authority", "Voluntary"))%>%                     
                     tidyr::expand_grid(., c("2014","2015","2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))%>%                     
                     dplyr::rename(year = `c(...)`,
                                   Sector= `c("Private", "Local Authority", "Voluntary")`),
                   by=c("Local.authority", "year", "Sector"))%>%
  dplyr::group_by(Local.authority, Sector, year)%>%
  dplyr::mutate(homes_Join = ifelse(is.na(homes_Join), 0, homes_Join),
                places_Join = ifelse(is.na(places_Join), 0, places_Join),
                homes_Leave = ifelse(is.na(homes_Leave), 0, homes_Leave),
                places_Leave = ifelse(is.na(places_Leave), 0, places_Leave),
                net_homes = homes_Join-homes_Leave,
                net_places = places_Join-places_Leave,
                year = as.numeric(year))%>%
  dplyr::ungroup()%>%
  full_join(., df, by=c("Local.authority", "year", "Sector"))
  
lag <- panel %>%
  dplyr::select("Local.authority", "year", "Sector", "Homes", "Places")%>%
  dplyr::rename(last_year_homes = Homes,
                last_year_places = Places)%>%
  dplyr::mutate(year = as.character(as.numeric(year)+1))


panel <- merge(panel, lag, by=c("Local.authority", "year", "Sector"), all.x=T)


panel <- panel %>% dplyr::mutate(home_change = Homes- last_year_homes,
                                 place_change = Places- last_year_places
)%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Net gain of children by responsible LA") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   net_gain = number)%>%
                     dplyr::select(Local.authority, year,  net_gain), by=c("Local.authority", "year"))%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Secure units children's homes and semi-independent living accommodation") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   residential_placements = number)%>%
                     dplyr::select(Local.authority, year,  residential_placements), by=c("Local.authority", "year"))





panel %>%
  dplyr::filter(year>2014)%>%
  dplyr::select(Local.authority, net_gain,net_places)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(net_gain = sum(as.numeric(net_gain), na.rm=T),
                   net_places = sum(net_places, na.rm=T)
  )%>%
ggplot(., aes(x=net_places, y=as.numeric(net_gain)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  geom_label(aes(x = net_places, y = net_gain, label = Local.authority), position = position_dodge(), size=2) +
  labs(x = "Total net place changes (2015-23)",y = "Total net child gain (2015-23)")




panel %>%
  dplyr::filter(year>2014)%>%
  dplyr::select(Local.authority, Places, residential_placements,net_places)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(Places = mean(as.numeric(Places), na.rm=T),
                   residential_placements = mean(as.numeric(residential_placements), na.rm=T),
                   net_places = sum(net_places, na.rm=T)
  )%>%
  dplyr::mutate(res_per_place = residential_placements/Places)%>%
  ggplot(., aes(x=log(res_per_place), y=as.numeric(net_places)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  geom_label(aes(label = Local.authority), position = position_dodge(), size=2) +
  labs(y = "Total net place changes (2015-23)",x = "Logged ratio of residential placements per children home place")








mapdata <- panel %>% dplyr::select(Local.authority, Places, residential_placements)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(Places = mean(as.numeric(Places), na.rm=T),
                   residential_placements = mean(as.numeric(residential_placements), na.rm=T))%>%
  dplyr::mutate(res_per_place = residential_placements/Places)

uaboundaries <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2019_FCB_UK_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

uaboundaries <- uaboundaries[grepl('^E', uaboundaries$ctyua19cd),] 

map <-uaboundaries %>%
  dplyr::rename(Local.authority = ctyua19nm)%>%
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
  


map <- merge(map, mapdata, by=c("Local.authority"),all=T)
map <- st_as_sf(map)

no_classes <- 6


quantiles <- quantile(as.double(map$res_per_place), 
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

# here I actually create a new 
# variable on the dataset with the quantiles
map$stable_quantiles <- cut(as.double(map$res_per_place), 
                            breaks = quantiles, 
                            labels = labels, 
                            include.lowest = T)

theme_map <- function(...) {
  #theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position="bottom",
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white", color = NA), # bg of the panel
    plot.background = element_rect(fill = "white", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    # legend.background = element_rect(fill = "transparent", color=NA), # get rid of legend bg
    #  legend.box.background = element_rect(fill = "transparent", color=NA),
    # panel.border = element_blank(),legend.title=element_text(size=8), 
    #  legend.text=element_text(size=7),legend.key.size = unit(0.3, "cm"),
    ...
  )
}



library(RColorBrewer)



unstable_map <- ggplot(data = map) +
  geom_sf(aes(fill = stable_quantiles), color = NA) +
  theme_map()+
  labs(x = NULL, 
       y = NULL)+
  scale_fill_brewer(
    palette = "OrRd",
    name = "Residential placements per children's home place", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))

miles <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
  # dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")%>%
  dplyr::filter(variable=="Placed outside the local authority boundary")%>%
  dplyr::rename(Local.authority = LA_Name)

