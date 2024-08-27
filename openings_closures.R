
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)


####dates are somehow wrong in exits dataaaaaa####



la_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
  dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")
exits <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))
ProviderData = read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/Provider_data.csv"))
#carehomes <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/Children's Care Homes Project/Data/Ben report dates.csv")

####ope close pre 2018####

pre <- rbind(
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 3, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 4, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 5, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 6, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 7, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 8, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 9, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 10, skip=3) %>%
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
)

####provider_data####

df <-rbind( read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2017.csv"), skip=1)%>%
              dplyr::mutate(year=2018)%>%
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
  dplyr::mutate(Sector= ifelse(Sector=="Health Authority", "Local Authority", Sector))%>%
  dplyr::group_by(Sector, year,  Local.authority)%>%
  dplyr::summarise(Places = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup() %>%
  tidyr::complete(Local.authority, Sector = c("Private", "Local Authority", "Voluntary"), year = c(2018,2019,2020,2021,2022,2023)) %>%
  # Replace NA Places with 0
  replace_na(list(Places = 0))%>%
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
  tidyr::pivot_wider(id_cols = c("Local.authority", "year"), names_from = "Sector", values_from = "Places", names_prefix = "C.Home_Places_")



