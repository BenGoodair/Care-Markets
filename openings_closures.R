#######JOBSSS#########



if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)

####proper dataset####



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
   dplyr::distinct(URN,leave_join, .keep_all = T)%>%
   tidyr::pivot_wider(id_cols = c("Local.authority", "Sector", "URN", "Places"), names_from = "leave_join", values_from = as.character("Date"))%>%
   dplyr::mutate(Join = substr(Join, 1, 10),
                 Leave = substr(Leave, 1, 10) )


leaves <- all %>%
  dplyr::select(URN,Leave)

all <- all %>%
  dplyr::select(-Leave)%>%
  full_join(., leaves, by= "URN")  %>%
  group_by(URN) %>%                      # Group by URN
  fill(Leave, .direction = "downup") %>% 
  fill(Join, .direction = "downup")%>%
  ungroup()%>%
  dplyr::distinct(URN, .keep_all = T)

df <-rbind( read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2015.csv"))%>%
              dplyr::mutate(year=2015,
                            Places= NA)%>%
              dplyr::rename(Registration.status=Reg.Status)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector,URN, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2016.csv"), skip=1)%>%
              dplyr::mutate(year=2016)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2017.csv"), skip=1)%>%
              dplyr::mutate(year=2017)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018.csv"), skip=1)%>%
              dplyr::mutate(year=2018)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2019.csv"), skip=1)%>%
              dplyr::mutate(year=2019)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
           read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2020.csv"), skip=1)%>%
              dplyr::mutate(year=2020)%>%
             dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
           read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2021.csv"), skip=1)%>%
              dplyr::mutate(year=2021)%>%
             dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2022_yep.csv"), skip=4)%>%
              dplyr::mutate(year=2022)%>%
             dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2023.csv"), skip=3)%>%
              dplyr::mutate(year=2023)%>%
             dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority))%>%
  dplyr::mutate(Sector= ifelse(Sector=="Health Authority", "Local Authority", Sector),
                Homes=1)%>%
  dplyr::distinct(.keep_all = T)%>%
  group_by(URN) %>%                      # Group by URN
  fill(Places, .direction = "downup") %>% 
  ungroup()%>%
  bind_rows(., all %>% 
              dplyr::select(URN, Places, Local.authority)%>%
              dplyr::mutate(Homes = NA, 
                            Sector=NA))%>%
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
  dplyr::full_join(., all%>%dplyr::select(-Places, -Local.authority), by=c("URN", "Sector"))
  







umm <- umm%>%
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



