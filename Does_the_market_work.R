
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary)

####dates are somehow wrong in exits dataaaaaa####



la_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
  dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")
exits <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))
ProviderData = read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/Provider_data.csv"))
#carehomes <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/Children's Care Homes Project/Data/Ben report dates.csv")



####provider_data####

df <-rbind( read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018.csv"), skip=1)%>%
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








####panel lags####
 panel <- exits %>% 
  dplyr::filter(Sector=="Local Authority")%>%
  dplyr::mutate(year = format(as.Date(Date),"%Y"))%>%
  dplyr::select(Local.authority, Places,leave_join, year)%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority, leave_join, year)%>%
  dplyr::summarise(homes = sum(yes),
                   places = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()
  
panele <- panel %>%
  dplyr::full_join(., tidyr:: expand_grid(panel %>% 
                                            dplyr::select(Local.authority)%>%
                                            dplyr::distinct(), 
                                           c("Join", "Leave"))%>%                     
                     dplyr::rename(leave_join = `c("Join", "Leave")`)%>%
                     tidyr::expand_grid(., c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))%>%                     
                     dplyr::rename(year = `c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")`),
                   by=c("Local.authority", "leave_join", "year"))%>%
  dplyr::group_by(Local.authority,  year)%>%
  dplyr::mutate(homes = ifelse(is.na(homes), 0, homes),
                homes = ifelse(leave_join=="Leave", homes*-1, homes),
                places = ifelse(is.na(places), 0, places),
                places = ifelse(leave_join=="Leave", places*-1, places),
                net_homes = sum(as.numeric(homes)),
                net_places = sum(as.numeric(places)),
                year = as.numeric(year))%>%
  dplyr::ungroup()%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Placed inside the local authority boundary",
                                            subcategory=="Locality of placement") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name), by=c("Local.authority", "year"))%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable=="Total" &subcategory=="Placement") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care = number)%>%
                     dplyr::select(Local.authority, year, children_in_care), by=c("Local.authority", "year"))%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Own provision (by the LA)") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   LA_per = percent,
                                   LA_no = number)%>%
                     dplyr::select(Local.authority, year, LA_per, LA_no), by=c("Local.authority", "year"))%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Unaccompanied asylum-seeking children"&subcategory=="Unaccompanied asylum-seeking children"&category=="child characteristic at 31st March") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   Asylum_per = percent,
                                   Asylum_no = number)%>%
                     dplyr::select(Local.authority, year, Asylum_per, Asylum_no), by=c("Local.authority", "year"))%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Foster placements") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   Foster_per = percent)%>%
                     dplyr::select(Local.authority, year,  Foster_per), by=c("Local.authority", "year"))%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Places"&subcategory=="Local Authority") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   LA_chomes_places_per = percent,
                                   LA_chomes_places_n = number)%>%
                     dplyr::select(Local.authority, year,  LA_chomes_places_per,LA_chomes_places_n), by=c("Local.authority", "year"))%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Places"&subcategory=="Private") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   FP_chomes_places_per = percent,
                                   FP_chomes_places_n = number)%>%
                     dplyr::select(Local.authority, year,  FP_chomes_places_n,FP_chomes_places_per), by=c("Local.authority", "year"))%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Total Children Looked After"&subcategory=="Own_provision") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   own_spend_per = percent,
                                   own_spend_number = number)%>%
                     dplyr::select(Local.authority, year,  own_spend_per,own_spend_number), by=c("Local.authority", "year"))%>%
  dplyr::full_join(.,la_df %>%dplyr::filter(variable == "Secure units children's homes and semi-independent living accommodation"&subcategory=="Placement") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   residential_per = percent,
                                   residential_no = number)%>%
                     dplyr::select(Local.authority, year,  residential_per,residential_no), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., ProviderData %>% dplyr::select(Government.Office.Region, Local.authority)%>%
                     dplyr::distinct()%>%
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
                     dplyr::filter(Government.Office.Region!= "North East, Yorkshire and the Humber",
                                   Government.Office.Region!= "NULL"),
                   by="Local.authority")%>%#add region FEs to pooled modelssss
  dplyr::distinct()



#summary(lm(as.numeric(percent)~net_places+as.factor(year)+Local.authority, data=panele))





house_price <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Economic_Political_Contextual/Average-prices-2024-02.csv"))  %>%
  dplyr::rename(Local.authority = Region_Name,
                Average_house_price = Average_Price)%>%
  dplyr::filter(Date=="01/12/2023"|
                  Date=="01/12/2022"|
                  Date=="01/12/2021"|
                  Date=="01/12/2020"|
                  Date=="01/12/2019"|
                  Date=="01/12/2018"|
                  Date=="01/12/2017"|
                  Date=="01/12/2016"|
                  Date=="01/12/2015")%>%
  dplyr::mutate(year = as.numeric(format(as.Date(Date, "%d/%m/%Y"), "%Y")))%>%
  dplyr::select(Local.authority,Average_house_price, year )%>%
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

panele <- panele%>%
  dplyr::filter(!is.na(year))%>%
  dplyr::select(Local.authority, year, net_places,net_homes, Government.Office.Region, residential_no,FP_chomes_places_n,FP_chomes_places_per,residential_per,LA_chomes_places_n,LA_chomes_places_per,own_spend_number,own_spend_per, percent, children_in_care,LA_per, LA_no, Asylum_no,Asylum_per, Foster_per)%>%
  dplyr::distinct()%>%
  dplyr::mutate(percent= as.numeric(percent))%>%
  dplyr::filter(Local.authority!="LONDON")%>%
  dplyr::left_join(., house_price, by=c("Local.authority", "year"))

panelplm <- plm::pdata.frame(panele, index=c("Local.authority", "year"))
panelplm$check <- lead(panelplm$net_places)

summary(plm(net_places~lag(percent)+lag(log(Average_house_price)), data=panelplm, model="pooling"))





####unregulated####



unregulated <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/Children's Care Homes Project/Data/FOI_2024-0014264_part_1.csv", skip=15) %>%
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
                  str_trim())

panele <- panele%>%
  dplyr::left_join(., unregulated, by=c("Local.authority", "year"))%>%
  dplyr::filter(!is.na(year))%>%
  dplyr::distinct()%>%
  dplyr::mutate(percent= as.numeric(percent),
                unreg_per = as.numeric(unregulated)/ as.numeric(children_in_care)*100,
                children_in_care = as.numeric(children_in_care),
                LA_per = as.numeric(LA_per),
                LA_no = as.numeric(LA_no),
                Asylum_per = as.numeric(Asylum_per),
                Foster_per = as.numeric(Foster_per),
                LA_chomes_places_per = as.numeric(LA_chomes_places_per),
                LA_chomes_places_n = as.numeric(LA_chomes_places_n),
                own_spend_number = as.numeric(own_spend_number),
                own_spend_per = as.numeric(own_spend_per))

#FD

yes <- panele %>%dplyr::select(LA_per, Local.authority,residential_no, percent,FP_chomes_places_per,FP_chomes_places_n, LA_chomes_places_per,LA_chomes_places_n, LA_no, net_places, own_spend_per)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(LA_per_average = mean(LA_per, na.rm=T),
                   LA_no_average = mean(LA_no, na.rm=T),
                   outside_boundary_average = mean(percent, na.rm=T),
                   LA_chomes_places_per_average = median(LA_chomes_places_per, na.rm=T),
                   LA_chomes_places_n_average =median(LA_chomes_places_n, na.rm=T),
                   FP_chomes_places_n_average =median(FP_chomes_places_n, na.rm=T),
                   FP_chomes_places_per_average =median(FP_chomes_places_per, na.rm=T),
                   total_net_places = sum(net_places, na.rm=T),
                   own_spend_average = mean(own_spend_per, na.rm=T),
                   residential_no_average = mean(as.numeric(residential_no), na.rm=T))%>%
  dplyr::ungroup()

panele <- full_join(panele, yes, by="Local.authority")
panele <- full_join(panele, df, by=c("Local.authority", "year"))

yes <- panele %>%
  dplyr::filter(year=="2018")%>%
  dplyr::select(LA_per, Average_house_price, Local.authority, percent,residential_no, `C.Home_Places_Local Authority`,C.Home_Places_Private, LA_no, net_places, own_spend_per)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::mutate(LA_per_2018 = LA_per,
                   LA_no_2018 = LA_no,
                residential_no_2018 = residential_no,
                   outside_boundary_2018 = percent,
                   own_spend_2018 = own_spend_per,
                Average_house_price_2017 = Average_house_price,
                C.Home_Places_LA_2018 = `C.Home_Places_Local Authority`,
                C.Home_Places_Private_2018 = C.Home_Places_Private)%>%
  dplyr::select(own_spend_2018, Average_house_price_2017,C.Home_Places_Private_2018,C.Home_Places_LA_2018, residential_no_2018, Local.authority, outside_boundary_2018, LA_no_2018, LA_per_2018)%>%
  dplyr::ungroup()

panele <- full_join(panele, yes, by="Local.authority")


panele <- panele %>%
  rename(C.Home_Places_LA = `C.Home_Places_Local Authority`)%>%
  dplyr::mutate(la_capacity_homes = C.Home_Places_LA-as.numeric(residential_no),
                fp_capacity_homes =C.Home_Places_Private-as.numeric(residential_no),
                all_capcaity_homes = (C.Home_Places_Private+C.Home_Places_LA+C.Home_Places_Voluntary)-as.numeric(residential_no),
                chome_places_all = (C.Home_Places_Private+C.Home_Places_LA+C.Home_Places_Voluntary))

panelplm <- plm::pdata.frame(panele, index=c("Local.authority", "year"))
#panelplm$check <- lead(panelplm$net_places)

summary(plm(as.numeric(unreg_per)~lag(as.numeric(children_in_care))+(as.numeric(fp_capacity_homes))*(as.numeric(Asylum_per))+lag(as.numeric(Foster_per)), data=panelplm, model="within", effect = "twoway"))

yes1 <- plm(as.numeric(unregulated)~percent+children_in_care, data=panelplm, model="pooling")
yes2 <- plm(as.numeric(unregulated)~percent*Asylum_per+children_in_care, data=panelplm, model="pooling")
yes3 <- plm(as.numeric(unregulated)~percent*Asylum_per+children_in_care+as.numeric(residential_per)+Government.Office.Region, data=panelplm, model="pooling")
yes4 <- plm(as.numeric(unregulated)~chome_places_all+children_in_care, data=panelplm, model="pooling")
yes5 <- plm(as.numeric(unregulated)~chome_places_all*Asylum_per+children_in_care, data=panelplm, model="pooling")
yes6 <- plm(as.numeric(unregulated)~chome_places_all*Asylum_per+children_in_care+as.numeric(residential_per)+Government.Office.Region, data=panelplm, model="pooling")

#summary(yes1)

#plot_model(yes3, "int")

yes1sum <- as.list(modelsummary(yes1, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
yes2sum <- as.list(modelsummary(yes2, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
yes3sum <- as.list(modelsummary(yes3, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
yes4sum <- as.list(modelsummary(yes4, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
yes5sum <- as.list(modelsummary(yes5, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
yes6sum <- as.list(modelsummary(yes6, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))




yes1sum$tidy$p.value <- coef_test(yes1, vcov = "CR2", cluster = panelplm$Local.authority, test = "Satterthwaite")$p
yes1sum$tidy$std.error <- coef_test(yes1, vcov = "CR2", cluster = panelplm$Local.authority, test = "Satterthwaite")$SE
yes1sum$tidy$conf.low <- yes1sum$tidy$estimate-(1.96*coef_test(yes1, vcov = "CR2", cluster = panelplm$Local.authority, test = "Satterthwaite")$SE)
yes1sum$tidy$conf.high <- yes1sum$tidy$estimate+(1.96*coef_test(yes1, vcov = "CR2", cluster = panelplm$Local.authority, test = "Satterthwaite")$SE)
yes1sum$tidy$estimate <- yes1sum$tidy$estimate

yes2sum$tidy$p.value <- coef_test(yes2, vcov = "CR2", cluster = panelplm$Local.authority, test = "Satterthwaite")$p
yes2sum$tidy$std.error <- coef_test(yes2, vcov = "CR2", cluster = panelplm$Local.authority, test = "Satterthwaite")$SE
yes2sum$tidy$conf.low <- yes2sum$tidy$estimate-(1.96*coef_test(yes2, vcov = "CR2", cluster = panelplm$Local.authority, test = "Satterthwaite")$SE)
yes2sum$tidy$conf.high <- yes2sum$tidy$estimate+(1.96*coef_test(yes2, vcov = "CR2", cluster = panelplm$Local.authority, test = "Satterthwaite")$SE)
yes2sum$tidy$estimate <- yes2sum$tidy$estimate

yes3sum$tidy$p.value <- coef_test(yes3, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$p
yes3sum$tidy$std.error <- coef_test(yes3, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE
yes3sum$tidy$conf.low <- yes3sum$tidy$estimate-(1.96*coef_test(yes3, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE)
yes3sum$tidy$conf.high <- yes3sum$tidy$estimate+(1.96*coef_test(yes3, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE)
yes3sum$tidy$estimate <- yes3sum$tidy$estimate

yes4sum$tidy$p.value <- coef_test(yes4, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$p
yes4sum$tidy$std.error <- coef_test(yes4, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE
yes4sum$tidy$conf.low <- yes4sum$tidy$estimate-(1.96*coef_test(yes4, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE)
yes4sum$tidy$conf.high <- yes4sum$tidy$estimate+(1.96*coef_test(yes4, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE)
yes4sum$tidy$estimate <- yes4sum$tidy$estimate


yes5sum$tidy$p.value <- coef_test(yes5, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$p
yes5sum$tidy$std.error <- coef_test(yes5, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE
yes5sum$tidy$conf.low <- yes5sum$tidy$estimate-(1.96*coef_test(yes5, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE)
yes5sum$tidy$conf.high <- yes5sum$tidy$estimate+(1.96*coef_test(yes5, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE)
yes5sum$tidy$estimate <- yes5sum$tidy$estimate


yes6sum$tidy$p.value <- coef_test(yes6, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$p
yes6sum$tidy$std.error <- coef_test(yes6, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE
yes6sum$tidy$conf.low <- yes6sum$tidy$estimate-(1.96*coef_test(yes6, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE)
yes6sum$tidy$conf.high <- yes6sum$tidy$estimate+(1.96*coef_test(yes6, vcov = "CR2", cluster = df$Local.authority, test = "Satterthwaite")$SE)
yes6sum$tidy$estimate <- yes6sum$tidy$estimate


#feonly

cm <- c("chome_places_all" = "Children's homes places (n)",
        "percent" = "Inside area placements (%)",
        "Asylum_per" = "Asylum-seeking children (%)",
        "children_in_care" = "Children in care (n)",
        "as.numeric(residential_per)" = "Residential placements (%)" ,
        "percent:Asylum_per" = "In area:Asylum interaction",
        "chome_places_all:Asylum_per" = "Home places:Asylum interaction")

rows <- tribble(~term,          ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,  ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`, 
                'Regional Fixed Effects', 'No',  'No', 'No',  'No','Yes',  'Yes','No',  'No', 'No',  'No', 'Yes',  'Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes')


table <- modelsummary(list("Unregulated Placements [.95 ci]"=yes1sum,"p-value"=yes1sum,"Unregulated Placements [.95 ci]"=yes2sum,"p-value"=yes2sum,"Unregulated Placements [.95 ci]"=yes3sum,"p-value"=yes3sum, "Unregulated Placements [.95 ci]" = yes4sum, "p-value" = yes4sum, "Unregulated Placements [.95 ci]" = yes5sum, "p-value" = yes5sum, "Unregulated Placements [.95 ci]" = yes6sum, "p-value" = yes6sum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm,
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate pooled regression models.',
                                   'Robust SEs are clustered at LA level and use a bias-reduced linearization estimator (CR2)'),
                      output = "gt") 
# add_header_above(c(" ", "Fixed Effects" = 2, "First Differences" = 2, "Covariate Balancing (1)" = 2, "Covariate Balancing (2)" = 2, "Multi-Level Model" = 2))

table


gt::gtsave(table, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/Care Markets/Tables/Table1.html")


panelplm <- panelplm%>% dplyr::mutate(time = as.numeric(year)-9)

growmodel<- lmerTest::lmer(as.numeric(unregulated)~time*chome_places_all*Asylum_per+children_in_care+Government.Office.Region+(1+time|Local.authority),  data=panelplm, na.action=na.exclude)
growmodel<- lmerTest::lmer(as.numeric(unregulated)~time*chome_places_all*Asylum_per+children_in_care+Government.Office.Region+(1+time|Local.authority),  data=panelplm, na.action=na.exclude)
growmodel<- lmerTest::lmer(as.numeric(unregulated)~time*percent*Asylum_per+children_in_care+Government.Office.Region+(1+time|Local.authority),  data=panelplm, na.action=na.exclude)



growmodel<- lmerTest::lmer(as.numeric(unregulated)~time*LA_per*Asylum_per+Government.Office.Region+(1+time|Local.authority),  data=panelplm, na.action=na.exclude)
growmodel<- lmerTest::lmer(as.numeric(unregulated)~time*children_in_care*percent+Asylum_per+Government.Office.Region+(1+time|Local.authority),  data=panelplm, na.action=na.exclude)
growmodel<- lmerTest::lmer(as.numeric(unregulated)~time*Asylum_per+Government.Office.Region+(1+time|Local.authority),  data=panelplm, na.action=na.exclude)



growmodel<- lmerTest::lmer(as.numeric(unregulated)~time*chome_places_all+children_in_care+Government.Office.Region+(1+time|Local.authority),  data=panelplm[panelplm$Asylum_per<8,], na.action=na.exclude)
yes6 <- plm(as.numeric(chome_places_all)~Asylum_per+children_in_care+Government.Office.Region, data=panelplm[panelplm$Asylum_per>8,], model="pooling")



summary(growmodel)
plot_model(growmodel, "int")


library(sjPlot)

plot_model(lm, type = "int")+
  theme_bw()+
  labs(x = "Asylum seeking children (% children in care)",
       y = "Predicted values of unregulated placements (%)",
       colour = "Public ownership (%)",
       title = "")



#Table 1



#Table 2

#Table 3






####descriptives####

#I think a - unregulated places map, b - unregulated rises since 2010, c - relationship between unregulated and asylum

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
  dplyr::full_join(., panele %>%
                     dplyr::mutate( unreg_per = as.numeric(unregulated)/ as.numeric(children_in_care)*100)%>%
                     dplyr::select(Local.authority, unreg_per, Asylum_per)%>%
                     dplyr::group_by(Local.authority)%>%
                     dplyr::summarise(unreg_per = mean(unreg_per, na.rm=T),
                                      Asylum_per = mean(as.numeric(Asylum_per), na.rm=T))%>%
                     dplyr::ungroup(),
                   by="Local.authority")%>%
  st_as_sf(.)
  
  


no_classes <- 6


quantiles <- quantile(as.double(map$unreg_per), 
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




a <- map %>%  dplyr::mutate(unreg_quantiles =cut(unreg_per, 
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
    name = "Unregulated Placements\n(Average %, 2019-2023)", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))



c <- panele %>%
  dplyr::mutate(unregper = as.numeric(unregulated)/ as.numeric(children_in_care)*100)%>%
  dplyr::filter(year>2018)%>%
  ggplot(., aes(x=(as.numeric(Asylum_no)/as.numeric(children_in_care)), y=as.numeric(unregper)))+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw()+
  labs(x="Asylum seeking children (% of children in care)", y="Unregulated Placements (% of children in care)")


b <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/Children's Care Homes Project/Data/FOI_2024-0014264_part_1.csv", skip=15) %>%
  dplyr::filter(geographic_level=="National") %>%  
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::select(time_period, number)%>%
  dplyr::bind_rows(., data.frame("time_period"=c(2013,2014,2015,2016,2017,2018),
                                 "number"=c(7580,7550,8310,9890,11020,11640)))%>%
  ggplot(., aes(x=time_period, y=as.numeric(number)))+
  geom_line(data = .%>%dplyr::filter(time_period<2019), alpha=0.2)+
  geom_point(data = .%>%dplyr::filter(time_period<2019), alpha=0.2)+
  geom_line(data = .%>%dplyr::filter(time_period>2018))+
  geom_point(data = .%>%dplyr::filter(time_period>2018))+
  geom_label(data = .%>%dplyr::filter(time_period<2019),aes(label = number), alpha=0.2, color="grey")+
  geom_label(data = .%>%dplyr::filter(time_period>2018),aes(label = number))+
  theme_bw()+
  labs(x="Year", y="Unregulated Placements (n)")+
  geom_vline(xintercept = 2018.5)+
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))
  




plot <- cowplot::plot_grid(b,c, ncol = 1, labels = c("A","B"))
plot <- cowplot::plot_grid(plot,a, ncol = 2, labels=c("","C"))

ggsave(plot=plot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/Care Markets/figures/figure_1.jpeg", width=14, height=9, dpi=600)








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
  dplyr::full_join(., panele %>%
                     dplyr::mutate( c_homes_per = as.numeric(chome_places_all)/ as.numeric(children_in_care)*100)%>%
                     dplyr::select(Local.authority, c_homes_per)%>%
                     dplyr::group_by(Local.authority)%>%
                     dplyr::summarise(c_homes_per = mean(c_homes_per, na.rm=T))%>%
                     dplyr::ungroup(),
                   by="Local.authority")%>%
  st_as_sf(.)




no_classes <- 6


quantiles <- quantile(as.double(map$c_homes_per), 
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




a <- map %>%  dplyr::mutate(unreg_quantiles =cut(c_homes_per, 
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
    name = "Children's home places\n(per child in care, 2019-2023)", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))


ggsave(plot=a, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/Care Markets/figures/map_chomes.jpeg", width=8, height=6, dpi=600)



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
  dplyr::full_join(., panele %>%
                     dplyr::mutate( c_homes_per = as.numeric(chome_places_all)/ as.numeric(children_in_care)*100)%>%
                     dplyr::select(Local.authority, c_homes_per)%>%
                     dplyr::group_by(Local.authority)%>%
                     dplyr::summarise(c_homes_per = mean(c_homes_per, na.rm=T))%>%
                     dplyr::ungroup(),
                   by="Local.authority")%>%
  st_as_sf(.)




no_classes <- 6


quantiles <- quantile(as.double(map$c_homes_per), 
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




a <- map %>%  dplyr::mutate(unreg_quantiles =cut(c_homes_per, 
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
    name = "Children's home places\n(per child in care, 2019-2023)", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))


ggsave(plot=a, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/Care Markets/figures/map_chomes.jpeg", width=8, height=6, dpi=600)





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
  dplyr::full_join(., panele %>%
                     dplyr::select(Local.authority, percent)%>%
                     dplyr::group_by(Local.authority)%>%
                     dplyr::summarise(percent = mean(percent, na.rm=T))%>%
                     dplyr::ungroup(),
                   by="Local.authority")%>%
  st_as_sf(.)




no_classes <- 6


quantiles <- quantile(as.double(map$percent), 
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




a <- map %>%  dplyr::mutate(unreg_quantiles =cut(percent, 
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
    name = "Inside area placements\n(Average %, 2019-2023)", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))


ggsave(plot=a, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/Care Markets/figures/map_inside_Area.jpeg", width=8, height=6, dpi=600)

































  fig_3 <- la_df %>%
    dplyr::filter(category == "Expenditure",
                  variable=="Asylum seeker services children"|variable=="Total Children Looked After")%>%
    dplyr::select(number, subcategory, year, variable)%>%
    dplyr::group_by(subcategory, year, variable)%>%
    dplyr::summarise(number = sum(as.numeric(number), na.rm=T))%>%
    dplyr::ungroup()%>%
    tidyr::pivot_wider(id_cols = c("year", "variable"), names_from = "subcategory", values_from = "number")%>%
    dplyr::mutate(percent_own = ((Own_provision+Other_public_provision) /Total_expenditure)*100)%>%
    ggplot(., aes(x= year, y= percent_own, colour = variable))+
    geom_line()+
    theme_bw()








panele %>%
  dplyr::mutate(unregper = as.numeric(unregulated)/ as.numeric(children_in_care)*100)%>%
  dplyr::select(unregper, net_homes, Local.authority)%>%
  dplyr::group_by(Local.authority)%>%
  dplyr::summarise(unregper = mean(unregper, na.rm=T),
                   net_homes = sum(net_homes, na.rm=T))%>%
  dplyr::ungroup()%>%
  ggplot(., aes(x=net_homes, y=unregper))+
  geom_point()+
  stat_smooth(method = "lm")

panele %>%
  dplyr::mutate(unregper = as.numeric(unregulated)/ as.numeric(children_in_care)*100)%>%
  dplyr::filter(year>2018)%>%
  ggplot(., aes(x=log(Average_house_price), y=as.numeric(unregper)))+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw()+
  labs(x="Average House Price (logged)", y="Unregulated Placements (% of children in care)")



panele %>%
  ggplot(., aes(x=(Average_house_price), y=as.numeric(net_homes)))+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw()+
  labs(x="Out of area placements", y="Unregulated Placements (% of children in care)")




#



























leaves <- exits %>%
  dplyr::select(Local.authority, Places,leave_join, Sector)%>%
  dplyr::filter(leave_join=="Leave")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority, Sector )%>%
  dplyr::summarise(homes = sum(yes),
                   places = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()

leaves <- leaves %>%
  dplyr::full_join(., tidyr:: expand_grid(leaves %>% 
                                            dplyr::select(Local.authority)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("Local.authority", "Sector"))


joins <- exits %>%
  dplyr::select(Local.authority, Places,leave_join, Sector)%>%
  dplyr::filter(leave_join=="Join")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority, Sector )%>%
  dplyr::summarise(homes_j = sum(yes),
                   places_j = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()

joins <- joins %>%
  dplyr::full_join(., tidyr:: expand_grid(joins %>% 
                                            dplyr::select(Local.authority)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("Local.authority", "Sector"))

plot <- dplyr::full_join(leaves, joins, by=c("Local.authority", "Sector"))%>%
  dplyr::mutate(homes_j = ifelse(is.na(homes_j), 0, homes_j),
                places_j = ifelse(is.na(places_j), 0, homes_j),
                homes = ifelse(is.na(homes), 0, homes),
                places = ifelse(is.na(places), 0, places),
                net = homes_j-homes,
                Sector = ifelse(Sector=="Private", "For-profit",
                                ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))%>%
  dplyr::full_join(., la_df %>% 
                     dplyr::filter(variable == "Placed inside the local authority boundary",
                                   subcategory=="Locality of placement",
                                   year>2015)%>%
                     dplyr::select(LA_Name,year, number)%>%
                     dplyr::group_by(LA_Name)%>%
                     arrange(year) %>%
                     mutate(diff_final_column = as.numeric(number) - dplyr::lag(as.numeric(number)))%>%
                     dplyr::select(-year, -number)%>%
                     summarise(diff_final_column= mean(as.numeric(diff_final_column), na.rm=T))%>%
                     dplyr::ungroup()%>%
                     dplyr::rename(Local.authority = LA_Name), by="Local.authority")%>%
  dplyr::filter(!is.na(Sector))%>%
  ggplot(. ,aes(x=as.numeric(diff_final_column), y=net))+
  geom_smooth(method = "lm")+
  geom_point(color = "black", alpha = 0.5)+
  facet_wrap(~Sector)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  labs(x="Average annual change in  number of children in area (% points 2016-23))", y="Net change to children's homes number\n(2016-2023)")+
  coord_cartesian(ylim = c(-30,30))



plot <- dplyr::full_join(leaves, joins, by=c("Local.authority", "Sector"))%>%
  dplyr::mutate(homes_j = ifelse(is.na(homes_j), 0, homes_j),
                places_j = ifelse(is.na(places_j), 0, homes_j),
                homes = ifelse(is.na(homes), 0, homes),
                places = ifelse(is.na(places), 0, places),
                net = homes_j-homes,
                Sector = ifelse(Sector=="Private", "For-profit",
                                ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))%>%
  dplyr::full_join(., la_df %>% 
                     dplyr::filter(variable=="Total" &subcategory=="Placement",
                                   year>2015)%>%
                     dplyr::select(LA_Name,year, number)%>%
                     dplyr::group_by(LA_Name)%>%
                     arrange(year) %>%
                     mutate(diff_final_column = as.numeric(number) - dplyr::lag(as.numeric(number)))%>%
                     dplyr::select(-year, -number)%>%
                     summarise(diff_final_column= mean(as.numeric(diff_final_column), na.rm=T))%>%
                     dplyr::ungroup()%>%
                     dplyr::rename(Local.authority = LA_Name), by="Local.authority")%>%
  dplyr::filter(!is.na(Sector))%>%
  ggplot(. ,aes(x=as.numeric(diff_final_column), y=net))+
  geom_smooth(method = "lm")+
  geom_point(color = "black", alpha = 0.5)+
  facet_wrap(~Sector)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  labs(x="Average annual change in  number of children in care (% points 2016-23))", y="Net change to children's homes number\n(2016-2023)")+
  coord_cartesian(ylim = c(-30,30))







leaves <- exits %>%
  dplyr::select(Local.authority, Places,leave_join, Sector)%>%
  dplyr::filter(leave_join=="Leave")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority, Sector )%>%
  dplyr::summarise(homes = sum(yes),
                   places = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()

leaves <- leaves %>%
  dplyr::full_join(., tidyr:: expand_grid(leaves %>% 
                                            dplyr::select(Local.authority)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("Local.authority", "Sector"))


joins <- exits %>%
  dplyr::select(Local.authority, Places,leave_join, Sector)%>%
  dplyr::filter(leave_join=="Join")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority, Sector )%>%
  dplyr::summarise(homes_j = sum(yes),
                   places_j = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()

joins <- joins %>%
  dplyr::full_join(., tidyr:: expand_grid(joins %>% 
                                            dplyr::select(Local.authority)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("Local.authority", "Sector"))

plot <- dplyr::full_join(leaves, joins, by=c("Local.authority", "Sector"))%>%
  dplyr::mutate(homes_j = ifelse(is.na(homes_j), 0, homes_j),
                places_j = ifelse(is.na(places_j), 0, homes_j),
                homes = ifelse(is.na(homes), 0, homes),
                places = ifelse(is.na(places), 0, places),
                net = homes_j-homes,
                Sector = ifelse(Sector=="Private", "For-profit",
                                ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))%>%
  dplyr::full_join(., la_df %>% 
                     dplyr::filter(variable=="Placed inside the local authority boundary",
                                   subcategory=="Locality of placement",
                                   year>2017)%>%
                     dplyr::select(LA_Name,percent)%>%
                     dplyr::group_by(LA_Name)%>%
                     summarise(percent= mean(as.numeric(percent), na.rm=T))%>%
                     dplyr::ungroup()%>%
                     dplyr::rename(Local.authority = LA_Name), by="Local.authority")%>%
  dplyr::filter(!is.na(Sector))%>%
  ggplot(. ,aes(x=as.numeric(percent), y=net))+
  geom_smooth(method = "lm")+
  geom_point(color = "black", alpha = 0.5)+
  #facet_wrap(~Sector)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  labs(x="Average Children placed inside LA (%))", y="Net change to children's homes number\n(2016-2023)")+
  coord_cartesian(ylim = c(-30,30))









leaves <- exits %>%
  dplyr::select(Local.authority, Places,leave_join)%>%
  dplyr::filter(leave_join=="Leave")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority )%>%
  dplyr::summarise(homes = sum(yes),
                   places = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()



joins <- exits %>%
  dplyr::select(Local.authority, Places,leave_join)%>%
  dplyr::filter(leave_join=="Join")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority )%>%
  dplyr::summarise(homes_j = sum(yes),
                   places_j = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()


plot <- dplyr::full_join(leaves, joins, by=c("Local.authority"))%>%
  dplyr::mutate(homes_j = ifelse(is.na(homes_j), 0, homes_j),
                places_j = ifelse(is.na(places_j), 0, homes_j),
                homes = ifelse(is.na(homes), 0, homes),
                places = ifelse(is.na(places), 0, places),
                net = homes_j-homes)%>%
  dplyr::full_join(., la_df %>% 
                     dplyr::filter(variable=="Placed inside the local authority boundary",
                                   subcategory=="Locality of placement",
                                   year>2017)%>%
                     dplyr::select(LA_Name,percent)%>%
                     dplyr::group_by(LA_Name)%>%
                     summarise(percent= mean(as.numeric(percent), na.rm=T))%>%
                     dplyr::ungroup()%>%
                     dplyr::rename(Local.authority = LA_Name), by="Local.authority")%>%
  #dplyr::filter(!is.na(Sector))%>%
  ggplot(. ,aes(x=as.numeric(percent), y=net))+
  geom_smooth(method = "lm")+
  geom_point(color = "black", alpha = 0.5)+
  #facet_wrap(~Sector)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  labs(x="Average Children placed inside LA (%))", y="Net change to children's homes number\n(2016-2023)")+
  coord_cartesian(ylim = c(-30,30))


























































































####DELETED####



####sii####

sii <- la_df %>%dplyr::filter(variable == "Placed inside the local authority boundary",
                              subcategory=="Locality of placement") %>%
  dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
  dplyr::rename(Local.authority = LA_Name)%>%
  dplyr::full_join(., exits %>% 
                     dplyr::select(IMD...Rank.of.average.score, Local.authority)%>%
                     dplyr::distinct(),
                   by="Local.authority" )


ggplot(sii, aes(x=IMD...Rank.of.average.score, y=as.numeric(percent)))+
  #  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~year)


# Initialize an empty dataframe to store results
sii_values <- data.frame(year = numeric(), value = numeric())

# Loop through the years
for (i in 2011:2023) {
  # Perform linear regression once for each year
  lm_model <- lm(as.numeric(percent) ~ IMD...Rank.of.average.score, data = sii[sii$year == i, ])
  
  # Calculate the value based on coefficients
  value <- lm_model$coefficients[1] + 151 * lm_model$coefficients[2] - lm_model$coefficients[1] + 1 * lm_model$coefficients[2]
  
  # Store the result in the dataframe
  sii_values <- rbind(sii_values, data.frame(year = i, value = value))
}


ggplot(sii_values, aes(x=year, y=as.numeric(value)))+
  geom_point()+
  geom_line()+
  labs(x= "year", y="Inequality of children placed inside area\n(higher = more inequality, 0 = perfect equality)",
       title="Deprived areas used to have worse placements... but this is now equal")+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks=c(2011, 2013,2015, 2017, 2019, 2021,2023))



####check####


er <- dplyr::full_join(exits, ProviderData %>% dplyr::mutate(regdate = as.Date(Registration.date, format =  "%d/%m/%Y")), by="URN")
























####errrr#####


library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Read the CSV file
exitdata <- read_csv("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv")

# Convert the 'Date' column to Date format and extract the year
exitdata$year <- format(as.Date(exitdata$Date),"%Y")

# Replace values in the 'Sector' column
exitdata$Sector <- recode(exitdata$Sector,
                          "Private" = "For-profit",
                          "Local Authority" = "Local Authority",
                          "Health Authority" = "Local Authority",
                          "Voluntary" = "Third Sector")

# Replace values in the 'leave_join' column
exitdata$leave_join <- recode(exitdata$leave_join,
                              "Leave" = "Exits",
                              "Join" = "Entries")

# Convert 'Places' column to numeric
exitdata$Places <- as.numeric(exitdata$Places)

# Add 'childrens_homes' column
exitdata$childrens_homes <- 1

# Group by specified columns and aggregate
exitdata_grouped <- exitdata %>%
  group_by(Sector, year, leave_join, Local.authority) %>%
  summarise(childrens_homes = sum(childrens_homes, na.rm = TRUE),
            Places = sum(Places, na.rm = TRUE)) %>%
  ungroup()

# Group by fewer columns for the 'all' dataset
all <- exitdata_grouped %>%
  group_by(Sector, year, leave_join) %>%
  summarise(childrens_homes = sum(childrens_homes, na.rm = TRUE),
            Places = sum(Places, na.rm = TRUE)) %>%
  ungroup()

all$Local.authority <- "All"

# Combine the datasets
exitdata <- bind_rows(exitdata_grouped, all)

# Melt the dataset
exitdata <- pivot_longer(exitdata, cols = c(childrens_homes, Places),
                         names_to = "Homes_or_places", values_to = "value")

# Replace 'Homes_or_places' values
exitdata$Homes_or_places <- recode(exitdata$Homes_or_places,
                                   "childrens_homes" = "Children's homes")

# Filter out specific years
exitdata <- exitdata %>% filter(!(year %in% c(2007, 2008)))

# Convert 'value' column to numeric
exitdata$value <- as.numeric(exitdata$value)

# Calculate net change and create a new dataset for it
grouped <- exitdata %>%
  group_by(year, Local.authority, Homes_or_places, Sector) %>%
  summarise(value = sum(value[leave_join == "Entries"], na.rm = TRUE) -
              sum(value[leave_join == "Exits"], na.rm = TRUE)) %>%
  ungroup()

grouped$leave_join <- "Net change"

# Filter out specific years from the 'grouped' dataset
grouped <- grouped %>% filter(!(year %in% c(2014, 2015, 2016)))

# Combine the 'grouped' dataset with the original 'exitdata'
exitdata <- bind_rows(exitdata, grouped)

# Sort the dataset
exitdata <- exitdata %>%
  arrange(year, Local.authority)

# Set the 'year' column as a factor with specific levels
exitdata$year <- factor(exitdata$year, levels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))



#### SIN ####

notifications <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/Children's Care Homes Project/Data/sin_region_local_authority_2019_2024.csv")%>%
  dplyr::rename(Local.authority = la_name,
                year= time_period)%>%
  dplyr::select(Local.authority,number_notifications, year )%>%
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

panele <- panele%>%
  dplyr::left_join(., notifications, by=c("Local.authority", "year"))%>%
  dplyr::filter(!is.na(year))%>%
  dplyr::distinct()%>%
  dplyr::mutate(percent= as.numeric(percent),
                no_deaths = ifelse(number_notifications=="0",1,0),
                notification_imp = ifelse(number_notifications=="c", 0, number_notifications))


panelplm <- plm::pdata.frame(panele, index=c("Local.authority", "year"))

panelplm <- panelplm %>%
  dplyr::mutate(notification_imp_per = as.numeric(notification_imp)/as.numeric(children_in_care),
                LA_per = as.numeric(LA_per),
                children_in_care = as.numeric(children_in_care),
                unregper = as.numeric(unregulated)/ as.numeric(children_in_care)*100)

summary(plm(as.numeric(notification_imp_per)~(as.numeric(children_in_care))+(as.numeric(unregper)), data=panelplm, model="within", effect="twoway"))

summary(lm(as.numeric(number_notifications)~(as.numeric(children_in_care))+(as.numeric(LA_per))+as.numeric(unregulated)+Local.authority+year, data=panelplm))





no_classes <- 6


quantiles <- quantile(as.double(map$Asylum_per), 
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




map <- map %>%  dplyr::mutate(Asylum_per_quantiles =cut(Asylum_per, 
                                                        breaks = quantiles, 
                                                        labels = labels, 
                                                        include.lowest = T) )%>%
  ggplot(.) +
  geom_sf(aes(fill = Asylum_per_quantiles), color = "black") +
  labs(x = NULL, 
       y = NULL)+
  scale_fill_brewer(
    palette = "OrRd",
    name = "Unregulated Placements\n(Average %, 2019-2023)", na.value="grey")+
  theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))



