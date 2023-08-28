

#' 
#' WRANGLING SALES AND FUTURE FORECAST
#' ___________________________________.


library(pacman)
p_load(dplyr,lubridate,tidyr,stringr,readxl,plotly)


rm(list = ls())
# ---- 0 / PARAMS ----
#setwd("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/Projects/Strat Plan extrapolation/SP 2022/Mar complete run")

# EG MISSING
# #eg_missing <- paste0("EG: ",
#                      c(845095,845097,845098,845100,688111,
#                        819525,817783,817785,817789,817791))


# ---- 1 / LOAD DATA ----
#check dmdg extractions tables wrangle_aggr_futurefcst for forecast on ibp cycle submission knx
load("~/work/longterm-forecast/FUTUREDATABASE_commercial_13_jun_23.RData")
#check salesdatabase script for sales extraction
load("~/work/longterm-forecast/commercial_sales_jun_23.RData")
#load("~/work/longterm-forecast/full_sales2.RData")
#check parloc is updated \\E21FLSBCNSCHUB\bcn_sc_hub\3 - Forecast\99 - Data
load("~/work/longterm-forecast/par_loc_gmid_masterdata_origin.RData")
#check with Alex.P pipos have been run for current month and make sure extractions have been refreshed to \\E21FLSBCNSCHUB\bcn_sc_hub\3 - Forecast\10 - Kinaxis Operating Cycle\0 - Data\Inputs 
load("~/work/longterm-forecast/pipo_jul_18_23.RData")


#NOT NEED 
# # load("00_data/aggr_future_fcst_CL.RData")
# load("00_data/aggr_future_fcst_latest.RData")
# load("00_data/SalesDataBase.RData")
#load("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/99 - Data/par_loc_gmid_masterdata_tmp.RData")

#Filter SPC sales exclude current cycle
spc_sales <- commercial_sales

#filter(!(type == "sales" & date == "2022-04-01")) %>%

#check that this_cycle corresponds to current extrapolation
this_cycle <- floor_date(today(),"month")

spc_sales <- spc_sales %>%
  filter(startdate < this_cycle)

#rename qty
spc_sales <- spc_sales %>%
  filter( demandtype=="Commercial") %>% 
  rename(
    Qty = quantity,
    date= startdate)

#group by channel
spc_sales <- spc_sales %>%
  group_by(loc, gmid, siso, date) %>% 
  summarise(Qty = sum(Qty)) %>% 
  ungroup()
  

#group by month not day
library(zoo)
spc_sales$date <- as.Date(as.yearmon(spc_sales$date))

#group by new date
spc_sales <- spc_sales %>%
  group_by(loc, gmid, siso, date) %>% 
  summarise(Qty = sum(Qty)) %>% 
  ungroup()

#####SUM check####
#check DSI
#spc_sales_dsi <- spc_sales %>%
  # dplyr::filter(siso=="DSI")

#sum(spc_sales_test$Qty)
#sum(spc_sales_dsi$Qty)

#filter only sell in
spc_sales <- spc_sales %>%
  dplyr::filter(siso=="Sales In") %>% 
dplyr::select(loc,gmid,date,Qty) %>% 
  unique()
#date as character is needed in order to rbind later sales and fcst
spc_sales_test <- spc_sales
spc_sales_test$date <- as.character(spc_sales_test$date)
str(spc_sales_test)

#WHAT TO DO WITH SALES OUT DSI? exclude

#FR including overseas territories will be performed after
# spc_sales_test  <-   spc_sales_test %>% 
#   dplyr::mutate(loc = ifelse(loc %in% c("FR002", "FR003", "FR004"), "FR001", loc))
# 
#                                                                     

#FORECAST####
#forecast startdate and fcstdate as numeric needed and lubridate check screenshot?
spc_forecast<-FUTUREDATABASE

spc_forecast <- spc_forecast %>%
  ungroup()%>%
  rename(
    date = startdate) %>%
  dplyr::select(loc,gmid,date,Qty)

####DROP FORECAST DATECURRENT MONTH check for dec update####
#check if today month is equal to cycle month otherwise adj this_cycle
this_cycle <- floor_date(today(),"month")
spc_forecast <- spc_forecast %>%
  filter(date >= this_cycle)

# spc_forecast <- spc_forecast %>%
#   filter(date >= "2022-11-01")
  

  
#addbu
library(stringr)
#drop var
parloc_spc<-par_loc_gmid_masterdata%>%
  filter(global_business_unit == "SPC")%>%
  transmute(loc,gmid=dmdunit,global_business_unit)%>%
  unique()

#gmid format not need
#parloc_spc$gmid<-str_pad(parloc_spc$gmid, 8, pad = "0")

#leftjoin
str(spc_forecast)
str(spc_sales_test)

#sales and forecast
longterm<-bind_rows(spc_forecast,spc_sales_test)

#add spc in database through parloc
longterm<-dplyr::left_join(longterm,parloc_spc,
            by = c("gmid","loc"))
#filter spc
longterm<-longterm %>% 
  dplyr::filter(global_business_unit == "SPC") %>% 
  rename(gbu = global_business_unit)

#sell in, missing 003 004 005 for fr
#longterm2 <- longterm
#adding overseas territories to main france
longterm<-longterm %>% 
  dplyr::mutate(loc = ifelse(loc == "FR002" | loc == "FR003" | loc == "FR004","FR001",loc)) %>% 
  filter(grepl("001",loc)) %>% 
  mutate(forecast_item=paste0(substr(loc,1,2),": ",as.numeric(gmid))) %>% 
  dplyr::select(-loc,-gmid)

#pipo
longterm<-longterm %>% 
  left_join(pipo,
            by=c("forecast_item"="po")) %>% 
  mutate(forecast_item=ifelse(is.na(pi),forecast_item,pi))

longterm_final<-longterm %>% 
  group_by(forecast_item,date) %>% 
  summarise(Qty=sum(Qty)) %>% 
  ungroup()
#nov cycle includes 2025 forecast complete why is GMID need?
sales_and_fcst <- longterm_final %>% 
  mutate(Date = ymd(date)) %>% 
  filter(date >= "2021-01-01" & date <= "2026-06-01") %>% 
  mutate(GMID=str_extract(forecast_item,"[0-9]{4,}")) %>% 
  mutate(LOC=paste0(substr(forecast_item,1,2),"001"))



# Get PIPO not used:
# configuration_files <- lapply(list.files("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/Inputs/Apr - 2022"),
#                               function(i){
#                                 
#                                 # i <- "[Forecast Item] Configuration{I0439729_JP}.xlsx"
#                                 
#                                 print(i)
#                                 # Load the config file
#                                 tmp <- read_xlsx(path = paste0("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/Inputs/Apr - 2022/",i),
#                                                  skip = 3)
#                                 
#                                 # Select only the needed columns
#                                 tmp <- tmp %>% 
#                                   transmute(forecast_item = paste0(str_extract(Item,"[A-Z]{2}"),
#                                                                    ": ",
#                                                                    str_extract(Item,"[0-9]{5,6}")),
#                                             Historical = as.numeric(Historical),
#                                             start_date = Start...63,
#                                             stop_date = Stop...64,
#                                             Other_Items = as.character(`Other Items`),
#                                             is_old_item = as.character(`Other Items?`),
#                                             Type = Type...17) %>% 
#                                   {if("POSIXt" %in% class(.$stop_date)){
#                                     mutate(.,stop_date = ymd(stop_date))}else{
#                                       mutate(.,
#                                              stop_date = as.character(stop_date),
#                                              stop_date = case_when(stop_date == "Past" ~ "44562",
#                                                                    stop_date == "Future" ~ "46357",
#                                                                    TRUE ~ stop_date)) %>%
#                                         mutate(stop_date = as.Date(as.numeric(stop_date),origin = "1899-12-30"))}} %>% 
#                                   {if("POSIXt" %in% class(.$start_date)){
#                                     mutate(.,start_date = ymd(start_date))}else{
#                                       mutate(.,
#                                              start_date = as.character(start_date),
#                                              start_date = case_when(start_date == "Past" ~ "44562",
#                                                                     start_date == "Future" ~ "46357",
#                                                                     TRUE ~ start_date))  %>%
#                                         mutate(start_date = as.Date(as.numeric(start_date),origin = "1899-12-30"))}}
#                                 
#                                 return(tmp)
#                               })

# # Binding not used
# configuration_files <- bind_rows(configuration_files) %>% 
#   mutate(Other_Items = strsplit(Other_Items,", ")) %>% 
#   unnest(Other_Items)

# Wrangling not used
# pipo_file <- configuration_files %>% 
#   transmute(pi = forecast_item,
#             po = Other_Items) %>% 
#   filter(!is.na(po)) %>% 
#   filter(pi != po) %>% 
#   unique()
# configuration_files_clean <- configuration_files %>% 
#   select(-Other_Items,-is_old_item)
# configuration_files_clean <- configuration_files_clean %>% 
#   group_by(forecast_item) %>% 
#   summarise(Historical = max(Historical,na.rm = T),
#             start_date = min(start_date, na.rm = T),
#             stop_date = max(stop_date, na.rm = T)) 

# 
# # ---- 2 / CLEANSING ----
# # SALESDATABASE:
# SALESDATABASE2 <- SALESDATABASE %>% 
#   transmute(forecast_item = paste0(substr(LOC,1,2),
#                                    ": ",
#                                    gsub("^0+","",GMID)),
#             date = dmy(Date),
#             type = "sales",
#             qty = MSales)
# 
# # Future Fcst:
# future_fcst2 <- FUTUREDATABASE %>%
#   ungroup() %>% 
#   filter(!grepl("999",loc)) %>% 
#   transmute(forecast_item = paste0(substr(loc,1,2),
#                                    ": ",
#                                    gsub("^0+","",gmid)),
#             date = ymd(startdate),
#             type = "final_fcst_202204",
#             qty = Qty)
# 
# # Apply cleansing
# sales_and_fcst <- future_fcst2 %>% 
#   bind_rows(.,SALESDATABASE2) %>% 
#   # filter(forecast_item %in% eg_missing) %>%
#   # filter(qty != 0) %>%
#   merge(.,pipo_file,
#         by.x = "forecast_item",
#         by.y = "po",
#         all.x = TRUE) %>% 
#   mutate(new_forecast_item = ifelse(is.na(pi),forecast_item,pi)) %>% 
#   as_tibble() %>%
#   group_by(forecast_item = new_forecast_item,date,type) %>% 
#   summarise(qty = sum(qty)) %>% 
#   ungroup() %>% 
#   merge(.,unique(par_loc_gmid_masterdata_tmp[,c("forecast_item","gbu")]),
#         by = "forecast_item",
#         all.x = TRUE) %>%
#   filter(!(type == "sales" & date == "2022-04-01")) %>% 
#   filter(grepl("GEM",gbu)) %>%
#   filter(!grepl("CN|US",forecast_item)) %>%
#   filter(qty != 0) %>% 
#   group_by(forecast_item) %>% 
#   complete(date = seq.Date(min(date),  max(date), by = "month")) %>% 
#   replace_na(list(qty = 0)) %>% 
#   mutate(type = case_when(is.na(type) & date <= "2022-04-01" ~ "sales",
#                           is.na(type) & date >= "2022-04-01"~ "final_fcst_202204",
#                           TRUE ~ type)) %>% 
#   arrange(forecast_item,date) %>% 
#   mutate(gbu = zoo::na.locf(gbu))

# sales_and_fcst <- sales_and_fcst %>% 
#   filter(grepl("CL",forecast_item))
# 
# # FILTER FOR IBP UPDATES:
# sales_and_fcst <- sales_and_fcst %>% 
#   filter(grepl("EG|GB",forecast_item)) %>% 
#   merge(.,par_loc_gmid_masterdata_tmp[,c("forecast_item","product_family")],
#         by = "forecast_item",
#         all.x = T) %>% 
#   filter((grepl("GB",forecast_item) & grepl("CLEXANE",product_family)))

# 
# 
# # Filter for date range, keep sales_and_fcst for Excel output:
# sales_and_fcst2 <- sales_and_fcst %>%
#   filter(date >= "2022-01-01") %>% 
#   filter(date < "2025-01-01") %>% 
#   as_tibble()
# sales_and_fcst <- sales_and_fcst %>%
#   filter(date >= "2020-01-01") %>% 
#   filter(date < "2025-01-01") %>% 
#   as_tibble()

#check
#date <= "2025-11-01"

# Filter out items that were not forecasted by IBP after 2024.
# Keep them to still include them in the study.
checkcount<-length(unique(sales_and_fcst$forecast_item))

# IBP_not_forecasted <- sales_and_fcst %>% 
#   filter(Qty != 0) %>%
#   group_by(forecast_item) %>% 
#   mutate(max_date = max(date)) %>% 
#   filter(max_date <= "2022-11-01") %>% 
#   pull(forecast_item) %>% 
#   unique()
sales_and_fcst_fi <- sales_and_fcst

IBP_not_forecasted <- sales_and_fcst %>% 
  filter(Qty != 0) %>%
  group_by(forecast_item) %>% 
  mutate(max_date = max(date)) %>% 
  filter(max_date <= this_cycle) %>% 
  pull(forecast_item) %>% 
  unique()



# IBP_stops_before_Q3_25 <-  sales_and_fcst %>% 
#   filter(Qty != 0) %>% 
#   group_by(forecast_item) %>% 
#   mutate(max_date = max(date)) %>% 
#   filter(max_date <= "2025-07-01") %>% 
#   pull(forecast_item) %>% 
#   unique()
# has_zeros_Q3_25 <- sales_and_fcst %>%
#   filter(!forecast_item %in% IBP_stops_before_Q3_25) %>% 
#   group_by(forecast_item) %>% 
#   filter(date >= "2025-07-01") %>% 
#   summarise(min_Qty = min(Qty)) %>% 
#   filter(min_Qty == 0) %>% 
#   pull(forecast_item) %>% 
#   unique()



# Format tables
sales_and_fcst <- sales_and_fcst %>% 
  filter(!forecast_item %in% c(IBP_not_forecasted))

checkcount<-length(unique(sales_and_fcst$forecast_item))

# sales_and_fcst2 <- sales_and_fcst %>% 
#   filter(date >= "2019-01-01") %>% 
#   filter(date < "2025-11-01") %>% 
#   # arrange(forecast_item,date) %>% 
#   arrange(date) %>%
#   pivot_wider(id_cols = c("forecast_item"),
#               values_from = "Qty",
#               names_from = "date") %>% 
#   filter(!forecast_item %in% IBP_not_forecasted)



# ---- 3 / EXTRACT ----
# RDATA
# save(sales_and_fcst,file = "00_data/IBP_to_extend_EG_MISSING.RData")
# # save(stoped_by_IBP,file = "00_data/IBP_stoped.RData")

# csv
write.csv(sales_and_fcst, file = "00_data/sales_and_fcst_commercial_si_18_jul_23_filter.csv")
#write.csv(IBP_stops_before_Q3_25, file = "00_data/IBP_stops_before_Q3_25_commercial_si_nov.csv")
#write.csv(has_zeros_Q3_25, file = "00_data/has_zeros_Q3_25_commercial_si_nov.csv")
write.csv(IBP_not_forecasted, file = "00_data/IBP_not_forecasted_commercial_si_18_jul_23.csv")

# 



# ----------------------------
# CHECKS
a <- sales_and_fcst %>% group_by(year = year(date)) %>% summarise(sales = list(summary(Qty)))
a$sales[1]

#drop
sales_and_fcst<- sales_and_fcst %>% 
  dplyr::select(-forecast_item) %>%
  rename(Quantity = Qty)
# 
# sales_and_fcst <- sales_and_fcst %>% 
#   select(-date)

save("sales_and_fcst", file="1 - Data/sales_and_fcst_commercial_si_18_jun_23-26_filter_parloc.RData")

#future_fcst2 %>% filter(forecast_item == "EG: 747825") %>% print(n = Inf)




#2028 to 2032 extrapolation gmid only
#long<-sales_and_fcst_28_uom_gmid
#2028 to 2032 extrapolation all
long<-sales_and_fcst_28_all

long <- long %>%
  group_by(LOC, GMID, date) %>% 
  summarise(Quantity = sum(Quantity)) %>% 
  ungroup()
long<-long %>% 
  dplyr::mutate(LOC = ifelse(LOC == "FR002" | LOC == "FR003" | LOC == "FR004","FR001",LOC)) %>% 
  filter(grepl("001",LOC)) %>% 
  mutate(forecast_item=paste0(substr(LOC,1,2),": ",as.numeric(GMID))) %>% 
  dplyr::select(-LOC,-GMID)
long<-long %>% 
  left_join(pipo,
            by=c("forecast_item"="po")) %>% 
  mutate(forecast_item=ifelse(is.na(pi),forecast_item,pi))

long<-long %>% 
  group_by(forecast_item,date) %>% 
  summarise(Quantity=sum(Quantity)) %>% 
  ungroup()
sales_and_fcst_28 <- long %>% 
  mutate(Date = ymd(date)) %>% 
  filter(date >= "2021-01-01" & date <= "2028-12-01") %>% 
  mutate(GMID=str_extract(forecast_item,"[0-9]{4,}")) %>% 
  mutate(LOC=paste0(substr(forecast_item,1,2),"001"))
checkcount<-length(unique(sales_and_fcst_28$forecast_item))
this_cycle <- floor_date(today(),"month")

IBP_not_forecasted <- sales_and_fcst_28 %>% 
  filter(Quantity != 0) %>%
  group_by(forecast_item) %>% 
  mutate(max_date = max(date)) %>% 
  filter(max_date <= this_cycle) %>% 
  pull(forecast_item) %>% 
  unique()
sales_and_fcst_28 <- sales_and_fcst_28 %>% 
  filter(!forecast_item %in% c(IBP_not_forecasted))
checkcount<-length(unique(sales_and_fcst_28$forecast_item))
write.csv(sales_and_fcst_28, file = "00_data/sales_and_fcst_28_all.csv")

a <- sales_and_fcst_28 %>% group_by(year = year(date)) %>% summarise(sales = list(summary(Quantity)))
a$sales[1]

#drop
sales_and_fcst_28<- sales_and_fcst_28 %>% 
  dplyr::select(-forecast_item) %>%
  rename(Quantity = Quantity)
save("sales_and_fcst_28", file="1 - Data/sales_and_fcst_28_all_run.RData")

####ALL####
#2028 to 2032 extrapolation npl
long<-sales_and_fcst_28_npl

long <- long %>%
  group_by(LOC, GMID, date) %>% 
  summarise(Quantity = sum(Quantity)) %>% 
  ungroup()


sales_and_fcst_28 <- long %>% 
  mutate(Date = ymd(date)) %>% 
  filter(date >= "2021-01-01" & date <= "2028-12-01")

checkcount<-length(unique(sales_and_fcst_28$GMID))
this_cycle <- floor_date(today(),"month")

write.csv(sales_and_fcst_28, file = "00_data/sales_and_fcst_28_npl.csv")

#why so many zeroes?
a <- sales_and_fcst_28 %>% group_by(year = year(date)) %>% summarise(sales = list(summary(Quantity)))
a$sales[1]


save("sales_and_fcst_28", file="1 - Data/sales_and_fcst_28_npl_run.RData")
