
options(java.parameters = "-Xmx8g")

library(RJDBC)

# Set up driver
download.file('https://jdbc.postgresql.org/download/postgresql-42.2.14.jar','postgresql-42.2.14.jar')
drv <- JDBC("org.postgresql.Driver", paste0( "postgresql-42.2.14.jar"))

# Set up connection
name <- dbConnect(drv,
                  driver = "PostgreSQL",
                  user = "pg2admin",
                  password = "Tm^uPI_Rb#7RLc",
                  url = "jdbc:postgresql://scidr-prod-emea.cgxjqdlzwzn9.eu-west-1.rds.amazonaws.com:5432/P1P48499")

#sales
#full_sales<-dbGetQuery(name,"select * from sc_coe_forecasting.salesdatabase")
#save(full_sales,file="full_sales_jan.RData")

#onlycommercial old
#commercial_sales<-dbGetQuery(name,"select * from sc_coe_forecasting.knx_sales")
#save(commercial_sales,file="commercial_sales_mar_23_test.RData")

#onlycommercial new
#commercial_sales<-dbGetQuery(name,"select * from sc_coe_flat.knx_sales")
commercial_sales<-dbGetQuery(name,"select * from sc_coe_flat.knx_sales as df where df.customerid like '%SPC%' or df.customerid like '%SGZ%' ")
save(commercial_sales,file="commercial_sales_jun_23.RData")

#test sales 2022
#library(lubridate)
#full_sales<-dbGetQuery(name,"select * from sc_coe_forecasting.salesdatabase_cleansed")
#full_sales$Year<-year(full_sales$date)
#year_sales <- full_sales %>%
  #group_by(loc, bu, gmid, Year) %>% 
  #summarise(msales = sum(msales)) %>% 
  #ungroup()
#save(year_sales,file="year_sales.RData")
#write.csv(year_sales, file = "00_data/year_sales.csv")


#forecast load
#check screenshot WRANGLE FUTURE FORECAST DATA in dmdg extractions

#pipo check if it is refreshed
pipo<-dbGetQuery(name,"select * from sc_coe_forecasting.pipo_list")

save(pipo,file="pipo_jul_18_23.RData")
getwd()

#parloc use the one which is updated in \\E21FLSBCNSCHUB\bcn_sc_hub\3 - Forecast\99 - Data par_loc_gmid_masterdata_origin
#drag and drop to working directory

#source parloc
#parloc<-dbGetQuery(name,"select * from sc_coe_bds.par_loc_gmid_masterdata")
#save(parloc,file="parloc_dec.RData")

#save(full_sales,file="full_sales2.RData")