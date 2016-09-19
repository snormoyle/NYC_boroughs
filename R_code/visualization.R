library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
nyc_file = "/Users/snormoyle/Desktop/nyc_311.csv"
nyc = fread(nyc_file) %>%
  as.data.frame() %>%
  tbl_df()

nyc = nyc %>%
  dplyr::select(Incident.Address, Borough, Complaint.Type,
  	Created.Date) %>%
  filter(Borough != "Unspecified") %>%
  filter(Incident.Address != "")


pluto_file = "/Users/snormoyle/Desktop/pluto.Rdata"
load(pluto_file)


names(nyc) <- c("Address", "Borough", "Type", "Date")


# change the boroughs name for merging 
boroughs = c("BRONX"="BX", "BROOKLYN"="BK", "MANHATTAN"="MN", 
             "QUEENS"="QN", "STATEN ISLAND"="SI")

for(borough in names(boroughs)){
  nyc$Borough = str_replace_all(nyc$Borough,
                                             borough, boroughs[borough])
}


merged_data = left_join(nyc, pluto, by = c("Address","Borough")) %>%
              filter(!is.na(x)) %>%
              filter(!is.na(y)) %>%
              distinct()



library(stringr)
library(lubridate)


vis_data =  filter(merged_data, Date != "")

vis_data$Date = vis_data$Date %>%
	            str_replace(" .*", "") %>%
	            mdy()




vis_data = filter(vis_data, year(Date) == 2014) %>%
	       filter(month(Date) == 12) %>%
	       filter(day(Date) %in% c(2,3))

incident_type = vis_data$Type
top_5 = sort(table(incident_type), decreasing = TRUE)[2:4]

vis_data = filter(vis_data, Type %in% names(top_5))



library(ggmap)
newyork_map <- get_map(location = 'New York', zoom = 10)


ggmap(newyork_map) +
  geom_point(aes(x = vis_data$x, y = vis_data$y, colour = vis_data$Type), 
  	data = vis_data, alpha = .5, size = 2) +
  	labs(fill = "")


