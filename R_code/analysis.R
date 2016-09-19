library(data.table)
library(dplyr)
library(lubridate) 
library(stringr)
library(e1071)
library(lubridate)
library(ggmap)

#GEOCODING
#load nyc 311 service data
nyc_file = "/home/vis/cr173/Sta523/data/nyc/nyc_311.csv"
#nyc_file = "/Users/snormoyle/Desktop/nyc_311.csv"
nyc = fread(nyc_file) %>%
  as.data.frame() %>%
  tbl_df() %>%
  dplyr::select(Incident.Zip, Incident.Address, Street.Name, Cross.Street.1,
         Cross.Street.2, Intersection.Street.1, Intersection.Street.2,
         City, Address.Type, Borough, Park.Borough, Unique.Key, Complaint.Type,
         Created.Date) %>%
  filter(Borough != "Unspecified") %>%
  filter(Incident.Address != "")

# load pluto data
pluto_file = "/home/vis/cr173/Sta523/data/nyc/pluto/pluto.Rdata"
# pluto_file = "/Users/snormoyle/Desktop/pluto.Rdata"
load(pluto_file)


# get address information from pluto
nyc_address_boroughs = nyc %>% 
  dplyr::select(Created.Date, Complaint.Type, Incident.Address, Borough) %>%
  filter(Incident.Address != "")

names(nyc_address_boroughs) <- c("Date", "Type", "Address", "Borough")


# change the boroughs name for merging 
boroughs = c("BRONX"="BX", "BROOKLYN"="BK", "MANHATTAN"="MN", 
             "QUEENS"="QN", "STATEN ISLAND"="SI")
for(borough in names(boroughs)){
  nyc_address_boroughs$Borough = str_replace_all(nyc_address_boroughs$Borough,
                                             borough, boroughs[borough])
}


# merge the point with same address and borough and ignore missing values
merged_data = left_join(nyc_address_boroughs, pluto, by = c("Address", "Borough")) %>%
              filter(!is.na(x)) %>%
              filter(!is.na(y))


################
# plot the map
# f = factor(Merge.address.clean$Borough)
# plot(c(Merge.address.clean[,"x"], Merge.address.clean[,"y"]),
#     pch=16, cex=0.3, 
#     col= adjustcolor(as.numeric(f), 0.2))
################


#PLOT NYC BOROUGHS
short_to_long = c("BK"="Brooklyn", 
                  "BX"="Bronx",
                  "MN"="Manhattan",
                  "QN"="Queens",
                  "SI"="Staten Island")

## create raster for prediction location
library(raster)
r = raster(nrows = 1000, ncols = 1000,
           xmn = -74.3, xmx = -73.6,
           ymn = 40.49, ymx = 40.92)
r[]= NA

# location to predict
pred_locs = data.frame(xyFromCell(r,1:(1000*1000)))
names(pred_locs) =c("long","lat")

# sample part of data  
n = nrow(merged_data) 
sample_1 = sample(1:n, 30000)

# subset of points from nyc 311 service
sub_points = dplyr::select(merged_data[sample_1,], x, y)
colnames(sub_points) = c("long", "lat")
sub_boroughs = as.factor(merged_data[sample_1,]$Borough)

nyc_points = merged_data[ , c("x", "y")]
colnames(nyc_points) = c("long", "lat")

# get points that are not in nyc
pred_locs_r = round(pred_locs,2)
nyc_points_r = round(nyc_points,2)
not_nyc_points = anti_join(pred_locs_r,  nyc_points_r)


# sample part of points not in nyc
n = nrow(not_nyc_points)
sample_2 = sample(1:n, 10000)
sub_not_nyc_points = not_nyc_points[sample_2,]

# combine all the points inside/outside nyc
points = bind_rows(sub_points, sub_not_nyc_points)
boroughs = c(as.vector(sub_boroughs), rep("ZOther", nrow(sub_not_nyc_points)))
boroughs = as.factor(boroughs)

# use svm model to get the boundary we want.
# we set the cost and gamma large, because after cleaning most of our data 
# should be correct (the data are wrong only when it is both wrong in pluto and 311) 
new_model = svm(points, boroughs, cost = 10, gamma = 15)
new_pred = fitted(new_model)

# Check accuracy:
table(new_pred, boroughs)

# predict all the points in pred_locs
r[] = predict(new_model, pred_locs)

## create Polygons
poly = rasterToPolygons(r,dissolve = TRUE)
# ignore the other points not in nyc
poly = poly[1:5, ]
# plot(poly,col=1:5)
names(poly@data) = "Name"
poly@data$Name = short_to_long[levels(new_pred)[1:5]]

source("write_geojson.R")
write_geojson(poly,"boroughs.json")

#Save the polygon to file 
png("poly.png")
plot(poly)
dev.off()

##VISUALIZATION

# clean date entries #

vis_data =  filter(merged_data, Date != "") %>%
            distinct()

vis_data$Date = vis_data$Date %>%
                str_replace(" .*", "") %>%
                mdy()

# subset one year, one month, two days #
vis_data = filter(vis_data, year(Date) == 2014) %>%
           filter(month(Date) == 12) %>%
           filter(day(Date) %in% c(2,3))

# subset 3 incident types #
incident_type = vis_data$Type
sub_types = sort(table(incident_type), decreasing = TRUE)[2:4]
vis_data = filter(vis_data, Type %in% names(sub_types))

save(vis_data, file = "vis_data.Rdata")



