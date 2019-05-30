library(tidyverse)

combined_data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

# Investigate some trends for North Carolina
# Stop rate tends to be higher for black drivers but search rate is higher
# for Hispanic drivers. Hit rate is lowest for Hispanic drivers overall.

NCdata <- filter(combined_data, state == "NC")

ggplot(NCdata) + 
  geom_boxplot(aes(y = stop_rate, color = driver_race))
ggplot(NCdata) + 
  geom_boxplot(aes(y = search_rate, color = driver_race))
ggplot(NCdata) + 
  geom_boxplot(aes(y = hit_rate, color = driver_race))

ggplot(NCdata) + 
  geom_point(aes(y = stop_rate, x = location, color = driver_race))
ggplot(NCdata) + 
  geom_point(aes(y = search_rate, x = location, color = driver_race))
ggplot(NCdata) + 
  geom_point(aes(y = arrest_rate, x = location, color = driver_race))
ggplot(NCdata) + 
  geom_point(aes(y = hit_rate, x = location, color = driver_race))



# Get data into format to map by county
# Reference - https://stackoverflow.com/questions/23714052/ggplot-mapping-us-counties-problems-with-visualization-shapes-in-r

library(data.table)   # use data table merge - it's *much* faster
map.county <- data.table(map_data('county'))
setkey(map.county,region,subregion)
test <- combined_data %>%
  mutate(county = str_replace(tolower(location), " county", ""),
         key = paste(state, "_", county, sep = ""))
statematch <- data.frame(cbind(state.abb, tolower(state.name)))
test <- left_join(test, statematch, by = c("state" = "state.abb"))
test_map <- data.table(test)
setkey(test_map,V2,county)

map.df      <- map.county[test_map]


# Make maps for Texas counties split by race

texas <- filter(map.df, region == "texas")

ggplot(texas, aes(x=long, y=lat, group=group, fill=stop_rate)) + 
  geom_polygon()+coord_map() + facet_wrap(~driver_race) +
  scale_fill_gradient(trans="log10", 
                      low = "darkolivegreen1", high = "darkolivegreen4") +
  ggtitle("Stop Rate in Texas") 

ggplot(texas, aes(x=long, y=lat, group=group, fill=search_rate)) + 
  geom_polygon()+coord_map() + facet_wrap(~driver_race) +
  scale_fill_gradient(trans="log10", 
                      low = "darkolivegreen1", high = "darkolivegreen4") +
  ggtitle("Search Rate in Texas")

ggplot(texas, aes(x=long, y=lat, group=group, fill=hit_rate)) + 
  geom_polygon()+coord_map() + facet_wrap(~driver_race) +
  scale_fill_gradient(trans="log10", 
                      low = "darkolivegreen1", high = "darkolivegreen4") +
  ggtitle("Hit Rate in Texas")






# Make maps for Ohio counties split by race

ohio <- filter(map.df, region == "ohio")

ggplot(ohio, aes(x=long, y=lat, group=group, fill=stop_rate)) + 
  geom_polygon()+coord_map() + facet_wrap(~driver_race) +
  scale_fill_gradient(trans="log10", 
                      low = "darkolivegreen1", high = "darkolivegreen4") +
  ggtitle("Stop Rate in Ohio") 

ggplot(ohio, aes(x=long, y=lat, group=group, fill=search_rate)) + 
  geom_polygon()+coord_map() + facet_wrap(~driver_race) +
  scale_fill_gradient(trans="log10", 
                      low = "darkolivegreen1", high = "darkolivegreen4") +
  ggtitle("Search Rate in Ohio")






# Make maps for Wisconsin counties split by race

wisconsin <- filter(map.df, region == "wisconsin")

ggplot(wisconsin, aes(x=long, y=lat, group=group, fill=stop_rate)) + 
  geom_polygon()+coord_map() + facet_wrap(~driver_race, nrow = 2) +
  scale_fill_gradient(trans="log10") +
  ggtitle("Stop Rate in Wisconsin") +
  labs(caption = "Stop rate is measured as stops per person of driving age (by race and location)")

ggplot(wisconsin, aes(x=long, y=lat, group=group, fill=search_rate)) + 
  geom_polygon()+coord_map() + facet_wrap(~driver_race) +
  scale_fill_gradient(trans="log10", 
                      low = "darkolivegreen1", high = "darkolivegreen4") +
  ggtitle("Search Rate in Wisconsin")
