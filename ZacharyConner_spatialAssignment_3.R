##################################################################################################
# Load packages
list.of.packages <- c("sf", "sp", "spatial", "maptools", "rgeos","rgdal",
                      "raster", "grid", "rasterVis",
                      "tidyverse", "magrittr",
                      "classInt", "RColorBrewer", "ggmap", "tmap", "leaflet", 
                      "spdep","GWmodel", "stringr", "ggpubr", "grid");

# Checks the packages I already have instlled against the list above
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Compares the two lists and downloads the neccessary packages
if(length(new.packages)>0) install.packages(new.packages)

# Load all of the packages
lapply(list.of.packages,function(x) {require(x,character.only = TRUE,quietly = TRUE)})
#################################################################################################

# Read in shapefile from last assignment 
nyc <- st_read('shapefiles\\nycFinal\\nycFinal.shp')

# Read in ACS Data
ACS <- st_read('R-Spatial_III_Lab\\R-Spatial_III_Lab\\R-Spatial_III_Lab\\acsPopByZip.shp')

# Set up a pallete 
pal <- brewer.pal(7, "OrRd")

## 1.

# Plot number of cases by zip

#create the pdf
pdf("covidplot.pdf") 

# make the plot
plot(covid_pop_zip["Positive"], 
     main = "Postive COVID-19 Cases by Zip Code in NYC", 
     breaks = "quantile", nbreaks = 7)
# turn off device
dev.off()


# Plot the distributution of black population by zipcode
pdf("blackPopplot.pdf") 

plot(covid_pop_zip["blackPop"], 
     main = "Distrubtion of Black Population Throughout NYC", 
     breaks = "quantile", nbreaks = 7) 
  
dev.off()

## 2.

covid_plot <- ggplot(covid_pop_zip) +
  geom_sf(aes(fill = Positive)) +
  ggtitle('Positive COVID-19 Cases in NYC') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x='Longitude', 
       y='Latitude', 
       title='Positive COVID-19 Cases',
       caption = 'Data Taken from NYC Department of Health',
       fill = 'Number of Positive Cases')
covid_plot



blkPop_plot <- ggplot(covid_pop_zip) +
  geom_sf(aes(fill = blackPop)) +
  geom_sf_label(data = covid_pop_zip %>%
                  filter(blackPop> 60000),
                aes(label = ZIPCODE),
                label.size = .05,
                size = 2) +
  labs(x='Longitude', 
       y='Latitude', 
       title='Distribution of Black Population in NYC',
       caption = 'Labeled Zipcodes represent \nareas with population greater than 60,000 people',
       fill = 'Number of People') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic()+
  theme(panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue"))
blkPop_plot


asiPop_plot <- ggplot(covid_pop_zip) +
  geom_sf(aes(fill = asianPop)) +
  geom_sf_label(data = covid_pop_zip %>%
                  filter(asianPop> 40000),
                aes(label = ZIPCODE),
                label.size = .05,
                size = 2) +
  labs(x='Longitude', 
       y='Latitude', 
       title='Distribution of Asian Population in NYC',
       caption = 'Labeled Zipcodes represent \nareas with population greater than 40,000 people ',
       fill = 'Number of People') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic()+
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"))

asiPop_plot


ggarrange(covid_plot, blkPop_plot, asiPop_plot, nrow = 3, ncol = 1)  %>%
  ggexport(filename = file.path(getwd(), '3map.pdf'),
           width=6.50, height=8.00, # the unit is inch
           pointsize = 9); 

## 3.

# Create palletes

palette <- colorQuantile("YlOrRd", NULL, n = 5)

blackpal <- colorQuantile("Blues", 
                             domain = c(min(covid_pop_zip$blackPop, na.rm = T), max(covid_pop_zip$blackPop, na.rm = T)),
                             alpha = TRUE)

asianpal <- colorQuantile("Greens", 
                          domain = c(min(covid_pop_zip$asianPop, na.rm = T), max(covid_pop_zip$asianPop, na.rm = T)),
                          alpha = TRUE)

hispPal <- colorQuantile("Oranges", 
                         domain = c(min(covid_pop_zip$hispanicPop, na.rm = T), max(covid_pop_zip$hispanicPop, na.rm = T)),
                         alpha = TRUE)

whitepal <- colorQuantile("plasma", 
                          domain = c(min(covid_pop_zip$whitePop, na.rm = T), max(covid_pop_zip$whitePop, na.rm = T)),
                          alpha = TRUE)

# Create leaflet map

nyc_lealfet <- leaflet(covid_pop_zip %>% sf::st_transform(4326)) %>%
  addPolygons(
    stroke = TRUE, 
    color = 'black',
    fillColor = ~palette(Positive),
    fillOpacity = 0.8,
    smoothFactor = 0.5,
    label = paste0(covid_pop_zip$Positive, " positive cases in ", covid_pop_zip$PO_NAME, ", ", covid_pop_zip$ZIPCODE),
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE),
    group = 'COVID Cases') %>%
  addPolygons(
    stroke = TRUE, color = 'black',
    fillColor = ~blackpal(blackPop),
    fillOpacity = 0.8, 
    smoothFactor = 0.5,
    label = paste0(covid_pop_zip$blackPop, " people in zipcode ", covid_pop_zip$ZIPCODE),
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE),
    group = 'Black Population') %>%
  addPolygons(
    stroke = TRUE, color = 'black',
    fillColor = ~asianpal(asianPop),
    fillOpacity = 0.8, 
    smoothFactor = 0.5,
    label = paste0(covid_pop_zip$asianPop, " people in zipcode ", covid_pop_zip$ZIPCODE),
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE),
    group = 'Asian Population') %>%
  addPolygons(
    stroke = TRUE, color = 'black',
    fillColor = ~hispPal(hispanicPop),
    fillOpacity = 0.8, 
    smoothFactor = 0.5,
    label = paste0(covid_pop_zip$hispanicPop, " people in zipcode ", covid_pop_zip$ZIPCODE),
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE),
    group = 'Hispanic Population') %>%
  addPolygons(
    stroke = TRUE, color = 'black',
    fillColor = ~whitepal(whitePop),
    fillOpacity = 0.8, 
    smoothFactor = 0.5,
    label = paste0(covid_pop_zip$whitePop, " people in zipcode ", covid_pop_zip$ZIPCODE),
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE),
    group = 'White Population') %>%
  
addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addLayersControl(baseGroups = c("OSM", "Carto"), 
                   overlayGroups = c("COVID Cases", "Black Population", "Asian Population", "Hispanic Population", "White Population")) %>%
  hideGroup(c("Black Population", "Asian Population", "Hispanic Population", "White Population")) # Hiding these layers will allow the covid layer to be "Default"
  
nyc_lealfet


# Save as html file
htmlwidgets::saveWidget(nyc_lealfet, 'nyc_covid_leaflet.html') 
 
