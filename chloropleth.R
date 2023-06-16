

# Importing the libraries -------------------------------------------------

library(pacman)
p_load(readxl, dplyr, ggplot2, sf, tmap, plotly, rgdal,leaflet)

# Importing data ----------------------------------------------------------

kenya_counties <- st_read("data/County.shp")

data2 <- data.frame(kenya_counties$COUNTY, kenya_counties$geometry, kenya_counties$COUNTY3_ID, kenya_counties$AREA)
colnames(data2) <- c("county", "geometry", "county_ID", "Area")


# Getting the population data ---------------------------------------------

pop <- rKenyaCensus::V1_T2.2

pop <- pop %>% 
  filter(County != "Total")

#View(pop)
#View(data2)


# Getting the Counties in Data that are not in Population -----------------

data2[data2$county %in% pop$County == FALSE,]

# Renaming the Counties appropriately -------------------------------------

data2$county = ifelse(data2$county == "Tharaka", "Tharaka-Nithi" ,data2$county )
data2$county = ifelse(data2$county == "Nairobi", "Nairobi City" ,data2$county )
data2$county = ifelse(data2$county ==   "Keiyo-Marakwet", "Elgeyo/Marakwet" ,data2$county )
data2$county = ifelse(data2$county == "Taita Taveta" ,"Taita/Taveta",  data2$county )

# Arranging the counties in both data sets in the same way ----------------

data2 <- data2 %>% 
  arrange(county)
indi <- match(pop$County, data2$county)

data2 <- data2[indi,]

data3 <- cbind(data2, pop[,-1])

# Plotting the data -------------------------------------------------------

data4 <- st_as_sf(data3)
pal <- colorNumeric(palette = "YlOrRd", domain = data3$Total)

labels <- sprintf(
  "<strong>%s</strong><br/>Male Population: %s<br/>Female Population: %s<br/>Intersex Population: %s<br/>Total Population: %s",
  data4$county,
  format(data4$Male, big.mark = ","),
  format(data4$Female, big.mark = ","),
  format(data4$Intersex, big.mark = ","),
  format(data4$Total, big.mark = ",")
) %>% lapply(htmltools::HTML)


leaflet() %>% 
  setView(lng = 36.8172, lat = -1.2864, zoom = 6) %>%
  addPolygons(
    data = data4,
    fillColor = ~pal(Total),
    fillOpacity = 1,
    color = "red",
    weight = 1,
    label = labels,
    
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto",
      offset = c(0,20),
      opacity = 0.77,
      color = "black"),
    
    highlightOptions = highlightOptions(
      weight = 5,
      fillOpacity = 0.7,
      color = "red",
      bringToFront = TRUE,
      stroke = TRUE)
    
  ) %>% 
  addLegend("bottomright", pal = pal, values = data4$Total, title = "Population", opacity = 0.7)

# With ggplotly -----------------------------------------------------------

plot <- data4 %>%
  ggplot() +
  geom_sf(aes(fill = Total)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue" ) +
  theme_classic() +
  labs(title = "Population in Kenya") +
  theme(plot.title = element_text(hjust = .5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggplotly(plot)




  
  
  
  
  
  
  
  

