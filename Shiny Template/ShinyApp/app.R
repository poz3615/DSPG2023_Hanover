
# This is a Shiny web application. You can run the application by clicking on
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# LAND USE PROJECT APP


library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)
library(stringr)
library(shinyjs)
library(ggplot2)
library(plotly)
library(rsconnect)
library(rgdal)
library(plyr)
library(tigris)
library(dplyr)
library(leaflet)
library(tidycensus)
library(tidyverse)
library(viridis)
library(readxl)
library(RColorBrewer)
library(highcharter)
library(sf) #for importing shp file
library(highcharter) #for transition matrix
library(htmlwidgets) #for transition matrix
library(png)
library(slickR)
library(shinydashboard)
library("writexl")
library(colorspace) 
library(leaflet.extras)
library(webshot)
library(purrr)
library(mapview)

options(scipen=999)
#options(shiny.maxRequestSize = 80*1024^2)

# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
 jscode <- 'var x = document.getElementsByClassName("navbar-brand");
    var dspgLink = "https://dspg.aaec.vt.edu/";
    var githubLink = "https://github.com/VT-Data-Science-for-the-Public-Good";
    var dspgLogoHTML = \'<a href="\' + dspgLink + \'"><img src="DSPG_black-01.png" alt="VT DSPG" style="height:42px;"></a>\';
    var githubLogoHTML = \'<a href="\' + githubLink + \'"><img src="github_logo.png" alt="GitHub" style="max-height: 30px; max-width: 100%;"></a>\';
    var logosHTML = dspgLogoHTML + githubLogoHTML;
    x[0].innerHTML = x[0].innerHTML + " " + logosHTML;
  '

# DATA --------------------------------------------------------------------------------------------------------------------

# DATA --------------------------------------------------------------------------------------------------------------------------



## SOCIODEMOGRAPHICS ===========================================================================================================


# Employment Graph
employ <- read.csv("data/Employment.csv")
employ <- head(employ, -1)
employ <- na.omit(employ)
employ_plot <- ggplot(employ, aes(x = reorder(EmploymentTypes, Percent), y = Percent, fill = EmploymentTypes,
                                  text = paste0("Employment Type:", `EmploymentTypes`, "\n","Percent: ", round(`Percent`, 1)))) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none") +
  labs(title = "Total Employment For Each Industry", 
       y = "Percent",
       caption = "Data Source: US Census ACS 5-Year 2019 Data") +
  theme(axis.title.y = element_blank())+
  coord_flip() 

employ_plot <- ggplotly(employ_plot, tooltip = "text")


## POLICY =========================================================================================================

### CONSERVATION ===============================================================================================================
shapefile <- st_read("data/conservation/conservation_pol.shp")
hanover_boundary <- st_read("data/conservation/Hanover_County_Boundary.shp")
boundaryleaflet <- hanover_boundary %>%
  st_as_sf() 


afd1 <- function(afd) {
  if (afd  == "1") {
    return("Agricultural Forest District")
  } else if (is.na(afd)) {
    return("Missing") 
  } else {
    return("Non Agricultural Forest District")
  }
}

shapefile$afd1 <- sapply(shapefile$afd, afd1)

colored_afd <- shapefile %>%
  filter(afd1 == "Agricultural Forest District")

color_map_afd <- colorFactor(palette = "#73D055FF", 
                             domain = colored_afd$afd1)

ease <- function(easement) {
  if (easement  == "1") {
    return("Conservation Easement")
  } else if (is.na(easement)) {
    return("Missing") 
  } else {
    return("Non Conservation Easement")
  }
}
shapefile$ease <- sapply(shapefile$easement, ease)

color_ease <- shapefile %>%
  filter(ease == "Conservation Easement")

color_map_ease <- colorFactor(palette = "#238A8DFF", 
                              domain = color_ease$ease)


labelled_nca <- function(nca) {
  if (nca  == "1") {
    return("Natural Conservation Areas")
  } else if (is.na(nca)) {
    return("Missing") 
  } else {
    return("Non Natural Conservation Areas")
  }
}

shapefile$labelled_nca <- sapply(shapefile$nca, labelled_nca)

colored_NCA <- shapefile %>%
  filter(labelled_nca == "Natural Conservation Areas")

color_map_NCA <- colorFactor(palette = "#FDE725FF", 
                             domain = colored_NCA$labelled_nca)


labelled_consland <- function(conland) {
  if (conland == "1") {
    return("Conservation Land")
  } else if (is.na(conland)) {
    return("Missing") 
  } else {
    return("Non Conservation Land")
  }
}

shapefile$labelled_consland <- sapply(shapefile$conland, labelled_consland)

colored_consland <- shapefile %>%
  filter(labelled_consland == "Conservation Land")

color_map_consland <- colorFactor(palette = "#453781FF", 
                                  domain = colored_consland$labelled_consland)


consleaf <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = colored_NCA, color = "transparent", fillColor =  ~color_map_NCA(colored_NCA$labelled_nca), fillOpacity = 1, stroke = TRUE, weight = 1, group = "National Conservation Areas") %>%
  addPolygons(
    data = boundaryleaflet,
    fillColor = "transparent",
    color = "black",
    fillOpacity = 0,
    weight = 0.1
  ) %>%
  addPolygons(data = colored_afd, color = "black", fillColor = ~color_map_afd(colored_afd$afd1), fillOpacity = 1, stroke = TRUE, weight = 1, group = "Agricultural Forest District") %>%
  addPolygons(
    data = boundaryleaflet,
    fillColor = "transparent",
    color = "black",
    fillOpacity = 0,
    weight = 1
  ) %>%
  addPolygons(data = color_ease, color = "black", fillColor = ~color_map_ease(color_ease$ease), fillOpacity = 1, stroke = TRUE, weight = 1, group = "Conservation Easement") %>%
  addPolygons(
    data = boundaryleaflet,
    fillColor = "transparent",
    color = "black",
    fillOpacity = 0,
    weight = 1
  ) %>%
  addPolygons(data = colored_consland, color = "transparent", fillColor =  ~color_map_consland(colored_consland$labelled_consland), fillOpacity = 1, stroke = TRUE, weight = 1, group = "Conservation Land") %>%
  addPolygons(
    data = boundaryleaflet,
    fillColor = "transparent",
    color = "black",
    fillOpacity = 0,
    weight = 0.1
  ) %>%
  addLayersControl(
    overlayGroups = c("Agricultural Forest District", "Conservation Easement", "Conservation Land", "National Conservation Areas"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  )

### SOLAR ===========================================================================================================

## LAND USE ANALYSIS =================================================================================================

### ZONING ==============================================================================================================================


merged_copy <- readRDS(file = "data/mergedNew.Rds")

colored_land <- merged_copy %>%
  dplyr::select(geometry, land_use)

color_map <- colorFactor(palette = viridis(length(unique(merged_copy$land_use))), 
                         domain = merged_copy$land_use)



zoneHan <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLegend(values = colored_land$land_use, pal = color_map, position = "topright",
            title = "Parcel Zoning in Hanover County") %>%
  addPolygons(data = colored_land, color = ~color_map(colored_land$land_use),
              stroke = 1,
              weight = 1,
              fillOpacity = 1,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 3,
                                                  bringToFront = TRUE),
              label = colored_land$land_use,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto"))
  
              



acres_landuse <- merged_copy %>%
  dplyr::select(LOT_ACRES, land_use)

acre <- as.data.frame(acres_landuse) %>%
  na.omit()%>%
  group_by(land_use)%>%
  summarize(mean_acres = mean(LOT_ACRES),
            median_acres = median(LOT_ACRES),
            min_acres = min(LOT_ACRES),
            max_acres = max(LOT_ACRES),
            sd_acres = sd(LOT_ACRES))


mean_a <-ggplot(acre, aes(reorder(land_use, mean_acres), mean_acres, fill = land_use))+
  geom_col()+
  aes(text= paste0("Land Use:", `land_use`, "<br>",
                   "Mean Lot Acres:", round(`mean_acres`, 2)))+
  scale_fill_viridis_d()+
  theme(legend.position = "none", axis.text.x = element_text(size = 7))+
  labs(x = "Land Use Type", y="Lot Acres", caption = "Data Source: Hanover County Assessor Data")+
  ggtitle("Mean Lot Acres By Land Use Type")

interactive_plot <- ggplotly(mean_a, tooltip = "text")


### CROP COVER ==========================================================================================================================

#setting a variable for the crop excel file
crop_excel <- paste0(getwd(), "/data/croplabelswithcategory.xlsx")

#reading in the crop excel file and assigning it to crop data
crop_data <- read_excel(crop_excel)

#selecting just Count and Category columns from the data and assigning it to category data
category_data <- crop_data[, c("Count", "Category")]

combcrop <- aggregate(Count ~ Category, data = category_data, FUN = paste, collapse = ",")

combcrop$Category <- factor(combcrop$Category)

# Sum up the integers by category
summed_cat <- category_data %>%
  group_by(Category) %>%
  summarise(SumCount = sum(Count)) %>%
  ungroup()

#creating bar graph of count by category
landAll <- ggplot(summed_cat, aes(x = reorder(Category, SumCount),
                                  y = SumCount, 
                                  fill = Category,
                                  text = paste0("Crop Cover Type: ", `Category`, "\n","Acres: ", round(`SumCount`, 2)))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs( 
       y = "Acres", 
       title = "Land Cover in Hanover County by Category", 
       caption = "Data Source: USDA Cropland-CROS, 2022") +
  theme(axis.title.y = element_blank())

landAll <- ggplotly(landAll, tooltip = "text")

#subsetting the crop data to only contain categories that are crops and assigning it to just crop
justcrop <- subset(crop_data, 
                   !(Category == "Forested" | 
                       Category == "Developed" | 
                       Category == "Wetlands" | 
                       Category == "Other" |
                       Category == "Water"))

#setting Category to a factor so we can run viridis
justcrop$Category <- factor(justcrop$Category) 

# Sum up the integers by category
summed_catSub <- justcrop %>%
  group_by(Category) %>%
  summarise(SumCount = sum(Count)) %>%
  ungroup()


#plotting count by category with just crop data in a sideways bar graph
landCropONLY <- ggplot(summed_catSub, aes(x = reorder(Category, SumCount), y = SumCount, fill = Category,
                                          text = paste0("Crop Cover Type: ", `Category`, "\n","Acres: ", round(`SumCount`)))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(
       y = "Acres", 
       title = " Land Crops in Hanover County by Category", 
       caption = "Data Source: USDA Cropland-CROS, 2022") +
  theme(axis.title.y = element_blank())


landCropONLY <- ggplotly(landCropONLY, tooltip = "text")


### SOIL QUALITY AND SOIL SUSTAINABILITY ================================================================================================


soillabels <- read_excel("data/soil/soillabelsranking.xlsx")

#assigning the data file path to soil excel
soil_excel <- "data/soil/soillabelsranking.xlsx"

#reading in soil excel and assigning it to soil data
soil_data <- read_excel(soil_excel)

#pulling just Rating, and Acres in AOI columns from soil data and assigning it to rateacre data
rateacre_data <- soil_data[, c("Rating", "Acres_in_AOI")]

#re-naming Rating and Acres in Aoi columns to soil rating and acres
colnames(rateacre_data) <- c("soil_rating", "acres")
rateacre_data_clean <- rateacre_data[-c(120), ]

#turning soil rating column to a factor to use viridis 
rateacre_data_clean$soil_rating <- factor(rateacre_data_clean$soil_rating) 

total_acres <- aggregate(acres ~ soil_rating, data = rateacre_data_clean, FUN = sum)

#creating a sideways bar plot showing the acres of each rating in the county with viridis
sR <- ggplot(total_acres, aes(x = reorder(soil_rating, acres), y = acres, fill = soil_rating,
                              text = paste0("Soil Rating: ", `soil_rating`, "\n","Acres: ", round(`acres`)))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none") +
  labs( y = "Acreage", title = "USDA Soil Rating by Acerage in Hanover County", caption = "Data Source: USDA, NRCC Web Soil Survey, 2019") +
  theme(axis.title.y = element_blank())

sR <- ggplotly(sR, tooltip = "text") 



#assigning the data file path to soil excel
solarsoil_excel <- "data/soil/solarsoils.xlsx"

#reading in soil excel and assigning it to soil data
solarsoil_data <- read_excel(solarsoil_excel,sheet="Sheet1")

#pulling just Rating, and Acres in AOI columns from soil data and assigning it to rateacre data
rateacre_solar <- solarsoil_data[, c("Rating", "Acres_in_AOI")]

#re-naming Rating and Acres in Aoi columns to soil rating and acres
colnames(rateacre_solar) <- c("soil_rating", "acres")

#turning soil rating column to a factor to use viridis 
rateacre_solar$soil_rating <- factor(rateacre_solar$soil_rating) 

# Sum all values to create a single value for each bar
total_acres <- aggregate(acres ~ soil_rating, data = rateacre_solar, FUN = sum)

#creating a sideways bar plot showing the acres of each rating in the county with viridis
rateacre <- ggplot(total_acres, aes(x = reorder(soil_rating, acres), y = acres, fill = soil_rating,
                                    text = paste0("Soil Rating: ", `soil_rating`, "\n","Acres: ", round(`acres`)))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none") +
  labs( 
       y = "Acreage", 
       title = "Suitability for Soil-Anchored Solar Array by Acerage in Hanover County", 
       caption = "Data Source: USDA, NRCC Web Soil Survey, 2019") +
  theme(axis.title.y = element_blank())

rateacre <- ggplotly(rateacre, tooltip = "text") 



# ui --------------------------------------------------------------------------------------------------------------------

ui <- navbarPage(selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                 useShinyjs(),
                 
                 ## Tab Overview--------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   h1(strong("Land Use and Solar Farming Assessment in Hanover County, Virginia"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 8px;",
                                   align = "left",
                                   column(4,
                                          h2(strong("Project Background")),
                                          h4(strong("Setting:")),
                                          p("Hanover County, Virginia is a predominantly rural area located twelve miles north of the state capital, 
                                            Richmond. The county ranges over 303,000 acres and is known for its farmlands, rolling hills and forests 
                                            bordered by the Chickahominy and Pamunkey Rivers. Hanover’s rich agricultural history has thrived from 1720 
                                            to the present day through its tobacco cultivation, crop diversification, dairy farming and small family 
                                            farms. Hence, the agricultural heritage has majorly influenced the landscape, community and rural charm of the county."),
               
                                          p(),
                                          h4(strong("Problem:")),
                                          p("The solar projects in Hanover County have mixed reactions from local residents. While some long-time members of the 
                                            community prefer to maintain the county's rich rural history, others recognize the value of transitioning to solar energy. 
                                            Many residents also see the solar farms as a better alternative to the potential overcrowding that could result from the 
                                            land being used for residential subdivisions."),
                                          p(),
                                          h4(strong("Project:")),
                                          p(" Virginia Tech Department of Agricultural and Applied Economics
                                            Data Science for the Public Good (DSPG) program assesses land 
                                            conversion and solar farm land usage in Hanover County through 
                                            the use of data analytics, agricultural economics and geospatial tools.")
                                   ),
                                   column(4,
                                          h2(strong("Project Goals")),
                                          p("This project utilizes geospatial data and administrative parcel records to assess land 
                                            parcels for their suitability for solar farm development, with a focus on agricultural and
                                            rural areas possessing prime farmland and favorable solar farm characteristics. By analyzing
                                            factors such as parcel zoning, soil quality, land cover, conservation areas, and relevant 
                                            policies, we create geospatial visualizations on an interactive dashboard, mapping key land
                                            characteristics countywide. Additionally, an index is constructed to rate each parcel based 
                                            on proximity to energy infrastructure, suitability for solar farms, prime farmland presence, 
                                            and road accessibility. We also consider an alternative mixed land-use approach known as 
                                            agrivoltaics, and address its potential within Hanover County. Leveraging these data in a statistical
                                            model, we investigate the relationship between prime agricultural land and land suitable for 
                                            solar farms. Our research provides valuable insights into areas vulnerable to solar farm development
                                            in Hanover County, aiding informed decision-making in solar energy planning and development."),
                                          p(),
                                          h4(strong("Research Questions:")),
                                          p("Which parcels in Hanover County have the most desirable characteristics for Solar Farm Development?"),
                                          p("1. How do these parcels compare to parcels with prime agricultural land?"),
                                          p("2. How do different factors (acreage, soil type, zoning, land cover, policy, etc.) play into solar farm development?"),
                                          p("3. How can we preserve agricultural land while developing solar farms?")
                                          #leafletOutput("baseHan") %>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                   ),
                                   column(4,
                                          h2(strong("County Overview")),
                                          p("Hanover County takes pride in their rural lifestyle and heritage, therefore as they 
                                            look to strategically manage development, challenges arise. The main problems facing
                                            this county as it looks to achieve economic growth are urban sprawl and land conversion 
                                            resulting in a loss of agricultural areas. Urban sprawl is the extension of urban 
                                            areas which in turn decreases the amount of rural land available. Furthermore, land conversion
                                            shifts land use from one purpose to another. For instance, agricultural land can be 
                                            re-zoned to commercial, residential or industrial land, opening it up to be developed for 
                                            new uses. With rapid growth amongst the renewable energy industry, solar farms are frequently 
                                            being developed in this region. Given its proximity to Richmond and large areas of suitable 
                                            land, Hanover County is a desirable location for solar farm development companies. The 
                                            installation of commercial solar farms on agricultural land has residents worried about 
                                            the loss of Hanover County’s rural heritage."),
                                          p(),
                                          HTML('<center><img src="HC.png" width="180"></center>')
                                          #img(src = "HC.png", width = "200px")
                                   )
                                   
                                   
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: July 2023')))
                          ) 
                 ),
                 
            
                 
                 ## Tab Policy --------------------------------------------
                 tabPanel("Background and Policy", value = "conclusion", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Land Use & Environmental Policies"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   
                          
      
                                   tabsetPanel(
                                     
                                     tabPanel("Background",
                                              titlePanel(h2(strong("Sociodemographic Background"))),
                                              column(6,
                                                     align="left",
                                                     p("To gain insight into the sociodemographic background of Hanover County for the year 2019, we relied on data 
                                                       provided by the American Community Survey (ACS) 5-year data, spanning from 2017 to 2021. The U.S. Census Bureau 
                                                       conducts the ACS, which encompasses demographic variables, such as race, median household income, median gross 
                                                       rent and population size. The creation of ACS visualizations involves the utilization of designated census tracts,
                                                       which act as statistical subdivisions of the county. These tracts are established by the Census Bureau and are 
                                                       adjusted according to the settlement’s density, ensuring accurate and comprehensive data representation. Assessing 
                                                       the sociodemographic background helps us gain an insight into the unique needs, preferences and priorities of local 
                                                       communities and assists us in understanding the economic dynamics of the county."),
                                                     h4(strong("Map Analysis")),
                                                     textOutput("selected_emp_text")),
                                              column(6,
                                                     tabsetPanel(
                                                       id="tabs3",
                                                       tabPanel("Demographic Factors",
                                                                p(),
                                                                selectInput(
                                                                  "acs.graphs",
                                                                  "ACS Graphs",
                                                                  c("Population Density" = "pop",
                                                                    "Median Population Income" = "inc"))
                                                                ,
                                                                imageOutput("acs", width = "650px", height = "500px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                                       ), 
                                                       tabPanel("Employment", 
                                                                p(),
                                                                column(12,
                                                                       plotlyOutput("employ_plot", height = "500px") %>% withSpinner(type = 6, color = "#861F41", size = 1.5)
                                                                ))
                                                       
                                                     ), 
                                                     p("Data Source: U.S. Census Bureau - American Community Survey")),

                                              p(),
                  
                                              
                                     ),
                                     tabPanel("Conservation Policy",
                                              fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                                       align = "center",
                                                       h2(strong("Conservation Land Map")),
                                                       leafletOutput("consleaf") %>% withSpinner(type = 6, color = "#861f41", size = 1.25)
                                              ),
                                              p(),
                                              column(12,
                                                     align = "center",
                                                     h2(strong("Conservation Policies")),
                                                     column(6,
                                                            align="left",
                                                            h4(strong("Conservation Land")),
                                                            p("The purple layer of the map highlights the conservation land within Hanover County, 
                                                              incorporating data provided by the Virginia Department of Conservation and Recreation. These lands encompass a variety of state, federal, local, and privately managed areas dedicated to conservation efforts. 
                                                              Their purpose is to protect and preserve natural habitats, wildlife, ecosystems, and scenic landscapes. Landowners are responsible for 
                                                              implementing conservation practices, such as regulating public access and managing resources.")),
                                                     
                                                            column(6,
                                                                   align="left",
                                                            h4(strong("Agricultural Forestal Districts")),
                                                            p("The green layer of the map presents the parcels in Hanover County designated as Agricultural/Forestal Districts (AFD),
                                                              utilizing data sourced from the Virginia Department of Emergency Management. These districts conserve rural land for agricultural production, 
                                                              forest management, timber production, and open space purposes. They are recognized and conserved for their environmental resources and economic 
                                                              importance to the region. AFDs are established through agreements between landowners and the local government, safeguarding the designated land from 
                                                              commercial, industrial, or residential development for a specified period of 4 to 10 years, depending on the agreement details. Importantly, AFDs
                                                              are immune to changes in zoning designations, which restricts certain powers of local governments and ensures a certain level of protection for the 
                                                              designated areas."),
                                                            p()
                                                     ),
                                           
                                                     column(6,
                                                            align="left",
                                                            h4(strong("Conservation Easements")),
                                                            p(),
                                                            p("The teal layer of the map displays the conservation easements present in Hanover County, utilizing data provided by the Virginia Department of Conservation and Recreation.
                                                              Conservation easements entail binding agreements between landowners and government agencies, serving as limitations on future land development and 
                                                              subdivision. Notably, landowners retain control over their properties and the right to sell them. The specifics of these easement agreements vary but 
                                                              consistently aim to preserve land for rural uses, such as agriculture, forest management, and recreational activities like hunting and fishing.")
                                                            ),
                                                     column(6,
                                                            align="left",
                                                            h4(strong("National Conservation Areas")),
                                                            p("The yellow layer of the map depicts parcels within Hanover County that have a portion of their land located within 100 ft of waterways. The data utilized in this analysis is
                                                              sourced from Hanover County GIS Hub, providing insights into parcels with land areas protected by the Chesapeake Bay Preservation Act. 
                                                              The Chesapeake Bay Preservation Act acknowledges the relationship between water quality and land use by limiting development within 100 ft of waterways. 
                                                              As a result of this legislation, affected parcels face restrictions on land development and encounter additional barriers if they intend to pursue such 
                                                              activities."),
                                                            p()
                                                     )
                                                     
                                                     
                                              ),
                                              p(),
                                              p(),
                                              p(),
                                              column(12,
                                                     align = "center",
                                                     h2(strong("Additional Conservation Policies")),
                                                     p(),
                                                     column(6,
                                                            align = "left",
                                                            p(),
                                                            h4(strong("Virginia Open-Space Land Act (Title 15.2, Chapter 18)")),
                                                            p(),
                                                            p("The Commonwealth of Virginia is authorized to form partnerships with landowners aimed at decreasing urban sprawl and protecting open space. 
                                                              It influences the localities to designate parcels of land in Hanover County for use as open-space land to conserve the land values. Open-space 
                                                              land is defined as “any land which is provided or preserved for (i) park or recreational purposes, (ii) conservation of land or other natural 
                                                              resources, (iii) historic or scenic purposes, (iv) assisting in the shaping of the character, direction, and timing of community development, 
                                                              (v) wetlands as defined in §28.2-1300, or (vi) agricultural and forestall production.” [6] ")),
                                                     column(6,
                                                            align="left",
                                                            h4(strong("Conservation Reserve Program (CRP)")),
                                                            p(),
                                                            p("The CRP is a land conservation program managed by the Farm Service Agency aimed to trade yearly rental payments to farmers enrolled in agreement to 
                                                              remove land sensitive to agriculture production and plant species to implement conservation practices. This is meant to ensure the enhancement of the 
                                                              environmental health and quality of land. The land registered in this program is contracted for 10 to 15 years. Desirable land for this program includes 
                                                              agricultural land easily susceptible to erosion, located near bodies of water or providing habitats to wildlife. Hence, the motivating factor behind this 
                                                              program is to reduce soil erosion, enhance wildlife habitats, improve water quality, and stimulate conservation and restoration of land."),
                                                            p()
                                                     ),
                                                     column(6,
                                                            align = "left",
                                                            p(),
                                                            h4(strong("Forest Legacy Program (FLP)")),
                                                            p(),
                                                            p("The FLP is a federal conservation program orchestrated by the U.S Forest Service collaborating with State agencies to preserve and protect private forests 
                                                              through land purchases or conservation easements. The FLP strives to determine forest areas susceptible to being transformed into areas used for non-forest 
                                                              purposes. Participants in the FLP can decide to sell their property or maintain ownership while only selling a segment of the property’s development rights. 
                                                              Additionally, conservation easements are implemented to arrange a legal agreement permitting private ownership of the land while guaranteeing that the land
                                                              values are maintained. The legal agreement is between the landowner and government agency or non-profit land trust. The FLP works to preserve working forests
                                                              in order to protect water quality, recreation opportunities, and forest products. To qualify for the FLP in Virginia, land must be established within a Forest Legacy 
                                                              Area.")),
                                                     column(6,
                                                            align="left",
                                                            h4(strong("Emergency Conservation Program (ECP)")),
                                                            p(),
                                                            p("The program aims to support agricultural producers, such as farmers and ranchers, in repairing and restoring any damaged farmland, conservation land and 
                                                              agricultural infrastructure due to natural disasters. This is done by supplying emergency funding, through cost-share assistance, and technical assistance 
                                                              to producers. The ECP enacts farmland restoration practices which consist of debris removal, land leveling, restoring fences, restoring conservation structures, 
                                                              and providing emergency water during severe drought to aid farmers in restoring their land. The ECP county committee assesses eligibility for assistance via 
                                                              on-site inspections and provides assistance to lands where damage is preventing productivity, repairs are too expensive without federal assistance and if the 
                                                              damage remains unrepaired the land will be further damaged. Funding is granted based on the severity of the damage but “up to 75% of the cost to implement 
                                                              emergency conservation practices can be provided.” Overall, the ECP aids in supporting agricultural producers in need of emergency funding and assistance 
                                                              after disasters.")
                                                     )
                                              ),
                                              
                                              column(12,
                                                     align = "left",
                                                     p(),
                                                     h4(strong("References")),
                                                     p("[1] “Conservation Lands Shapefiles & Metadata,” Virginia Department of Conservation and Recreation, www.dcr.virginia.gov. https://www.dcr.virginia.gov/natural-heritage/cldownload (accessed Jul. 20, 2023). "),
                                                     p("[2] “Conservation Lands Shapefiles & Metadata,” Virginia Department of Conservation and Recreation, www.dcr.virginia.gov. https://www.dcr.virginia.gov/natural-heritage/cldownload (accessed Jul. 20, 2023). "),
                                                     p("[3] “Conservation easements,” Virginia Department of Forestry, https://dof.virginia.gov/forest-management-health/forestland-conservation/conservation-easements/#:~:text=Under%20a%20conservation%20easement%2C%20landowners,public%20access%20to%20their%20land. (accessed Jul. 20, 2023). "),
                                                     p("[4] Agricultural/Forestal Districts - vafb.com, https://www.vafb.com/Portals/FBA/PDFs_and_Resources/membership_at_work/Agricultural%20-%20Forestal%20Districts%20FAQ%20Sheet%2011-1-17.pdf (accessed Jul. 20, 2023)."), 
                                                     p("[5] “Chesapeake Bay Preservation Act,” Virginia Department of Environmental Quality, https://www.deq.virginia.gov/our-programs/water/chesapeake-bay/chesapeake-bay-preservation-act#:~:text=The%20Bay%20Act%20recognizes%20that,local%20land%20use%20decision%2Dmaking. (accessed Jul. 20, 2023)."),
                                                     p("[6] “Virginia Law,” Code of Virginia Code - Chapter 17. Open-Space Land Act, https://law.lis.virginia.gov/vacodefull/title10.1/chapter17/ (accessed Jul. 20, 2023)."),
                                                     p("[7] “Forest Legacy Program,” US Forest Service, https://www.fs.usda.gov/managing-land/private-land/forest-legacy (accessed Jul. 20, 2023)."),
                                                     p("[8] “Forest Legacy Program,” Virginia Department of Forestry, https://dof.virginia.gov/forest-management-health/forestland-conservation/forest-legacy-program/#:~:text=To%20be%20eligible%20for%20the,%2C%20state%2C%20or%20local%20sources. (accessed Jul. 20, 2023)."),
                                                     p("[9] USDA Farm Service Agency, “About the Conservation Reserve Program (CRP),” Conservation Reserve Program, https://www.fsa.usda.gov/programs-and-services/conservation-programs/conservation-reserve-program/index (accessed Jul. 20, 2023)."),
                                                     p("[10] USDA Farm Service Agency, “What Is The Emergency Conservation Program (ECP)?,” Emergency Conservation Program, https://www.fsa.usda.gov/programs-and-services/conservation-programs/emergency-conservation/index (accessed Jul. 20, 2023).")
                                              )
                                              
                                     ),
                                     tabPanel("Solar Policy",
                                              p(),
                                        
                                              
                                              column(12,
                                                     align = "center",
                                                     h2(strong("Solar Policies")),
                                                     p(),
                                                     column(6,
                                                            align = "left",
                                                            p(),
                                                            h4(strong("Virginia Clean Economy Act (VCEA)")),
                                                            p(),
                                                            p("This act is intended to promote the reduction of carbon emissions by increasing wind and solar power usage while creating clean energy jobs. 
                                                              The Renewable Portfolio Standard (RPS) program is established under this act in aims for Virginia to achieve 100% clean energy by 2050. 
                                                              This program specifies a percentage of utility sales or megawatt hour capacity that must be provided by a renewable resource by a specific 
                                                              date from Dominion Energy and Appalachian Power. [1] Additionally, this act enacts a new net energy metering cap. Net Energy Metering is another 
                                                              solar policy that creates “an electric billing tool that uses the electric grid to “store” excess energy produced by your solar panel system.” 
                                                              Net Energy Metering allows unused energy produced from your solar panels to be credited back to you. [2] Virginia General Assembly increased the 
                                                              maximum capacity of renewable energy generation from residential generators from 20 kilowatts to 25 kilowatts and non-residential generators from 
                                                              one to three megawatts. [3] Additionally, the aggregated capacity of solar energy from agricultural generators is 500 kilowatts. [4]")),
                                                     column(6,
                                                            align="left",
                                                        h4(strong("VA Creation of Solar Easements (VA. Code 55.1-137)")),
                                                            p(),
                                                            p("Virginia law allows property owners to create solar easements enacted in writing which grant them rights to prohibit neighboring property owners from
                                                              implementing anything on their land projecting shade onto easement owner’s property. These easements must include “The vertical and horizontal angles, 
                                                              expressed in degrees, at which the solar easement extends over the real property subject to the solar easement; Any terms or conditions under which the
                                                              solar easement is granted or will be terminated; and Any provisions for compensation of the owner of the property subject to the solar easement.” [5] "),
                                                            p()
                                                     ),
                                                     column(12,p()),
                                                     column(6,
                                                            align = "left",
                                                            p(),
                                                            h4(strong("Virginia Solar Panel Covenants and Restrictions (§ 67-701)")),
                                                            p(),
                                                            p("This regulation highlights that community associations can not forbid property owners from installing solar energy devices on their property unless 
                                                              highlighted in the recorded declaration of the community declaration. Nonetheless, community associations are allowed to implement reasonable restrictions 
                                                              on size, place, and placement of solar energy devices on personal property. Restrictions can be deemed unreasonable if the solar panel design “increases 
                                                              the cost of installation of the solar energy collection device by five percent over the projected cost of the initially proposed installation or (ii) 
                                                              reduces the energy production by the solar energy collection device by 10 percent below the projected energy production of the initially proposed 
                                                              installation.” [7] Additionally, community associations have authority to restrain solar farm developments in common areas.")),
                                                     column(6,
                                                            align="left",
                                                     h4(strong("Community Solar Pilot Program")),
                                                            p(),
                                                            p("Dominion Energy created the Community Solar Pilot program which allows customers to purchase energy and Renewable Energy Certificates (RECs) from solar 
                                                              facilities without having to install solar panels directly onto their property. This goal is to help reduce the carbon footprint by providing solar energy 
                                                              to those who may not have personal access to solar panels. There are two options that customers can enroll in – block option or match option. The block 
                                                              option allows customers to match portions of their electricity usage with solar energy or RECs by purchasing “blocks” which represent 100 kilowatt-hours 
                                                              for $2.013. This block charge will be added onto the overall monthly bill and remained fixed. If interested in the block option, residential customers can 
                                                              purchase a maximum of 5 blocks (500 kWh) and commercial customers can purchase a maximum of 10 blocks (1,000 kWh). The match option provides customers with 
                                                              the possibility of a 100% match of their electricity usage with solar energy and RECs for the additional cost of $0.02013 per kilowatt-hour. This is 
                                                              available to residential and small commercial customers under 500kW. [8]  ")
                                                     ),
                                                     column(12,
                                                            align= "left",
                                                            p(),
                                                            h4(strong("References")),
                                                            p("[1] Center for the New Energy Economy, “Virginia - Renewable Portfolio Standard,” The State Policy Opportunity Tracker (SPOT) for Clean Energy, https://spotforcleanenergy.org/state/virginia/renewable-portfolio-standard/ (accessed Jul. 20, 2023). "),
                                                            p("[2]  EnergySage, Inc., “What is net metering and how does it work?,” EnergySage, https://www.energysage.com/solar/solar-101/net-metering/?_gl=1%2A1id3nhy%2A_gcl_au%2AODk2MTI4MTgzLjE2ODg1ODc5OTQ. (accessed Jul. 20, 2023)."),
                                                            p("[3] Virginia Energy, “Solar Power,” Virginia Energy - Renewable Energy - Solar Power, https://energy.virginia.gov/renewable-energy/SolarPower.shtml#:~:text=Legislation%20from%20the%202020%20Virginia,from%20one%20to%20three%20megawatts. (accessed Jul. 20, 2023)."),
                                                            p("[4] Solar United Neighbors, “Net metering in Virginia,” Solar United Neighbors, https://www.solarunitedneighbors.org/virginia/learn-the-issues-in-virginia/net-metering-in-virginia/ (accessed Jul. 20, 2023)."),
                                                            p("[5] Virginia Code Commission, “Virginia Law,” Code of Virginia Code - 55.1-138. Contents of solar easement agreements, https://law.lis.virginia.gov/vacode/title55.1/chapter1/section55.1-138/ (accessed Jul. 20, 2023)."),
                                                            p("[6] “State of Virginia Solar Policy Summary,” Town of Blacksburg Virginia, https://www.blacksburg.gov/home/showpublisheddocument?id=8520 (accessed Jul. 20, 2023)."),
                                                            p("[7] Virginia Code Commission, “Title 67. Virginia Energy Plan,” § 67-701. (Repealed effective October 1, 2021) Covenants regarding solar power, https://law.lis.virginia.gov/vacode/title67/chapter7/section67-701/ (accessed Jul. 20, 2023)."),
                                                            p("[8] Dominion Energy, “Virginia Community Solar Pilot Program,” Dominion Energy, https://www.dominionenergy.com/virginia/renewable-energy-programs/community-solar (accessed Jul. 20, 2023).")
                                                       
                                                     )
                                              )
                                     )
                                   ) 
                          ) 
                          
                          
                 ),
                 ## Tab Land Use --------------------------------------------
                 
                 #navbarMenu("Land Use" , 
                           
                            tabPanel("Land Use", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Variables to Consider"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              tabsetPanel(
                                                
                                                tabPanel("Zoning",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Land Use and Zoning Analysis")),
                                                                         h4(strong("Background")),
                                                                         p("Zoning is important to understand land use because it helps authorities regulate and control land to balance community needs. 
                                                                           These zoning rules are listed in The Code of Virginia which states that each locality can determine land use according to the following factors [1]:"),
                                                                         p("1. The use of land, buildings, structures, and other premises for agricultural, business, industrial, residential, floodplain, and other specific uses; "),
                                                                         p("2. The size, height, area, location, construction, reconstruction, repair, maintenance, or removal of structures;"),
                                                                         p("3. The areas of land, water, and air space to be occupied by buildings, structures, and uses;"),
                                                                         p("4. The excavation or mining of soil or other natural resources."),
                                                                         h4(strong("Analysis")),
                                                                         p("Using geospatial data from the Hanover County GIS Hub [4], the map shows six main zoning categories used to identify zoning classification for each parcel. 
                                                                           The bar chart incorportes different zoning classifications to establish the average lot acreage for each classification."),
                                                                         p("To help with interpretability, 40+ zoning codes were collapsed into these overarching categories:"),
                                                                         p("1. Residential: These parcels shown have an average lot size of around 0.56 acres. The residential area makes up the middle and bottom parts of the county. 
                                                                           This consists of living quarters, houses, apartments, condominiums, and other household types."),
                                                                         p("2. Planned Unit Development: There is only one zoning code in this category that has an estimated mean lot acre size of around 0.75 acres and is found in 
                                                                           the Suburban Service Area (SSA) in the middle of the county. With the last satellite snapshot, the parcel was still under construction based on ARC GIS.
                                                                           Planned Unit Development is a community of single-family homes, condos, or townhomes belonging to the homeowners association or HOA."),
                                                                         p("3. Commercial: Commercial parcels have an estimated averge lot acre size of 2.94 acres. The parcels are seen around residential and industrial areas while 
                                                                           also being near roads. Most of these parcels are found in the condensed residential area at the bottom of the county and the SSA. If an area consists of office 
                                                                           buildings, malls, shops, and various other buildings used for commercial purposes, it can be categorized under commercial zoning."),
                                                                         p("4. Conservation: The parcels are scattered are Hanover County with a mean lot acre size of around 5.87 acres. Most of the parcels are away from residential areas 
                                                                           except for some near the SSA. It can be consistent with protected open land for cultural, natural, historical, or work purposes."),
                                                                         p("5. Industrial: Industrial parcels have an average lot acre size of around 9.72 acres. These are shown to surround and make up most of the SSA and run along the 
                                                                           bottom of the county. Some examples of these include factories, solar energy facilities, chemical plants, etc."),
                                                                         p("6. Agriculture: Parcels in this category have a mean lot acre size of around 12.08 acres. It is shown on the map that most of the county is made up of Agricultural parcels. 
                                                                           This is especially the case on the upper half or the left side of the county. These include barns, farmhouses, farms, and other agricultural land."),
                                                                         p("The bar graph displays mean lot acres for each zoning category from 0.56 acres to 12.08 acres. The largest average value shown at 12.08 mean lot acres belongs
                                                                           to agriculture which shows this type of zoning has the largest parcels. It shows that the county uses its land for agriculture the most. However, the category
                                                                           with the most parcels is residential which is much smaller on average at 0.56 acres."),
                                                                         p("Towards the middle of the county, the Suburban Service Area (SSA) is based on the General Land Use Plan where higher residential densities and more intensive 
                                                                           non-residential uses, such as commercial zoning, are being designated. At the bottom-right side of the county is where the most residential parcels and a decent 
                                                                           amount of industrial parcels are. This is because when getting closer to the city of Richmond, the residential areas will be more dense. Richmond tends to have this 
                                                                           trend due to being the capital of Virginia and one of the largest population-dense cities in the state. Compared to smaller areas in Virginia, when going closer 
                                                                           to the city of Richmond the trend of the number of industrial parcels increases. Cities and more population-dense areas host heavy industrial zones such as power 
                                                                           plants, airports, and factories.")),
                                                                  column(6,
                                                                         tabsetPanel(
                                                                           tabPanel("Land Use Map",
                                                                                    p(),
                                                                                    align = "justify"#,
                                                                                    #leafletOutput("zoneHan") %>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                                                                    
                                                                                    
                                                                                    
                                                                           ), 
                                                                           tabPanel("Land Use in Acreage", 
                                                                                    p(),
                                                                                    plotlyOutput("interactive_plot", height = "500px") %>% withSpinner(type = 6, color = "#861F41", size = 1.5),
                                                                                    
                                                                           ) 
                                                                         ),
                                                                         p("Data Source: Hanover County GIS Hub")),
                                                                  column(12,
                                                                         align= "left",
                                                                         p(),
                                                                         h4(strong("References")),
                                                                         p("[1] Virginia Code Commission, “Virginia Law,” Code of Virginia Code - Article 7. Zoning, https://law.lis.virginia.gov/vacodefull/title15.2/chapter22/article7/ (accessed Jul. 20, 2023)."),
                                                                         p("[2] W. Kenton, “Zoning: What it is, how it works, classification examples,” Investopedia, https://www.investopedia.com/terms/z/zoning.asp (accessed Jul. 20, 2023)."),
                                                                         p("[3] Board of Supervisors, “Section 2 Land Use”, Envision Hanover, https://www.hanovercounty.gov/DocumentCenter/View/2606/Section-2-Land-Usepdf (accessed Jul. 20, 2023)."),
                                                                         p("[4] Hanover County GIS, Assessor Data. (July 7, 2023). Distributed by Information Technology Department Mapping Services Office, Hanover County. Accessed: July 20, 2023. [Online]. Available: https://data-hanovercounty.hub.arcgis.com/documents/hanovercounty::assessor-data/about "),
                                                                         p("[5] A. Fontinelle, “What is a planned unit development (PUD)?,” Forbes, https://www.forbes.com/advisor/mortgages/what-is-a-planned-unit-development/ (accessed Jul. 20, 2023).")
                                                                         
                                                                  ),

                                                         ), 
                                                                ),
                                                tabPanel("Land Cover",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Land Cover Information")),
                                                                         p("Land cover is defined as the physical material on the Earth’s surface, including vegetation, water, ice,
                                                                           bare rock, sand, man-made construction, and other surface types. Analyzing the land cover provides an overarching 
                                                                           view of the county's composition, revealing possibilities and limitations concerning potential changes, utilization, 
                                                                           and management. This becomes particularly significant when considering diverse regulations governing each land cover 
                                                                           and its suitability for various land use projects."),
                                                                         p(),
                                                                         p("The USDA’s National Agricultural Statistics Service and Agricultural Research Service (NASS) web application called CroplandCROS/CropScape 
                                                                           is a satellite database that imparts geospatial data called the Cropland Data Layer (CDL). The CropScape web application allows 
                                                                           users to efficiently conduct area and statistical analysis of planted U.S. commodities. It allows users to geolocate 
                                                                           farms and map areas of interest. “The Cropland Data Layer (CDL) provides raster, geo-referenced, crop-specific land cover map 
                                                                           for the United States. Layers are shown in the CDL which includes a crop mask layer, planting frequency layers, boundary 
                                                                           layers, water layers, and road layers. All of the data is recreated annually using moderate-resolution satellite imagery and extensive agricultural ground truth [1].”"),
                                                                         p(),
                                                                         h2(strong("Map Analysis")),
                                                                         textOutput("selected_crop_text2"),
                                                                         p(),
                                                                         textOutput("selected_crop_text")),
                                                                  column(6,
                                                                         tabsetPanel(
                                                                           id = "tabs2",
                                                                           tabPanel("Land Cover by Parcel",
                                                                                    selectInput(inputId = "crop_type", label = "Select Variable:", choices = c(
                                                                                      "Row Crops" = "RC",
                                                                                      "Horticulture Crops" = "HC",
                                                                                      "Small Grains" = "SG",
                                                                                      "Double Cropped" = "DC",
                                                                                      "Forages" = "F",
                                                                                      "Tree Crops" = "TC",
                                                                                      "Other" = "O",
                                                                                      "Forested" = "FR",
                                                                                      "Wetlands" = "WL",
                                                                                      "Water" = "W",
                                                                                      "Developed" = "DEV")
                                                                                    ),
                                                                                    imageOutput("crop_typePNG", width = "600px", height = "400px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25),
                                                                                    
                                                                                    
                                                                           ), 
                                                                           tabPanel("Land Cover Acreage", 
                                                                                    plotlyOutput("landAll", height = "500px") %>% withSpinner(type = 6, color = "#861F41", size = 1.5)
                                                                                    
                                                                           ),
                                                                           tabPanel("Crop Cover Acreage",
                                                                                    plotlyOutput("landCropONLY", height = "500px") %>% withSpinner(type = 6, color = "#861F41", size = 1.5))
                                                                           
                                                                         ),
                                                                         p("Data Source: USDA, National Agricultural Statistics Service, Cropland - CROS")),
                                                                  column(12,
                                                                         align= "left",
                                                                         p(),
                                                                         h4(strong("References")),
                                                                         p("[1] USDA Ag Data Commons, CropScape - Cropland Data Layer. (November 21, 2022). Distributed by the U.S. Department of Agriculture. Accessed: July 20, 2023. [Online]. Available: https://data.nal.usda.gov/dataset/cropscape-cropland-data-layer"),
                                                                         p("[2] Agricultural Marketing Service, “USDA Definition of Specialty Crop - Agricultural Marketing Service,” What is a Specialty Crop?, https://www.ams.usda.gov/sites/default/files/media/USDASpecialtyCropDefinition.pdf (accessed Jul. 20, 2023)."),
                                                                         p("[3] Cornell CALS, “Small Grains,” CornellCALS College of Agriculture and Life Sciences, https://cals.cornell.edu/field-crops/small-grains (accessed Jul. 20, 2023)."),
                                                                         p("[4] United States Environmental Protection Agency, “Why are Wetlands Important?,” EPA, https://www.epa.gov/wetlands/why-are-wetlands-important#:~:text=Far%20from%20being%20useless%2C%20disease,our%20use%20at%20no%20cost. (accessed Jul. 20, 2023)."),
                                                                         p("[5] A. Kerr, “USDA California Climate Hub,” USDA California Climate Hub - Actionable climate information for California farmers, ranchers, and foresters, https://caclimatehub.ucdavis.edu/2016/07/21/a-cornucopia-of-categories-for-crops/# (accessed Jul. 20, 2023)."),
                                                                         p("[6] N. Grover, “Double cropping - agriculture notes - PREPP,” Prepp By Collegedunia, https://prepp.in/news/e-492-double-cropping-agriculture-notes (accessed Jul. 20, 2023)."),
                                                                         p("[7] USDA, “Forage,” forage | NAL Agricultural Thesaurus, https://agclass.nal.usda.gov/vocabularies/nalt/concept?uri=https%3A%2F%2Flod.nal.usda.gov%2Fnalt%2F6298 (accessed Jul. 20, 2023)."),
                                                                         p("[8] United States Environmental Protection Agency, “What is a Wetland?,” EPA, https://www.epa.gov/wetlands/what-wetland#:~:text=Wetlands%20are%20areas%20where%20water,including%20during%20the%20growing%20season. (accessed Jul. 20, 2023)."),
                                                                         p("[9] Multi-Resolution Land Characteristics Consortium, “National Land Cover Database Class Legend and description,” National Land Cover Database Class Legend and Description | Multi-Resolution Land Characteristics (MRLC) Consortium, https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description# (accessed Jul. 20, 2023)."),
                                                                         p("[10] T. Roberts, “The Importance of Tree Crops in Sustainable Agriculture,” The Permaculture Research Institute, https://www.permaculturenews.org/2017/11/27/importance-tree-crops-sustainable-agriculture/ (accessed Jul. 20, 2023). ")
                                                                    
                                                                  ),
                                                                  
                                                         ), 
                                                ) ,
                                                tabPanel("Soil Quality",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              column(6,
                                                     align="left",
                                                     h2(strong("Background")),
                                                     p("The USDA Natural Resources Conservation Service (NCRS) Web Soil Survey provides a detailed classification of farmland in the United States. 
                                                       The Soil Survey Geographic Database (SSURGO) collects soil data by walking over the land to observe the soil and obtaining soil samples to be 
                                                       analyzed in laboratories. The SSURGO Database uses the data acquired to map various farmland classifications onto specified areas of interest. 
                                                       The USDA NCRS considers these factors when classifying soil: water moisture regimes, soil temperature range, acid-alkali balance, water table, 
                                                       soil sodium content, flooding, erodibility, permeability rate, rock fragment content, and soil rooting depth."),
                                                     h2(strong("Soil Quality Analysis")),
                                                     p("The Web Soil Survey ranks Hanover County’s soil quality by identifying the soil as either prime farmland, farmland of statewide importance, 
                                                       prime farmland if drained, or not prime farmland. The USDA defines prime farmland as “land that has the best combination of physical and chemical 
                                                       characteristics for producing food, feed, forage, fiber, and oilseed crops and is available for these uses.” [1] Prime farmland is the highest 
                                                       ranking and must possess a suitable soil quality to sustainably produce high yields of crops with adequate moisture, water supply, and temperature
                                                       permissible for crop growing seasons. Therefore, this land cannot be susceptible to erosion or flooding, and must have minimal slope. Farmland of 
                                                       statewide importance, which is the second-best ranking, describes soil that almost meets the nutrient requirements to be classified as prime 
                                                       farmland, but is still able to produce high crop yields once treated with acceptable farming methods, or during favorable conditions. Prime 
                                                       farmland if drained describes good soils located in wetlands or waterways currently covered in water. Not prime farmland is soil considered not 
                                                       productive.") , 

                                                      p("The Web Soil Survey Farmland Classifications were mapped onto Hanover County illustrating the spatial relationships between each classification. 
                                                      Most land falls under the classification “All areas are prime farmland” and are centered towards the eastern end, spanning across 103,063.2 acres.
                                                      This category possesses the largest number of acres with 34% of the county’s total acreage designated as prime farmland. The classification “Not 
                                                      Prime Farmland” also makes up 34% of the area and has the second largest number of acres with 103,051.1 spanning vastly across the county. Soil 
                                                      classified as “Farmland of Statewide importance” is concentrated towards the northwestern region making up 31.19% of the county, with 94,545.2 
                                                      acres of land. “Prime Farmland if drained” contains the least number of acres and is in the center of the county encompassing 0.816% of the area, 
                                                      with 2,473 acres.  ")),
                                              column(6,
                                                     tabsetPanel(
                                                tabPanel("Soil Type Map",
                                                         p(),
                                                         imageOutput("soilRate", width = "700px", height = "500px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                                         
                                                         
                                                         
                                                ), 
                                                tabPanel("Soil Type in Acreage", 
                                                         p(),
                                                         plotlyOutput("sR", height = "500px") %>% withSpinner(type = 6, color = "#861F41", size = 1.5)
                                                         
                                                ) 
                                              ),
                                              p("Data Source: USDA, Natural Resource Conservation Service, Web Soil Survey")),
                                              column(
                                                12,
                                                align= "left",
                                                p(),
                                                h4(strong("References")),
                                                p("[1] “Soil Data Access (SDA) Prime and Other Important Farmlands,” U.S. Department of Agriculture Natural Resources Conservation Service, https://efotg.sc.egov.usda.gov/references/public/LA/Prime_and_other_Important_Farmland.html (accessed Jul. 20, 2023)."),
                                                p("[2] “Soil Survey Geographic Database (SSURGO).” U.S. Department of Agriculture Natural Resources Conservation Service, www.nrcs.usda.gov/resources/data-and-reports/soil-survey-geographic-database-ssurgo (accessed Jul. 20, 2023).")
                                              ),

                                              
                                     ),
) ,
                                           
                                              ) 
                                     )), 
                            
                            
                            
                 #),
                 
                 ## Tab Parcellation --------------------------------------------
                 
                 #navbarMenu("Parcellation" , 
                            
                            tabPanel("Solar Farming Assessment", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Solar Farming Assessment"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              tabsetPanel(
                                                tabPanel("Land Suitability",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Land Suitability Analysis")),
                                                                         p("This map shows areas of land within Hanover County based on the level of limitation they pose to solar farm development. 
                                                                           Using NRCS Web Soil Survey data we were able to map the soil types across the county into three different categories, “Not Suitable”, 
                                                                           “Suitable” and “Not Rated”. The index takes into account slope, slope aspect, rock fragment content, corrosivity, saturation and 
                                                                           shrink-swell properties of the soil. The best land for solar farm development within Hanover County is the category of “suitable”. 
                                                                           This land type accounts for 58.15% of Hanover and is concentrated on the eastern end of the county where the majority of prime farmland 
                                                                           is also located. The category of “not suitable” accounts for 40.33% of the area. This type of land is not suitable for solar farms 
                                                                           and will only be considered if there are no other options. Areas categorized as “not rated” provide insufficient data for the index 
                                                                           and are only 1.52% of the total area with quarries filling the majority of that space. This variable is important to consider for a 
                                                                           solar assesment as having flat and workable land is crucial for the installation of solar farms."),
                                                                         p()
                                                                         
                                                                  ),
                                                                  column(6,
                                                                         tabsetPanel(
                                                                           tabPanel("Land Suitability Map",
                                                                                    p(),
                                                                                    imageOutput("SoilLimit", width = "700px", height = "500px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                                                                    
                                                                                    
                                                                           ), 
                                                                           tabPanel("Land Suitability in Acreage", 
                                                                                    p(),
                                                                                    plotlyOutput("rateacre", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                                                                    
                                                                           ) 
                                                                           

                                                                         
                                                                  ),
                                                                  p("Data Source: USDA, Natural Resource Conservation Service, Web Soil Survey"))
                                                         )
                                                         
                                                ), 
                                                tabPanel("Infastructure Proximity",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Energy Infrastructure Proximity")),
                                                                         h3(strong("Background")),
                                                                         p("Solar farms need to be within close proximity of infrastructure that can distribute the power generated from a farm throughout the grid.
                                                                           Connecting a farm directly to a substation is ideal as substations already have the majority of necessary technology that can increase or 
                                                                           decrease the voltage coming from a farm. Connecting solar farms to transmission lines is a possibility, but requires the implementation of
                                                                           new voltage regulating technology. As the development location moves further away from energy infrastructure, the project becomes more expensive 
                                                                           and eventually is not financially feasible. A distance rule of thumb is that solar farms should be developed within 2 miles of a substation or 1000 
                                                                           feet of a transmission line in order to keep development costs low.[1]"), 

                                                                          p("This map was created using transmission line location data from the Homeland Infrastructure Foundation Level Database (HIFLD) and a separate dataset
                                                                          distributed by The Office for Coastal Management which used HIFLD metadata to map all substations within 20 miles of the ocean. The substation metadata 
                                                                          set from HIFLD is only accessible for federal employees, and the substation data we used from The Office for Coastal Management only had half of all 
                                                                          substations within Hanover County. To map all of the substations within the county we used Open Street Map and Google Earth to locate the other substation 
                                                                          locations and add them to our dataset. "),
                                                                         h3(strong("Analysis")),
                                                                         p("The map displayed shows parcels in Hanover County categorized into three buffer zones. Parcels that have land within either 2 miles of a substation or 
                                                                           1000 feet of a transmission line, are part of Buffer zone 1. We created a second and third buffer zone to account for parcels that have very good 
                                                                           characteristics for solar farms, but lack adequate access to infrastructure. Buffer zone 2 contains parcels within 4 miles of a substation or 2,000 
                                                                           ft of a transmission line, and Buffer zone 3 contains parcels within 6 miles of a substation or 3,000 ft of a transmission line. If there is a parcel
                                                                           with great charictaristics for solar farms, development companies will be more likely to spend money on building infrastructure, because the costs can 
                                                                           be regained through revenues from energy production. Only parcels close to energy infrastructure are shown on the map in Buffer zone 1 while in Buffer 
                                                                           zone 3 nearly the entire county is captured. The only area of Hanover County that is not accounted for in a buffer zone is a small region in the northwest.")),
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Infastructure Map")),
                                                                         imageOutput("InfastructurePNG", width = "700px", height = "500px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25),
                                                                         p("Note: Distortion due to high density of residential parcels in Mechanicsville."),
                                                                         p("Data Source: Homeland Infrastructure Foundation - Level Data Base")
                                                                         
                                                                  ),
                                                                  column(12,
                                                                         align= "left",
                                                                         p(),
                                                                         h4(strong("References")),
                                                                         p("[1] “Solar Farm Land Requirements & Solar Developments,” YSG Solar, https://www.ysgsolar.com/blog/solar-farm-land-requirements-solar-developments-ysg-solar (accessed Jun. 20, 2023).")
                                                                    
                                                                  ),
                                                         ),
                                                         
                                                ), 
                                                tabPanel("Road Access",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Road Access Analysis")),
                                                                         p("Solar farms require large machinery and materials to build, and once constructed they must be regularly serviced and maintained. 
                                                                           Therefore, adequate road acess is necessary when determining suitable sites for solar farm development. Hanover County GIS Hub provides a 
                                                                           dataset displaying the centerline of all public roadways within the county. We were able to use this data to select all parcels within 
                                                                           100 feet of the roadway centerlines. A 100 foot buffer was necessary to account for all roadside ditches and marginal land. This variable 
                                                                           helps add to our solar assesment by showing parcels within Hanover County that have adequate access to be developed[1].")
                                  
                                                                  ),
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Road Access Map")),
                                                                         imageOutput("RoadPNG", width = "700px", height = "500px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25),
                                                                         p("Note: Distortion due to high density of residential parcels in Mechanicsville."),
                                                                         p("Data Source: Hanover County GIS Hub")
                                                                         
                                                                  ),
                                                                  column(12,
                                                                         align= "left",
                                                                         p(),
                                                                         h4(strong("References")),
                                                                         p("[1] A. Davis, “Solar Farming Considerations,” Department of Agricultural Economics, University of Kentucky, https://agecon.ca.uky.edu/solar-farming-considerations (accessed May 30, 2023). ")
                                                                    
                                                                  )
                                                                  
                                                         ),
                                                         
                                                ),
                                                tabPanel("Solar Index",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(12,
                                                                         align="left",
                                                                         h2(strong("Background")),
                                                                         p("Hanover County is currently home to a large solar farm, Mechanicsville Solar PV Park. This 28-megawatt solar farm has been in operation since 2018. 
                                                                           Developed by SunEnergy1, the park spans 222 acres and consists of 93,000 modules. The electricity generated by the solar farm is being sold to Dominion 
                                                                           Energy and has the capacity to power approximately 5,000 households [1]. In addition to the Mechanicsville Solar PV Park, Hanover County has recently 
                                                                           approved a new solar farm. Developed by Ameriesco Solar, this 22-acre facility is estimated to generate 5 megawatts of power, which is estimated to 
                                                                           meet the energy needs of 1,500 homes. The farm is located on Peppertown road and is expected to have a lifespan of 40 years. As part of their environmental
                                                                           commitment, the developer plans to plant pollinator-friendly vegetation between the solar panels [2]. Virginia's renewable energy goals have made the 
                                                                           construction of solar farms more common. The state has implemented a policy, the Virginia Clean Economy Act that requires Dominion Energy to achieve 
                                                                           100% renewable energy by 2045, and Virginia Power, a subsidiary of Dominion, to do the same by 2050 [3]. This policy encourages energy companies to develop 
                                                                           more sources of renewable energy, and with the development of more energy sources, a degree of environmental impact and loss of agricultural land is inevitable."),
                                                                         p(),
                                                                         p("To analyze suitable locations for solar farm development within Hanover County we created an index map displaying the Solar Suitablity 
                                                                           Score of parcels with the most desireable characteristics. The parcels displayed all have at least 10 acres of suitable land for solar, 
                                                                           are within 100 feet of road access, and are not zoned for residential use. The Solar Suitability Score ranks parcels with all of these 
                                                                           characteristics on a range of 0 – 100 based on the amount of suitable solar farm land available. We then organized the maps based on 
                                                                           their respective buffer zones, and colored them according to the score, yellow being the largest amount, and purple being the least. 
                                                                           These index maps help give a comprehensive understanding of the areas within Hanover County that are most suitable for solar farm development.")),
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Description of Map")),
                                                                         textOutput("ssindex_write")),
                                                                  column(6,
                                                                         h2(strong("Index Map")),
                                                                         selectInput(
                                                                           "ssbufferType",
                                                                           "Solar Index Buffer",
                                                                           c("Buffer 1" = "buffer_1",
                                                                             "Buffer 2" = "buffer_2",
                                                                             "Buffer 3" = "buffer_3"),
                                                                           
                                                                         ),
                                                                         imageOutput("ssIndexPNG", width = "500px", height = "400px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                                                  ),
                                                                  column(12,
                                                                         align= "left",
                                                                         p(),
                                                                         h4(strong("References")),
                                                                         p("[1] “Power plant profile: Mechanicsville solar PV Park, US,” Power Technology, https://www.power-technology.com/marketdata/power-plant-profile-mechanicsville-solar-pv-park-us/ (accessed May 20, 2023)."),
                                                                         p("[2] J. Cordes, “Hanover approves Solar Farm with $420,000 County windfall,” WRIC ABC 8News, https://www.wric.com/news/local-news/hanover-county/hanover-approves-solar-farm-with-420000-county-windfall/ (accessed May 20, 2023)."),
                                                                         p("[3] R. C. Sullivan, “HB 1526 Electric utility regulation; environmental goals.,” Virginia’s Legislative Information System, https://lis.virginia.gov/cgi-bin/legp604.exe?201%2Bsum%2BHB1526 (accessed May 20, 2023).")
                                                                    
                                                                  )
                                                                  
                                                         ),
                                                         
                                                ),
                                                tabPanel("Agrivoltaic Index",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(12,
                                                                         align="left",
                                                                         h2(strong("Background")),
                                                                         p("Agrivoltaics encompasses utilizing land for the dual purposes of agriculture and photovoltaic energy generation. This system empowers farms to diversify income by 
                                                                           allowing agriculture and solar production to coexist while reducing the carbon footprint. Agrivoltaics involves cultivating crops beneath solar panels, forming a 
                                                                           mutually beneficial ‘cooling’ relationship between the two. By offering shade, the plants are shielded by the panels from intense sun rays thus reducing the amount of 
                                                                           water evaporating from the soil and air temperature. Simultaneously, the plants release water vapor, cooling the panels from beneath [1]. The implementation of cooling 
                                                                           systems enhances solar panel efficiency as they are influenced by temperature. Solar panels generate energy by energizing electrons to a higher level. However, during 
                                                                           periods of elevated temperatures, the number of electrons already energized is increased, thus reducing the panel’s voltage and overall generation [2]. Ideal land for 
                                                                           agrivoltaics entails agricultural lands that satisfy the criteria for solar farming. These requirements consist of flat land covering a minimum of 10 acres with minimal 
                                                                           incline and free from obstructions that may potentially cast shade onto the panels. Additionally, convenient proximity to substations and service roads are preferred [3].")),
                                                                  column(6,
                                                                         align="left",
                                                                         h4(strong("Analysis")),
                                                                         p("To assess the feasibility of implementing agrivoltaics on farms in Hanover, we devised an index map that illustrates the Agrivoltaic Viability Rating. This rating incorporates 
                                                                           specific favorable attributes as defined in the index. The visualizations feature parcels with a minimum of 10 acres of land eligible for both solar farm development and prime 
                                                                           agricultural production, located within 100ft of road access, and not designated for residential use. The rating evaluates parcels that satisfy these characteristics and assigns 
                                                                           them a score ranging from 0 – 100. The rating is based on the size of the parcels and the quantity of available solar farmland and prime farmland within each parcel. Moreover, 
                                                                           the maps were organized into buffer zones and color-coded according to their respective scores. Parcels with a closer proximity to 10 acres are depicted in purple, while those 
                                                                           with the largest acreage are shown in yellow."),
                                                                         h2(strong("Description of Map")),
                                                                         textOutput("arindex_write"),
                                                                         h4(strong("Benefits")),
                                                                         p("Agrivoltaics provides a multitude of benefits including:"),
                                                                         p("1. Economic expansion- This system allows farmers to generate income from traditional agriculture practices and solar energy production. This creates an option for farmers 
                                                                           to sell harvested crops and electricity generated by the solar panels. Hence, providing farmers with the opportunity to diversify their income streams by entering the solar industry."),
                                                                         p("2. Increased land productivity – Implementing this system initiates increased land productivity by addressing the issue of determining the allocation of land for agricultural or solar 
                                                                           farming purposes. Prior to the introduction of agrivoltaics, farms had to decide whether they wanted to designate land solely for agriculture or solar panels. However, the installation 
                                                                           of this system permits land to be effectively utilized for both simultaneously. This approach optimizes land usage and aids in boosting land productivity."),
                                                                         p("3. Water conservation – The shade provided by the panels aids in reducing water evaporation from the soil which allows plants to use less water for irrigation. This is beneficial for 
                                                                           farms located in regions that experience extreme weather such as droughts to extend their growing season and cultivate more crops."),
                                                                         p("4. Solar Grazing – Solar grazing is an approach that utilizes grazing livestock to control plant growth by managing the vegetation under the solar arrays. Livestock assist with maintenance 
                                                                           by trimming beneath the panels and reducing the need for herbicides while benefiting from the shade that the panels provide. Sheep are ideal when considering solar grazing as they eat many 
                                                                           types of weeds and invasive species and can withstand labor intensive grazing."),
                                                                         p("5. Increase Solar Panel Energy Capacity- The mutually beneficial cooling relationship promotes the panel’s temperature reduction, permitting them to achieve peak energy output. 
                                                                           Additionally, farmers can use energy produced by the panels to power their equipment.")
                                                                        
                                                                         
                                                                         ),
                                                                  column(6,
                                                                         h2(strong("Index Map")),
                                                                         selectInput(
                                                                           "arbufferType",
                                                                           "Agrivoltaic Index Buffer",
                                                                           c("Buffer 1" = "buffer_1",
                                                                             "Buffer 2" = "buffer_2",
                                                                             "Buffer 3" = "buffer_3"),
                                                                           
                                                                         ),
                                                                         imageOutput("arIndexPNG", width = "500px", height = "400px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                                                  ),
                                                                  column(12,
                                                                         align= "left",
                                                                         p(),
                                                                         h4(strong("References")),
                                                                         p("[1] USDA Climate Hubs, “Agrivoltaics: Coming Soon to a Farm Near You?,” Agrivoltaics: Coming Soon to a Farm Near You? | USDA Climate Hubs, https://www.climatehubs.usda.gov/hubs/northeast/topic/agrivoltaics-coming-soon-farm-near-you (accessed Jul. 20, 2023)."),
                                                                         p("[2] L. Villazon, “Do solar panels work better on Hot Days?,” BBC Science Focus Magazine, https://www.sciencefocus.com/science/do-solar-panels-work-better-on-hot-days/ (accessed Jul. 20, 2023)."),
                                                                         p("[3] YSG Solar, “Top 5 Solar Farm Land Requirements,” YSG Solar, https://www.ysgsolar.com/blog/top-5-solar-farm-land-requirements-ysg-solar (accessed Jul. 20, 2023)."),
                                                                         p("[4] Enel Green Power, “All the benefits of Agrivoltaics,” Enel Green Power, https://www.enelgreenpower.com/stories/benefits-agrivoltaics (accessed Jul. 20, 2023)."),
                                                                         p("[5] M. Boyd, “The Potential of Agrivoltaics for the U.S. Solar Industry, Farmers, and Communities,” Energy.gov, https://www.energy.gov/eere/solar/articles/potential-agrivoltaics-us-solar-industry-farmers-and-communities#:~:text=Research%20in%20the%20drylands%20of,extreme%20weather%2C%20such%20as%20droughts. (accessed Jul. 20, 2023)."),
                                                                         p("[6] NREL Transforming Energy, “Agrivoltaics,” NREL.gov, https://www.nrel.gov/solar/market-research-analysis/agrivoltaics.html (accessed Jul. 20, 2023)."),
                                                                         p("[7] American Solar Grazing Association, “What Is Solar Grazing And How Does It Work?,” American Solar Grazing Association, https://solargrazing.org/wp-content/uploads/2019/06/Solar-Grazing-Brochure.pdf (accessed Jul. 20, 2023)."),
                                                                         p("[8] Oregon State University College of Agricultural Sciences, “Sustainable Farm Agrivoltaic,” Oregon State University College of Agricultural Sciences News and Accolades, https://agsci.oregonstate.edu/newsroom/sustainable-farm-agrivoltaic#:~:text=Agrivoltaics%20is%20a%20symbiotic%20relationship,helps%20further%20reduce%20water%20usage. (accessed Jul. 20, 2023). ")
                                                                    
                                                                  )
                                                                  
                                                         ),
                                                         
                                                ),
                                                tabPanel("Methodology",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Index Methodology")),
                                                                         p("The Solar Suitability Score and Agrivoltaic Viability Rating Data Frame assembles the key attributes essential for constructing our index. The data frame 
                                                                           incorporates data essential for defining buffer zones, assessing road access, distinguishing between current land use patterns, and determining prime farmland 
                                                                           and land suitability for solar farm development. The first step of calculating the index is multiplying (1) an indicator variable that indicates whether or not 
                                                                           a parcel is within the designated buffer zone around the electric grid, (2) an indicator variable that indicates whether or not a parcel has immediate road access, 
                                                                           (3) an indicator variable that tells us if a parcel is residential or not (this allows us to rule out parcels coded as residential as these will not be suitable for solar 
                                                                           farm development), (4) an indicator variable that tells us if a parcel has at least ten acres of land suitable for solar farm development. Specifically, for the 
                                                                           Agrivoltaic Viability Rating, to complete step one we multiply (5) an indicator variable that ensures that land eligible for both solar farming and prime farmland coincide 
                                                                           with a minimum of 10 acres, by the other variables to complete the index. Parcels that take on a value of zero are omitted from the index as they are not candidates 
                                                                           for solar farm development or agrivoltaics, while parcels that receive a positive value are passed on to step two. The second step of calculating the index is applying 
                                                                           a double natural logarithm transformation to the values from step one. When taking the natural log of value, we add 1 to the input values to ensure that the function is 
                                                                           well-defined, i.e., we compute ln(ln(x+1)+1). The third and final step of calculating the index is to take the values from step two and map them to the interval [0,100]. 
                                                                           This is done by subtracting the minimum value of all parcels from step two from each parcel value, and then dividing this result by the range of values, or the maximum 
                                                                           value of all parcels minus the minimum value. Ultimately, the final result is scaled by multiplying by 100. When the minimum value is subtracted from itself, the result is 
                                                                           0. If the maximum value is used, both the numerator and denominator of the function become identical, yielding a value of 100. All other values receive values between 0 and 100."),
                                                                         h2(strong("Buffer Analysis")),
                                                                         textOutput("selected_buffer_text"),
                                                                         p(),
                                                                         p(),
                                                                         
                                                                         
                                                                         ),
                                                                  column(6,
                                                                         tabsetPanel(
                                                                           id = "tabs",
                                                                           tabPanel("Solar Suitability Score", 
                                                                                    selectInput(
                                                                                      "solar.score",
                                                                                      "Solar Suitabilty Score",
                                                                                      c("Buffer 1" = "buffer_1",
                                                                                        "Buffer 2" = "buffer_2",
                                                                                        "Buffer 3" = "buffer_3")),
                                                                                    imageOutput("ssMethodPNG", width = "550px", height = "500px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25),
                                                                                    p("Note: With the expansion of buffers, the frequencies rise as additional parcels emerge, introducing more data into the index."),
                                                                                    
                                                                           ),
                                                                           tabPanel("Agrivoltaic Viability Rating",
                                                                                    selectInput(
                                                                                      "av.rating",
                                                                                      "Agrivoltaic Viability Rating",
                                                                                      c("Buffer 1" = "buffer_1",
                                                                                        "Buffer 2" = "buffer_2",
                                                                                        "Buffer 3" = "buffer_3")
                                                                                    ),
                                                                                    imageOutput("arMethodPNG", width = "550px", height = "500px")%>% withSpinner(type = 6, color = "#861F41", size = 1.25),
                                                                                    p("Note: With the expansion of buffers, the frequencies rise as additional parcels emerge, introducing more data into the index."),
                                                                         )),

                                                                         )
                                                                  
                                                                  
                                                         ),
                                                         
                                                )
                                              ) 
                                     ), 
                            ) ,
                            
                            
                            


                 ## Tab Data Sources --------------------------------------------
                 tabPanel("Data Sources",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Sources"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                          fluidRow(style = "margin: 6px;", align = "justify",
                                                   column(4,
                                                   img(src = "USDA.png", style = "display: inline; float: left;", width = "150px"),
                                                   p(strong("USDA National Agricultural Statistics Service"), "The National Agricultural Statistics Service (NASS) is 
                                                   an agency within the USDA and provides comprehensive agricultural data, 
                                                     including land use and crop information. We used this data to analyze land and crop cover in Hanover County.")),
                                                   column(4,
                                                   img(src = "NRCS.png", style = "display: inline; float: left;", width = "180px"),
                                                   p(strong("Natural Resource Conservation Service"), "The Natural Resource Conservation Service (NRCS) Web Soil Survey offers detailed 
                                                     soil information, such as soil types, properties, and suitability for various land uses, on areas of land nationwide. This data 
                                                     was used for classifications and mapping of prime farmland and suitable solar farm land.")),
                                                   column(4,
                                                   img(src = "VADCR.png", style = "display: inline; float: left;", width = "150px"),
                                                   p(strong("Virginia Department of Conservation and Recreation"), "The Virginia Department of Conservation and Recreation provides 
                                                     geospatial data on conservation areas, natural resources, and environmental protection efforts. We used data on Virginia conservation 
                                                     areas and easements for the conservation section of our project.")),
                                          ),

                                          fluidRow(style = "margin: 6px;", align = "justify",
                                                   column(4,
                                                   img(src = "VDEM.png", style = "display: inline; float: left;", width = "150px"),
                                                   p(strong("Virginia Department of Emergency Management"), "The Virginia Department of Emergency Management offers data on emergency management, 
                                                     hazard risks, and resilience planning. They also provide a geospatial data layer of all tax parcels in Virginia designated as Agricultural 
                                                     Forestal Districts which was used in our conservation analysis.")),
                                                   column(4,
                                                   img(src = "HC.png", style = "display: inline; float: left;", width = "150px"),
                                          p(strong("Hanover County GIS Hub"), "The Hanover County GIS Hub is a local resource providing geospatial data specific to Hanover County, including parcel information, 
                                            roadways across the county, and all natural conservation areas. We leveraged these specific datasets in contribution to a county-wide analysis.")),
                                          column(4,
                                          img(src = "HIFLD.png", style = "display: inline; float: left;", width = "150px"),
                                          p(strong("Homeland Infrastructure Foundation Level Database"), "The Homeland Infrastructure Foundation Level Database provides geospatial data on critical infrastructure. 
                                            We used data on transmission line and substation location to detect parcels within ideal proximity of existing infrastructure.")),
                                          fluidRow(style = "margin: 6px;", align = "justify",
                                          column(4,
                                                 img(src = "ACS.png", style = "display: inline; float: left;", width = "150px"),
                                                 p(strong("American Community Survey"), "The ACS is an ongoing survey conducted by the U.S. Census Bureau that provides social, economic, housing, 
                                                   and demographic data for smaller geographic areas such as Hanover County. We used this data to gain insight on the sociodemographic and socioeconomic background of Hanover County.")))
                                   ),
                                   ),

                          ),


                 ## Tab Team --------------------------------------------
                 tabPanel("Meet the Team", 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   align = "center",
                                   h1(strong("Meet the Team")),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good")),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program held at the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural'), "and", a(href = 'https://ext.vt.edu/','Applied Economics and the Virginia Cooperative Extension Service.'),
                                     "In its third year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical
                                social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how 
                                information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply,
                                and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Undergraduate Interns")),
                                          img(src = "Ari_Tynes.JPG", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "Deep_Datta.JPG", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          br(), 
                                          img(src = "Gabe_Wiggins.JPG", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href='https://www.linkedin.com/in/ariadne-tynes-236701269/','Ari Tynes', target = '_blank'), "(Berea College, Undergraduate in Economics with a Concentration in Methods and Models);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/deep-datta/', 'Deep Datta', target = '_blank'), "(Virginia Tech, Undergraduate in Computational Modeling and Data Analytics and Computer Science);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/gabe-wiggins/', 'Gabe Wiggins', target = '_blank'), "(Virginia Tech, Undergraduate in Environmental Economics, Management, and Policy)."),
                                          p("", style = "padding-top:10px;"),
                                          
                                          h4(strong("DSPG Graduate Student Fellows and Research Assistants")),
                                          img(src = "Piper_Zimmerman.JPG", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "Samantha Rippley.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                          
                                           p(a(href = 'https://www.linkedin.com/in/piper-zimmerman-509625282', 'Piper Zimmerman', target = '_blank'), "(Virginia Tech, Graduate Student Fellow in Statistics);",
                                             br(), 
                                            a(href = 'https://www.linkedin.com/in/samantha-rippley-58846119b/', 'Samantha Rippley', target = '_blank'), "(Virgina Tech, Graduate Student Fellow in Agricultural Economics)"),
                                           p("", style = "padding-top:10px;")
                                    ),
                                   column(6, align = "center",
                                          h4(strong("VT Faculty Member")),
                                          img(src = "SusanChen.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/susanchenja/", 'Dr. Susan Chen', target = '_blank'), "(Associate Professor of Agricultural and Applied Economics);",
                                          ),
                                          p("", style = "padding-top:10px;"),

                                           h4(strong("Project Stakeholders")),
                                             p(a(href = "https://hanover.ext.vt.edu/staff/maxey-nay-laura.html", 'Laura Maxey-Nay', target = '_blank'), "(Virginia Cooperative Extension, Hanover County)"),
                                              
                                            p("", style = "padding-top:10px;"),
                                            
                                          
                                   )

                          )) ,
                 inverse = T)


# server --------------------------------------------------------------------------------------------------------------------

server <- function(input, output){
  
  shinyjs::runjs(jscode)
  
  output$interactive_plot <- renderPlotly({
    interactive_plot
  })
  
  output$crop_typePNG <- renderImage(deleteFile = FALSE,{
    if (input$crop_type == "RC") {
      return(list(src = "www/RowCrops.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "HC") {
      return(list(src = "www/HorCrops.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "SG") {
      return(list(src = "www/SmallGR.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "DC") {
      return(list(src = "www/DobCrop.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "F") {
      return(list(src = "www/Forages.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "TC") {
      return(list(src = "www/TreeCrops.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "O") {
      return(list(src = "www/Other.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "FR") {
      return(list(src = "www/Forests.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "WL") { 
      return(list(src = "www/Wetlands.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "W") {
      return(list(src = "www/Water.png", width = "100%", height = "100%"))
    }
    else if (input$crop_type == "DEV") {
      return(list(src = "www/Developed.png", width = "100%", height = "100%"))
    }
  })
  
  
  output$soilRate <- renderImage(deleteFile = FALSE,{
    
    return(list(src = "www/soilRate.png", width = "100%", height = "100%"))
    
  })
  
  output$SoilLimit <- renderImage(deleteFile = FALSE,{
    
    return(list(src = "www/SoilLimit.png", width = "100%", height = "100%"))
    
  })

  output$RoadPNG <- renderImage(deleteFile = FALSE,{
    
    return(list(src = "www/Roads.png", width = "100%", height = "100%"))
    
  })
  
  output$InfastructurePNG <- renderImage(deleteFile = FALSE,{
    
    return(list(src = "www/Infastructure.png", width = "100%", height = "100%"))
    
  })
  
  # For buffer images later will be leaflets
  
  # output$bufferType <- renderLeaflet(deleteFile = FALSE, {
  #   
  #   
  # })
  
  output$employ_plot <- renderPlotly({
    employ_plot
  })
  
  output$landAll <- renderPlotly({
    
    landAll
    
  })
  
  output$landCropONLY <- renderPlotly({
    
    landCropONLY
    
  })
  
  output$sR <- renderPlotly({
    
    sR
    
  })
  
  output$rateacre <- renderPlotly({
    
    rateacre
    
  })
  
  # LEAFLET GRAPHS 
  
  # output$hansoil<- renderLeaflet({
  #   hansoil
  # })
  
  output$baseHan<- renderLeaflet({
    limitS
  })
  
  output$zoneHan<- renderLeaflet({
    
    zoneHan
    
  })
  output$consleaf <- renderLeaflet({
    
    consleaf
    
  })
  
  output$socio_write <- renderText({
    if (input$acs.graphs == "pop") {
      return("population")
    }
    else if (input$acs.graphs == "inc") {
      return("income")
    }

  })
  
  output$crop_type_write <- renderText({
    if (input$crop_type == "RC") {
      return("Write up for row crops")
    }
    else if (input$crop_type == "HC") {
      return("Write up for horticulture crops")
    }
    else if (input$crop_type == "SG") {
      return("Write up for small grains")
    }
    else if (input$crop_type == "DC") {
      return("Write up for double cropped")
    }
    else if (input$crop_type == "F") {
      return("Write up for forages")
    }
    else if (input$crop_type == "TC") {
      return("Write up for tree crops")
    }
    else if (input$crop_type == "O") {
      return("Write up for other")
    }
    else if (input$crop_type == "FR") {
      return("Write up for forest")
    }
    else if (input$crop_type == "WL") { 
      return("Write up for wetlands")
    }
    else if (input$crop_type == "W") {
      return("Write up for water")
    }
    else if (input$crop_type == "DEV") {
      return("Write up for developed")
    }

  })
  
  #ACS graphs
  output$acs <- renderImage(deleteFile = FALSE,{
    if (input$acs.graphs == "pop") {
      return(list(src = "www/PopDen.png", width = "125%", height = "100%"))
    }
    else if (input$acs.graphs == "inc") {
      return(list(src = "www/MedPopInc.png", width = "125%", height = "100%"))
    }

  })
  
  #Solar methodology pictures
  output$ssMethodPNG <- renderImage(deleteFile = FALSE,{
    if (input$solar.score == "buffer_1") {
      return(list(src = "www/SSB1.png", width = "125%", height = "100%"))
    }
    else if (input$solar.score == "buffer_2") {
      return(list(src = "www/SSB2.png", width = "125%", height = "100%"))
    }
    else if (input$solar.score == "buffer_3") {
      return(list(src = "www/SSB3.png", width = "125%", height = "100%"))
    }
  })
  #AV methodology pictures
  output$arMethodPNG <- renderImage(deleteFile = FALSE,{
    if (input$av.rating == "buffer_1") {
      return(list(src = "www/ARB1.png", width = "125%", height = "100%"))
    }
    else if (input$av.rating == "buffer_2") {
      return(list(src = "www/ARB2.png", width = "125%", height = "100%"))
    }
    else if (input$av.rating == "buffer_3") {
      return(list(src = "www/ARB3.png", width = "125%", height = "100%"))
    }
  })
#Solar index pictures
  output$ssIndexPNG <- renderImage(deleteFile = FALSE,{
    if (input$ssbufferType == "buffer_1") {
      return(list(src = "www/SSMapB1.png", width = "125%", height = "100%"))
    }
    else if (input$ssbufferType == "buffer_2") {
      return(list(src = "www/SSMapB2.png", width = "125%", height = "100%"))
    }
    else if (input$ssbufferType == "buffer_3") {
      return(list(src = "www/SSMapB3.png", width = "125%", height = "100%"))
    }
  })
  #solar index write up
  output$ssindex_write <- renderText({
    if (input$ssbufferType == "buffer_1") {
      return("This map shows the most ideal parcels for solar farm development within buffer zone 1. Buffer zone 1 contains the most desireable 
             parcels as these are within closest range to existing energy infrastructure, either 2 miles from a substation or 1,000 feet from a 
             transmission line. Therefore, solar farm development companies are able to spend less when constructing in these areas. The parcels 
             displayed roughly outline transmission line and substation locations, with the large amounts of suitable parcels running along 
             Interstate 95 in the northen end of Hanover.")
    }
    else if (input$ssbufferType == "buffer_2") {
      return("The parcels within buffer zone 2 span farther away from energy infrastructure than the parcels within buffer zone 1. This helps to 
             account for parcels that have desireable solar farm characteristics, but aren’t within close range of energy infrastructure. Buffer zone 
             2 captures parcels within 4 miles of a substation or 2,000 ft of a transmission line. If a parcel within this zone is very desireable, 
             solar development companies are likely to develop the needed infrastructure themselves in hopes that an efficient solar farm will generate 
             enough revenue to cover the costs. There are many parcels with large amounts of suitable land located northwest of Ashland.")
    }
    else if (input$ssbufferType == "buffer_3") {
      return("The parcels within buffer zone 3 span the furthest away from energy infrastructure, and cover nearly the whole county, with the exception 
             of an area in the northwest region of Hanover. Buffer zone 3 captures parcels within 6 miles of a substation or 3,000 ft of a transmission line. 
             This helps to account for parcels that have desireable solar farm characteristics, but aren’t within close range of energy infrastructure. 
             If a parcel within this zone is very desireable, solar development companies are likely to develop the needed infrastructure themselves in 
             hopes that an efficient solar farm will generate enough revenue to cover the costs. Parcels along the northern edge of the county, next to the 
             North Anna and Pamunkey Rivers, have the most amount of suitable solar farm land within buffer zone 3.")
    }
  })
  #AV index pictures
  output$arIndexPNG <- renderImage(deleteFile = FALSE,{
    if (input$arbufferType == "buffer_1") {
      return(list(src = "www/ARMapB1.png", width = "125%", height = "100%"))
    }
    else if (input$arbufferType == "buffer_2") {
      return(list(src = "www/ARMapB2.png", width = "125%", height = "100%"))
    }
    else if (input$arbufferType == "buffer_3") {
      return(list(src = "www/ARMapB3.png", width = "125%", height = "100%"))
    }
  })
 #AV write up 
  output$arindex_write <- renderText({
    if (input$arbufferType == "buffer_1") {
      return("This map illustrates parcels highly suitable for agrivoltaics due to their optimal positioning in close proximity to infrastructure while retaining prime 
             farmland attributes. The optimal positioning entails being 2 miles from a substation or 1,000 feet from a transmission line. Parcels falling under the buffer 
             1 category are distributed across the county, with a significant majority of those scoring higher on the rating being located to the north of Ashland.")
    }
    else if (input$arbufferType == "buffer_2") {
      return("The map showcases parcels falling under buffer 2, which is situated at a greater distance from energy infrastructure but possess the desirable characteristics for 
             agrivoltaics. The new distance from energy infrastructure captures parcels within 4 miles of a substation or 2,000 ft of a transmission line. The inclusion of 
             buffer 2 brings forth new parcels surrounding the city of Ashland and extending towards both the western and eastern edges of the county.")
    }
    else if (input$arbufferType == "buffer_3") {
      return("The map displays parcels situated in Buffer 3, representing the farthest distance from energy infrastructure while still exhibiting desirable traits for agrivoltaics. 
             The revised distance from energy infrastructure encompasses parcels within 6 miles from a substation or 3000ft from a transmission line. buffer 3, being the most distant 
             from energy infrastructure, introduces the largest number of new parcels covering nearly the entire county. Consequently, if solar developers and farmers are willing 
             to invest in developing closer infrastructure, if necessary, to offset the distance from existing infrastructure, agrivoltaic farms can be established within these parcels.")
    }
  })
 #Methodology write up 
  selected_tab <- reactive({
    input$tabs
  })
  
  selected_buffer_text <- reactive({
    selected <- selected_tab()
    
    if (selected == "Solar Suitability Score") {
      if(input$solar.score == "buffer_1"){
        return("The Solar Suitability Score for Buffer 1 encompasses ideal parcels for solar farming located within 2 miles from a substation or 1000ft from a transmission line, 
               land within 100ft of roads, 10 acres or more and non-residential zoned land. The index arranges parcels satisfying these criteria based on their size. Parcels that score 
               closer to 0 fulfill all the categories but closer to 10 acres in size, while parcels scoring closer to 100 occupy the largest areas above 10 acres.")
      }
      else if (input$solar.score == "buffer_2"){
        return("The Solar Suitability Score for Buffer 2 encompasses parcels for solar farming located within 4 miles from a substation or 2000ft from a transmission line, within 100ft of roads, 
               have 10 acres or more and non-residential zoned. The index arranges land satisfying these criteria based on their size. Parcels scoring closer to 0 fill all the categories and closer 
               to 10 acres in size, while parcels scoring closer to 100 occupy the largest areas above 10 acres.")
      }
      else if (input$solar.score == "buffer_3"){
        return("The Solar Suitability Score for Buffer 3 encompasses parcels suitable for solar farming located within 6 miles from a substation or 3000ft from a transmission line, within 100ft of roads, 
               have 10 acres or more and non-residential zoned land. The index arranges land satisfying these criteria based on their size. Parcels scoring closer to 0 fill all the categories and are closer 
               to 10 acres in size, while parcels scoring closer to 100 occupy the largest areas above 10 acres.")
      }
    } else if (selected == "Agrivoltaic Viability Rating") {
      if(input$av.rating == "buffer_1"){
        return("The Agrivoltaic Viability Rating for Buffer 1 encompasses parcels desirable for solar farming and areas of prime farmland located within 2 miles of a substation or 1000ft from a transmission 
               line, within 100ft of roads, 10 acres or more and non-residential zoned land. The index arranges land satisfying these criteria based on their size. Parcels scoring closer to 0 fill all the 
               categories and are closer to 10 acres in size, while parcels scoring closer to 100 occupy the largest areas above 10 acres.")
      }
      else if (input$av.rating == "buffer_2"){
        return("The Agrivoltaic Viability Rating for Buffer 2 encompasses parcels desirable for solar farming and areas of prime farmland for agriculture located within 4 miles from a substation or 2000ft from 
               a transmission line, within 100ft of roads, have 10 acres or more and non-residential zoned land. The index arranges land satisfying these criteria based on their size.  Parcels scoring closer to 
               0 fill all the categories and are closer to 10 acres in size, while parcels scoring closer to 100 occupy the largest areas above 10 acres.")
      }
      else if (input$av.rating == "buffer_3"){
        return("The Agrivolaic Viability Rating for Buffer 3 encompasses parcels desirable for solar farming and areas of prime farmland for agriculture located within 6 miles from a substation or 3000ft from a 
               transmission line, within 100ft of roads, have 10 acres or more and non-residential zoned land. The index arranges land satisfying these criteria based on their size. Parcels scoring closer to 0 
               fill all the categories and are closer to 10 acres in size, while parcels scoring closer to 100 occupy the largest areas above 10 acres.")
      }
    } 
  })
  
  output$selected_buffer_text <- renderText({
    selected_buffer_text()
  })
  
  #Crop fixing
  selected_tab_crop <- reactive({
    input$tabs2
  })
  
  selected_crop_text <- reactive({
    selected2 <- selected_tab_crop()
    
    if (selected2 == "Land Cover by Parcel") {
      if (input$crop_type == "RC") {
        return("Row crops refer to annual crops harvested on a large scale and consist of corn, 
               soybeans and similar crops grown in lines. They take up around 9.52% of total land 
               cover and around 41.89% out of all crop covers in the county. Parcels holding row 
               crops are concentrated throughout the county except towards residential areas found 
               at the lower end of the county around Mechanicsville which is closer to Richmond.")
      }
      else if (input$crop_type == "HC") {
        return("Horticulture crops are fruits, vegetables, edible nuts, some ornamental crops, and 
               nursery crops that are grown for their contribution to the flavor and interest of food 
               and the supply of minor but essential nutrients. These crops make up around 0.015% of 
               land cover and around 0.064% of crop covers that are part of Hanover County. “These plants are 
               grown mainly for the purposes of food, medicinal purposes, and aesthetic gratification” [2].
               Horticulture Crops are sparsely located where the biggest parcels are at the top middle of the 
               county. It is to note that some horticulture crops are actually grown in greenhouses, 
               so they are not counted in the satellite data and instead are being shown as the developed 
               land category on the map.")
      }
      else if (input$crop_type == "SG") {
        return("Small grains are a category of crop typically referred to as cereal such as wheat (winter and spring versions),
               oats, barley (winter or spring), rye, and rice. It makes up around 0.22% of all land cover and around 0.99% of crop cover.
               If soil conditions and management practices are efficient, then “small grains can produce profitable yields of grain 
               for the cash market or farm feeding” [3]. On the map, the parcels with small grains are more populated towards the northern
               part of the county with bigger parcels on the north central half touching the borders.")
      }
      else if (input$crop_type == "DC") {
        return("Double cropped land makes up around 1.37% of land cover and around 6.05% of crop cover which consists of growing 
               multiple crops one after the other on the same land. This system helps farmers to double their productivity. An example of 
               this can include winter wheat in the spring and soybeans in the fall. Being the third most common crop cover on the map, 
               the parcels are all around the county, and are most dense on the northern borders of the county and least dense at the southern 
               borders of the county.")
      }
      else if (input$crop_type == "F") {
        return("For forage crops, the land cover proportions are at around 11.59%, and as a crop cover, it comes out with the highest percentage 
               at around 51% out of all other crop covers. These crops are used for farm grazing and as habitats for animals. Their value comes 
               from contributing to quality animal products. Examples include hay, silages, green chop, grasses, alfalfa, clovers, and other 
               crops. These are concentrated everywhere except in the center of Ashland and Mechanicsville due to the Suburban Service Area and 
               residential areas, respectively.")
      }
      else if (input$crop_type == "TC") {
        return("In terms of tree crops, the proportion of land they cover is at around 0.0012% at the lowest value, and comes around to 0.0055% as 
               the lowest value out of all crop covers. Tree crops consist of groves and orchards grown that assist with the general welfare and 
               environment of the area. Some harvested tree crop examples include tree fruits and tree nuts such as hazelnuts, chestnuts, and almonds. 
               These crops have the benefit of producing food and timber while also providing overall environmental benefits such as oxygen 
               production and soil improvements. They also don’t have to be tilled, unlike other crops, but it will take longer to establish this 
               system of crops. Being the least populated, there are not many parcels seen and are also concentrated towards the northwestern end of 
               the county. However, it is important to note that there could be issues with satellite imagery determining the differences between forest area and tree crops.")
      }
      else if (input$crop_type == "FR") {
        return("Reserved for forest growth, forested land comprises areas dedicated to the growth of forests, either individually or in combination. 
               The forested area makes up the majority of Hanover County at around 54.85% of land cover. Some examples of trees found natively in 
               Virginia are oak, cedar, and pine. These forests offer wildlife, plants, and other things home including providing environmental and foraging benefits. 
               Forested parcels make up almost all of the county except for the SSA and the residential areas of Mechanicsville.")
      }
      else if (input$crop_type == "O") {
        return("Other land covers make up the “Other” category at around 0.21% of the county. These land covers consist of barren land, fallow land, idle 
               cropland, and non-categorized crops. Throughout the county, these parcels are distributed, with a higher concentration observed 
               on the northern and western sides.")
      }
      else if (input$crop_type == "WL") { 
        return("Wetlands are areas where water either covers the soil or is near the surface of the soil layer. This status could happen either all year long, 
               during the growing season, or could permanently stay the same. Out of all of the land covers, it makes up 8.19% of the county. Like forests, 
               wetlands provide habitats and resources for animals (especially aquatic based) and terrestrial plants. “Far from being useless, disease-ridden places, 
               wetlands provide values that no other ecosystem can. These include natural water quality improvement, flood protection, shoreline erosion control, 
               opportunities for recreation and aesthetic appreciation, and natural products for our use at no cost. Protecting wetlands can protect our safety 
               and welfare” [4]. The depicted parcels are populated throughout, with the southern half of the county showing slightly lower population density.")
      }
      else if (input$crop_type == "W") {
        return("This is where water or bodies of water preside in the county. Although satellite imagery reveals that water is present in most of the county's parcels, 
               the overall land comprises only about 0.59% of water. Many of the bigger parcels hug the northern order of the county and appear larger and frequent more 
               on the west ern sides of the county than the eastern sides. The water land cover is important due to the environment and resources it provides to animals and the plant life in the area.")
      }
      else if (input$crop_type == "DEV") {
        return("Developed land makes up around 13.45% of land cover. What consists of developed land is a mixture of land constructions, homes, impervious 
               surfaces, and other developed land. Even though the land cover only makes up a small amount, almost every parcel has some form of developed land shown on the map.")
      }
    } 
    else if (selected2=="Land Cover Acreage"){
      return("Forested area makes up most of Hanover County at around 54.85% with 166,000+ acres of land in total. 
             These dense forested areas serve to absorb carbon dioxide from the atmostphere, and provide haitat to native 
             species. The second most common land cover type is developed land making up 13.45% with 40,000+ acres, and 
             forages is the third most common land cover type making up 11.59% occupying 35,000+ acres.")
    }
    else if (selected2=="Crop Cover Acreage"){
      return("To gain insight on agricultural production, land cover types are seperated to just look at crops. 
             Crop cover makes up around 22.7% of Hanover County. These consist of tree crops, small grain crops,
             row crops, horticulture crops, forages, and double cropped. Comparing the maps, row crops take up 
             the most area occupying 41.89% out of all crop covers. Specifically when looking at crop covers, crops 
             that can make high profit margins are called cash crops. These crops support the agricultural economy 
             of Hanover and are seen on the map throughout the county, especially on the northern parts of Hanover. 
             These crops include row crops and horticulture crops.")
    }
     
  })
  
  selected_crop_text2 <- reactive({
    selected2 <- selected_tab_crop()
    
    if (selected2 == "Land Cover by Parcel") {
      return("Parcels of land shown are based on different land covers mapped at pixel level through the Cropland Data Layer where 
             each pixel represents 0.22 acres of a land cover type. There are 50 different land cover types throughout Hanover County, 
             and we categorized them into 11 distinctive groups. If there is a minimum of 0.22 acres of a land cover type within a parcel, 
             \that parcel is displayed on the map.")
    }
    else{
      return(" ")
    }
  })
  
  
  output$selected_buffer_text <- renderText({
    selected_buffer_text()
  })
  output$selected_crop_text <- renderText({
    selected_crop_text()
  })
  output$selected_crop_text2 <- renderText({
    selected_crop_text2()
  })
  
  selected_tab_emp <- reactive({
    input$tabs3
  })
  
  selected_emp_text <- reactive({
    selected3 <- selected_tab_emp()
    
    if (selected3 == "Demographic Factors") {
      if (input$acs.graphs == "pop") {
        return("Hanover’s population density measures the average population per square mile. Examining this aspect 
               enables us to explore the composition of the county and identify regions that lean towards rural settings, 
               characterized by lower population densities, and areas that are more urbanized, featuring higher 
               population densities and well-developed infrastructure. The yellow tract has the highest population density, 
               recorded at 8,558 residents per square mile, which can be attributed to its close proximity to Mechanicsville, 
               on the outskirts of Richmond. This connection provides a plausible explanation for the heightened density in 
               this particular region. Moreover, the area's primary zoning classification as Residential further justifies 
               the substantial population density it sustains. The dark purple tract, situated northeast of Mechanicsville, 
               exhibits the lowest population density at 2,473 residents per square mile. This can be attributed to its predominant
               categorization under the Agricultural zone, which provides an explanation for the sparse population in this area.")
      }
      else if (input$acs.graphs == "inc") {
        return("Hanover’s median population income is based on the distribution of the total number of households and families including those 
               with no income. This tracks the income of the population by comparing the variables – population, median household income and 
               median gross rent. Examining this aspect allows us to grasp the county’s economic prosperity by observing how a higher median income 
               suggests a greater average earning potential among its residents. This factor is closely connected with the county’s employment opportunities, 
               as a higher median income reflects the workforce dynamics and the prevalence of industries or sectors offering higher-paying jobs within the 
               region. The yellow tract possesses the maximum income value of $127,394. This tract is located east of Glen Allen, on the outskirts of the capital, 
               Richmond. The dark purple tract, located within the Ashland area, possesses the minimum income value of $55,924.")
      }
      
    } 
    else {
      return("The U.S. Census Bureau ACS Data offered information on Hanover County’s total employment in each industry. By examining this aspect, we gain 
             insights into the dominant industries within the county, which significantly impact its sociodemographic standing. Public Services account for 
             over 20% of the county's total employment, encompassing education, healthcare, and social services. On the other hand, the agriculture industry 
             holds the lowest employment share, comprising only 5% of the county's workforce.")
      
    }
    
  })
  
  output$selected_emp_text <- renderText({
    selected_emp_text()
  })
  
}



shinyApp(ui = ui, server = server)
