
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
       x = "Industry",
       y = "Percent",
       caption = "Data Source: US Census ACS 5-Year 2019 Data") +
  coord_flip() 

employ_plot <- ggplotly(employ_plot, tooltip = "text")


## POLICY =========================================================================================================

### CONSERVATION ===============================================================================================================
shapefile_afd <- st_read("data/conservation/hanover_afd_parcels.shp")
easeshape <- st_read("data/conservation/hanover_easeland_parcels.shp")
easeshape <- na.omit(easeshape)
shapefile_NCA <- st_read("data/conservation/hanover_nca_parcels.shp")
consland_shape <- st_read("data/conservation/hanover_consland_parcels_labels_real.shp")
hanover_boundary <- st_read("data/conservation/Hanover_County_Boundary.shp")
boundaryleaflet <- hanover_boundary %>%
  st_as_sf() 


afd <- function(nca) {
  if (nca  == "1") {
    return("Agricultural Forest District")
  } else if (is.na(nca)) {
    return("Missing") 
  } else {
    return("Non Agricultural Forest District")
  }
}

shapefile_afd$afd <- sapply(shapefile_afd$nca, afd)

colored_afd <- shapefile_afd %>%
  filter(afd == "Agricultural Forest District")

color_map_afd <- colorFactor(palette = "#73D055FF", 
                             domain = colored_afd$afd)

ease <- function(easement) {
  if (easement  == "1") {
    return("Conservation Easement")
  } else if (is.na(easement)) {
    return("Missing") 
  } else {
    return("Non Conservation Easement")
  }
}
easeshape$ease <- sapply(easeshape$easement, ease)

color_ease <- easeshape %>%
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

shapefile_NCA$labelled_nca <- sapply(shapefile_NCA$nca, labelled_nca)

colored_NCA <- shapefile_NCA %>%
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

consland_shape$labelled_consland <- sapply(consland_shape$conland, labelled_consland)

colored_consland <- consland_shape %>%
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
        addPolygons(data = colored_afd, color = "black", fillColor = ~color_map_afd(colored_afd$afd), fillOpacity = 1, stroke = TRUE, weight = 1, group = "Agricultural Forest District") %>%
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

color_map <- colorFactor(palette = turbo(length(unique(merged_copy$land_use))), 
                         domain = merged_copy$land_use)

mapviewOptions(legend.pos = "bottomleft")

zoneHan <- mapview(colored_land, zcol = "land_use", lwd = .25, legend.opacity = .2, layer.name = "Zone Categories", alpha.regions = 1, stroke = TRUE)#+
#addLegends()



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
  labs(x = "Use Category", 
       y = "Acres", 
       title = "Land Cover in Hanover County by Category", 
       caption = "Data Source: USDA Cropland-CROS, 2022") 

landAll <- ggplotly(landAll, tooltip = "text")

#subsetting the crop data to only contain categories that are crops and assigning it to just crop
justcrop <- subset(crop_data, 
                   !(Category == "forested" | 
                       Category == "developed" | 
                       Category == "wetlands" | 
                       Category == "other" |
                       Category == "water"))

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
  labs(x = "Crop Type", 
       y = "Acres", 
       title = " Land Crops in Hanover County by Category", 
       caption = "Data Source: USDA Cropland-CROS, 2022") 


landCropONLY <- ggplotly(landCropONLY, tooltip = "text")


### SOIL QUALITY AND SOIL SUSTAINABILITY ================================================================================================


soillabels <- read_excel("data/soil/soillabelsranking.xlsx")

#assigning the data file path to soil excel
soil_excel <- "data/soil/soillabelsranking.xlsx"

#reading in soil excel and assigning it to soil data
soil_data <- read_excel(soil_excel)

#pulling just Rating, and Acres in AOI columns from soil data and assigning it to rateacre data
rateacre_data <- soil_data[, c("Rating", "Acres in AOI")]

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
  labs(x = "Soil Rating", y = "Acreage", title = "USDA Soil Rating by Acerage in Hanover County", caption = "Data Source: USDA, NRCC Web Soil Survey, 2019") 

sR <- ggplotly(sR, tooltip = "text") 



#assigning the data file path to soil excel
solarsoil_excel <- "data/soil/solarsoils.xlsx"

#reading in soil excel and assigning it to soil data
solarsoil_data <- read_excel(solarsoil_excel)

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
  labs(x = "Soil Rating", 
       y = "Acreage", 
       title = "Suitability for Soil-Anchored Solar Array by Acerage in Hanover County", 
       caption = "Data Source: USDA, NRCC Web Soil Survey, 2019") 

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
                                          p("Hanover county, Virginia is a predominantly rural area 
                                            located twelve miles north of the state capital, Richmond. 
                                            The county ranges over 474 square miles and is known for 
                                            its farmlands, rolling hills and forests bordered by the 
                                            Chickahominy and Pamunkey Rivers. Hanover’s rich agricultural 
                                            history has thrived from 1720 to present day through its
                                            tobacco cultivation, crop diversification, dairy farming
                                            and small family farms. Hence, the agricultural heritage has
                                            majorly influenced the landscape, community and rural charm of 
                                            the county."),
                                          p(),
                                          h4(strong("Problem:")),
                                          p("Hanover county takes pride in their rural lifestyle and heritage 
                                            therefore, as they look to attain economic growth challenges arise. 
                                            The main problems facing this county as it looks to achieve
                                            economic growth are urban sprawl, land conversion and solar
                                            farm land usage. Urban sprawl is the extension of urban areas
                                            which in turn cuts into the rural land that makes up Hanover County.
                                            Furthermore, land conversion shifts land use from one purpose to another.
                                            For instance, agricultural land to commercial, residential and industrial
                                            land. Solar farm land development also cuts into the land that
                                            can be used for agricultural purposes."),
                                          p(),
                                          h4(strong("Project:")),
                                          p(" Virginia Tech Department of Agricultural and Applied Economics
                                            Data Science for the Public Good (DSPG) program assesses land 
                                            conversion and solar farm land usage in Hanover County through 
                                            the use of data analytics, agricultural economics and geospatial tools.")
                                   ),
                                   column(4,
                                          h2(strong("Project Goals")),
                                          tags$li("Use GIS analysis to asses current land use patterns"),
                                          tags$li("Evaluate protected land and prime farmland"),
                                          tags$li("Analyze competing demands for prime farmland from solar energy"),
                                          tags$li("Identify parcels with the highest likelihood of transitioning to solar farms"),
                                          p()
                                          #leafletOutput("baseHan") %>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                   ),
                                   column(4,
                                          h2(strong("County Overview")),
                                          p("The county consists of towns and cities including Ashland, Beaverdam,
                                            Doswell, Hanover, Mechanicsville, Montpellier, and Rockville.
                                            According to the US Census as of July 2022, the population
                                            of the county is estimated to be above 110,000 people with
                                            around 50% of the population being female and slightly lower
                                            for males with around 18% under 18, about 5% under 5, and 
                                            about 18% over 65 years and over where it is white predominant
                                            at 85.6%. Regarding the population, it is said by the US Census
                                            that the population per square mile in 2020 is 235.2. With the 
                                            rich history that the county has, Hanover boasts many historical
                                            sites and landmarks. Adding to the fact it has a strong agricultural
                                            heritage, with fertile farmlands that add to its long history of farming.
                                            Hanover hosts a mix of suburban and rural environments with residential
                                            areas, agricultural farmland, commercial districts, industrial zones,
                                            and natural landscapes. With the county promoting outdoor recreation 
                                            activities, maintaining parks, trails, and recreational facilities 
                                            and even the amusement park Kings Dominion, there is much to do and see in 
                                            Hanover County to keep you occupied."),
                                          p()
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
                                                     p("write up")),
                                              column(6,
                                                     tabsetPanel(
                                                       tabPanel("Demographic Factors",
                                                                p(),
                                                                selectInput(
                                                                  "acs.graphs",
                                                                  "ACS Graphs",
                                                                  c("Population Density" = "pop",
                                                                    "Median Population Income" = "inc"))
                                                                ,
                                                                imageOutput("acs", width = "650px", height = "500px")
                                                       ), 
                                                       tabPanel("Employment", 
                                                                p(),
                                                                column(12,
                                                                       plotlyOutput("employ_plot", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                                                )) 
                                                     )),

                                              p(),
                  
                                              column(12,
                                                     h4(strong("References")),
                                                     p("References go here")
                                              )
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
                                                     h2(strong("Conservation Policy")),
                                                     column(6,
                                                            align="left",
                                                            h4(strong("Conservation Land")),
                                                            p("This map highlights the conservation land within Hanover County, incorporating data provided by the Virginia Department of Conservation and 
                                                              Recreation. These lands encompass a variety of state, federal, local, and privately managed areas dedicated to conservation efforts. 
                                                              Their purpose is to protect and preserve natural habitats, wildlife, ecosystems, and scenic landscapes. Landowners are responsible for 
                                                              implementing conservation practices, such as regulating public access and managing resources.")),
                                                     
                                                            column(6,
                                                                   align="left",
                                                            h4(strong("Agricultural Forestal Districts")),
                                                            p("The map presents the parcels in Hanover County designated as Agricultural/Forestal Districts (AFD),
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
                                                            p("This map displays the conservation easements present in Hanover County, utilizing data provided by the Virginia Department of Conservation and Recreation.
                                                              Conservation easements entail binding agreements between landowners and government agencies, serving as limitations on future land development and 
                                                              subdivision. Notably, landowners retain control over their properties and the right to sell them. The specifics of these easement agreements vary but 
                                                              consistently aim to preserve land for rural uses, such as agriculture, forest management, and recreational activities like hunting and fishing.")
                                                            ),
                                                     column(6,
                                                            align="left",
                                                            h4(strong("Natural Conservation Easements")),
                                                            p("The map depicts parcels within Hanover County that have a portion of their land located within 100 ft of waterways. The data utilized in this analysis is
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
                                                     align = "center",
                                                     p(),
                                                     h4(strong("References")),
                                                     p("References go here")
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
                                                                         p("write upp")),
                                                                  column(6,
                                                                         tabsetPanel(
                                                                           tabPanel("Land Use Map",
                                                                                    p(),
                                                                                    leafletOutput("zoneHan") %>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                                                                    
                                                                                    
                                                                           ), 
                                                                           tabPanel("Land Use in Acreage", 
                                                                                    p(),
                                                                                    plotlyOutput("interactive_plot", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                                                           ) 
                                                                         ))

                                                         ), 
                                                                ),
                                                tabPanel("Land Cover",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Land Cover Information")),
                                                                         #textOutput("crop_type_write"),
                                                                         textOutput("selected_crop_text")),
                                                                  column(6,
                                                                         tabsetPanel(
                                                                           id = "tabs2",
                                                                           tabPanel("Land Cover by Parcel",
                                                                                    selectInput(inputId = "crop_type", label = "Select Variable:", choices = c(
                                                                                      "Row crops" = "RC",
                                                                                      "Horticulture crops" = "HC",
                                                                                      "Small grains" = "SG",
                                                                                      "Double cropped" = "DC",
                                                                                      "Forages" = "F",
                                                                                      "Tree crops" = "TC",
                                                                                      "Other" = "O",
                                                                                      "Forested" = "FR",
                                                                                      "Wetlands" = "WL",
                                                                                      "Water" = "W",
                                                                                      "Developed" = "DEV")
                                                                                    ),
                                                                                    imageOutput("crop_typePNG", width = "600px", height = "400px")
                                                                                    
                                                                                    
                                                                           ), 
                                                                           tabPanel("Land Cover Acreage", 
                                                                                    plotlyOutput("landAll", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                                                                    
                                                                           ),
                                                                           tabPanel("Crop Cover Acreage",
                                                                                    plotlyOutput("landCropONLY", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5))
                                                                         ))
                                                                  
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
                                                         imageOutput("soilRate", width = "700px", height = "500px")
                                                         
                                                         
                                                ), 
                                                tabPanel("Soil Type in Acreage", 
                                                         p(),
                                                         plotlyOutput("sR", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5),
                                                ) 
                                              )),

                                              
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
                                                                                    imageOutput("SoilLimit", width = "700px", height = "500px")
                                                                                    
                                                                                    
                                                                           ), 
                                                                           tabPanel("Land Suitability in Acreage", 
                                                                                    p(),
                                                                                    plotlyOutput("rateacre", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                                                                    
                                                                           ) 
                                                                           

                                                                         
                                                                  ))
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
                                                                           feet of a transmission line in order to keep development costs low."), 

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
                                                                         imageOutput("InfastructurePNG", width = "700px", height = "500px"),
                                                                         p("*Distortion due to high density of residential parcels in Mechanicsville.")
                                                                         
                                                                  )
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
                                                                           helps add to our solar assesment by showing parcels within Hanover County that have adequate access to be developed[2].")
                                  
                                                                  ),
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Road Access Map")),
                                                                         imageOutput("RoadPNG", width = "700px", height = "500px"),
                                                                         p("Highlighted parcels are within 100 feet of a roadway centerline.")
                                                                         
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
                                                                         imageOutput("ssIndexPNG", width = "500px", height = "400px")
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
                                                                         p()),
                                                                  column(6,
                                                                         align="left",
                                                                         h2(strong("Description of Map")),
                                                                         textOutput("arindex_write")),
                                                                  column(6,
                                                                         h2(strong("Index Map")),
                                                                         selectInput(
                                                                           "arbufferType",
                                                                           "Agrivoltaic Index Buffer",
                                                                           c("Buffer 1" = "buffer_1",
                                                                             "Buffer 2" = "buffer_2",
                                                                             "Buffer 3" = "buffer_3"),
                                                                           
                                                                         ),
                                                                         imageOutput("arIndexPNG", width = "500px", height = "400px")
                                                                  )
                                                                  
                                                         ),
                                                         
                                                ),
                                                tabPanel("Methodology",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         h2(strong("Index Methodology")),
                                                                         textOutput("selected_buffer_text"),
                                                                         
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
                                                                                    imageOutput("ssMethodPNG", width = "550px", height = "500px")
                                                                                    
                                                                           ),
                                                                           tabPanel("Agrivoltaic Viability Rating",
                                                                                    selectInput(
                                                                                      "av.rating",
                                                                                      "Agrivoltaic Viability Rating",
                                                                                      c("Buffer 1" = "buffer_1",
                                                                                        "Buffer 2" = "buffer_2",
                                                                                        "Buffer 3" = "buffer_3")
                                                                                    ),
                                                                                    imageOutput("arMethodPNG", width = "550px", height = "500px")
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
    
    zoneHan@map
    
  })
  output$consleaf <- renderLeaflet({
    
    consleaf
    
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
      return(list(src = "wwwARSB3.png", width = "125%", height = "100%"))
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
      return("This map shows the most ideal parcels for solar farm development within buffer zone 1. buffer zone 1 contains the most desireable 
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
      return("Writ eup buffer 1")
    }
    else if (input$arbufferType == "buffer_2") {
      return("Write up buffer 2")
    }
    else if (input$arbufferType == "buffer_3") {
      return("Write up buffer 3")
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
        return("Write up solar buffer 1")
      }
      else if (input$solar.score == "buffer_2"){
        return("Write up solar buffer 2")
      }
      else if (input$solar.score == "buffer_3"){
        return("Write up solar buffer 3")
      }
    } else if (selected == "Agrivoltaic Viability Rating") {
      if(input$av.rating == "buffer_1"){
        return("Write up av buffer 1")
      }
      else if (input$av.rating == "buffer_2"){
        return("Write up av buffer 2")
      }
      else if (input$av.rating == "buffer_3"){
        return("Write up av buffer 3")
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
    } 
    else {
      return(" ")

      }
     
  })
  
  output$selected_buffer_text <- renderText({
    selected_buffer_text()
  })
  output$selected_crop_text <- renderText({
    selected_crop_text()
  })
  
  
}



shinyApp(ui = ui, server = server)
