# Ari
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
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }
           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }
            var mytype = getUrlParam('type','Empty');
            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");
                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }
           var x = document.getElementsByClassName('navbar-brand');
           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('economic'); 
           }
           "

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
       title = "Land Use in Hanover County by Category", 
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


soillabels <- read_excel("data/soillabelsranking.xlsx")

#assigning the data file path to soil excel
soil_excel <- "data/soillabelsranking.xlsx"

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
solarsoil_excel <- "data/solarsoils.xlsx"

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



## NECESSITIES =================================================

# necessary imports for many of our plots (county boundary shape files)
po_cnty<- st_read("data/cnty_bndry/Powhatan_Boundary.shp") %>% st_transform("+proj=longlat +datum=WGS84")


## HANOVER Sociodemographics =================================================

popdist<-read.csv("data/popdist.csv", header = TRUE) #for Shiny ap
industry <- read.csv("data/industry.csv", header=TRUE) #for Shiny app
inc <- read.csv("data/inc.csv", header=TRUE) 

educ_earn <- read.csv("data/educ_earn.csv", header=TRUE) 


age.func <- function(inputYear, inputCounty) {
  
  age <- popdist %>%
    filter(county == inputCounty, year==inputYear) %>%
    ggplot(aes(x=agecat , y=value, fill=agecat))+
    geom_bar(stat="identity") + 
    coord_flip() + 
    scale_fill_viridis(discrete=TRUE) + 
    theme_light() + 
    theme(legend.position="none") + 
    theme(axis.text.y = element_text(hjust=0)) +
    labs(title="Age Distribution of Population", y= "Percent", x= "Age Group", caption="Source: ACS5 2016-2020") +
    ylim(0,35)
  age
}

ind.func <- function(inputYear, inputCounty) {
  
  ind <- industry %>% 
    filter(county == inputCounty, year==inputYear) %>%
    ggplot(aes(x = name, y = value, fill = name)) + 
    geom_bar(stat = "identity") + theme(legend.position = "none") +
    coord_flip() + scale_fill_viridis_d()  + 
    theme_light() + 
    theme(legend.position="none") + 
    theme(axis.text.y = element_text(hjust=0)) +
    labs(title="Employment By Industry", y = "Percent", x = "Industry", caption="Source: ACS5 2016-2020") +
    ylim(0,25)
  ind
}

inc.func <- function(inputYear, inputCounty) {
  
  inc <- inc %>% 
    filter(county == inputCounty, year==inputYear) %>%
    mutate(inccat = fct_relevel(inccat, "<35K", "35K - 50K", "50K - 75K","75K-100K", ">100K")) %>%
    ggplot(aes(x = inccat, y = estimate, fill = inccat))+ 
    geom_bar(stat = "identity") + 
    theme(legend.position = "none") + 
    scale_fill_viridis(discrete=TRUE) + 
    theme_light() + 
    theme(legend.position="none") + 
    theme(axis.text.y = element_text(hjust=0)) +
    labs(title = "Income Distribution", y = "Percent", x = "Income", caption="Source: ACS5 2016-2020") +
    coord_flip() +
    ylim(0,50)
  inc
}

edu.func <- function(inputYear, inputCounty) {
  
  edu <- educ_earn %>% 
    filter(county == inputCounty, year==inputYear) %>%
    ggplot(aes(x = name, y = values)) + 
    geom_bar(stat = "identity", mapping=(aes(fill = name))) + 
    theme(legend.position = "none") + scale_fill_viridis(discrete=TRUE) +
    labs(title = "Median Earnings By Educational Attainment (Age > 25 years)", x = "Highest Education", y = "Median Earnings", caption = "Source: ACS5 2016-2020") + 
    geom_text(aes(label = values), vjust = -0.25) +
    scale_x_discrete(labels = c("Below\nhighschool", "Highschool\ngraduate", "Some college/\nAssociates'", "Bachelor's", "Graduate")) + 
    theme_light() + 
    theme(legend.position="none") + 
    theme(axis.text.y = element_text(hjust=0)) +
    ylim(0, 200000)
  edu
}


## POLICY =================================================

pcon <- st_read("data/Conservation/Powhatan_Natural_Conservation.shp") %>% st_transform("+proj=longlat +datum=WGS84")

powhatan_con <- leaflet()%>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  setView(lng=-77.9188, lat=37.5415, zoom=10.48) %>% 
  addPolygons(data=pcon[1,], weight=0, fillOpacity = 0.5, fillColor = "#f89540", group = "Priority Conservation Areas")%>%
  addPolygons(data=pcon[2,], weight=0, fillOpacity = 0.5, fillColor = "#b73779", group = "Protected Lands")%>%
  addPolygons(data=pcon[3,], weight=0, fillOpacity = 0.5, fillColor = "#21918c", group = "AFD")%>%
  addPolygons(data=po_cnty, weight=2, color="black", fillOpacity=0, opacity = 1)%>%
  addLayersControl(
    overlayGroups = c("Priority Conservation Areas", "Protected Lands", "AFD"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE))


## LAND USE =================================================


### LAND USE =================================================


#transition matrix
agLabels <- c("Agricultural / Undeveloped (20-99 Acres) (before)", "Agricultural / Undeveloped (100+ Acres) (before)")
p.sankey <- read.csv("data/luParcelData/p_sankey.csv") %>% select(MLUSE_old,MLUSE_new)  %>% filter(MLUSE_old %in% agLabels, MLUSE_old != MLUSE_new)

thm.p <- hc_theme(colors = c("#fde725", "#fde725", "#1fa187", "#addc30", "#3b528b",  "#5ec962", "#1fa187"),
                chart = list(backgroundColor = "#ffffff"),
                title = list(style = list(color ='#000000',
                                          fontFamily = "Lumen")),
                subtitle = list(style = list(color ='#000000',
                                             fontFamily = "Lumen")),
                labels=list(color="#333333", position="absolute"),
                legend = list(itemStyle = list(fontFamily ='Lumen',color ='#000000')
                              ,y=50,align='right',itemHoverStyle = list(color ="#FFFf43")))


### CROP LAYER =================================================

  # setting up labels depending on whether it has little to no acreage

pcrop_values <- c("Developed", 
                  "Double cropped", 
                  "Forages", 
                  "Forested", 
                  "Horticulture crops", 
                  "Other", 
                  "Row crops", 
                  "Small grains", "Tree crops", "Water", "Wetlands")

croplayer1 <- read.csv("data/ag_analysis.csv")
cropPlot.func <- function(county, year){
  data <- croplayer1 %>% filter(County == county, Year == year)
  crop.plt <- data %>% ggplot(aes(x = Combined, y = Area.Acre, fill = Combined,
                                  text = paste0(Combined, "\nTotal Acres: ", round(Area.Acre, 0)))) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.text.y = element_text(hjust=0), legend.position = "none") +
    scale_fill_manual(values = gcrop_colors) + 
    ylim(0, 130000) + 
    labs(title = "Total Acreage by Land Type", x = "Acreage", y = "Land type") 
  crop.plt <- ggplotly(crop.plt, tooltip = "text")
  crop.plt
}


### SOIL QUALITY =================================================

soilClass <- c("Capability Class-I","Capability Class-II","Capability Class-III","Capability Class-IV",
               "Capability Class-V","Capability Class-VI","Capability Class-VII","Capability Class-VIII",
               "NODATA")
soilClass <- factor(soilClass, levels = soilClass)

soilPalette <- colorBin(palette = "viridis", as.numeric(soilClass), bins = 9)
soilColors <- soilPalette(unclass(soilClass))
soilColors[9] <- "#4D4D4D" # undefined gets a gray color
soilLegend <- colorFactor(palette = soilColors,levels=soilClass)


soil_quality <- read.csv("data/Soil_Quality_Analysis.csv")


psoil <- ggplot(soil_quality, aes(x = `P_Value`, y = `P_Area_acre`, fill = `P_Value`,
                                  text = paste0(`P_Value`, "\n", "Total Acres: ", round(`P_Area_acre`, 0)))) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values=soilColors)+
  labs( title = "Total Acreage by Soil Quality Classification", y = "Acreage", x = "Soil Quality Classification") 
psoil <-ggplotly(psoil, tooltip = "text")


### TRAFFIC ===============================================


## PARCELLATION ============================================


pow_parcellation <- read_sf("data/parcellationData/Powhatan_Parcellation_LT.shp") %>%
  st_transform(crs = st_crs("EPSG:4326"))
pow_parcellation$year <- substr(pow_parcellation$UNIQ_ID_12, 1, 4)

  # both have substr to get the year from the first four characters of UNIQID_1/_12 

### PARCELS ============================================

parc.func <- function(data, range, county, cnty){
  
  # Declares initial leaflet, nothing added to it.
  my.parc.plt <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = cnty, fillColor = "transparent")
  
  # Sets view based on county
  if(county == "Powhatan"){
    my.parc.plt <- my.parc.plt %>% setView(lng=-77.9188, lat=37.5415 , zoom=10)
  }
  else{
    my.parc.plt <- my.parc.plt %>% setView(lng=-77.885376, lat=37.73143, zoom = 10)
  }
  
  # for loop to add polygons based on what the max year is vs. subsequent years prior
  for(i in range){
    # Adds most recent year's parcellations
    if(i == max(range)){
      my.parc.plt <- my.parc.plt %>%
        addPolygons(data = data %>% filter(year == i), 
                    fillColor = "red", smoothFactor = 0.1, fillOpacity = 1, stroke = FALSE)
    }
    # Adds subsequent year's parcellations
    else {
      my.parc.plt <- my.parc.plt %>%
        addPolygons(data = data %>% filter(year == i), 
                    fillColor = "red", smoothFactor = 0.1, fillOpacity = .25, stroke = FALSE)
    }
  }
  my.parc.plt
}

### HOTSPOTS ============================================

hotspot.func <- function(county, range){
  
  hotspot.plt <- leaflet()%>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  # Sets view based on county
  if(county == "Powhatan"){
    hotspot.plt <- hotspot.plt %>% setView(lng=-77.9188, lat=37.5415 , zoom=10)
    file_list <- paste("data/Parcel_Hotspot/powhatan/pow_hotspot_",range,".shp",sep = "")
    hotspot.plt <- hotspot.plt %>% addPolygons(data = po_cnty, fillOpacity = 0)
  }
  
  
  for (file in file_list){
    #import the heatspot maps of the selected years
    hotspot.data<- st_read(file) %>% st_transform("+proj=longlat +datum=WGS84")
    hotspot.plt <- hotspot.plt %>% addPolygons(stroke = FALSE,
                                               data = hotspot.data,
                                               weight = 1,
                                               smoothFactor=1,
                                               fillColor = "red",
                                               fillOpacity = 0.2)
  }
  hotspot.plt
}

# ui --------------------------------------------------------------------------------------------------------------------

ui <- navbarPage(title = "DSPG 2023",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                 useShinyjs(),
                 
                 ## Tab Overview--------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   h1(strong("Land Use in Rural Counties on the Urban Fringe: the case of Goochland and Powhatan Counties​"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 8px;",
                                   align = "center",
                                   column(4,
                                          h2(strong("Project Background")),
                                          h4(strong("Setting")),
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
                                          h4(strong("Problem")),
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
                                          h4(strong("Project")),
                                          p(" Virginia Tech Department of Agricultural and Applied Economics
                                            Data Science for the Public Good (DSPG) program assesses land 
                                            conversion and solar farm land usage in Hanover County through 
                                            the use of data analytics, agricultural economics and geospatial tools.")
                                   ),
                                   column(4,
                                          h2(strong("Aims")),
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
                                              p(),
                                              
                                              column(6,
                                                     h2(strong("Hanover County Overview")),
                                                     p("Hanover is known to have a rich history and background.
                                            Formed by the Virginia General Assembly on November 26th,
                                            1720 named in honor of King George the First of England, it is
                                            iconic for the historic landmarks in the county and other historical places.
                                            Hanover currently has 39 sites registered in the National Register of 
                                            Historical Places/ Virginia Landmarks Register and 56 sites in Virginia Historical Markers.
                                            In general, it has over 1700 historical sites within the locality! The National
                                            Historic Landmarks (NHL) are historic properties that illustrate the heritage of
                                            the United States and are officially recognized by the US government. The historic
                                            properties found in Hanover County include Hanover Courthouse, Scotchtown, and
                                            Malbourne/Edmund Ruffin Plantation. In particular, Hanover Courthouse is the symbol
                                            and pride of Hanover County which is dated back to around 1740 where it is one of
                                            the oldest courthouses in Virginia. Some exceptional historic resources found here
                                            include Hanover Tavern, Hanover Meeting House, Garthwright-Kelley House, Gaines Mill
                                            Battlefield, Cold Harbor Battlefield, Rural Plains and so much more. For more points
                                            of interest, there is a whole slew of Century Farms recognized by the Virginia 
                                            Department of Agriculture and Consumer Affairs where these farms, as the name
                                            suggests to some extent, have each been owned by farmer families for 100 years or more. 
                                            When visiting you can’t forget the fascinating battlefield sites that revolved around the 
                                            Civil War and the Revolutionary War. During the Civil War, in particular, Hanover County 
                                            was a frequented battlefield by Union and Confederate troops where Union troops, commanded
                                            by generals, fought their way through to Richmond against the Confederate Army led by Robert
                                            E. Lee. Hanover County has a vast array of historical and cultural land sites, markers, and resources
                                            that provide it the opportunity for benefits on all accounts from economic to cultural for the owners
                                            of the county as a whole."),
                                                     p(),
                                                     h2(strong("Statistics Summary")),
                                                     p("Will be done once graphs are set and ready to be analyzed")
                                              ) , 
                                              column(6, 
                                                     fluidRow(style = "margin: 6px;", align = "justify",
                                                     p("", style = "padding-top:10px;"),
                                                     p(strong("Chesapeake Bay Preservation Act:")),
                                                     p("This program was developed in 1988 as an element of Virginia's NPS management program. The goal is to protect and improve water quality in the Chesapeake 
                                                     Bay by requiring effective land use management practices [6]."), 
                                                     p('"The Bay Act program is the only program administered by the Commonwealth of Virginia that comprehensively addresses the effects of land use planning and 
                                                     development on water quality. The Bay Act recognizes that local governments have the primary responsibility for land use decisions and expands their authority 
                                                     to manage water quality, and establish a direct relationship between water quality protection and local land use decision-making" [6].'),
                                                     br(),
                                                     p(strong("Total Maximum Daily Load (TMDL):")),
                                                     p("Significant portions of the Chesapeake Bay have been identified as not meeting water quality standards. Despite the Chesapeake Bay program, water quality goals 
                                                     have not been met. In December of 2010, the EPA issued a TMDL, a “pollution diet” to protect the Bay [7]. This TMDL is divided among all the Bay states. However,
                                                       “regional or statewide consistency is rare in Virginia's land use planning process - even statewide requirements such as the Chesapeake Bay Regulations are interpreted 
                                                       differently by different jurisdictions” [1]."),
                                              )),
                                              column(12,
                                                     h4("References:"),
                                                     p(tags$small("[1] Land use planning in Virginia. Virginia Places. (n.d.). Retrieved July 25, 2022, from http://www.virginiaplaces.org/landuseplan/", tags$br(), 
                                                                  "[2] USDA. (n.d.). Conservation reserve enhancement program. USDA Farm Service Agency. Retrieved July 25, 2022, from https://www.fsa.usda.gov/programs-and-services/conservation-programs/conservation-reserve-enhancement/index" , tags$br(),
                                                                  "[3] Virginia General Assembly. (n.d.). Code of Virginia. Virginia's Legislative Information System. Retrieved July 25, 2022, from https://law.lis.virginia.gov/vacodepopularnames/agricultural-and-forestal-districts-act/ ",tags$br(),
                                                                  "[4] Virginia Department of Forestry. (n.d.). Agricultural &amp; forestal district program- Louisa County. Virginia Department of Forestry. Retrieved July 25, 2022, from https://dof.virginia.gov/wp-content/uploads/afd-program-brochure_11212019-stone-version.pdf", tags$br(),
                                                                  "[5] Virginia Nonpoint Source Management Program Plan (2019 Update). (2019).", tags$br(),
                                                                  "[6] Virginia Department of Environmental Quality. (n.d.). Chesapeake Bay preservation act. Virginia Department of Environmental Quality. Retrieved July 25, 2022, from https://www.deq.virginia.gov/water/chesapeake-bay/chesapeake-bay-preservation-act#:~:text=Under%20the%20Bay%20Act%20framework%2C%20the%20Chesapeake%20Bay,and%20implement%20in%20administering%20their%20Bay%20Act%20programs.", tags$br(),
                                                                  "[7] Virginia Department of Environmental Quality. (n.d.). Chesapeake Bay TMDLs. Virginia Department of Environmental Quality. Retrieved July 25, 2022, from https://www.deq.virginia.gov/water/chesapeake-bay/chesapeake-bay-tmdls ")),
                                                     p("", style = "padding-top:10px;")),
                                     ),
                                     tabPanel("Conservation Policy",
                                              p(),
                                              p('State-level officials work within the confines of both federal and local policy. They aim to simultaneously enhance federal policy while enabling local officials to make comprehensive 
                                              land-use plans. The state of Virginia is under the Dillon Rule which states that local ordinances must be consistent with state law [1]. Local officials are the ones approving parcel-specific 
                                              land use plans, but state and federal officials play a key role [1]. The state courts are the "referees" to determine if land use decisions violated some aspect of various state laws, or if 
                                                the land use rules violated the state constitution in some way [1].'),
                                              column(6,
                                                     fluidRow(style = "margin: 6px;", align = "justify",
                                                              p("", style = "padding-top:10px;"),
                                                              p(strong("Conservation Reserve Enhancement Program (CREP):")), 
                                                              p("This is a state-sponsored enhancement to the federal CRP. It is a cost-share program where federal reimbursement is made through the FSA for up to 
                                                       “50% of a participant's eligible expenses for implementing best management practices (BMP)”. BMP examples include adding fencing, alternative watering 
                                                       systems, and restoring wetlands. Participation in this program is voluntary, and the contract period is around 10-15 years [2]."),
                                                              br(),
                                                              p(strong("Agriculture and Forestal Districts (AFD):")),
                                                              p("The AFD program in Virginia was designed to “preserve and protect open spaces, forested areas, and agricultural lands” [3]. This program makes 
                                                       it so land taxes are based on use rather than taxing solely on the market value. Land used for growing crops, for example, is taxed differently than 
                                                       developed property. This state-level policy encourages localities to be purposeful with their property taxes. The hope is that this policy will be used 
                                                       to conserve and protect agricultural and forest land. These lands can be valued as “natural and ecological resources which provide essential open spaces 
                                                       for clean air sheds, watershed protection, wildlife habitat, aesthetic quality and other environmental purposes” [3]. This program was formed in 1977 
                                                       [4]. The potential benefits are to lower property taxes, safeguard the rural character of the community, and offer protection from the eminent domain [4]."),
                                                              br(),
                                                              p(strong("Nonpoint Source (NPS) Pollution Management Program:")), 
                                                              p('This is a diverse network of state and local government programs that “help to prevent water quality degradation and to restore 
                                                       the health of lakes, rivers, streams, and estuaries by promoting and funding state and local watershed planning efforts, stream and wetland restoration and protection, 
                                                       education and outreach, and other measures to reduce and prevent NPS pollution from affecting the Commonwealth’s waters" [5].'),
                                                              p(),
                                                              p(),
                                                     )) , 
                                              column(6, 
                                                     fluidRow(style = "margin: 6px;", align = "justify",
                                                              p("", style = "padding-top:10px;"),
                                                              p(strong("Chesapeake Bay Preservation Act:")),
                                                              p("This program was developed in 1988 as an element of Virginia's NPS management program. The goal is to protect and improve water quality in the Chesapeake 
                                                     Bay by requiring effective land use management practices [6]."), 
                                                              p('"The Bay Act program is the only program administered by the Commonwealth of Virginia that comprehensively addresses the effects of land use planning and 
                                                     development on water quality. The Bay Act recognizes that local governments have the primary responsibility for land use decisions and expands their authority 
                                                     to manage water quality, and establish a direct relationship between water quality protection and local land use decision-making" [6].'),
                                                              br(),
                                                              p(strong("Total Maximum Daily Load (TMDL):")),
                                                              p("Significant portions of the Chesapeake Bay have been identified as not meeting water quality standards. Despite the Chesapeake Bay program, water quality goals 
                                                     have not been met. In December of 2010, the EPA issued a TMDL, a “pollution diet” to protect the Bay [7]. This TMDL is divided among all the Bay states. However,
                                                       “regional or statewide consistency is rare in Virginia's land use planning process - even statewide requirements such as the Chesapeake Bay Regulations are interpreted 
                                                       differently by different jurisdictions” [1]."),
                                                     )),
                                              column(12,
                                                     h4("References:"),
                                                     p(tags$small("[1] Land use planning in Virginia. Virginia Places. (n.d.). Retrieved July 25, 2022, from http://www.virginiaplaces.org/landuseplan/", tags$br(), 
                                                                  "[2] USDA. (n.d.). Conservation reserve enhancement program. USDA Farm Service Agency. Retrieved July 25, 2022, from https://www.fsa.usda.gov/programs-and-services/conservation-programs/conservation-reserve-enhancement/index" , tags$br(),
                                                                  "[3] Virginia General Assembly. (n.d.). Code of Virginia. Virginia's Legislative Information System. Retrieved July 25, 2022, from https://law.lis.virginia.gov/vacodepopularnames/agricultural-and-forestal-districts-act/ ",tags$br(),
                                                                  "[4] Virginia Department of Forestry. (n.d.). Agricultural &amp; forestal district program- Louisa County. Virginia Department of Forestry. Retrieved July 25, 2022, from https://dof.virginia.gov/wp-content/uploads/afd-program-brochure_11212019-stone-version.pdf", tags$br(),
                                                                  "[5] Virginia Nonpoint Source Management Program Plan (2019 Update). (2019).", tags$br(),
                                                                  "[6] Virginia Department of Environmental Quality. (n.d.). Chesapeake Bay preservation act. Virginia Department of Environmental Quality. Retrieved July 25, 2022, from https://www.deq.virginia.gov/water/chesapeake-bay/chesapeake-bay-preservation-act#:~:text=Under%20the%20Bay%20Act%20framework%2C%20the%20Chesapeake%20Bay,and%20implement%20in%20administering%20their%20Bay%20Act%20programs.", tags$br(),
                                                                  "[7] Virginia Department of Environmental Quality. (n.d.). Chesapeake Bay TMDLs. Virginia Department of Environmental Quality. Retrieved July 25, 2022, from https://www.deq.virginia.gov/water/chesapeake-bay/chesapeake-bay-tmdls ")),
                                                     p("", style = "padding-top:10px;")),
                                     ),
                                     tabPanel("Solar",
                                              p(),
                                              p('"In urbanizing areas such as the suburbs near Richmond, Hampton Roads, and Northern Virginia, control over how private property 
                                                is developed may be a contentious process involving landowners and their lawyers, neighbors, or local residents upset over additional 
                                                development, and local officials. In Fairfax, Loudoun, and Prince William counties over the last 30 years, the Board of County Supervisor 
                                                election campaigns have been based on growth management issues. Local officials have reacted to citizen complaints, and incumbents have 
                                                been voted out of office because they were either too supportive of growth or too restrictive” [1].'),
                                              
                                              column(6,
                                                     h1(strong("Powhatan"), align = "center"),
                                                     p("", style = "padding-top:10px;"),
                                                     fluidRow(style = "margin: 6px;", align = "justify",
                                                              leafletOutput("powhatan_con") %>% withSpinner(type = 4, color = "#861F41", size = 1.25),
                                                              p("The map above highlights the types of conservation districts in Powhatan County."), 
                                                              tags$ul(
                                                                
                                                                tags$li("The green layer represents", strong("Agricultural Forestal Districts (AFD)"),"which are areas of land that are recognized by the county as being economically and environmentally valuable resources for all [7]."),
                                                                
                                                                tags$li("The orange layer represents", strong("Priority Conservation Areas"), "which are protected for long term conservation."),
                                                                
                                                                tags$li("The red layer represents", strong("Protected Lands"), "which are protected due to their natural, cultural, or ecological value."),
                                                                
                                                                
                                                              ),
                                                              p('Powhatan County land use policy includes a land use deferral program, Powhatan County code Section 70-76, which states that the purpose of land use is
                                                     to “preserve real estate devoted to agricultural, horticultural, forest and open space uses within its boundaries in the public interest..." 
                                                     The land use deferral program “offers a deferral of a portion of the real estate taxes for qualifying properties”. This ordinance was adopted by the
                                                     county in 1976 and approximately 40% of the county is in land use today [8]. Powhatan County also has an Agricultural and Forestal District (AFD)
                                                     Program which allows the county, with the landowner’s consent, to take land out of development in exchange for a land use tax rate as opposed to market
                                                     value tax rate. As of September/October 2020, there are approximately 5,640 acres of AFD land. This program serves to protect natural lands as well as prevent
                                                     landowners from having to sell their land as market values and tax rates continue to increase. One benefit that the AFD program has over the land use deferral
                                                     program is that it is officially included in the County’s Comprehensive Plan [9]. '),
                                                              p('The county’s zoning ordinance categorizes rural districts into 6 groups. The main agricultural districts are A-20 (min 20 ac), A-10 (min 10 ac), and A-C.
                                                     The 3 other rural districts are largely dedicated to residential zoning. The 2010 long range comprehensive plan also includes sections on natural conservation
                                                     and rural preservation which outline land use policies to be “used when addressing development and land use issues” [10]. These policies promote the
                                                     conservation of open land and farmland and recognize agriculture as an economic driver of the community.'))),
                                              column(12, 
                                                     h4("References:"),
                                                     p(tags$small("[1] Land use planning in Virginia. Virginia Places. (n.d.). Retrieved July 25, 2022, from http://www.virginiaplaces.org/landuseplan/", tags$br(),
                                                                  "[2] Planning and Zoning Initiatives. Planning and Zoning Initiatives | Goochland County, VA - Official Website. (n.d.). Retrieved July 18, 2022, from https://www.goochlandva.us/1058/Planning-and-Zoning-Initiatives ", tags$br(), 
                                                                  "[3] Goochland County. (n.d.). Land use program information. Goochland County, VA - Official Website. Retrieved July 25, 2022, from https://www.goochlandva.us/339/Land-Use" , tags$br(),
                                                                  "[4] Goochland County, Virginia - Code of Ordinances. Municode Library. (n.d.). Retrieved July 25, 2022, from https://library.municode.com/va/goochland_county/codes/code_of_ordinances?nodeId=COOR_CH15ZO", tags$br(),
                                                                  "[5] Goochland County. (n.d.). Goochland County 2035 Comprehensive Plan. Retrieved July 25, 2022, from https://capitalregionland.org/wp-content/uploads/2021/11/Goochland-County-Comprehensive-Plan-Land-Use-chapter.pdf", tags$br(),
                                                                  "[6] Goochland County Agricultural Center. (n.d.). A.C.R.E.S. Initiative. Retrieved July 25, 2022, from https://www.goochlandva.us/DocumentCenter/View/6731/ACRES-2019?bidId=", tags$br(),
                                                                  "[7] Powhatan County. (n.d.). Agricultural &amp; forestal district program. Powhatan County, VA - Official Website. Retrieved July 25, 2022, from http://www.powhatanva.gov/1784/Agricultural-Forestal-District-Program", tags$br(),
                                                                  "[8] Powhatan County. (n.d.). Land use deferral. Powhatan County, VA - Official Website. Retrieved July 25, 2022, from http://www.powhatanva.gov/216/Land-Use-Deferral#:~:text=Per%20Powhatan%20County%20code%20Section,adopted%20this%20ordinance%20in%201976.", tags$br(),
                                                                  "[9] Powhatan County Agricultural and Forestal District Advisory Committee. (2020). Powhatan county agricultural and forestal district (AFD) Review. Retrieved July 25, 2022, from http://www.powhatanva.gov/DocumentCenter/View/5923/AFDAC-Review-of-Agricultural-and-Forestal-Districts-AFDs-October-2020", tags$br(),
                                                                  "[10] Powhatan County. (2019). 2010 Long-Range Comprehensive Plan. Retrieved July 25, 2022, from http://www.powhatanva.gov/DocumentCenter/View/85/2010-Powhatan-County-Long-Range-Comprehensive-Plan-")),
                                                     p("", style = "padding-top:10px;")) 
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
                                                tabPanel("Land Use and Zoning",
                                                         p("", style = "padding-top:10px;"),
                                                                fluidRow(style = "margin: 6px;", align = "justify",
                                                                         column(4,
                                                                         h4(strong("Land Use in Powhatan County")),
                                                                         p("To classify land, we used the state of Virginia’s land use codes for our analyses. This involved condensing some administrative categories from Powhatan’s system into Virginia’s land use codes. 
                                                                           The map shows all the parcels in Powhatan County classified by their land use type. In the administrative data, some parcels were unclassified. These parcels make up the undefined category that 
                                                                           you see in our analyses. Our types are Single Family Urban, Single Family Suburban, Multi-Family Residential, Commercial & Industrial, Agriculture / Undeveloped (20-99 Acres), Agriculture / Undeveloped (100+ Acres), 
                                                                           Other, and Undefined."),
                                                                         p("The map shows that Agriculture / Undeveloped (20-99 Acres) and Agriculture / Undeveloped (100+ Acres) have the largest number of parcels for all years. Single Family Suburban is the third largest number of parcels.")
                                                                         
                                                                ), 
                                                         column(8, 
                                                                h4(strong("Land Use Distribution and Change by Year")),
                                                                
                                                                radioButtons(inputId = "pow_lu_year", label = "Select year: ",
                                                                             choices = c("2015", "2016", "2017", "2018", "2019", "2020", "2021"),

                                                                             selected = "2021", inline = TRUE),

                                                                imageOutput("pow_lu_map", width = "100%", height = "50%"),
                                                                


                                                         
                                                )), 
                                                                fluidRow(style = "margin: 6px;", align = "justify",
                                                                         column(4,
                                                                                br(),
                                                                                h4(strong("Land Use Transition Matrix")),
                                                                                p("We constructed a transition matrix with our data to understand how land converts across time. The matrix shows the total number of parcels of agricultural land converted from 2012-2022 to other uses. 
                                                                                  Note in this analysis, a parcel can convert multiple times across the period under study. If we ignore the undefined category, most of the land in agriculture is being converted into Residential Parcels. 
                                                                                  The residential category that had the most parcels added was Single Family Suburban. This category gained 533 parcels of land.")),
                                                                         column(8,
                                                                                br(),
                                                                                h4(strong("Land Use Conversion in Powhatan (Counts): 2012-2021")),
                                                                                highchartOutput("pow_sankey",height = 600) %>% withSpinner(type = 4, color = "#861F41", size = 1.25),
                                                                                
                                                                                p(tags$small("Data Source: Powhatan County Administrative Data"))
                                                                ))),
                                                tabPanel("Crop Cover",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4,
                                                                fluidRow(style = "margin: 6px;", align = "justify",
                                                                         h4(strong("Crops Grown in Powhatan County")),
                                                                         p("The map and bar chart on the right show the crop layer data for Powhatan County. Powhatan County is heavily forested with forested lands account for 67.84% of all 
                                                                  land. This number is a decrease from the 75.82% in 2012. A big reason why that number is reduced is that Powhatan is rapidly developing. 
                                                                  Developed land in Powhatan increased from 3.46% to 6.88% in 10 years. Most of this developed land is in the east side of the county closer to Richmond, VA. Forages 
                                                                  is the second biggest crop layer category with 15.42%. Forage is bulky food such as grass or hay for horses and cattle. Croplands are spread out throughout the 
                                                                  county and make up only use 4.1% of the land in the county. From an agricultural perspective, the land is most often used for raising livestock instead of growing crops. 
                                                                  There is a heavy concentration of row crops on the north boundary of Powhatan. The James River also acts as a boundary between Powhatan County and Goochland County.")
                                                                )), 
                                                         column(8, 
                                                                h4(strong("Crop Layer Map")),
                                                                
                                                                radioButtons(inputId = "pow_crop", label = "Select year: ",
                                                                             choices = c("2012", "2021"),
                                                                             selected = "2021", inline = TRUE),
                                                                imageOutput("pow_crop_img", width = "300px", height = "600px"),
                                                                
                                                                
                                                                #slickROutput("p.CropPNG", width = "100%", height = "50%"),
                                                                
                                                                h4(strong("Crop Layer Graphs")),
                                                                
                                                                
                                                                selectInput("pcrop", "Select Variable:", width = "100%", choices = c(
                                                                  "Total Acreage by Land Type 2021" = "pcrop21",
                                                                  "Total Acreage by Land Type 2012" = "pcrop12")
                                                                ),
                                                                
                                                                plotlyOutput("pcrop_graph", height = "500px"),
                                                                p(tags$small("Data Source: United States Department of Agriculture")),
                                                         ),
                                                ) ,
                                                tabPanel("Soil Type",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4,
                                                                fluidRow(style = "margin: 6px;", align = "justify",
                                                                         h4(strong("Soil Quality in Powhatan County")),
                                                                         p("Good quality soil is essential for crops to produce. Which makes soil quality a factor that could result in land conversion. 
                                                                  The National Cooperative Soil Survey is a survey done to classify soil into classes based on its usefulness. Those classes are: "),
                                                                         p(strong("Good Agricultural Soil:")),
                                                                         tags$ul(
                                                                           
                                                                           tags$li(strong("Class 1"), "soils have few limitations that restrict their use."),
                                                                           
                                                                           tags$li(strong("Class 2"), "soils have moderate limitations that reduce the choice of plants or that require moderate conservation practices.")),
                                                                         
                                                                         p(strong("Restricted Agricultural Soil:")),
                                                                         tags$ul(
                                                                           
                                                                           tags$li(strong("Class 3"), "soils have severe limitations that reduce the choice of plants, require special conservation practices, or both."),
                                                                           
                                                                           tags$li(strong("Class 4"), "soils have very severe limitations that reduce the choice of plants, require very careful management, or both.")),
                                                                         
                                                                         p(strong("Pasture, Rangeland & Wildlife:")),
                                                                           tags$ul(
                                                                             
                                                                             tags$li(strong("Class 5"), "soils are subject to little or no erosion but have other limitations, impractical to remove, that restrict their use mainly to pasture, rangeland, forestland, or wildlife habitat."),
                                                                             
                                                                             tags$li(strong("Class 6"), "soils have severe limitations that make them generally suitable for cultivation and that restrict their use mainly to pasture, rangeland, forestland, or wildlife habitat."),
                                                                             
                                                                             tags$li(strong("Class 7"), "soils have very severe limitations that make them unsuitable for cultivation and that restrict their use mainly to grazing, forestland, or wildlife habitat."),
                                                                             
                                                                             tags$li(strong("Class 8"), "soils and miscellaneous areas have limitations that preclude commercial plant production and that restrict their use to recreational purposes, wildlife habitat, watershed, or esthetic purposes."),
                                                                             
                                                                           ),
                                                                         p("Powhatan County soil is mostly in Class 2. As mentioned above, Class 2 is considered good qualy soil so crops can be grown here. Powhatan also has land that is in Class 1. This is the best land
                                                                  in the county, but it only makes up 1,686 acres. Class 4 soil is also prevalent in Powhatan. However, this soil class is unfavorable for farming as it has very severe limitations. The graph 
                                                                  on the right can be zoomed in on Class 8. This class is the least suitable soil class for any activity. Powhatan has only 29 acres in the class. Overall, Powhatan has good farmland and can remain agricultural. "),
                                                                )), 
                                                         column(8, 
                                                                h4(strong("Soil Quality Map")),
                                                                img(src = "Powhatan.png", style = "display: inline; float: left;", width = "100%", height = "50%"),
                                                                h4(strong("Soil Quality Graph")),
                                                                plotlyOutput("psoil", height = "500px") %>% withSpinner(type = 4, color = "#CF4420", size = 1.25),
                                                                p(tags$small("Data Source: National Cooperative Soil Survey"))),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] USDA. U.S. Land Use and Soil Classification. (n.d.). Retrieved July 26, 2022, from https://www.ars.usda.gov/ARSUserFiles/np215/Food%20security%20talk%20inputs%20Lunch%203-15-11.pdf")), 
                                                         )) ,
                                           
                                              ) 
                                     )), 
                            
                            
                            
                 #),
                 
                 ## Tab Parcellation --------------------------------------------
                 
                 #navbarMenu("Parcellation" , 
                            
                            tabPanel("Solar Farming Assessment", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Land Parcellation"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              tabsetPanel(
                                                tabPanel("Land Suitability",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                fluidRow(style = "margin: 6px;", align = "justify",
                                                                         h4(strong("Land Parcels in Powhatan County")),
                                                                         p("The dark red layer represents land that was parcellated during the most recent year selected. The lighter pink layer represent land that 
                                                                           parcellated during all previous years selected. If the years 2012 and 2020 are selected, the dark layer would show 2020 parcellations and 
                                                                           the light pink layer would show parcellation between 2012 and 2019. "), 
                                                                         p("There are large parcels being broken down along the northern border of the county. There is also significant parcellation in the center of 
                                                                           the county along Route 60 where most development has occurred in the last several years.")
                                                                )), 
                                                         column(8, 
                                                                h4(strong("Land Parcellation Map")),
                                                                sliderInput(inputId = "p.parcellationRange",
                                                                            label = "Years of Parcellation:",
                                                                            min = 2012,
                                                                            max = 2020,
                                                                            value = c(2012, 2020),
                                                                            sep = "", 
                                                                            width = "150%"),
                                                                leafletOutput("p.parcellationPlot") %>% withSpinner(type = 4, color = "#861F41", size = 1.5),
                                                                p(tags$small("Data Source: Powhatan County Administrative Data")),
                                                                p(),
                                                         ),
                                                         
                                                ), 
                                                tabPanel("Infastructure Proximity",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                fluidRow(style = "margin: 6px;", align = "justify",
                                                                         h4(strong("Land Parcels in Powhatan County")),
                                                                         p("The dark red layer represents land that was parcellated during the most recent year selected. The lighter pink layer represent land that 
                                                                           parcellated during all previous years selected. If the years 2012 and 2020 are selected, the dark layer would show 2020 parcellations and 
                                                                           the light pink layer would show parcellation between 2012 and 2019. "), 
                                                                         p("There are large parcels being broken down along the northern border of the county. There is also significant parcellation in the center of 
                                                                           the county along Route 60 where most development has occurred in the last several years.")
                                                                )), 
                                                         column(8, 
                                                                h4(strong("Land Parcellation Map")),
                                                                sliderInput(inputId = "p.parcellationRange",
                                                                            label = "Years of Parcellation:",
                                                                            min = 2012,
                                                                            max = 2020,
                                                                            value = c(2012, 2020),
                                                                            sep = "", 
                                                                            width = "150%"),
                                                                leafletOutput("p.parcellationPlot") %>% withSpinner(type = 4, color = "#861F41", size = 1.5),
                                                                p(tags$small("Data Source: Powhatan County Administrative Data")),
                                                                p(),
                                                         ),
                                                         
                                                ), 
                                                
                                                tabPanel("Index",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                fluidRow(style = "margin: 6px;", align = "justify",
                                                                         h4(strong("Parcellation Hot Spots in Powhatan County")),
                                                                         p("There are new parcels split from their mother parcels every year in Goochland. The hot spot map shows the area where parcellation happens 
                                                                  the most frequently with red polygons. After selecting the year range via the slider, the map will show the parcellation frequency during the period. 
                                                                  The more solid the circle is, the more frequently parcellation has happened in this area during the selected period."),
                                                                         p("From the hot spot map of parcellation in Powhatan over years, a pattern can be observed. Parcellation happened more frequently in 
                                                                  the center part, east and west edges of Powhatan. The high frequency of parcellation in the center part persisted in 2015-2021. 
                                                                  In the middle area, it became more often in 2021 and 2022. Parcellation in the east area might be driven by the proximity to the metropolis.")
                                                                )), 
                                                         column(8, 
                                                                h4(strong("Parcellation Hot Spot Map")),
                                                                sliderInput(inputId = "p.hotspotInput", 
                                                                            label = "Choose the starting and ending years",
                                                                            min = 2015,
                                                                            max = 2021,
                                                                            step = 1,
                                                                            value = c(2015,2021),
                                                                            width = "150%",
                                                                            sep = ""),
                                                                leafletOutput("p.hotspotMap") %>% withSpinner(type = 4, color = "#861F41", size = 1.5),
                                                                p(tags$small("Data Source: Powhatan County Administrative Data")),
                                                                p(),
                                                         ),
                                                         
                                                )
                                              ) 
                                     ), 
                            ) ,
                            
                            
                            
                 #),
                 
                 ## Tab Findings --------------------------------------------
                 tabPanel("Findings & Predictions", value = "conclusion", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Project Findings and Predictions"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   p("Given the rich agricultural histories of the two counties, we are interested in how agricultural land has changed over the last several years. 
                                     This research uses quantitative tools to understand how some key natural and social factors affect the parcellation and conversion with administrative data and county-level geospatial data."),
                                   fluidRow(style = "margin: 6px;", align = "justify",
                                            h4(strong("Goochland")),
                                            p("In Goochland, agricultural land was converted to residential, mainly single-family residential urban, and suburban. 
                                              There were also 5 parcels (about 671 acres) of large agricultural lands that have been parcellated into smaller agricultural plots."),
                                            p("Parcellation is occurring predominantly in the southeast of Goochland County near Richmond, around the U.S. Routes I64, 250, and 288. This pattern might reflect the urban influence on the county. 
                                              This pattern might also imply some correlation between parcellation and transportation. On the crop and land type map, those Routes are labeled as “Developed.” 
                                              High traffic volumes can also be seen along those Routes."),
                                            br(),
                                            h4(strong("Powhatan")),
                                            p("Large amounts of agricultural land were converted to 
                                              residential-suburban uses during the decade in Powhatan (including recurrences). Parcellation among agricultural land 
                                              is also noticeable, as 28 parcels (about 5,750 acres) of large agricultural lands have been parcellated
                                              into smaller agricultural plots."),
                                            p("Parcellation is occurring predominantly in the heart of Powhatan County, around the U.S. Routes 60 and 522. 
                                              On the east end near Richmond, high parcellation rates are seen along the U.S. Routes 60 and 288 within 
                                              the county and this might reflect the urban influence on the county. The high parcellation around 
                                              those Routes might imply some correlation between parcellation and transportation. On the map of crop and land type, 
                                              those Routes are labeled as “Developed”. High traffic volumes can also be seen along U.S. Routes 60 and 288. Hence the 
                                              correlation between parcellation and those Routes is also a correlation between parcellation and developed areas (traffic volumes)."),
                                            p("There is no obvious sign that poor soil quality can be a driver of land conversion out of agriculture from the maps."),
                                            p("In addition to the univariate spatial analysis, we also conducted a statistical analysis that examined the association between land conversion out of 
                                              agriculture and the characteristics of the land parcel, which include parcel acreage, whether the owner lives in the county, distance to the city of Richmond, the traffic volume and the soil class. 
                                              The analysis was conducted for Powhatan County only due to data availability. The findings from a logistic regression model show that the probability of converting out of agriculture: 
                                              decreases as the size of the parcel increases, decreases if the land owner lives in Powhatan, decreases with distance from Richmond. The association with traffic volume shows a U shaped impact 
                                              on the probability of conversion. Soil quality is not significantly associated with land conversion. Note these are not causal effects. They are associations."),
                                   ), 
                                   
                                   
                                   
                          )),
                 
                 ## Tab Data Sources --------------------------------------------
                 tabPanel("Data Sources", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Sources"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                          fluidRow(style = "margin: 6px;", align = "justify",
                                                   column(4,
                                                   img(src = "data-acs.png", style = "display: inline; float: left;", width = "180px"),
                                                   p(strong("American Community Survey"), "The American Community Survey (ACS) is an demographics survey conducted by the U.S Census Bureau. The ACS samples households to compile 1-year and 5-year datasets 
                                      providing information on social and economic characteristics including employment, education, and income. This project utilizes ACS 2016/2020 5-year
                                      estimates to obtain county- and census tract-level data to explore Goochland and Powhatan Counties' resident characteristics.")),
                                                   column(4,
                                                   img(src = "goochland.jpg", style = "display: inline; float: left;", width = "150px"),
                                                   p(strong("Goochland County Administrative Data"), "Goochland County provided us with parcel/property data which allowed us to gain a better understanding of the different land uses and parcellation
                                            that has occured over a 5 year period (2018 - 2022). The team used this data to create visualizations, specifically focusing on the distribution and change in land use in the county.")),
                                                   column(4,
                                                   img(src = "powhatan.jpg", style = "display: inline; float: left;", width = "150px"),
                                                   p(strong("Powhatan County Administrative Data"), "Powhatan County provided us with parcel/property data which allowed us to gain a better understanding of the different land uses and parcellation
                                            that has occured over a 8 year period (2014 - 2021). The team used this data to create visualizations, specifically focusing on the distribution and change in land use in the county.")),
                                          ),
                                          
                                          fluidRow(style = "margin: 6px;", align = "justify",
                                                   column(4,
                                                   img(src = "nass.jpg", style = "display: inline; float: left;", width = "130px"),
                                                   p(strong("USDA National Agricultural Statistics Service"), "The National Agricultural Statistics Service (NASS) under the United States Department of Agriculture (USDA) provides statistics on a wide variety
                                                    of agricultural topics. This project specifically relies on crop layer data to create maps and to conduct a statistical analysis on the probablity of land use conversion.")),
                                                   column(4,
                                                   img(src = "ncss.jpg", style = "display: inline; float: left;", width = "150px"),
                                          p(strong("USDA National Cooperative Soil Survey"), "The National Cooperative Soil Survey (NCSS) under the USDA provides soil data which was used to generate soil quality maps for both counties. 
                                            The data was also used for our statistical analysis to predict the occurrence of land use conversion.")),
                                          column(4,
                                          img(src = "vdot_crop.png", style = "display: inline; float: left;", width = "180px"),
                                          p(strong("VDOT Traffic Data"), "The Virginia Department of Transportation (VDOT) is responsible for building, maintaining and operating the state's roads, bridges and tunnels. VDOT also conducts 
                                          a program where traffic data are gathered from sensors in or along streets and highways and other sources.  This data includes estimates of the average number of vehicles that traveled each segment
                                          of road and daily vehicle miles traveled for specific groups of facilities and vehicle types are calculated. This project utilizes VDOT data to create traffic volume and commute maps for both counties."))
                                   )),
                                   
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
                                          p(a(href='https://www.linkedin.com/in/ariadne-tynes-236701269/','Ari Tynes', target = '_blank'), "(Berea College, Undergraduate in Economics with a Concentration in Methods and Modeling);",
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

                                            # h4(strong("Project Stakeholders")),
                                            # p(a(href = "https://www.linkedin.com/in/rachel-henley-335a0345/", 'Rachel Henley', target = '_blank'), "(Virginia Cooperative Extension, Powhatan County);",
                                            #   br(), 
                                            #   a(href = 'https://goochland.ext.vt.edu/staff/Maxwell-Charlotte.html', 'Nichole Shuman', target = '_blank'), "(Virginia Cooperative Extension, Goochland County)."),
                                            # p("", style = "padding-top:10px;"),
                                            
                                          
                                   )

                          )) ,
                 inverse = T)


# server --------------------------------------------------------------------------------------------------------------------

server <- function(input, output){
  
  runjs(jscode)
  
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
  ### SOCIODEMOGRAPHICS  =================================================
  output$employ_plot <- renderPlotly({
    employ_plot
  })
  
  
  powhatan_soc <- reactive({
    input$powhatan_soc
  })
  
  output$psoc <- renderPlot({
    
    if(powhatan_soc() == "page"){
      age.func(input$yearSelect_psoc, "Powhatan ")
    }
    else if(powhatan_soc() == "pind"){
      ind.func(input$yearSelect_psoc, "Powhatan ")
    }
    else if(powhatan_soc() == "pinc"){
      inc.func(input$yearSelect_psoc, "Powhatan ")
    }
    else if(powhatan_soc() == "pedu"){
      edu.func(input$yearSelect_psoc, "Powhatan ")
    }
    
  })
  
  
  output$powhatan_con<- renderLeaflet({
    powhatan_con
  })
  
  ### CROP LAYERS ================================================
  
  
  output$pow_crop_img <- renderImage(deleteFile = FALSE,{
    if(input$pow_crop == "2012"){
      return(list(src = "www/CroplandPngs/powCrop12.png", width = "270%", height = "100%"))    
    }
    else{
      return(list(src = "www/CroplandPngs/powCrop21.png", width = "270%", height = "100%"))    
    }
  })
  
  
  pcrop <- reactive({
    input$pcrop
  })
  
  output$pcrop_graph <- renderPlotly({
    if(pcrop() == "pcrop12"){
      cropPlot.func("Powhatan", 2012)
    }
    else if(pcrop() == "pcrop21"){
      cropPlot.func("Powhatan", 2021)
    }
  })
  

  
  output$p.soilPNG <- renderSlickR({
    img <- "data/Soil_Quality/Powhatan.png"
    slickR.func(img)
  })
  
  
  
  output$psoil <- renderPlotly({
    psoil
  })
  

  output$pow_trafficPNG <- renderImage(deleteFile = FALSE,{
    if(input$pow_traffic == "pvol"){
      return(list(src = "www/trafficPNGs/powVol.png", width = "100%", height = "100%"))
    }
    else if(input$pow_traffic == "prich"){
      return(list(src = "www/trafficPNGs/powProx.png", width = "100%", height = "100%"))
    }
  })
  
  ### LAND USE ======================================

  
  
  output$pow_lu_map <- renderImage({
    if(input$pow_lu_year == "2015"){
      return(list(src = "www/luPNGs/Pow_LU15.png", width = "100%", height = "75%"))    
    }
    else if(input$pow_lu_year == "2016"){
      return(list(src = "www/luPNGs/Pow_LU16.png", width = "100%", height = "75%"))    
    }
    else if(input$pow_lu_year == "2017"){
      return(list(src = "www/luPNGs/Pow_LU17.png", width = "100%", height = "75%"))    
    }
    else if(input$pow_lu_year == "2018"){
      return(list(src = "www/luPNGs/Pow_LU18.png", width = "100%", height = "75%"))    
    }
    else if(input$pow_lu_year == "2019"){
      return(list(src = "www/luPNGs/Pow_LU19.png", width = "100%", height = "75%"))    
    }
    else if(input$pow_lu_year == "2020"){
      return(list(src = "www/luPNGs/Pow_LU20.png", width = "100%", height = "75%"))    
    }
    else if(input$pow_lu_year == "2021"){
      return(list(src = "www/luPNGs/Pow_LU21.png", width = "100%", height = "75%"))    
    }
  },deleteFile = FALSE)
  
  
  output$pow_sankey <- renderHighchart({ 
    hchart(data_to_sankey(p.sankey), "sankey") %>%
      hc_add_theme(thm.p) %>%
      hc_plotOptions(series = list(dataLabels = list(style = list(fontSize = "10px",color="black", textOutline = "none"))))
  })
  
  
  ### HOT SPOTS ======================================

  
  output$p.hotspotMap <- renderLeaflet({
    begin_year <- input$p.hotspotInput[1]-2000
    end_year <- input$p.hotspotInput[2]-2000
    yrRange <- c(begin_year:end_year)
    
    hotspot.func("Powhatan", yrRange)
  })
  
  
  
  ### PARCELLATION ======================================

  
  output$p.parcellationPlot <- renderLeaflet({
    yearRange <- abs(input$p.parcellationRange[1]:input$p.parcellationRange[2])
    parc.func(pow_parcellation, yearRange, "Powhatan", po_cnty)
    
  })
  
}

shinyApp(ui = ui, server = server)
