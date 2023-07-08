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

## NECESSITIES =================================================

# necessary imports for many of our plots (county boundary shape files)
gl_cnty<- st_read("data/cnty_bndry/Goochland_Boundary.shp") %>% st_transform("+proj=longlat +datum=WGS84")
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

gcon <- st_read("data/Conservation/Gooch_Preservation.shp") %>% st_transform("+proj=longlat +datum=WGS84")
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
g.sankey <- read.csv("data/luParcelData/g_sankey.csv") %>% select(LUC_old,LUC_new) %>% filter(LUC_old %in% agLabels, LUC_old != LUC_new)
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
  else{
    hotspot.plt <- hotspot.plt %>% setView(lng=-77.885376, lat=37.73143, zoom = 10)
    file_list <- paste("data/Parcel_Hotspot/goochland/gooch_hotspot_",range,".shp",sep = "")
    hotspot.plt <- hotspot.plt %>% addPolygons(data = gl_cnty, fillOpacity = 0)
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

ui <- navbarPage(title = "DSPG 2022",
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
                          fluidRow(style = "margin: 6px;",
                                   align = "justify",
                                   column(4,
                                          h2(strong("Project Background")),
                                          p(strong("The setting:"), "Goochland and Powhatan Counties are on the urban fringe outside Richmond, Virginia. Communities on the urban fringe are located between both rural and urban areas. Both counties are known for
                                            their rich agricultural histories and proximity to the state capital. Although Powhatan County has been growing and evolving faster than the average rate, they, like Goochland County, would like to keep their agricultural roots."),
                                          p(),
                                          p(strong("The problem:"), "Both Goochland and Powhatan County are approximately 40 minutes away from Richmond. Being this close to a large metropolitan area has its advantages and disadvantages. One of the most significant challenges 
                                            with this location is land conversion. Land conversion is when land shifts its use from one to another. In the case of the two counties, the land is changing from agricultural land primarily to residential housing development. 
                                            This can challenge communities that now have to support a new economic growth model. Both counties have enacted policies to help manage land conversion: Powhatan implemented a land tax deferral program, and Goochland, 
                                            with their 85-15 rural land commitment, maintains 85% of land as rural. Both counties would like to use their administrative data to help inform and analyze land conversion in their counties. This is a critical step in understanding the 
                                            factors that cause land conversation and the probability that a parcel of land will parcelize further."),
                                          p(),
                                          p(strong("The project:"), "This Virginia Tech", a(href = "https://aaec.vt.edu/index.html", "Department of Argicultural and Applied Economics", target = "_blank"),
                                            "Data Science for Public Good (DSPG) project uses data analytics, agricultural economics, and geospatial tools to analyze land conversion in Goochland and Powhatan. This project provides stakeholders with information 
                                            they can use to more fully understand how land is being redistributed, specifically from agriculture.")
                                   ),
                                   column(4,
                                          h2(strong("Our Work")),
                                          p('Our team worked with Goochland County and Powhatan County to help analyze land conversion and agriculture loss using existing administrative data. Both counties want to retain their rural/agricultural character and have 
                                            introduced policies to encourage land conservation and manage development that may require further land conversion. Our team researched background information on both counties and compiled data from public sources, 
                                            including the counties, the US Census, and the Virginia Department of Transportation. All the data sources referenced in our analysis can be found under the Data Sources tab. Throughout the 10-week summer program, 
                                            the team met with the stakeholders to receive feedback, data, and suggestions on their work.'),
                                          p(),
                                          p("We collected data from several sources to create graphs, maps, and tables. These visualizations allowed us to analyze and present our findings in a more digestible manner. More specifically, we:"),
                                          tags$li("Provided county-level graphs of Goochland and Powhatan County residents'", strong("sociodemographic and socioeconomic"), "characteristics."),
                                          tags$li("Used USDA data to map ", strong("crop and land types"), "in the counties. "),
                                          tags$li("Used USDA data to map ", strong("soil quality"), "throughout the counties."),
                                          tags$li("Mapped traffic data to show ", strong("traffic volume and commute times"), "to Richmond, Virginia."),
                                          tags$li("Mapped the locations of", strong("land parcels"), "to provide insight on the frequency of parcellation and identify any patterns."),
                                          tags$li("Estimated a", strong("statistical model"), "to understand some of the factors associated with conversion of land out of agriculture. "), 
                                          p(),
                                          p("This dashboard compiles our findings and allows stakeholders and other users to explore the information interactively."),
                                   ),
                                   column(4,
                                          h2(strong("Dashboard Aims")),
                                          p("Our dashboard is aimed at:"),
                                          p(strong("Goochland and Powhatan County governments."), "Our analyses provide further insights and allow more understanding of land conversion within each county. Our statistical analysis examines and explains the association 
                                            between land conversion and several factors, including soil quality and traffic volume. More information on our statistical analysis can be found under our Findings and Predictions tab. We hope that our results will be helpful 
                                            in decision-making regarding land conversion and conservation within the counties."),
                                          p(strong("Researchers working on land use conversion."), "Land conversion is a global problem, not just in Goochland and Powhatan counties. We hope our dashboard can act as a starting point for those researching this topic in 
                                            Goochland, Powhatan, or further afield."),
                                          br(),
                                          img(src = "powhatan_crops.JPG", style = "display: inline; float: center;", width = "400px"),
                                          br(),
                                          p(tags$small("Photo courtesy of Rachel Henley, VCE"))
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2022')))
                          ) 
                 ),
                 
                 ## Tab sociodemographics --------------------------------------------
                 #navbarMenu("Sociodemographics" , 

                            tabPanel("Sociodemographics", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Powhatan"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(4, 
                                                     h4(strong("County Background")),
                                                     p("Powhatan County is located in Virginia's Central Piedmont. It is 272 square miles and is home to a population of 29,253. The county is approximately 20 miles from Richmond and 2 hours from Washington, D.C. 
                                                       The James River borders the county's north end, contributing to the formation of many creeks stretching southward. There are five districts within the county and 12 polling places interspersed through them. 
                                                       Powhatan is rich in history and offers abundant tourist attractions contributing to its thriving economy [1]. The county has three parks/wildlife management areas: Powhatan Wildlife Management Area, Fighting Creek Park, 
                                                       and Powhatan State Park. Where once were several farms, the Powhatan Wildlife Management Area is 4,462 acres that provide many experiences such as hunting, fishing, and other forested activities that aim to maintain the 
                                                       diversity of its natural wildlife species [2]."),
                                                     
                                                     h4(strong("Summary Statistics")),
                                                     p("The total population of Powhatan County is 29,253, split between 51% male (15,188), and 49% female (14,065) [3]. 28,762 identify as one race, where 25,732 are white, 2,505 are African American, 64 are American Indian and Alaska Native, 
                                                     169 are Asian, 24 are Native Hawaiian and Other Pacific Islander, and 268 are some other race [4]."),
                                                     p("24,715 or 57.3% of the population within Powhatan County is employed. The unemployment rate is 1.4% [5]"),
                                                     p("Of the 13,938 civilian employed population, very few are employed in agriculture, forestry, fishing, hunting, and mining. Around 0.94% of the civilian-employed population falls under that category. Of that 0.94% of the workers, approximately half serve roles in management, 
                                                       business, science, and art, while 14.5% of that population work in natural resources, construction, and maintenance occupations [6]."),
                                                     p("Of the 10,392 households, the median income is $93,833, with a margin of error of around 5,342. Approximately 30.2% of the 8,220 families have one earner, while 44.8% have two earners [7]. 
                                                       The greatest proportion (23.4%) of earners in Powhatan make between $100,000 to $149,999 [8]."),
                                                     p("Nearly 89.6% of the population 25 and over have graduated high school. Approximately 2,032 people, or 9.3% of the population over 25, have attained a graduate or professional degree [9]."),
                                                     p("According to the 2017 agricultural census, there were approximately 263 farms with an average farm size of 132 acres in 2017. This makes the total land coverage of farms to be 34,585 acres. 
                                                       $11,249,000 was generated from agricultural products sold to the market. 54% of farms sold less than $2,500, and 13% of farms sold between $25,000 and $24,999. 
                                                       Grains, oilseeds, dry beans, and dry peas were the main crops that were sold ($2,542,000), and poultry and eggs were the main livestock and poultry products sold ($6,056,000) [10]."),
                                                     p("1.9% of Powhatan's population moved within the county, 7.4% moved into the county from a different county in VA, 0.8% moved from a completely different state, and 0.1% moved from abroad [11]."),
                                                     
                                              ) ,
                                              column(8, 
                                                     h4(strong("Sociodemographics")),
                                                     selectInput("powhatan_soc", "Select Variable:", width = "100%", choices = c(
                                                       "Age Distribution of Population" = "page",
                                                       "Employment by Industry" = "pind",
                                                       "Income Distribution" = "pinc",
                                                       "Median Earnings By Educational Attainment (Age > 25 years)" = "pedu")
                                                     ),
                                                     radioButtons(inputId = "yearSelect_psoc", label = "Select Year: ", 
                                                                  choices = c("2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     plotOutput("psoc", height = "500px"),
                                                     h4(strong("Visualization Summaries")),
                                                     p("The", strong("age distribution"), "graphs shows that the 45-64 age group has consistently been the largest in the county, making up more than 30% of the population since 2017. The 25-44 age group has been 
                                                       the second largest, but has faced more inconsistency and has seen a decrease since 2018."),
                                                     p("The", strong("employment"), "graphs indicates that the education, health, and social services industry group has been the largest by a wide margin, and specifically saw a large increase in 2019. The agricultural, forestal,
                                                       fishing, hunting, and mining industry group has consistently been one of the smallest."),
                                                     p("The", strong("income distribution"), "graphs illustrate the consistent growth in individuals and households earning at least $100,000 each year. This growth has been accompanied by a consistent decrease in earnings below $75,000."),
                                                     p("The", strong("median earnings"), "graphs highlight the fact that those with a highest educational attainment of Some college/Associates earn the most. The median earnings for this group were significantly higher than others up until 2019, but saw 
                                                       a significant decrease to $66,915 in 2020. This number is nearly identical to the median earnings for those with less than a high school education at $66,716."),
                                              )),
                                     column(12, 
                                            h4("References: "), 
                                            p(tags$small("[1] About Powhatan. About Powhatan | Powhatan County, VA - Official Website. (n.d.). Retrieved July 15, 2022, from http://www.powhatanva.gov/317/About-Powhatan", tags$br(),
                                                         "[2] Powhatan WMA. Virginia Department of Wildlife Resources. (n.d.). Retrieved July 15, 2022, from https://dwr.virginia.gov/wma/powhatan/", tags$br(),
                                                         "[3] U.S. Census Bureau (2022). Age and sex, 2020: ACS 5-Year estimates subject tables. Retrieved July 18, 2022, from https://data.census.gov/cedsci/table?t=Populations%20and%20People&g=0500000US51145&y=2020&tid=ACSST5Y2020.S0101", tags$br(), 
                                                         "[4] U.S. Census Bureau (2022). Race, 2020: DEC redistricting data (PL 94-171). Retrieved July 18, 2022, from https://data.census.gov/cedsci/table?g=0500000US51145&y=2020&tid=DECENNIALPL2020.P1", tags$br(),
                                                         "[5] U.S. Census Bureau (2022). Employment status, 2020: ACS 5-Year estimates subject tables. Retrieved July 18, 2022, from https://data.census.gov/cedsci/table?t=Employment%3AEmployment%20and%20Labor%20Force%20Status&g=0500000US51145&y=2020&tid=ACSST5Y2020.S2301" , tags$br(),
                                                         "[6] U.S. Census Bureau (2022). Industry by occupation for the civilian employed population 16 years and over, 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Occupation&g=0500000US51145&y=2020&tid=ACSST5Y2020.S2405", tags$br(),
                                                         "[7] U.S. Census Bureau (2022). Median income in the past 12 months (in 2020 inflation-adjusted dollars), 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Income%20%28Households,%20Families,%20Individuals%29&g=0500000US51145&y=2020&tid=ACSST5Y2020.S1903", tags$br(),
                                                         "[8] U.S. Census Bureau (2022). Income in the past 12 months (in 2020 inflation-adjusted dollars), 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Income%20%28Households,%20Families,%20Individuals%29&g=0500000US51145&y=2020&tid=ACSST5Y2020.S1901", tags$br(),
                                                         "[9] U.S. Census Bureau (2022). Educational attainment, 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Education&g=0500000US51145&y=2020&tid=ACSST5Y2020.S1501", tags$br(),
                                                         "[10] United States Department of Agriculture. Powhatan County Virginia - county profile. National Agricultural Statistics Survey. Retrieved July 25, 2022, from https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/Virginia/cp51145.pdf", tags$br(), 
                                                         "[11] U.S. Census Bureau (2022). Geographic mobility by selected characteristics in the United States, 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Residential%20Mobility&g=0500000US51145&y=2020&tid=ACSST5Y2020.S0701")),
                                            p("", style = "padding-top:10px;")) 
                                     , 
                            ) ,
                            
                            
                            
                 #),
                 
                 ## Tab Policy --------------------------------------------
                 tabPanel("Policy", value = "conclusion", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Land Use & Environmental Policies"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   p("Policy plays a key role in land use. At every level - federal, state, and local - officials develop land-use plans, with a wide 
                                     variety of different objectives and long-term visions. These plans drive changes in land use, and it is important to investigate 
                                     policies at every level to get a full picture of land-use conversion."),
                                   tabsetPanel(
                                     tabPanel("Federal",
                                              column(6, 
                                                     fluidRow(style = "margin: 6px;", align = "justify",
                                                     p("", style = "padding-top:10px;"),
                                                     p(strong("The Conservation Reserve Program (CRP):")), 
                                                     p("The CRP is a federal land conversion program administered by the Farm Service Agency (FSA). 
                                                       The goal of this program is to reduce cropland acreage - a land retirement program that pays farmers to retire some of their crop land. 
                                                       This program has been a major driver of land retirement since it was implemented in 1985. The program is motivated by environmental 
                                                       protection goals. To get approved for the land retirement program, your land must hit specific criteria based on targeted environmental 
                                                       factors. There is then a bidding process. To farmers, this is an incentive to retire land. Studies show that this policy has led to farmers 
                                                       retiring their less productive land. In 2005, “CRP paid $1.7 billion to keep a land area almost the size of Iowa out of production” [1]. 
                                                       This federal land conversion program incentivizes farmers to retire their land and lower production. The goal is to protect the environment."),
                                                     br(),
                                                     p(strong("Federal Crop Insurance Program:")),
                                                     p("This program is a partnership between the federal government and insurers - connecting the public and private sectors. 
                                                       This program does the opposite of the CRP and raises incentives to grow crops. The goal of the Federal Crop Insurance Program is not directly to affect 
                                                       land use, but it does influence conversion rates. In 1993, after some catastrophic flooding, congress passed the Federal Crop Insurance Reform Act. This 
                                                       act increased the premium subsidies for all crop insurance products- now the program includes a revenue insurance option and catastrophic coverage. About 
                                                       60% of cultivated cropland in the Unites States is covered by the Federal Crop Insurance Program. This program raises incentives to grow crops and could 
                                                       influence farmers to cultivate riskier, less productive land [1]."),
                                                     br(),
                                                     p(strong("Emergency Relief Program (ERP):")), 
                                                     p("The purpose of this program is to help agriculture producers offset the damage caused by natural disasters such as drought or 
                                                     flooding [2]. Funds are distributed in two phases, to aid livestock producers impacted by natural disasters. The USDA announced in May of 2022 that 
                                                       “commodity and specialty crop producers impacted by natural disaster events in 2020 and 2021 will soon begin receiving emergency relief payments totaling 
                                                       approximately $6 billion through the Farm Service Agency’s (FSA) new Emergency Relief Program (ERP) to offset crop yield and value losses” [3]."),
                                                     p(),
                                                     p(),
                                                     
                                              )) , 
                                              column(6, 
                                                     fluidRow(style = "margin: 6px;", align = "justify",
                                                     p("", style = "padding-top:10px;"),
                                                     p(strong("Emergency Conservation Program (ECP):")), 
                                                     p('This program “provides funding and technical assistance for farmers and ranchers to restore farmland damaged by natural disasters and for emergency water 
                                                       conservation measures in severe droughts" [4]. This program does so by giving landowners funding to install water conservation systems or to repair 
                                                       damaged farmland. This is another example of a conservation program that gives farmers insurance, which could incentive farmers to continue to cultivate their 
                                                       land - regardless of the potential risks associated with damage from storms and droughts. Farms are eligible for this assistance if the damage is affecting 
                                                       productivity, there is evidence that conditions will worsen without intervention, and the repairs will be too costly without federal assistance [4]. 
                                                       Up to 75% of the costs can be provided. The FSA County Committee can “approve applications up to $125,000 while $125,001 to $250,000 requires state committee 
                                                       approval" [4].'),
                                                     br(), 
                                                     p(strong("Source Water Protection Program (SWPP):")),
                                                     p("This program is a joint project with the U.S. Department of Agriculture (USDA) Farm Service Agency (FSA) and the National Rural Water Association (NRWA), 
                                                       a non-profit water and wastewater utility membership organization [5]. It was designed with the goal of protecting surface and ground water that is 
                                                       used as drinking water by rural residents. The NRWA employs full-time rural source water technicians that work with state and county FSA staff to make decisions 
                                                       on where pollution prevention is needed. The SWPP works at the local level, to educate and encourage farmers to prevent source water prevention. With this program, 
                                                       it is the local community to create and invest in a water protection plan."),
                                                     br(),
                                                     p(strong("Agriculture Risk Coverage (ARC) and Price Loss Coverage (PLC):")),
                                                     p('The ARC program is an “income support program that provides payments when actual crop revenue declines below a specified guaranteed level" [6]. PLC program “provides 
                                                     income support payments when the effective price for a covered commodity falls below its effective reference price" [6]. Both programs provide financial protection 
                                                     to farmers. They serve as a safety net from drops in crop revenues and prices.'))),
                                              column(12,
                                                     h4("References:"),
                                                     p(tags$small("[1] Lubowski, R. N., Bucholtz, S., Claassen, R., Roberts, M. J., Cooper, J. C., Gueorguieva, A., & Johansson, R. (n.d.). Environmental Effects of Agricultural Land-Use Change United States Department of Agriculture The Role of Economics and Policy. Retrieved July 25, 2022, from www.ers.usda.gov", tags$br(), 
                                                                  "[2] USDA. (n.d.). Emergency relief. USDA Farm Service Agency. Retrieved July 25, 2022, from https://www.fsa.usda.gov/programs-and-services/emergency-relief/index ", tags$br(),
                                                                  "[3] USDA. (n.d.). USDA to provide approximately $6 billion to commodity and specialty crop producers impacted by 2020 and 2021 natural disasters. USDA Farm Service Agency. Retrieved July 25, 2022, from https://www.fsa.usda.gov/state-offices/Virginia/news-releases/usda-to-provide-approximately-6-billion-to-commodity-and-specialty-crop-producers-impacted-by-2020-and-2021-natural-disasters- ", tags$br(),
                                                                  "[4] USDA. (n.d.). Emergency conservation program. USDA Farm Service Agency. Retrieved July 25, 2022, from https://www.fsa.usda.gov/programs-and-services/conservation-programs/emergency-conservation/index", tags$br(),
                                                                  '[5] "Grassroots" source water protection program. USDA Farm Service Agency. (n.d.). Retrieved July 25, 2022, from https://www.fsa.usda.gov/programs-and-services/conservation-programs/source-water-protection/index', tags$br(),
                                                                  "[6] USDA. (n.d.). Agriculture risk coverage (ARC) &amp; Price Loss Coverage (PLC). USDA Farm Service Agency. Retrieved July 25, 2022, from https://www.fsa.usda.gov/Assets/USDA-FSA-Public/usdafiles/FactSheets/2019/arc-plc_overview_fact_sheet-aug_2019.pdf")),
                                                     p("", style = "padding-top:10px;"),
                                              )), 
                                     tabPanel("State",
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
                                     tabPanel("County",
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
                                                tabPanel("Land Use",
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
                                                tabPanel("Crop Layer",
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
                                                tabPanel("Soil Quality",
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
                                                tabPanel("Traffic Data",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                fluidRow(style = "margin: 6px;", align = "justify",
                                                                         h4(strong("Traffic in Powhatan County")),
                                                                         p("Traffic information provides a metric to understand the congestion of roadways in the county and to examine whethere there were correlations with residential housing location. 
                                                                  Here we present maps of traffic volume and maps of proximity to the city of Richmond. A land parcels proximity to major urban centers and its traffic volume may affect it likelihood of conversion out of agriculture."),
                                                                         p("Although Powhatan has no interstates running through it, the two state routes that it does have are able to provide fast travel throughout the county. State Route 60, which runs horizontally through the county holds a lot of annual traffic, while Route 525 provides travel through the county vertically. Although Powhatan is on the edge of the City of Richmond, travel times are pretty high while driving from Richmond through Powhatan."),
                                                                )), 
                                                         
                                                         column(8, 
                                                                h4(strong("Traffic Visualizations")),
                                                                selectInput(inputId = "pow_traffic", label = "Select Variable:", width = "100%", choices = c(
                                                                  "Traffic Volume" = "pvol",
                                                                  "Proximity to Richmond" = "prich"), 
                                                                ),
                                                                imageOutput("pow_trafficPNG", height = "110%"),
                                                                
                                                                br(),
                                                                p(tags$small("Data Source: Virginia Department of Transportation")),
                                                                
                                                         ),
                                                         
                                                )
                                              ) 
                                     )), 
                            
                            
                            
                 #),
                 
                 ## Tab Parcellation --------------------------------------------
                 
                 #navbarMenu("Parcellation" , 
                            
                            tabPanel("Parcellation", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Land Parcellation"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              tabsetPanel(
                                                tabPanel("Parcels",
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
                                                
                                                tabPanel("Hot Spots",
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
  
  ### SOCIODEMOGRAPHICS  =================================================
  
  
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
