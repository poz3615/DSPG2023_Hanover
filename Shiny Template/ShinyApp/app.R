
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
                                                     h2(strong("Visualizations")),
                                                     p("Visualizations go here"),
                                                     plotlyOutput("employ_plot", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                              ),
                                              column(12,
                                                     h4(strong("References")),
                                                     p("References go here")
                                              )
                                     ),
                                     tabPanel("Conservation Policy",
                                              p(),
                                              # p('State-level officials work within the confines of both federal and local policy. They aim to simultaneously enhance federal policy while enabling local officials to make comprehensive 
                                              # land-use plans. The state of Virginia is under the Dillon Rule which states that local ordinances must be consistent with state law [1]. Local officials are the ones approving parcel-specific 
                                              # land use plans, but state and federal officials play a key role [1]. The state courts are the "referees" to determine if land use decisions violated some aspect of various state laws, or if 
                                              #   the land use rules violated the state constitution in some way [1].'),
                                              
                                              column(4,
                                                     h2(strong("Policies")),
                                                     h4(strong("Forest Legacy Program (FLP)")),
                                                     p("The FLP is a federal conservation program orchestrated
                                                    by the U.S Forest Service collaborating with State agencies
                                                    to preserve and protect private forests through land
                                                    purchases or conservation easements. This program promotes
                                                    feasible forest management by granting economic benefits
                                                    to landowners persuading them to sustain their forest land.
                                                    The FLP has been successful in conserving more than 2.8
                                                    million acres of forest land spanning across the United States.
                                                    To qualify for this program, the land must be located within an 
                                                    identified Forest Legacy Area and a non-federal match of 25% to obtain the grant."),
                                                     p(),
                                                     h4(strong("Conservation Reserve Program (CRP)")),
                                                     p("The CRP is a land conservation program managed by the Farm
                                                    Service Agency aimed to trade yearly rental payments to farmers
                                                    enrolled for the agreement of removing land sensitive to agriculture 
                                                    production and plant species to implement conservation practices.
                                                    This is meant to ensure the enhancement of the environmental health 
                                                    and quality of land. The land registered in this program is contracted
                                                    for 10 to 15 years. Land desired to participate in this program
                                                    is agricultural land easily susceptible to erosion, located near
                                                    bodies of water or providing habitats to wildlife. Hence, the
                                                    motivating factor behind this program is to reduce soil erosion,
                                                    enhance wildlife habitats, improve water quality, and stimulate
                                                    conservation and restoration of land.  Conservation Reserve
                                                    Enhancement Program (CREP) is a sub-program that troubleshoots
                                                    priority conservation issues identified by localities."),
                                                     p(),
                                                     h4(strong("Emergency Conservation Program (ECP)")),
                                                     p("The ECP aims to support agricultural producers in repairing
                                                    and restoring any damaged farmland and agricultural infrastructure
                                                    due to natural disasters. Assistance from this program includes
                                                    financial support in order to remove debris, restore fencing,
                                                    repair water sources, and seed damaged areas. Funding is granted 
                                                    based on the severity of the damage to handle a portion of the expenses.")
                                              ),
                                              column(4,
                                                     h2(strong("Additional Policies")),
                                                     h4(strong("The Code of Virginia")),
                                                     p("The code of Virginia enacts various policies that allow localities
                                                    to enter into voluntary agreements with landowners across the state within
                                                    districts designated to protect farm lands and forest lands. 
                                                    There are several land conservation policies outlined in the Code of Virginia:"),
                                                     tags$li("Virginia Conservation Easement Act (Title 10.1, Chapter 10) – 
                                                          This act develops a voluntary legal agreement to permanently 
                                                          limit the uses of the landowner’s land in order to protect its 
                                                          natural values. The act ensures that landowners maintain their 
                                                          rights to own and use their land or sell or pass it onto heirs."),
                                                     tags$li("Virginia Open-Space Land Act (Title 15.2, Chapter 18): 
                                                          The Commonwealth of Virginia is authorized to form partnerships
                                                          with landowners to decrease urban sprawl and protect open space
                                                          through this act. It influences the localities to designate parcels
                                                          of land in Hanover County for use as open-space land. Open-space 
                                                          land is defined as any “any land which is provided or preserved for
                                                          (i) park or recreational purposes, (ii) conservation of land or other 
                                                          natural resources, (iii) historic or scenic purposes, (iv) assisting
                                                          in the shaping of the character, direction, and timing of community development,
                                                          (v) wetlands as defined in § 28.2-1300, or (vi) agricultural and forestal production.” 
                                                          (Code of Virginia Code - Chapter 17. Open-Space Land Act)"),
                                                     tags$li("Agricultural and Forest Districts Act (Title 15.2, Chapter 43): 
                                                          This act develops legal agreements aimed to stimulate the preservation 
                                                          of land occupied by forest and land used for agriculture. For a duration 
                                                          of 4-10 years, the land is sustained with its pre-existing use once landowners 
                                                          agree to partake in this act."),
                                                     tags$li(" Land Preservation Tax Credit (Title 58.1, Chapter 32): Through this policy,
                                                          landowners who register property under the conservation land easements 
                                                          are granted income tax credit for 40% of the value of the land donated."),
                                                     p(),
                                                     h4(strong("Purchase of Development Rights Program (PDR)")),
                                                     p("The PDR program is conducted by the Virginia Department of 
                                                    Agriculture and Customer Services in order to assist in funding 
                                                    PDR programs with local agencies to compensate landowners who 
                                                    voluntarily partake in agricultural conservation easements.")
                                                     
                                              ),
                                              column(4,
                                                     h2(strong("County")),
                                                     h4(strong("Zoning Regulations")),
                                                     p("Zoning districts promote the preservation of open space, 
                                                    particularly in areas containing environmentally sensitive land.
                                                    This strategy encourages rural conservation districts to protect 
                                                    open spaces while providing the flexibility to develop residential areas."),
                                                     p(),
                                                     h4(strong("Rural Conservation Subdivisions")),
                                                     p("Since January 2013, Hanover county has established
                                                    34 Rural Conservation subdivisions. Rural Conservation zoning 
                                                    districts require a minimum of 70% of the acreage of a district 
                                                    to be placed in a conservation area. Thus, this zoning tool has ensured 
                                                    that a total of 5466 acres are included in conservation areas.")
                                              ),
                                              column(12,
                                                     align = "center",
                                                     h4(strong("References")),
                                                     p("References go here")
                                              )
                                           
                                     ),
                                     tabPanel("Solar Policy",
                                              p(),
                                              p('"In urbanizing areas such as the suburbs near Richmond, Hampton Roads, and Northern Virginia, control over how private property 
                                                is developed may be a contentious process involving landowners and their lawyers, neighbors, or local residents upset over additional 
                                                development, and local officials. In Fairfax, Loudoun, and Prince William counties over the last 30 years, the Board of County Supervisor 
                                                election campaigns have been based on growth management issues. Local officials have reacted to citizen complaints, and incumbents have 
                                                been voted out of office because they were either too supportive of growth or too restrictive” [1].'),
                                              
                                              column(6,
                                                     h2(strong("Solar Policy Background")),
                                                     p("Virginia's renewable energy goals have made the construction of
                                                     solar farms more common. The state has implemented a policy that requires
                                                     Dominion Energy to achieve 100% renewable energy by 2045, and Virginia Power,
                                                     a subsidiary of Dominion, to do the same by 2050. This policy encourages energy
                                                     companies to develop more sources of renewable energy. With the development of more
                                                     energy sources, a degree of environmental impact is inevitable. Certain developments,
                                                     like the new solar farm in Hanover County, off of Peppertown Road, have plans to
                                                     retire the solar field at the end of their useful lives. The developers plan to
                                                     return the area to its natural state as best as they can. However, it is important
                                                     to note that Hanover County currently does not have specific requirements for native or
                                                     pollinator-friendly vegetation at solar facilities, as outlined in their zoning ordinance."),
                                                     p()
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
                                                tabPanel("Land Use and Zoning",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         h2(strong("Land Use by Parcel")),
                                                                         p("This map shows the parcel-level zoning 
                                                       classification for Hanover County. 
                                                       We used Virginia's land use codes, along 
                                                       with assessor data from Hanover County to display 
                                                       areas in the county according to their zoning ordinances.  
                                                       Here we see that the majority of the county is zoned for 
                                                       agricultural and residential use."),
                                                                         p(),
                                                                         p("In the map above we saw that the majority of 
                                                       area in Hanover county is zoned for agriculture. 
                                                       However, from using the same data we derived this bar
                                                       graph that shows the number of residential parcels is more 
                                                       than the number of agriculture parcels.")
                                                                  ),
                                                                  column(6,
                                                                         h2(strong("Visualizations")),
                                                                         p("Visualizations go here"),
                                                                         plotlyOutput("interactive_plot", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5),
                                                                         #leafletOutput("zoneHan") %>% withSpinner(type = 6, color = "#861F41", size = 1.25),
                                                                         leafletOutput("zoneHan") %>% withSpinner(type = 6, color = "#861F41", size = 1.25),
                                                                         #mapview:::plainViewOutput("zoneHan")
                                                                  )
                                                         ), 
                                                                ),
                                                tabPanel("Land Cover",
                                                         p("", style = "padding-top:10px;"),
                                                          
                                                         column(6,
                                                                h2(strong("What we have so far")),
                                                                p("Used Arc GIS Pro with USDA Cropland-CROS data 
                                                       to map types of cropland cover over Hanover County."),
                                                                p(),
                                                                p("Joined the data from the cropland survey 
                                                       and parcel data in Stata to create a clean data 
                                                       file we will use for maps and graphs in R.")
                                                         ),
                                                         
                                                         fluidRow(style = "margin: 8px;",
                                                                  column(6, 
                                                                         h4(strong("Land Covers")),
                                                                         selectInput(inputId = "crop_type", label = "Select Variable:", width = "100%", choices = c(
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
                                                                         imageOutput("crop_typePNG", width = "400px", height = "400px"),
                                                                         p(),
                                                                         plotlyOutput("landAll", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5),
                                                                         p(),
                                                                         plotlyOutput("landCropONLY", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                                                         
                                                                         
                                                                  )
                                                                  
                                                         ),
                                                         column(6,
                                                                h2(strong("Agricultural History")),
                                                                p("Agriculture is a dominant economic, cultural, 
                                                       and social force in Hanover County. Dating back to 
                                                       the colonial era, tobacco was the dominant cash crop and
                                                       was cultivated starting in the early 17th century and 
                                                       continuing well into the 19th century. However, after the 
                                                       soil become depleted due to overuse and market conditions
                                                       changed. During the turn of the 19th century, the focus shifted 
                                                       to other crops including corn, oats, and wheat, and to livestock 
                                                       farming, including cattle, pigs, sheep, poultry, 
                                                       etc. It is also seen that the landscape diversified 
                                                       agriculturally and many farmers started using different
                                                       crop rotation and soil conservation techniques. Once the 
                                                       railroad was introduced, transportation made it easier for 
                                                       farmers to bring their products to the market and at the same
                                                       time helped facilitate the growth of fruits, vegetables, 
                                                       and dairy farming. As time went on leading to the early 20th
                                                       century there were many advancements in technology regarding 
                                                       agriculture. Manual labor was replaced by machinery and tractors 
                                                       which led to increased productivity and efficiency. Then farmers 
                                                       adopted more modern techniques such as crop cultivation, pest control,
                                                       and irrigation. After multiple wars throughout centuries and
                                                       especially following World War II, much of the agricultural
                                                       landscape was converted to residential and commercial areas
                                                       which prompted suburban development. It is still the case 
                                                       though that currently nearly half of the county is covered by 
                                                       forests and/or a mixture of agriculture-forest land. Despite
                                                       many changes many farms have adapted into niche markets such as 
                                                       organic farming, agri-tourism, and farmers markets including the
                                                       support for initiatives to promote sustainable agriculture and to
                                                       preserve farmland. Currently today, Hanover County’s agricultural
                                                       presence plays a vital role in its economy, heritage, and local culture.  "),
                                                                p()
                                                         )
                                                ) ,
                                                tabPanel("Soil Type",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              column(6,
                                                     h2(strong("Soil Quality Analysis")),
                                                     p("The USDA Natural Resources Conservation Service (NCRS) Web Soil Survey provides a detailed classification of farmland in the United States. 
                                                       The Soil Survey Geographic Database (SSURGO) collects soil data by walking over the land to observe the soil and obtaining soil 
                                                       samples to be analyzed in laboratories. 
                                                       The SSURGO Database uses the data acquired to map various farmland classifications onto specified areas of interest. 
                                                       The USDA NCRS considers these factors when classifying soil: 
                                                       water moisture regimes, soil temperature range, acid-alkali balance, water table, soil sodium content, flooding, erodibility, 
                                                       permeability rate, rock fragment content, and soil rooting depth."),
                                                     p(),
                                                     p("The Web Soil Survey ranks Hanover County’s soil quality by identifying the soil as either prime farmland, farmland of statewide importance, 
                                                       prime farmland if drained, or not prime farmland. The USDA defines prime farmland as “land that has the best combination of physical and chemical 
                                                       characteristics for producing food, feed, forage, fiber, and oilseed crops and is available for these uses.” [1] Prime farmland is the highest ranking 
                                                       and must possess a suitable soil quality to sustainably produce high yields of crops with adequate moisture, water supply, and temperature permissible 
                                                       for crop growing seasons. Therefore, this land cannot be susceptible to erosion or flooding, and must have minimal slope. Farmland of statewide 
                                                       importance, which is the second-best ranking, describes soil that almost meets the nutrient requirements to be classified as prime farmland, 
                                                       but is still able to produce high crop yields once treated with acceptable farming methods, or during favorable conditions. 
                                                       Prime farmland if drained describes good soils located in wetlands or waterways currently covered in water. Not prime farmland is soil considered 
                                                       not productive."),
                                                     p(),
                                                     p("The Web Soil Survey Farmland Classifications were mapped onto Hanover County illustrating the spatial relationships between each classification. 
                                                       Most land falls under the classification “All areas are prime farmland” and are centered towards the eastern end, spanning across 103,063.2 acres. 
                                                       This category possesses the largest number of acres with 34% of the county’s total acreage designated as prime farmland. The classification 
                                                       “Not Prime Farmland” also makes up 34% of the area and has the second largest number of acres with 103,051.1 spanning vastly across the county. 
                                                       Soil classified as “Farmland of Statewide importance” is concentrated towards the northwestern region making up 31.19% of the county, with 94,545.2 acres 
                                                       of land. “Prime Farmland if drained” contains the least number of acres and is in the center of the county encompassing 0.816% of the area, with 2,473 acres."),
                                                     p(),
                                                     h4(strong("What We Have So Far")),
                                                     p(),
                                                     p("Used USDA-NRCS Soil Survey Geographic database to map
                                                       soil types over Hanover County in Arc GIS Pro."),
                                                     p(),
                                                     p("Joined the data from soil survey and parcel data in State
                                                       to create a clean data file we will use for maps and graphs in R.")
                                              )
                                              
                                     ),
                                     fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              column(6,
                                                     h2(strong("Soil Types")),
                                                     p(),
                                                     plotlyOutput("sR", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5),
                                                     p(),
                                                     imageOutput("SoilLimit", width = "400px", height = "400px"),
                                                     
                                                     
                                                    
                                              
                                     ),
                                     column(6,
                                            h2(strong("Soil Quality Leaflet Placeholder Title")),
                                            p(),
                                            #leafletOutput("hansoil") %>% withSpinner(type = 6, color = "#861F41", size = 1.5),
                                            imageOutput("soilRate", width = "400px", height = "400px")
                                           
                                     )
                            )) ,
                                           
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
                                                                         h2(strong("Land Suitability Write Up Goes Here")),
                                                                         p("stuff"),
                                                                         p()
                                                                         
                                                                  ),
                                                                  column(6,
                                                                         h2(strong("Visualizations")),
                                                                         p("Visualizations go here"),
                                                                         plotlyOutput("rateacre", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5),
                                                                         leafletOutput("limitS") %>% withSpinner(type = 6, color = "#861F41", size = 1.5)
                                                                         
                                                                  )
                                                         )
                                                         
                                                ), 
                                                tabPanel("Infastructure Proximity",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         h2(strong("Infrastructure Proximity Write Up Goes Here")),
                                                                         p("This map was created using transmission line location data 
                                                       from the Homeland Infrastructure Foundation Level Database 
                                                       (HIFLD) and a separate dataset distributed by The Office for
                                                       Coastal Management which used HIFLD metadata to map all substations
                                                       within 20 miles of the ocean. The substation metadata set from
                                                       HIFLD is only accessible for federal employees, and the substation
                                                       data we used from The Office for Coastal Management only had half
                                                       of all substations within Hanover County. To map all of the substations
                                                       within the county we used Open Street Map and Google Earth to locate
                                                       the other substation locations and add them to our dataset."),
                                                                         p(),
                                                                         p("Solar farms need to be within close proximity of infrastructure 
                                                       that can distribute the power generated from a farm throughout the
                                                       grid. Connecting a farm directly to a substation is ideal as
                                                       substations already have the majority of necessary technology
                                                       that can increase or decrease the voltage coming from a farm.
                                                       Connecting to transmission lines is also a possibility, but 
                                                       requires the implementation of new voltage regulating machines.
                                                       As the development location moves further away from energy infrastructure, 
                                                       the project becomes more expensive and eventually is not financially
                                                       feasible. A distance rule of thumb is that solar farms should be 
                                                       developed within 2 miles of a substation or 1000 feet of a transmission
                                                       line in order to keep development costs low."),
                                                                         p(),
                                                                         p("The map displayed shows parcels that have land within either 2 
                                                       miles of a substation or 1000 feet of a transmission line. The 
                                                       highlighted parcels are also subsetted to show only lots that are
                                                       zoned for agriculture and are at least 10 acres in size."),
                                                                         p(),
                                                                         p("The second layer displays a map that shows parcels which are
                                                       centered within the most suitable land for solar farm development.
                                                       Using NRCS Web Soil Survey data we were able to map the soil types
                                                       across the county based on the level of limitation that they pose 
                                                       for solar farm development. The index takes into account slope,
                                                       slope aspect, rock fragment content, corrosivity, saturation and shrink-swell properties of the soil.
                                                       The parcels displayed are also subsetted to only show lots that are a minimum of 10 acres. The most 
                                                       suitable land for solar farm development within Hanover County is concentrated on the eastern end of 
                                                       the county where the majority of prime farmland is located."),
                                                                         p()
                                                                  ),
                                                                  column(6,
                                                                         h2(strong("Visualizations")),
                                                                         p("Visualizations go here")
                                                                         
                                                                  )
                                                         ),
                                                         
                                                ), 
                                                
                                                tabPanel("Index",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         h2(strong("Hanover Solar Assessment")),
                                                                         p("Hanover County is currently home to Mechanicsville Solar PV Park. 
                                                       This 28-megawatt solar farm has been in operation since 2018. 
                                                       Developed by SunEnergy1, the park spans 222 acres and consists of 93,000 modules. 
                                                       The electricity generated by the solar farm is being sold to Dominion Energy 
                                                       and has the capacity to power approximately 5,000 households."),
                                                                         p(),
                                                                         p("In addition to the Mechanicsville Solar PV Park, Hanover County has
                                                       approved a new solar farm. Developed by Ameriesco Solar, this 22-acre 
                                                       facility is capable of generating 5 megawatts of power, which is estimated
                                                       to meet the energy needs of 1,500 homes. The farm is located on Peppertown
                                                       road and is expected to have a lifespan of 40 years. As part of their environmental 
                                                       commitment, the developer plans to plant pollinator-friendly vegetation between 
                                                       the solar panels."),
                                                                         p(),
                                                                         h2(strong("Optimal Solar Farm Locations")),
                                                                         p("Solar farms require large areas of space, clear from any obstructions,
                                                       such as trees or buildings that could potentially cast shadows onto the panels.
                                                       This helps to ensure that they have maximum exposure to sunlight
                                                       throughout the day. Having flat land is also preferred for solar farms
                                                       as it simplifies installation, and allows for better panel
                                                       positioning and alignment. The energy produced from the solar 
                                                       farm has to be sent to a substation where it is stored before being
                                                       released onto the grid. Therefore, a solar farm in close proximity to a
                                                       substation is beneficial as it reduces the need to run lines long distances.
                                                       Solar farms will also need to be inspected and maintained throughout their 
                                                       operation, so service road access is very important when determining suitable properties."),
                                                                         p()
                                                                         
                                                                  ),
                                                                  column(6,
                                                                         h2(strong("Visualizations")),
                                                                         p("Visualizations go here"),
                                                                         checkboxGroupInput(inputId = "bufferType", label = h3("Select buffer types: "),
                                                                                            choices = c("Buffer One", "Buffer Two", "Buffer Three")),
                                                                         p(),
                                                                         checkboxInput(inputId = "road", label = "Show road variable on map?",
                                                                                       value = FALSE),
                                                                         p(),
                                                                         radioButtons(inputId = "farmlandType", label = h3("Select farmland type: "),
                                                                                      choices = c("Prime Acre Farmland", "State Acre Farmland", "Good Solar Farm Acre Farmland")),
                                                                         p(),
                                                                         checkboxInput(inputId = "zoning", label = "Show zoning types on map?",
                                                                                       value = FALSE),
                                                                         p(),
                                                                         sliderInput("lotacre", label = h3("Lot Acreage (by GPIN)"),
                                                                                     min = 0,
                                                                                     max = 100000,
                                                                                     value = 50000,
                                                                                     step = 20000)
                                                                  )
                                                                  
                                                         ),
                                                         
                                                ),
                                                tabPanel("Methodology",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(style = "margin: 8px;",
                                                                  align = "center",
                                                                  column(6,
                                                                         h2(strong("Solar Suitability Score")),
                                                                         p("Hanover"),
                                                                         p(),
                                                                         p("In"),
                                                                         p(),
                                                                         h2(strong("Agrivoltaic Viability Rating")),
                                                                         p("Solar"),
                                                                         p()
                                                                         
                                                                  ),
                                                                  column(6,

                                                                         selectInput(
                                                                           "solar.score",
                                                                           "Solar Suitabilty Score",
                                                                           c("Buffer 1" = "buffer_1",
                                                                             "Buffer 2" = "buffer_2",
                                                                             "Buffer 3" = "buffer_3")),
                                                                           selectInput(
                                                                             "solar.score",
                                                                             "Agrivoltaic Viability Rating",
                                                                             c("Buffer 1" = "buffer_1",
                                                                               "Buffer 2" = "buffer_2",
                                                                               "Buffer 3" = "buffer_3")
                                                                         )
                                                                  )
                                                                  
                                                         ),
                                                         
                                                )
                                              ) 
                                     ), 
                            ) ,
                            
                            
                            
                 #),
                 
                 ## Tab Findings --------------------------------------------
                 # tabPanel("Findings & Predictions", value = "conclusion",
                 #          fluidRow(style = "margin: 6px;",
                 #                   h1(strong("Project Findings and Predictions"), align = "center"),
                 #                   p("", style = "padding-top:10px;"),
                 #                   p("Given the rich agricultural histories of the two counties, we are interested in how agricultural land has changed over the last several years.
                 #                     This research uses quantitative tools to understand how some key natural and social factors affect the parcellation and conversion with administrative data and county-level geospatial data."),
                 #                   fluidRow(style = "margin: 6px;", align = "justify",
                 #                            h4(strong("Goochland")),
                 #                            p("In Goochland, agricultural land was converted to residential, mainly single-family residential urban, and suburban.
                 #                              There were also 5 parcels (about 671 acres) of large agricultural lands that have been parcellated into smaller agricultural plots."),
                 #                            p("Parcellation is occurring predominantly in the southeast of Goochland County near Richmond, around the U.S. Routes I64, 250, and 288. This pattern might reflect the urban influence on the county.
                 #                              This pattern might also imply some correlation between parcellation and transportation. On the crop and land type map, those Routes are labeled as “Developed.”
                 #                              High traffic volumes can also be seen along those Routes."),
                 #                            br(),
                 #                            h4(strong("Powhatan")),
                 #                            p("Large amounts of agricultural land were converted to
                 #                              residential-suburban uses during the decade in Powhatan (including recurrences). Parcellation among agricultural land
                 #                              is also noticeable, as 28 parcels (about 5,750 acres) of large agricultural lands have been parcellated
                 #                              into smaller agricultural plots."),
                 #                            p("Parcellation is occurring predominantly in the heart of Powhatan County, around the U.S. Routes 60 and 522.
                 #                              On the east end near Richmond, high parcellation rates are seen along the U.S. Routes 60 and 288 within
                 #                              the county and this might reflect the urban influence on the county. The high parcellation around
                 #                              those Routes might imply some correlation between parcellation and transportation. On the map of crop and land type,
                 #                              those Routes are labeled as “Developed”. High traffic volumes can also be seen along U.S. Routes 60 and 288. Hence the
                 #                              correlation between parcellation and those Routes is also a correlation between parcellation and developed areas (traffic volumes)."),
                 #                            p("There is no obvious sign that poor soil quality can be a driver of land conversion out of agriculture from the maps."),
                 #                            p("In addition to the univariate spatial analysis, we also conducted a statistical analysis that examined the association between land conversion out of
                 #                              agriculture and the characteristics of the land parcel, which include parcel acreage, whether the owner lives in the county, distance to the city of Richmond, the traffic volume and the soil class.
                 #                              The analysis was conducted for Powhatan County only due to data availability. The findings from a logistic regression model show that the probability of converting out of agriculture:
                 #                              decreases as the size of the parcel increases, decreases if the land owner lives in Powhatan, decreases with distance from Richmond. The association with traffic volume shows a U shaped impact
                 #                              on the probability of conversion. Soil quality is not significantly associated with land conversion. Note these are not causal effects. They are associations."),
                 #                   ),
                 #
                 #
                 #
                 #          )),

                 ## Tab Data Sources --------------------------------------------
                 # tabPanel("Data Sources",
                 #          fluidRow(style = "margin: 6px;",
                 #                   h1(strong("Data Sources"), align = "center"),
                 #                   p("", style = "padding-top:10px;"),
                 #                          fluidRow(style = "margin: 6px;", align = "justify",
                 #                                   column(4,
                 #                                   img(src = "data-acs.png", style = "display: inline; float: left;", width = "180px"),
                 #                                   p(strong("American Community Survey"), "The American Community Survey (ACS) is an demographics survey conducted by the U.S Census Bureau. The ACS samples households to compile 1-year and 5-year datasets
                 #                      providing information on social and economic characteristics including employment, education, and income. This project utilizes ACS 2016/2020 5-year
                 #                      estimates to obtain county- and census tract-level data to explore Goochland and Powhatan Counties' resident characteristics.")),
                 #                                   column(4,
                 #                                   img(src = "goochland.jpg", style = "display: inline; float: left;", width = "150px"),
                 #                                   p(strong("Goochland County Administrative Data"), "Goochland County provided us with parcel/property data which allowed us to gain a better understanding of the different land uses and parcellation
                 #                            that has occured over a 5 year period (2018 - 2022). The team used this data to create visualizations, specifically focusing on the distribution and change in land use in the county.")),
                 #                                   column(4,
                 #                                   img(src = "powhatan.jpg", style = "display: inline; float: left;", width = "150px"),
                 #                                   p(strong("Powhatan County Administrative Data"), "Powhatan County provided us with parcel/property data which allowed us to gain a better understanding of the different land uses and parcellation
                 #                            that has occured over a 8 year period (2014 - 2021). The team used this data to create visualizations, specifically focusing on the distribution and change in land use in the county.")),
                 #                          ),
                 #
                 #                          fluidRow(style = "margin: 6px;", align = "justify",
                 #                                   column(4,
                 #                                   img(src = "nass.jpg", style = "display: inline; float: left;", width = "130px"),
                 #                                   p(strong("USDA National Agricultural Statistics Service"), "The National Agricultural Statistics Service (NASS) under the United States Department of Agriculture (USDA) provides statistics on a wide variety
                 #                                    of agricultural topics. This project specifically relies on crop layer data to create maps and to conduct a statistical analysis on the probablity of land use conversion.")),
                 #                                   column(4,
                 #                                   img(src = "ncss.jpg", style = "display: inline; float: left;", width = "150px"),
                 #                          p(strong("USDA National Cooperative Soil Survey"), "The National Cooperative Soil Survey (NCSS) under the USDA provides soil data which was used to generate soil quality maps for both counties.
                 #                            The data was also used for our statistical analysis to predict the occurrence of land use conversion.")),
                 #                          column(4,
                 #                          img(src = "vdot_crop.png", style = "display: inline; float: left;", width = "180px"),
                 #                          p(strong("VDOT Traffic Data"), "The Virginia Department of Transportation (VDOT) is responsible for building, maintaining and operating the state's roads, bridges and tunnels. VDOT also conducts
                 #                          a program where traffic data are gathered from sensors in or along streets and highways and other sources.  This data includes estimates of the average number of vehicles that traveled each segment
                 #                          of road and daily vehicle miles traveled for specific groups of facilities and vehicle types are calculated. This project utilizes VDOT data to create traffic volume and commute maps for both counties."))
                 #                   )),
                 #
                 #          ),
                 #
                 #
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

  
}

shinyApp(ui = ui, server = server)
