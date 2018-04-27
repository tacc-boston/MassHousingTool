# ------------------------------------------------------------------------------------------
#                                  0. Intro (packages & datasets)
# ------------------------------------------------------------------------------------------

# Setting working directory
# setwd("C:/Users/Lauti/Google Drive/Lauti-TACC/Housing/Address_locator_tool/AddressLocatorTool/")
# setwd("C:/Users/bariuser4/Google Drive/Lauti-TACC/Housing/Address_locator_tool/AddressLocatorTool/")
# dirname <- "C:/Users/Lauti/Google Drive/Lauti-TACC/Housing/Address_locator_tool/app3"
# if (!dir.exists(dirname))dir.create(dirname,recursive=TRUE)

# Loading the packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(rvest)
library(DT)
library(shinydashboard)
library(placement)
library(stringr)
library(jsonlite)
library(shinythemes)


### From here on, there are several lines that are setting the ground that will be used in the app.

# Opening files
data.points <- read.csv("data/data_points.csv", stringsAsFactors = F)
data.points$FamPovPer <- round(data.points$FamPovPer*100, 0)
data.points$UnempRate <- round(data.points$UnempRate*100, 0)


#Load the BHA Service Area census tracts
BHASA <- readOGR('data', "bha_sa")

# ZIP code data
zip_coord <- read.csv('data/zip_coord.csv')
zip_coord$ZIP <- paste0("0", zip_coord$ZIP)

# Transit data
Tlines <- readOGR("data", "theT")
commuterLines <- readOGR("data", "commuter_rail")
stations <- readOGR("data", "stations")
Tlinecolors <- c("blue2", "green4", "orange3", "red3", "ivory3")    # Assigning color to the T-Lines
commColors <- "purple4"                                             # Assigning color to the Commuter Rail
Tlinecolor <- colorFactor(Tlinecolors, Tlines$LINE)
commlinecolor <- colorFactor(commColors, commuterLines$COMM_LINE)


# Schools data
schools <- read.csv('data/GreatSchools.csv', stringsAsFactors = F)
schoolspal <- colorBin("Blues", schools$gsRating, 6, pretty = FALSE) # Assigning color to the schools

# API Key
api_key <- #Your Google Maps API
walkscore_api_key <- #Your Walkscore API

# Default view location. It will be use later when we buil the map.
lat <- 42.361145
lng <-  -71.057083
zoom <- 11

# Functions
createLink <- function(URL) {
  paste0('<a href="',
         URL,
         '" target="_blank">Website</a>')
}


# ------------------------------------------------------------------------------------------
#                                  1. User Interface
# ------------------------------------------------------------------------------------------

# Here we will be setting everything related to the User Interface, what the user will end up looking at.
# To create the UI, we'll write every piece inside a "ui" function. Inside, we'll create the different
# columns where we are going to place the visual things.

ui <- shinyUI(fluidPage(
  
  fluidRow(
    
    # Column for the Introduction 
    column(width = 10, offset = 1,
                h4("Introduction"),
                helpText("Welcome to the Address Lookup Tool. Here, you will be able to find information about the
                         surrounding area of the address you are interested in.")
                ),
    
    # Column for the Indicators box
    column(width = 4, offset = 1,
                h4("Select your address"),
                textInput(inputId = "address.text",
                          # value = "2136 Washington St, Boston, MA 02119",
                          value = "Type the address",
                          label = "Address Lookup"),
                actionButton("search", "Search"),
                br(),
                h4("Indicators"),
                valueBoxOutput("COI", width = 6),
                valueBoxOutput("middleincome", width = 6),
                valueBoxOutput("poverty", width = 6),
                valueBoxOutput("unemployment", width = 6),
                valueBoxOutput("walkscore", width = 6),
                valueBoxOutput("transportation.ws", width = 6)
                ),
    
    # Column for the Map
    column(width = 6, offset = 0,
                h4("Map"),
                leafletOutput("leaflet", height = "500")
                ),
    
    column(width = 10, offset = 1,
                helpText(HTML("<br>
<br><p><strong>What is the Address Locator Tool?</strong></p><p>We strongly believe that more and better information leads to a better decision&mdash;especially when it comes to finding a place to live! That is why we created this tool, to help people make better housing decisions. We provide important information from different sources about the social and economic situation of the surrounding area of an address.</p>
                              <p><strong>How does it work?</strong></p>
                              <p>Once you have found a unit that you are interested in, type the address in the &ldquo;Address Lookup&rdquo; box. Try to enter as complete as possible, for example &ldquo;<em>2136 Washington St, Boston, MA 02119</em>&rdquo;. The application is going to use that address and a few seconds later it&rsquo;s going to show some indicators on the left and the address mapped on the right.</p>
                              <p><strong>How to interpret the indicators?</strong></p>
                              <p>We try to provide information to the smallest geographical unit possible, which is at the Census Tract level. We have selected the following indicators for you:</p>
                              <ul>
                              <li><strong>COI Index:</strong> The Child Opportunity Index (COI) is a measure of relative opportunity across all neighborhoods and is created by <a href=\"http://www.diversitydatakids.org/data/childopportunitymap\">diversitydatakids.org - Kirwan Institute</a>. There are five levels: <em>Very Low, Low, Moderate, High </em>and<em> Very High</em>. The higher the score the more opportunity, the more opportunity the area provides for children!</li><br>
                              <li><strong>Median Household Income: </strong>The Median Household Income is based on individuals 15 years old and over with income. This information is from ACS 2016 (5-Year Estimates) data.</li><br>
                              <li><strong>Poverty Index:</strong> The Poverty Index is calculated with ACS 2016 (5-Year Estimates) data and calculates the percentage of families that are below the poverty threshold in the total number of families from that census tract. The higher the score, the more unemployment around the address.</li><br>
                              <li><strong>Unemployment Index:</strong> The Unemployment Index is calculated with ACS 2016 (5-Year Estimates) data and calculates the percentage of people 16 Years and over that are unemployed in that census tract. The highest, the more unemployment around the address.</li><br>
                              <li><strong>Walk Score: </strong>This index is created by walkscore.com and it is number between 0 and 100 that measures the walkability of any address (0 being the lowest value, 100 the highest). If you click on the value, you will have more information about this index.</li><br>
                              <li><strong>Transit Score: </strong>This index is also created by walkscore.com and it is number between 0 and 100 that measures the availability of public transportation around any address (0 being the lowest value, 100 the highest). If you click on the value, you will have more information about this index.</li>
                              </ul>")))
    )
  )
  
  )




# ------------------------------------------------------------------------------------------
#                                  2. Server Interface
# ------------------------------------------------------------------------------------------

# Here we will be building the real application, the real muscle of the app. To create the 
# "Server", we'll write every piece inside a "server" function. 

server <- (function(input, output, session) {
  
  ## -------------- REACTIVE FUNCTIONS -------------- ##
  
  ### Working with the address 
  
  # This funtion has the following purpose: once the user provides the address that it's looking for,
  # this function is going to geocode that address to obtain the lat/long and the is going to use that
  # lat/long to search the Census Tract ID. Once that we have that, we merge the census tract ID with
  # the other information that we will show.
  
  address.input <- eventReactive(input$search, {
    
    # Using the address as an input for getting our lat and long
    add <- as.character(input$address.text)
    
    # For checking purposes only
    # address <- "10 Nevada Ave Malden, Massachusetts"
    # address <- "24 woodbury st, Arlington, ma 02476"
    # add <- as.character(address)
    
    # Geocoding the address
    dat <- geocode_url(add, auth="standard_api", privkey= api_key,
                       clean=TRUE, add_date='today', verbose=TRUE)
    
    # Obtaining the lat/long
    add.1 <- dat$formatted_address
    lng <- dat$lng
    lat <- dat$lat
    
    # Creating the URL to look for the Census Tract ID from the FCC API.
    url <- paste0(
      "https://geo.fcc.gov/api/census/block/find?latitude=", lat,
      "&longitude=", lng, "&showall=true&format=xml")
    
    
    geoid <- read_xml(url) %>% 
      str_extract("[0-9]{11}") %>% 
      as.numeric()
    
    ws_url <- paste0("http://api.walkscore.com/score?format=json&lat=", lat,
                     "&lon=", lng, "&transit=1&bike=1&wsapikey=", walkscore_api_key)
    
    ws_score <- fromJSON(txt=ws_url)
    ws_score_walk <- ifelse(!is.null(ws_score$walkscore), ws_score$walkscore, NA)
    ws_score_transit <- ifelse(!is.null(ws_score$transit$score), ws_score$transit$score, NA)
    
    # For debugging purposes only
    # return(paste("Address = ", add, "<br>",
    #              "Long = ", lng, 
    #              "Lat = ", lat,
    #              "Census Tract =", census.tract.id))
    
    dat.1 <- data.frame(add.1, lng, lat, geoid, ws_score_walk, ws_score_transit,
                        stringsAsFactors = F)
    
    
    
    # Merging with different datastets
    dat.1 <- merge(dat.1, data.points, by.x = "geoid", by.y ="CT_ID_10")
    
    dat.1
    
  })
  
  # This function here waits for the output of the "address.input" function. The output is going to
  # be a marker in the map with the address that the user provided.
  observeEvent(input$search, {
    
    tomap <- address.input()[complete.cases(address.input()[,2:3]), ]
    if (nrow(tomap) > 0) {
      leafletProxy("leaflet") %>%
        clearMarkerClusters() %>%
        clearMarkers() %>%
        addMarkers(data = tomap,
                   ~lng, ~lat) %>%
        fitBounds(min(tomap$lng), min(tomap$lat), max(tomap$lng), max(tomap$lat))
    } else {
      leafletProxy("leaflet") %>%
        clearMarkerClusters() %>%
        clearMarkers()
    }
  })
  
  # With the code below we add information to the map.
  # To be more precise, we are adding the shapefiles for the following
  # * T-lines (Subway lines in Boston Area)
  # * Commuter Rail lines
  # * Location of the stations for the T and Commuter Rail
  # * Location of Public Schools
  
  observe({
    
    leafletProxy("leaflet") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolylines(data = commuterLines,
                   popup = paste0(as.character(commuterLines$COMM_LINE)), 
                   color = commlinecolor(commuterLines$COMM_LINE), opacity = .8,
                   group = "Transit") %>% 
      addPolylines(data = Tlines,
                   popup = as.character(Tlines$ROUTE), 
                   color = Tlinecolor(Tlines$LINE), opacity = .8,
                   group = "Transit") %>% 
      addCircles(data = stations, color = "white", opacity = 1, radius = 7, weight = 3, 
                 popup = paste(as.character(stations$STATION), "STATION"),
                 group = "Transit") %>%
      addCircles(data = schools, color = ~schoolspal(gsRating), radius = 20, opacity = 1,
                 popup = ~paste0("<strong>", name, "</strong></br>",
                                 "Grade range: ", gradeRange, "</br>",
                                 "Great Schools Rating: ", gsRating, "</br>",
                                 createLink(paste0("https://www.greatschools.org/", profileUrl))
                 ),
                 group = "Public Schools") %>%
      addLayersControl(
        position = 'bottomleft',
        #baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Stamen.TonerLite"),
        overlayGroups = c("Transit", "Public Schools"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup("Public Schools")
    
  })
  
  
  
  ## -------------- RENDERING THINGS -------------- ##
  
  # With this code we will be rendering the map
  output$leaflet <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = tileOptions(minZoom=10, maxZoom=16)) %>% 
      setView(lat = lat,
              lng = lng,
              zoom = zoom) 
  })
  
  # Here we'll printout the COI index for the CT that comes out from the address look-up
  output$COI <- renderValueBox({
    valueBox(
      address.input()$COI.ALLCOMPLEV, 
      "Child Opportunity Index", 
      color = ifelse(address.input()$COI.ALLCOMPLEV == "Very Low" | address.input()$COI.ALLCOMPLEV == "Low", "red", 
                     ifelse(address.input()$COI.ALLCOMPLEV == "Moderate", "yellow", 
                            "green"))
    )
  })
  # Here we'll printout the Poverty index for the CT that comes out from the address look-up
  output$poverty <- renderValueBox({
    valueBox(
      paste0(address.input()$FamPovPer, "%"), "Poverty Rate", 
      color = ifelse(address.input()$FamPovPer >= 0.15, "red", 
                     ifelse(address.input()$FamPovPer <= 0.02, "green", 
                            "yellow"))
    )
  })
  
  # Unemployment
  output$unemployment <- renderValueBox({
    valueBox(
      paste0(address.input()$UnempRate, "%"), "Unemployment Rate", 
      color = ifelse(address.input()$UnempRate >= 0.15, "red", 
                     ifelse(address.input()$UnempRate <= 0.05, "green", 
                            "yellow"))
    )
  })
  
  # Middle Household Income
  output$middleincome <- renderValueBox({
    valueBox(
      paste0("$", format(round(as.numeric(address.input()$MedHouseIncome), 0), big.mark=",")), "Median Househould Income", 
      color = ifelse(address.input()$MedHouseIncome <= 51431, "red", 
                     ifelse(address.input()$MedHouseIncome >= 94604, "green", 
                            "yellow"))
    )
  })
  
  # Walk Score from Walk Score
  output$walkscore <- renderValueBox({
    valueBox(
      value = paste0(address.input()$ws_score_walk),
      # icon = icon("registration-mark", lib = "glyphicon"),
      # subtitle = HTML("<a href=\"https://www.walkscore.com/how-it-works/\"><b>Walk Score</b></a>"),
      subtitle = HTML("<p>Walk Score <sup>&reg;</sup> </p>"),
      href = "https://www.walkscore.com/how-it-works/",
      color = ifelse(address.input()$ws_score_walk <= 49 | is.na(address.input()$ws_score_walk), "red", 
                     ifelse(address.input()$ws_score_walk >= 70, "green", 
                            "yellow"))
    )
  })
  
  # Transportation Score from Walk Score
  output$transportation.ws <- renderValueBox({
    valueBox(
      value = paste0(address.input()$ws_score_transit),
      # icon = icon("registration-mark", lib = "glyphicon"),
      # subtitle = HTML("<a href=\"https://www.walkscore.com/how-it-works/\"><b>Walk Score</b></a>"),
      subtitle = HTML("<p>Transit Score <sup>&reg;</sup> </p>"),
      href = "https://www.walkscore.com/how-it-works/",
      color = ifelse(address.input()$ws_score_transit <= 49 | is.na(address.input()$ws_score_transit), "red", 
                     ifelse(address.input()$ws_score_transit >= 70, "green", 
                            "yellow"))
    )
  })
  
  
})

# ------------------------------------------------------------------------------------------
#                                  3. Final code
# ------------------------------------------------------------------------------------------

# This line is going to compile the User Interface chunk and the Server Interface and make the app work

shinyApp(ui, server)
