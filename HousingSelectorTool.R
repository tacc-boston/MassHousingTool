




# ------------------------------------------------------------------------------------------
#                                  0. Intro (packages & datasets)
# ------------------------------------------------------------------------------------------

# Loading the packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(rvest)
library(DT)

### From here on, there are several lines that are setting the ground that will be used in the app. 

#options(shiny.trace=TRUE)

#Load the BHA Service Area census tracts
BHASA <- readOGR('data', "bha_sa")

#Payment standards (skipping SRO column)
PayStd <- read.csv('data/paystd_by_br_size.csv')[-2]

#BHA PMSA by ZIP
PMSA <- read.csv('data/pmsa_by_zip.csv')

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


# Correctly Order COI first, which is already a factor
myfactors <- c("Very Low", "Low", "Moderate", "High", "Very High")

#Color palette for the choropleths
colorpal <- colorFactor("Greens", myfactors, ordered = T, na.color = "#bdbdbd")

# Function to scale range from 0 to 1 when plotting choropleth values
normalized <- function(x) (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))


# Default view location. It will be use later when we buil the map.
lat <- 42.361145
lng <-  -71.057083
zoom <- 11

# Craigslist apartment search base url, and default search radius (miles)
base_url <- "http://boston.craigslist.org"
radius <- 1

# Turn query errors into NAs
na_error <- function(expr) {
  try_it <- try(expr, silent = TRUE)
  if (class(try_it) == "try-error") {
    return(NA)
  }
  else {
    return(try_it)
  }
}

# Function: Make URLS clickable.
#   This function takes as:
#     Input: the URL from the Craiglist add
#     Output: a link that will take the user from the App to the Craiglist add

createLink <- function(URL) {
  paste0('<a href="',
         URL,
         '" target="_blank" class="btn btn-primary">Check listing</a>')
}


# Create HTML for the map marker popups
popup_content <- function(Title, Price, Bedrooms, sqft, URL) {
  paste0(
    "<b>",
    Title,
    "</b>",
    "<br/>",
    "Rent: ",
    Price,
    "<br/>",
    "Bedrooms: ",
    Bedrooms,
    "<br/>",
    "SqFt: ",
    sqft,
    "<br/>",
    "<b><a href='",
    URL,
    "' target='_blank'>Full details</a></b>"
  )
}

# Priority modifiers - it's going to be used as a parameter for the upcoming steps.
choices <- list("High" = 2,
                "Neutral" = 1,
                "Low" = .5)

# ------------------------------------------------------------------------------------------
#                                  1. User Interface
# ------------------------------------------------------------------------------------------

# Here we will be setting everything related to the User Interface, what the user will end up looking at.
# To create the UI, we'll write every piece inside a "ui" function. Inside, we'll create the different
# columns where we are going to place the visual things.

ui <- shinyUI(fluidPage(
  
  # # With this code we will create the top box where the title of the app is going to be.
  # fluidRow(column(
  #   7, offset = 1,
  #   br(),
  #   div(h2(
  #     "Housing Selection Guidance Tool v.1", align = "left"
  #   ),
  #   style = "color:black"),
  #   br()
  # )),
  
  # This code will create a box below the Title and will include two different columns inside:
  # one for the map and another one for the filters and selection boxes.
  fluidRow(
    
    # Column for the MAP
    column(
      7,
      offset = 1,
      leafletOutput("map", height = "700"),
      #br(),
      #actionButton("reset_button", "Reset view"),
      br()
    ),
    
    # Column for the filters and selectors
    column(
      3,
      wellPanel(
        h3("Housing requirements"),
        
        # With the code below we will create two tabs for the column of Housing requirements
        tabsetPanel(id = "tabs",
          
          # This panel is for "Size and Rent"
          tabPanel("Size and rent", 
                   uiOutput("rent", align = "left"),
                   uiOutput("bedrooms", align = "left")), 
          
          # This panel is for "Payment Standard"
          tabPanel("Payment Standard", uiOutput("paystd"), align = "left")),
        h6("Zoom in on the map in the area you are looking for before searching for apartments"),
        
        # With this code we will create a button that will trigger a search in the appartments
        actionButton("search", "Search for apartments")
      ),
      br(),                    # This is creating a line break
      h3("Location priorities"),
      
      # The next four lines will create the button selection 
      uiOutput("COI", align = "left"),                  
      uiOutput("school", align = "left"),
      uiOutput("safety", align = "left"),
      uiOutput("transport", align = "left"),
      br()
    )
  ),
  
  # The final code for the UI is a list of all the results from the search. It will appear when you run
  # the app.
  fluidRow(column(10, offset = 1,
                  conditionalPanel(
                    condition = "output.nresults > 0",
                    downloadButton('downloadData', 'Download listings')
                  ),
                  conditionalPanel(
                    condition = "input.search",
                    h3("Results:"),
                    dataTableOutput("listingsTable")
                  )))
))


# ------------------------------------------------------------------------------------------
#                                  2. Server Interface
# ------------------------------------------------------------------------------------------

# Here we will be building the real application, the real muscle of the app. To create the 
# "Server", we'll write every piece inside a "server" function. 

server <- (function(input, output, session) {
  
  # Code related to the "Size and Rent" selection. 
  output$rent <- renderUI({
    sliderInput(
      "rent",
      label = "Rent range:",
      min = 0,
      max = 5000,
      value = c(1000, 2000),
      step = 50
    )
  })
  
  # Code related to the "Payment Standard" selection. 
  output$paystd <- renderUI({
    sliderInput(
      "paystd",
      label = "Payment Standard:",
      min = 0,
      max = 6,
      value = 2,
      step = 1,
      sep = " "
    )
  })
  
  # Code related to the "Bedroom" selection. 
  output$bedrooms <- renderUI({
    sliderInput(
      "bedrooms",
      "Bedrooms (minimum):",
      min = 0,
      max = 6,
      value = 2,
      step = 1,
      #post = " or +",
      sep = " "
    )
  })
  
  
  choices = list("Low" = .5,
                 "Neutral" = 1,
                 "High" = 2)
  
  output$COI <- renderUI({
    radioButtons(
      "xCOI",
      "Child exposure to opportunity",
      choices,
      selected = 1,
      inline = TRUE
    )
  })
  
  output$school <- renderUI({
    radioButtons("xschool",
                 "School quality",
                 choices,
                 selected = 1,
                 inline = TRUE)
  })
  
  output$safety <- renderUI({
    radioButtons("xsafety",
                 "Safety",
                 choices,
                 selected = 1,
                 inline = TRUE)
  })
  
  output$transport <- renderUI({
    radioButtons("xtrans",
                 "Transport cost",
                 choices,
                 selected = 1,
                 inline = TRUE)
  })
  
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("Esri.NatGeoWorldMap",
                       options = tileOptions(minZoom=10, maxZoom=16)) %>% 
      setView(lat = lat,
              lng = lng,
              zoom = zoom) 
  })
  #CartoDB.Positron -> this was the original map
  
  # This function is going to define the ZIPCODE that the app will look for in Craiglist. It's a 
  # reactive function that takes the map bounds (the ones that the user is currently looking at)
  # and calculates what's the closest ZIPCODE based on that. So, for example, if the user is
  # looking at Quincy in the map, this function is going to calculate the corresponding ZIPCODE
  # of Quincy.
  
  centeredZIP <- reactive({
    if (is.null(input$map_bounds))
      return(NULL)
    bounds <- input$map_bounds
    center <- c(mean(c(bounds$east, bounds$west)),
                mean(c(bounds$north, bounds$south)))
    
    nearest.zip <- zip_coord[which.min(colSums((t(zip_coord[-1]) - center)^2)),1]
    nearest.zip
    
  })
  
  observe({
    input$xschool
    input$xtrans
    input$xCOI
    input$xsafety
  })
  
  
  # Reactive function based on the user selections of the 
  greenerIndex <- reactive({
    
    req(input$xCOI, input$xschool, input$xsafety, input$xtrans)
    
    GI <- as.integer(input$xCOI) * BHASA@data$COI +
      as.integer(input$xschool) * BHASA@data$School_Per +
      as.integer(input$xsafety) * BHASA@data$Safety +
      as.integer(input$xtrans) * BHASA@data$Transport_
    
    normalized(GI) %>%
      cut(5, labels = myfactors)
  })
  
  
  foundListings <- eventReactive(input$search, {
    zipcode <- centeredZIP()
    if (input$tabs == "Size and rent") {
      min_price <- input$rent[1]
      max_price <- input$rent[2]
      min_bedrooms <- input$bedrooms
    } else {
      search_PMSA <- PMSA[PMSA$POSTCODE == as.integer(zipcode),2]
      min_price <- 500
      #max_price <- 3500
      max_price <- PayStd[PayStd$PMSA == search_PMSA, input$paystd +2] 
      min_bedrooms <- input$paystd
    }
    
    # This function is going to build the URL that will be used to scraped the data from Craiglist.
    # It takes as INPUT many parameters that we defined in early stages.
    # The OUTPUT is going to the URL. 
    query <- function(base_url, radius, zipcode, min_price, max_price, min_bedrooms) {
      return(paste0(base_url,
                    "/search/aap?",
                    "search_distance=", radius, 
                    "&postal=", zipcode,
                    "&min_price=", min_price,
                    "&max_price=", max_price,
                    "&bedrooms=", min_bedrooms))
    }
    

    # This line is going to "called" the previous function, so we have each URL that we need.
    search_url <- query(base_url, radius, zipcode, min_price, max_price, min_bedrooms)
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = 'Searching for apartments\n',
                 detail = 'Please be patient, it takes a while...',
                 value = 0)
    
    listings <- read_html(search_url) %>%
      html_nodes(css = ".result-row") 
    
    if (length(listings) > 0) {
      result_names <- c("titles",
                        "prices",
                        "dates",
                        "urls",
                        "locales",
                        "beds",
                        "sqfts",
                        "longs",
                        "lats")
      
      
      
      for (i in seq_along(result_names)) {
        assign(result_names[i], vector())
      }
      
      
      for (i in seq_along(listings)) {
        title <- listings[i] %>% html_node(".hdrlnk") %>%
          html_text()
        price <- na_error({
          listings[i] %>% html_node(".result-price") %>%
            html_text() %>% stringr::str_extract("[0-9]+") %>%
            as.numeric()
        })
        date <- na_error({
          listings[i] %>% html_node(".result-date") %>%
            html_attr("datetime")
        })
        # url <- na_error({
        #   listings[i] %>% html_node("a") %>% html_attr("href") %>%
        #     paste0(base_url, .)
        # })
        url <- na_error({listings[i] %>% html_node("a") %>% html_attr("href")
        })
        
        locale <- na_error({
          listings[i] %>% html_node(".result-hood") %>%
            html_text() %>% gsub("[ \\(\\)]", "", .)
        })
        size_beds <- listings[i] %>% html_node(".housing") %>%
          html_text() %>% gsub(" |\\n", "", .) %>% strsplit("-")
        bed <- na_error({
          size_beds[[1]][1] %>% stringr::str_extract("[0-9]*br") %>%
            stringr::str_replace("br", "") %>% as.integer()
        })
        sqft <- na_error({
          size_beds[[1]][2] %>% stringr::str_extract("[0-9]*ft") %>%
            stringr::str_replace("ft", "") %>% as.integer()
        })
        coords <- read_html(url) %>% html_node(xpath = '//*[@id="map"]')
        long <-  html_attr(coords, "data-longitude") %>% as.numeric()
        lat <- html_attr(coords, "data-latitude")  %>% as.numeric()
        
        titles <- c(titles, title)
        prices <- c(prices, price)
        dates <- c(dates, date)
        urls <- c(urls, url)
        locales <- c(locales, locale)
        beds <- c(beds, bed)
        sqfts <- c(sqfts, sqft)
        longs <- c(longs, long)
        lats <- c(lats, lat)
      }
      
      listingsdf <- data.frame(
        Title = titles,
        Date = dates,
        Price = prices,
        Bedrooms = beds,
        SqFt = sqfts,
        Location = locales,
        long = longs,
        lat = lats,
        URL = urls,
        stringsAsFactors = F
      )
      
      ## 1. Remove duplicates.
      
      # We define duplicates as listings with 
      # * The exact same title
      # * The same asking price
      # * The same number of bedrooms

      key <- c("Title", "Price", "Bedrooms") 
      listingsdf <- listingsdf[!duplicated(listingsdf[key]),]
      
      ## 2. Remove shared apartment listings
      
      # Shared apartments and roommate requests were filtered out if their titles matched any of these patterns (case insensitive):
      # * Beginning with "Room"
      # * Beginning with "Shared"
      # * Containing "roommate" anywhere
      # * Containing "private room" anywhere
      # * Containing "room for rent" anywhere
      
      shared <- grep("^room| roommate|^shared|private room|[ .]room for rent|Sub Lease", listingsdf$Title, ignore.case = T)
      
      if(as.numeric(length(shared)) > 0){
        listingsdf <- listingsdf[-shared, ]
      } else {
        listingsdf <- listingsdf
      } 
      
      
    } else {
      # Create a Progress object
      no.res.message <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(no.res.message$close())
      
      no.res.message$set(message = 'NO APARTMENTS FOUND!',
                   detail = 'Please try other areas or preferences',
                   value = 0)
      Sys.sleep(3)
      
      listingsdf <- data.frame(
        Title = "NO RESULTS FOUND",
        Date = NA,
        Price = NA,
        Bedrooms = NA,
        SqFt = NA,
        Location = NA,
        long = NA,
        lat = NA,
        URL = NA
      )
    }# Empty results set
    
    listingsdf
  })
  
  output$listingsTable <- renderDataTable(cbind(foundListings()[1:6],
                                                Link = createLink(foundListings()$URL)),
                                          escape = FALSE,
                                          extensions = c("Buttons"),
                                          options = list(dom = 'Bfrtip', 
                                                         buttons = c('excel', 'pdf', 'print'))
                                          )
  


  output$downloadData <- downloadHandler(
    filename = function() { paste(input$bedrooms, 'bedrooms.csv', sep = '') },
    content = function(file) {
      write.csv(foundListings()[-(7:8)], file)
    }
  )
  
  # With the code below we add information to the map. To be more precise, we are adding the 
  # shapefiles for the following:
  # * T-lines (Subway lines in Boston Area)
  # * Commuter Rail lines
  # * Location of the stations for the T and Commuter Rail
  # * Location of Public Schools
  
  observe({
    leafletProxy("map", data = BHASA) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = colorpal(greenerIndex()),
        fillOpacity = 0.7,
        color = "grey40",
        weight = 1
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorpal,
        values = greenerIndex(),
        title = "Priority matching"
      ) %>%
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
                                     "Rating: ", gsRating),
                       group = "Public Schools") %>%
      addLayersControl(
        position = 'bottomleft',
        #baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Stamen.TonerLite"),
        overlayGroups = c("Transit", "Public Schools"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup("Public Schools")
    
  })
  
  observe({
    tomap <- foundListings()[complete.cases(foundListings()[7:8]), ]
    if (nrow(tomap) > 0) {
      leafletProxy("map") %>% 
        clearMarkerClusters() %>% 
        clearMarkers() %>%   
        addMarkers(data = tomap, 
                   ~long, ~lat, popup = ~popup_content(Title,
                                                       Price,
                                                       Bedrooms,
                                                       SqFt,
                                                       URL), 
                   clusterOptions = markerClusterOptions()) %>% 
        fitBounds(min(tomap$long), min(tomap$lat), max(tomap$long), max(tomap$lat)) 
    } else {
      leafletProxy("map") %>% 
        clearMarkerClusters() %>% 
        clearMarkers()
    }
  })

  
})

# ------------------------------------------------------------------------------------------
#                                  3. Final code
# ------------------------------------------------------------------------------------------

# This line is going to compile the User Interface chunk and the Server Interface and make the app work
shinyApp(ui, server)