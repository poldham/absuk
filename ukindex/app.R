library(shiny)
library(tidyverse)
library(leaflet)

# to do, add link to company website where available using the bame
# to do, add colours for the sectors (based on the number of entities involved)
# to do, add ggplot summaries based on the species occurrence table? Make sure they match up. 

data <- read.csv("ukindex1.csv", stringsAsFactors = FALSE)
uk_bounds <- read.csv("uk_bounds.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
   
  titlePanel("UK Company Index for ABS"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("ukindex_out"),
      br(),
      h3("Using the Map"),
      p("This map is interactive. Choose a sector and then click on a point of interest to look up the organization on Google, at Companies House or in the esp@cenet patent database."),
      # br(),
      # downloadButton("downloadData", "Excel Download"),
      # br(),
      # br(),
      p("Download the notes", a("here.", href="https://www.google.co.uk/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwj51J7R5bXSAhWpBcAKHR_-C0kQFgghMAA&url=http%3A%2F%2Frandd.defra.gov.uk%2FDocument.aspx%3FDocument%3D13485_WC1085NagoyaABSUKCompanyIndexOverview.pdf&usg=AFQjCNH6BorZWhOOJ5e_H4dF2ggvKul7Gg", target = "_blank")),
      br(),
      h3("About"),
      p("This site provides a map of UK companies and organisations involved in research and development on genetic resources in the period to 2013. The index was prepared for DEFRA by", a("One World Analytics", href="http://www.oneworldanalytics.com/", target = "_blank"), "in preparation for UK ratification of the", a("Nagoya Protocol", href="https://www.cbd.int/abs/about/", target = "_blank"), "under the", a("United Nations Convention on Biological Diversity.", href="https://www.cbd.int/", target = "_blank"), "In particular, the index was used in the 2015 consultation exercise described", a("here", href = "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/415474/nagoya-consult-sum-resp.pdf", target = "_blank")),
      p("The index is based on a review of UK international patent activity that is available for download", a("here.", href="https://www.google.co.uk/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&cad=rja&uact=8&ved=0ahUKEwjR7IucrpnPAhVHIMAKHatqAg0QFggqMAE&url=http%3A%2F%2Frandd.defra.gov.uk%2FDocument.aspx%3FDocument%3D13500_WC1068UKIPGR_Final_2015.pdf&usg=AFQjCNFpFtytdJuMSANlu1PtZ8jK_kZaEQ", target = "_blank"), "The index is not expected to be complete, may contain some noisy records, and is not being actively maintained. The site does not use cookies.")
      
    ),
    mainPanel(width = 8,
      tabsetPanel(type = "tabs", 
        tabPanel("Map", leafletOutput("mymap")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  output$ukindex_out <- renderUI({
    selectInput(inputId = "sector_input", "sector",
      sort(unique(data$sector)),
      selected = "Academic")
  })
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      addTiles() %>% fitBounds(lng1 = uk_bounds$west, lat1 = uk_bounds$north, lng2 = uk_bounds$east, 
        lat2 = uk_bounds$south)
    #%>% ADD BOUNDS FOR UK HERE
      # addCircleMarkers(lng = longditude, lat = latitude, popup = data$company_name, radius = 1, weight = 2, opacity = 0.5, fill= TRUE, fillOpacity = 0.2)
  })
   
  # Use leafletProxy for elements that change
  observe({
    set <- data %>% 
      filter(data$sector %in% input$sector_input)

  leafletProxy("mymap") %>% clearMarkers() %>% 
    addCircleMarkers(lng = set$longitude, lat = set$latitude, popup = set$combined_label, radius = 1, weight = 2, opacity = 0.5, fill= TRUE, fillOpacity = 0.2)
  })
  output$table <- renderTable({
    table <- data %>%
      filter(data$sector %in% input$sector_input)
    })
   
}


# Run the application 
shinyApp(ui = ui, server = server)

