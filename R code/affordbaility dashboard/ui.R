library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(viridis)
library(DT)

# Load necessary data
data_rates <- readRDS("data/crosswalk_rate_ids.rds")
municipalities <- unique(data_rates$rate_structure)

# Define footer function
createFooter <- function() {
  tags$footer(
    class = "footer",
    div(class = "footer-container",
        div(class = "footer-content",
            div(class = "footer-logo-info",
                div(class = "footer-logo-section",
                    img(src = "logo.png", height = "30px", width = "330px")
                ),
                div(class = "footer-info",
                    div(class = "footer-address",
                        tags$i(class = "fas fa-map-marker-alt"),
                        " Knapp-Sanders Building",
                        tags$br(),
                        "Campus Box 3330, UNC Chapel Hill",
                        tags$br(),
                        "Chapel Hill, NC 27599-3330"
                    ),
                    div(class = "footer-contact",
                        tags$i(class = "fas fa-phone"), "T:919 966 5381 F:919 843 2528"
                    ),
                    div(class = "footer-contact",
                        tags$i(class = "fas fa-envelope"), " efc@sog.unc.edu"
                    )
                )
            ),
            div(class = "footer-follow",
                h4("Follow us"),
                div(class = "footer-social-icons",
                    a(href="https://www.facebook.com/UNCEnvironmentalFinanceCenter", img(src="facebook.png", alt="Facebook")),
                    a(href="https://twitter.com/efcatunc", img(src="x.png", alt="X (Twitter)")),
                    a(href="https://www.youtube.com/user/efcunc", img(src="youtube.png", alt="YouTube")),
                    a(href="https://www.linkedin.com/company/unc-environmental-finance-center", img(src="linkedin.png", alt="LinkedIn"))
                )
            ),
            div(class = "footer-links",
                h4("Quick Links"),
                tags$ul(
                  tags$li(a(href="https://efc.sog.unc.edu/efc-events/", "Event")),
                  tags$li(a(href="https://efc.sog.unc.edu/about/", "About")),
                  tags$li(a(href="https://efc.sog.unc.edu/blog/",  "Blog")),
                  tags$li(a(href="https://efc.sog.unc.edu/resource/", "Resource"))
                )
            )
        )
    )
  )
}

# Dashboard description
dashboard_description <- HTML("Welcome to the Water and Wastewater Affordability Assessment Tool Dashboard. This tool helps utility managers evaluate the impact of water rates on customers across different income levels. Features include:<br><br>1. Easy input of location and rates<br>2. Display of key community economic indicators<br>3. Comparison of rate impacts on various income groups<br><br>Explore now to develop equitable water pricing policies that balance financial sustainability with community affordability.")

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Arsenal&display=swap", rel = "stylesheet"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Bebas+Neue&display=swap", rel = "stylesheet"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Kanit:wght@400;700&display=swap", rel="stylesheet"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Anton&display=swap", rel="stylesheet")
  ),
  
  # Custom header
  div(class = "custom-header",
      img(src = "logo_white.png", height="30px"),
      div(class = "header-menu",
          tags$ul(
            tags$li(a(href = "#", onclick = "Shiny.setInputValue('nav', 'home')", "Home")),
            tags$li(a(href = "#", onclick = "Shiny.setInputValue('nav', 'dashboard')", "Dashboard")),
            tags$li(a(href = "#", onclick = "Shiny.setInputValue('nav', 'table')", "Table")),
            tags$li(a(href = "#", onclick = "Shiny.setInputValue('nav', 'plot')", "Plot")),
            tags$li(a(href = "#", onclick = "Shiny.setInputValue('nav', 'learnmore')", "Learn More"))
          )
      )
  ),
  
  # Main content
  div(class = "main-content",
      uiOutput("page_content")
  ),
  
  # Footer
  createFooter()
)

# Server
server <- function(input, output, session) {
  
  # Render page content based on navigation
  output$page_content <- renderUI({
    nav <- ifelse(is.null(input$nav), "home", input$nav)
    switch(nav,
           "home" = homeUI(),
           "dashboard" = dashboardUI(),
           "table" = tableUI(),
           "plot" = plotUI(),
           "learnmore" = learnMoreUI(),
           homeUI()
    )
  })
  
  # Home page UI
  homeUI <- function() {
    div(class = "home-background",
        div(class = "ui grid",
            div(class = "home-content",
                h1("AFFORDABILITY TOOL", class = "big-heading"),
                p("Reach your goals with personalized insights, custom budgets, spend tracking, and subscription monitoring—all for free.", class = "normal-text"),
                div(class = "button-container",
                    tags$a(href = "#", onclick = "Shiny.setInputValue('nav', 'dashboard')", class = "ui button custom-button", "View Dashboard"),
                    tags$a(href = "https://efc.sog.unc.edu/", class = "ui button custom-button", "View our Website")
                )
            ),
            div(class = "sustainability-text",
                h2("We can help you achieve water sustainability!")
            )
        )
    )
  }
  
  # Dashboard page UI
  dashboardUI <- function() {
    div(
      h2("Dashboard Overview", class = "ui dashheader"),
      p(class = "dashboard-description", dashboard_description),
      fluidRow(
        column(width = 4,
               div(class = "box",
                   div(class = "box-header", "Select Municipality"),
                   div(class = "box-body",
                       selectInput("municipality", "Choose a municipality:", choices = c("Select a municipality" = "", municipalities), selected = ""),
                       actionButton("reset_municipality", "Reset Selection")
                   )
               )
        ),
        column(width = 8,
               div(class = "box",
                   div(class = "box-header", "Geographical Distribution"),
                   div(class = "box-body",
                       leafletOutput("map")
                   )
               )
        )
      )
    )
  }

  # Plot page UI
  plotUI <- function() {
    div(
      h2("Iris Data Visualization", class = "ui dashheader"),
      p(class = "dashboard-description", "This plot visualizes the relationship between Sepal Length and Sepal Width in the Iris dataset."),
      plotOutput("irisPlot"),
      br(),
      selectInput("color_var", "Color by:", choices = c("Species", "Petal.Length", "Petal.Width"))
    )
  }
  
  # Table page UI
  tableUI <- function() {
    div(
      h2("Iris Data Table", class = "ui dashheader"),
      p(class = "dashboard-description", "This table shows the Iris dataset."),
      dataTableOutput("irisTable")
    )
  }
    
  # Learn more page UI
  learnMoreUI <- function() {
    div(class = "learn-more-container",
        # Section for Resources on Affordability Metrics
        div(class = "resources-container",
            fluidRow(
              column(width = 6,
                     h2("Resources on Affordability Metrics"),
                     tags$ul(style = "list-style-type: none; padding-left: 0;",
                             tags$li(class = "article-item",
                                     div(style = "display: flex; align-items: center; cursor: pointer;",
                                         tags$span(class = "toggle-icon", HTML("&#9656;"), style = "margin-right: 8px;"),
                                         tags$a(id = "link1", href = "https://efc.web.unc.edu/2013/01/09/percent-mhi-indicator-of-affordability-of-residential-rates-using-the-u-s-census-bureaus-median-household-income-data/",
                                                "Percent MHI as an Indicator of Affordability of Residential Rates: Using the U.S. Census Bureau's Median Household Income Data", class = "title-link")
                                     ),
                                     div(id = "content1", style = "display: none; margin-left: 28px;",
                                         p("Shadi Eskaf is a senior project director for the Environmental Finance Center at the University of North Carolina at Chapel Hill."),
                                         p('What is the [national/state/recommended] threshold of affordable rates? Is it 2.5 percent MHI?'),
                                         p("If I had a dollar for every time I get asked this question, I don't think I'd have to worry about affording my own water and wastewater bill. Percent MHI has become a popular indicator for utilities, agencies, and organizations across the country, and even we use it in our Rates Dashboards. Although different groups have their own unique interpretation of the resulting value, the calculation is relatively standard. One of the two variables needed to calculate this indicator—the Median Household Income (MHI)—is usually obtained from the U.S. Census Bureau and taken on face value. Digging deeper into this variable, however, reveals that it is not as simple as most people consider it to be. Using the Census Bureau's MHI as-is automatically builds in important qualifications into the percent MHI indicator that could significantly affect the interpretation of its value.")
                                     )
                             ),
                             tags$li(class = "article-item",
                                     div(style = "display: flex; align-items: center; cursor: pointer;",
                                         tags$span(class = "toggle-icon", HTML("&#9656;"), style = "margin-right: 8px;"),
                                         tags$a(id = "link2", href = "https://efc.web.unc.edu/2012/05/29/the-increasing-need-to-address-customer-affordability/",
                                                "The Increasing Need to Address Customer Affordability", class = "title-link")
                                     ),
                                     div(id = "content2", style = "display: none; margin-left: 28px;",
                                         p("Stacey Isaac Berahzer is a Senior Project Director for the Environmental Finance Center at the University of North Carolina, and works from a satellite office in Georgia."),
                                         p("Water prices are rising faster than any other utility service nationally. Of course, there is good reason for this – the industry has a large backlog of infrastructure needs."),
                                         p("While options such as public private partnerships represent promising areas for financing this backlog, it is mainly water customers who will be writing monthly checks to pay for these infrastructure projects."),
                                         p("With all indicators pointing toward a continued increase in water bills, the historic underpricing of water seems to be slowly righting itself. But, with this comes a greater need for utilities to consider the affordability issues of low income customers. Defining customer affordability has been a tough nut to crack. Perhaps the most quoted affordability threshold is 2.5% of median household income (MHI), but this 'rule of thumb' has been criticized for blanketing small pockets of poverty within a census block. On the other hand, the same threshold is cited as sometimes pressuring a water utility to keep rates too low, while many of the utility's customers can easily handle a higher rate. The bottom line is that defining affordability at the national scale is not easy!")
                                     )
                             ),
                             tags$li(class = "article-item",
                                     div(style = "display: flex; align-items: center; cursor: pointer;",
                                         tags$span(class = "toggle-icon", HTML("&#9656;"), style = "margin-right: 8px;"),
                                         tags$a(id = "link3", href = "https://efc.web.unc.edu/2014/03/26/affordability-of-government-utility-services/",
                                                "Understanding the Financial Position of Households Using the American Community Survey", class = "title-link")
                                     ),
                                     div(id = "content3", style = "display: none; margin-left: 28px;",
                                         p("In previous posts, we have talked about publicly available data on inflationary measures including the Consumer Price Index and the Construction Cost Index as well as on commercial energy use from the US Energy Information Administration (EIA) and the US Census. The US Census also has a rich set of data on the financial position of households within our community. These data are especially relevant and helpful for determining the affordability of government utility services such as water and wastewater rates."),
                                         p("For communities with populations of at least 65,000, survey data are collected annually. Areas with populations of 20,000 and above have data collected once every three years, and all areas have data collected at least once every five years.")
                                     )
                             )
                     ),
                     # Include JavaScript code
                     tags$script(HTML("
                      $(document).ready(function() {
                        $('.article-item').click(function(event) {
                          if ($(event.target).closest('a').length > 0) {
                            return;
                          }
                          var contentDiv = $(this).find('div[id^=\"content\"]');
                          contentDiv.toggle();
                          var icon = $(this).find('.toggle-icon');
                          if(contentDiv.is(':visible')) {
                            icon.html('&#9662;');
                          } else {
                            icon.html('&#9656;');
                          }
                        });
                        $('.title-link').click(function(event) {
                          event.stopPropagation();
                        });
                      });
                    "))
              ),
              column(width = 6,
                     img(src = "learnmore.png", style = "width:85%;")
              )
            )
        ),
        
        # Section for Mission Quote
        div(class = "quote-container",
            div(class = "quote-content",
                p('Our mission is to help deliver sustainable, fair, and effective environmental programs and services.')
            )
        ),
        
        # Section for Contributors
        div(class = "contributors-container",
            h2("Contributors"),
            fluidRow(
              column(width = 6, align = "center",
                     div(class = "contributor",
                         img(src = "eva.png", class = "contributor-image"),
                         h3("Eva Ramirez-Flores"),
                         p("Data Specialist"),
                         p(icon("envelope"), "everamif@live.unc.edu")
                     )
              ),
              column(width = 6, align = "center",
                     div(class = "contributor",
                         img(src = "ar.png", class = "contributor-image"),
                         h3("Ahmed Rachid El-Khattabi"),
                         p("Interim Executive Director"),
                         p(icon("envelope"), "arelkhattabi@sog.unc.edu")
                     )
              )
            )
        )
    )
  }
 
   
  # Render map
  output$map <- renderLeaflet({
    filtered_data <- data_rates
    if (!is.null(input$municipality) && input$municipality != "") {
      filtered_data <- data_rates %>% filter(rate_structure == input$municipality)
    }
    
    leaflet(filtered_data) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat,
                 popup = ~paste("<strong>", rate_structure, "</strong><br>Rate Structure Spot: ", rate_structure_spot),
                 clusterOptions = markerClusterOptions())
  })
  
  # Render iris plot
  output$irisPlot <- renderPlot({
    color_var <- input$color_var
    
    if (color_var == "Species") {
      ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point() +
        labs(title = "Iris Dataset: Sepal Length vs Sepal Width",
             x = "Sepal Length", y = "Sepal Width")
    } else {
      ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = .data[[color_var]])) +
        geom_point() +
        scale_color_viridis_c() +
        labs(title = "Iris Dataset: Sepal Length vs Sepal Width",
             x = "Sepal Length", y = "Sepal Width",
             color = color_var)
    }
  })
  
  # Render iris table
  output$irisTable <- renderDataTable({
    datatable(iris, options = list(pageLength = 10))
  })
  
  
  # Observe reset button click
  observeEvent(input$reset_municipality, {
    updateSelectInput(session, "municipality", selected = "")
  })
  
  # Toggle content visibility for dropdown items in Learn More page
  observeEvent(input$link1, {
    shinyjs::toggle("content1")
  })
  
  observeEvent(input$link2, {
    shinyjs::toggle("content2")
  })
  
  observeEvent(input$link3, {
    shinyjs::toggle("content3")
  })
}


shinyApp(ui = ui, server = server)
