library(shiny)
library(ggplot2)
library(dplyr)
library(mapdata)
library(fmsb)
library(leaflet)

source("food-pairing.R")

# Open Datasets
foods <- read.csv("./Datasets/foods.csv")
wines <- read.csv("./Datasets/wines.csv")
wines <- wines %>% arrange(name)
wines$price_category <- factor(wines$price_category, levels = c("15-25$", "26-50$", "51-75$", "76-100$", "101-150$", "151-250$", "251-500$", "501-1000$", "1001$+"))

# Define variables
world_map <- map_data("world")
aromas <- c("weight", "bitter", "fat", "piquant", "salt",  "acid", "sweet")

wine_colors <- c("Red Wine" = "red", "White Wine" = "blue", "Rose Wine" = "deeppink", 
                 "Champagne" = "darkgoldenrod", "Sweet and Fortified Wine" = "darkgreen", 
                 "NA" = "black")
unique_wine_types <- c("Red Wine", "White Wine", "Rose Wine", "Champagne", 
                       "Sweet and Fortified Wine", "NA")
pal <- colorFactor(wine_colors, levels = unique_wine_types)

# Define functions
taste_chart <- function(wine_or_food) {
  chart <- wine_or_food %>% select(aromas) %>% rbind(rep(0,7), rep(4,7)) %>% 
    arrange(desc(row_number())) %>%
    radarchart(
      pcol=rgb(0.8,0.3,0.3,0.9), pfcol=rgb(0.8,0.3,0.3,0.5), plwd=4,
      cglcol="grey", cglty=1, caxislabels=seq(0,20,5), cglwd=0.8
    )
  return(chart)
}

# Define a function to plot a box plot with anomalies within $100 from the quartiles
# and a dot for a particular wine
plot_with_anomalies_and_dot <- function(df, wine_name = NULL) {
  # Check if the wine_name is provided
  if (is.null(wine_name)) {
    cat("Please provide a wine name.\n")
    return(NULL)
  }
  
  # Get the wine type for the specified wine name
  wine_type <- df$type[df$name == wine_name]
  
  # Check if the wine type is found
  if (length(wine_type) == 0) {
    cat("No data available for the specified wine name.\n")
    return(NULL)
  }
  
  # Check if wine_type is "NA", then plot all wine prices
  if (is.na(wine_type)) {
    subset_df <- df
    wine_type <- "All Wines"
    # Calculate quartiles for all wines
    q1 <- quantile(df$price, 0.25, na.rm = TRUE)
    q3 <- quantile(df$price, 0.75, na.rm = TRUE)
    # Determine upper and lower bounds for anomalies
    lower_bound <- q1 - 200
    upper_bound <- q3 + 200
    # Filter out values beyond $100 from the quartiles for all wines
    subset_df <- subset_df[subset_df$price >= lower_bound & subset_df$price <= upper_bound, ]
  } else {
    # Calculate quartiles for the specified wine type, handling missing values
    q1 <- quantile(df$price[df$type == wine_type], 0.25, na.rm = TRUE)
    q3 <- quantile(df$price[df$type == wine_type], 0.75, na.rm = TRUE)
    
    # Check if quartiles are finite
    if (!is.finite(q1) || !is.finite(q3)) {
      cat("Quartiles are not finite for", wine_name, "\n")
      return(NULL)
    }
    
    # Determine upper and lower bounds for anomalies         
    lower_bound <- q1 - 100
    upper_bound <- q3 + 100
    
    # Filter out values beyond $100 from the quartiles for the specified wine type
    subset_df <- df[df$type == wine_type & df$price >= lower_bound & df$price <= upper_bound, ]
  }
  
  # Create horizontal box plot
  plot <- boxplot(subset_df$price, 
                  horizontal = TRUE,
                  main = paste("Price position of", wine_name, "(blue dot) among", wine_type, sep = " "),
                  cex.main = 0.9,
                  xlab = "Price",
                  col = "lightcoral")
  
  # Add a dot for the specified wine if provided
  if (!is.na(wine_type) && length(subset_df$name[subset_df$name == wine_name]) > 0) {
    wine_prices <- subset_df$price[subset_df$name == wine_name]
    points(wine_prices, rep(1, length(wine_prices)), col = "blue", pch = 16, cex = 1.5)
  }
  
  return(plot)
}

# Define UI
ui <- navbarPage(
  position = 'fixed-top',
  id = "navbar",
  selected = "home",
  
  tags$head(includeCSS("www/styles.css")), 
  
  # HOME PAGE ----------------
  tabPanel("Home", value = "home",
           
           div(class = "home-page",
               div(class = "home-page-layer",
                   div(class = "home-page-small-container",
                       h1(class = "home-title", 
                          "The Perfect Wine,", br(), "Anywhere,", br(), "Anytime."),
                       actionButton("home_button1", "Discover wines...", class = "home-button")
                       )
                   )
               ),  
           
           h2(class = "h2-title", "In Our Website..."),
           
           div(class = "graph-grid",
             div(class = "graph", h3(class = "graph-title", "Number of wines by type"),
                 plotOutput("wine_type"), 
                 p(class = "graph-subtitle", "You can choose between more than 3500 wines of all possible types: from red wine, to white wine, champagne and much more. Find out yourself!")
                 ),
    
             div(class = "graph", h3(class = "graph-title", "Number of wines by price category (SGD)"),
                 plotOutput("wine_price_distribution"),
                 p(class = "graph-subtitle", "We have wines of all prices. Whether you want to impress your friends with an expensive wine or save money with a cheaper one, we have what you need!")
                 ),
             
             div(class = "graph map-graph", h3(class = "graph-title", "Number of wines by country of origin"),
                 plotOutput("wine_map"),
                 p(class = "graph-subtitle", "On our website you can find the best and most prestigious wines from all over the world: from those produced in the beautiful Italian countryside to those coming from the sunny land of Australia")
                 )
             ),
           
           actionButton("home_button2", "Discover our wines", class = "home-button graph-home-button"),
           
           div(class = "tastes-section",
               img(class = "tastes-section-image", src = "food-wine-picture.jpg"),
               h2(class = "h2-title", "Find the Wine that Best Matches your Food"),
               
               div(class = "tastes-section-container",
                   p("We have developed an algorithm that finds the perfect wine for your food. Tell us what you will be eating and we'll take care of the rest: we will build a taste map of your food and suggest you the perfect wine"),
                   plotOutput("home_wine_taste_chart")
                   ),
               actionButton("home_button3", "Match your food", class = "home-button graph-home-button")
               )
           ),

  # FILTER WINES PAGE -----------------------
  tabPanel("Search wine", value = "search", 
           div(class = "food-pairing-picture", 
               h1("Search for the Perfect Wine"),
               p("Specify all your criteria and we will find you the wine you are looking for")),
           sidebarLayout(
             sidebarPanel(
               selectInput("order", "Order by", c("Price (ascending)", "Price (descending)", "Name (ascending)", "Name (descending)")), 
               textInput("searchbar", "Search name"),
               checkboxGroupInput("wine_type", "Type", unique(wines$type[!is.na(wines$type)]), inline = T),
               sliderInput("price_range", "Price", min = min(wines$price), max = 1000, value = c(min(wines$price), 1000)),
               checkboxGroupInput("country_of_origin", "Country", unique(wines$country[!is.na(wines$country)]), inline = T),
               sliderInput("year_range", "Year", min = min(wines$year[!is.na(wines$year)]), 
                           max = max(wines$year[!is.na(wines$year)]), 
                           value = c(min(wines$year[!is.na(wines$year)]), max(wines$year[!is.na(wines$year)])),
                           sep = "", step = 1),
               checkboxGroupInput("additional_filters", "Additional options", c("Sparkling", "Organic", "Vegan")),
               radioButtons("filter_by_taste", "Filter by taste profile", c("No", "Yes")),
               conditionalPanel(
                 condition = "input.filter_by_taste == 'Yes'",
                 sliderInput("filter_weight", "Weigth", 0, 4, step = 0.01, ticks = F, value = c(0, 4)),
                 sliderInput("filter_sweet", "Sweet", 0, 4, step = 0.01, ticks = F, value = c(0, 4)),
                 sliderInput("filter_acid", "Acid", 0, 4, step = 0.01, ticks = F, value = c(0, 4)),
                 sliderInput("filter_salt", "Salt", 0, 4, step = 0.01, ticks = F, value = c(0, 4)),
                 sliderInput("filter_piquant", "Piquant", 0, 4, step = 0.01, ticks = F, value = c(0, 4)),
                 sliderInput("filter_fat", "Fat", 0, 4, step = 0.01, ticks = F, value = c(0, 4)),
                 sliderInput("filter_bitter", "Bitter", 0, 4,step = 0.01, ticks = F, value = c(0, 4))
               )
             ),
             
             mainPanel(
               h2(class ="food-pairing-title", "Filtered Wines"),
               div(class = "filter_buttons_container",
                   actionButton("prev_filter", "Previous page"),
                   uiOutput("page_count"),
                   actionButton("next_filter", "Next page")    
                   ),
               uiOutput("filtered_wines_list")
             )
           )
           ),
  
  # FOOD PAIRING PAGE -------------------
  tabPanel("Food pairing", value = "food_pairing", 
           
           div(class = "food-pairing-picture", 
               h1("Find the Perfect Match for Your Food"),
               p("Select a food or build the food taste profile you want us to find the match for")),

           sidebarLayout(
             
             sidebarPanel(width = 3,
                          radioButtons("match_type", "Choose the type of match", choices = c("Contrasting taste", "Similar taste")),
                          radioButtons("food_taste_type", "Choose how you want to create the food taste profile", choices = c("Select a food", "Create you own food taste profile")),
                          selectInput("food", "Select a food", foods$food),
                          div(
                            h4(class = "create-profile-label", "Create your own food taste profile"),
                            sliderInput("food_pairing_weight", "Weight", 0, 4, value = 0 ,step = 0.01, ticks = F),
                            sliderInput("food_pairing_sweet", "Sweet", 0, 4, value = 0 ,step = 0.01, ticks = F),
                            sliderInput("food_pairing_acid", "Acid", 0, 4, value = 0 ,step = 0.01, ticks = F),
                            sliderInput("food_pairing_salt", "Salt", 0, 4, value = 0 ,step = 0.01, ticks = F),
                            sliderInput("food_pairing_piquant", "Piquant", 0, 4, value = 0 ,step = 0.01, ticks = F),
                            sliderInput("food_pairing_fat", "Fat", 0, 4, value = 0 ,step = 0.01, ticks = F),
                            sliderInput("food_pairing_bitter", "Bitter", 0, 4, value = 0 ,step = 0.01, ticks = F)
                            )
                          ),
             mainPanel(
               h2(class = "food-pairing-title", "Food Taste Profile"),
               div(class = "food-pairing-chart", 
                   plotOutput("food_pairing_taste_chart")
               ),
               h2(class = "food-pairing-title", "Suggested Wines"),
               uiOutput("matching_wines_list")
               )
             )
           ),
  
  # GEOGRAPHIC DISTRIBUTION PAGE -----------------
  tabPanel("Geographic distribution", value = "geographic_distribution", 
           
           div(class = "food-pairing-picture", 
               h1("Discover the Geographic Distribution of our Wines")
           ),
           
           sidebarLayout(
             sidebarPanel(
               radioButtons("geographic_filter_by", "View Distribution By:",
                            choices = c("Wine Type", "Grape Variety"), selected = "Wine Type"),
               conditionalPanel(
                 condition = "input.geographic_filter_by == 'Wine Type'",
                 checkboxGroupInput("geographic_wine_types", "Select Wine Types:", 
                                    choices = unique(wines$type[!is.na(wines$type)]), selected = unique(wines$type[!is.na(wines$type)]))
               ),
               conditionalPanel(
                 condition = "input.geographic_filter_by == 'Grape Variety'",
                 selectInput("geographic_grape_type", "Select Grape Variety:", 
                             choices = c("", unique(wines$grape_variety[!is.na(wines$grape_variety)])))
               )
             ),
             mainPanel(
               div(class = "margin-top",
                   leafletOutput("map")
               )    
             )
           )
  ),      
  
  # COMPARE WINES PAGE -----------------
  tabPanel("Compare wines", value = "compare_wines", 
           div(class = "compare-wines-page",
               div(class = "compare-input-container", 
                   selectInput("wine_to_compare1", "Select first wine", choices = wines$name, selected = wines[10, "name"]),
                   selectInput("wine_to_compare2", "Select second wine", choices = wines$name, selected = wines[11, "name"])
                   ),
               uiOutput("render_compared_wines")
               )
           ),  
  
  # WINE DETAILS PAGE -----------------
  tabPanel("Selected wine", value = "selected_wine",
           uiOutput("render_selected_wine")
  )
)     

# Define server logic
server <- function(input, output, session) {
  
   # HOME PAGE ----------------
  observeEvent(input$home_button1, {
    updateNavbarPage(session=session, inputId="navbar", selected="search")
  })
  
  observeEvent(input$home_button2, {
    updateNavbarPage(session=session, inputId="navbar", selected="search")
  })
  
  observeEvent(input$home_button3, {
    updateNavbarPage(session=session, inputId="navbar", selected="food_pairing")
  })
  
  # Render bar chart for distribution of wines by type
  output$wine_type <- renderPlot(
    wines %>% filter(!is.na(wines$type)) %>% group_by(type) %>% summarise(count = n()) %>% 
      ggplot(aes(x = reorder(type, -count), y = count)) + 
      geom_bar(stat = 'identity', fill = "#82373D") +
      geom_text(aes(label = count), vjust = -0.5, size = 7) + 
      labs(x="", y="") + ylim(0, 2600) + 
      theme(panel.background = element_rect(fill = "white"), axis.text = element_text(size = 12), 
            axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_blank())
  )
  
  # Render bar chart for distribution of wines by price
  output$wine_price_distribution <- renderPlot(
    wines %>% group_by(price_category) %>% summarise(count = n()) %>%
    ggplot(aes(x = price_category, y = count)) + 
      geom_bar(fill = "#82373D", stat = 'identity') +  
      labs(x="", y="") + ylim(0, 1100) + 
      geom_text(aes(label = count), vjust = -0.5, size = 7) + 
      theme(panel.background = element_rect(fill = "white"), axis.text = element_text(size = 12), 
                          axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_blank())
  )
  
  # Render map for geographical distribution of wines
  output$wine_map <- renderPlot(
    wines %>% group_by(country) %>% summarize(count = n()) %>% 
      mutate(number = cut(count, breaks = c(0, 10, 50, 200, 500, Inf), labels = c("0-10", "11-50", "51-200", "201-500", "501+"))) %>%
      ggplot() + 
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = '#bebebea3') +
      geom_map(aes(map_id = country, fill = number), map = world_map) + 
      scale_fill_manual(name = "Number of wines", values = c("#81363c4d", "#c33e49a8", "#9b2731a8", "#712229", "#3d1b1e")) +
      labs(x="", y="") +
      theme(panel.background = element_rect(fill = "white"), 
            axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
            axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
            legend.key.size = unit(2.5, "line"), legend.title = element_text(size = 16))
  )
  
  # Render home paage taste profile chart
  output$home_wine_taste_chart <- renderPlot(
      taste_chart(wines[wines$name == "Colle Corviano Sangiovese", ])
    )
  
  # FOOD PAIRING PAGE -------------------
  food_taste_profile <- reactiveVal()
  matching_wines <- reactiveVal()
  selected_wine <- reactiveVal(wines[wines$id == "wine1", ])
  
  # Observer to detect whether the user wants to find wines matching a particular food or build his own taste profile
  observe({
    if (input$food_taste_type == "Select a food") {
      food_taste_profile(foods[foods$food == input$food, ])
    } else {
      food_taste_profile(data.frame("weight" = input$food_pairing_weight, "sweet" = input$food_pairing_sweet, "acid" = input$food_pairing_acid, "salt" = input$food_pairing_salt, "piquant" = input$food_pairing_piquant, "fat" = input$food_pairing_fat, "bitter" = input$food_pairing_bitter))
    }
    
    matching_wines(find_match(wines[!is.na(wines$weight), ], food_taste_profile(), input$match_type))
  })
  
  # Render taste profile chart
  output$food_pairing_taste_chart <- renderPlot(
    taste_chart(food_taste_profile())
    )
  
  # Render matching wines
  output$matching_wines_list <- renderUI({
    div(class = "matching-wines-container", 
      apply(matching_wines(), 1, function(x) {
        div(class = "wine-card",
          h2(x["name"]),
          h3("$", paste(x["price"], sep = "")),
          h4(paste(x["region"], x["country"], sep = ", ")),
          actionButton(paste("match_button", x["id"], sep = ""), "See details")
        )
      })
    )
  })

  # Create observe event linking to the wines detail page for every wine displayed 
  observe({
    apply(matching_wines(), 1, function(x) {
      observeEvent(input[[paste("match_button", x["id"], sep = "")]], {
        selected_wine(wines[wines$id == x["id"], ])
        updateNavbarPage(session=session, inputId="navbar", selected="selected_wine")
      })
    })
  })
  
  # WINE DETAILS PAGE -----------------
  output$render_selected_wine <- renderUI({
    div(class = "wine-page",
        h1(selected_wine()$name),
        h3(paste("$", selected_wine()$price, sep = "")),
        plotOutput("boxplot"),
        a(href = selected_wine()$link, "Purchase wine", class = "link-button"),
        div(class = "characteristics-container", 
            p(span(class = "bold", "Type: "), selected_wine()$type),
            p(span(class = "bold", "Grape variety: "), selected_wine()$grape_variety),
            p(span(class = "bold", "Region: "), paste(selected_wine()$country, ", ", selected_wine()$region, sep = "")),
            p(span(class = "bold", "Producer: "), selected_wine()$producer),
            p(span(class = "bold", "Year: "), selected_wine()$year),
            p(span(class = "bold", "Bottle size: "), paste(selected_wine()$bottle_size, "ml")),
            p(class = "description", span(class = "bold", "Description: "), br(), selected_wine()$desription),
            p(class = "short-description", selected_wine()$short_description)
        ),
      div(class = "selected-wine-charts-container", 
          div(class = "selected-wine-taste-map",
              h2("Taste Profile Chart"),
              plotOutput("selected_wine_taste_chart")
          ),
          div(
            h2("Geographical Origin"),
            leafletOutput("selected_wine_map")
          )
      )
    )
  })
  
  output$boxplot <- renderPlot(
    plot_with_anomalies_and_dot(wines, selected_wine()$name)
  )
  
  # Render taste profile chart
  output$selected_wine_taste_chart <- renderPlot(
    taste_chart(selected_wine())
  )
  
  # Render position map
  output$selected_wine_map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = selected_wine()$longitude, lat = selected_wine()$latitude, zoom = 3) %>% 
      addMarkers(lng = selected_wine()$longitude, lat = selected_wine()$latitude, 
                 popup = paste(selected_wine()$country, selected_wine()$region, sep = ", "))
  })
  
  # FILTER WINES PAGE -----------------------
  filtered_wines <- reactiveVal(wines)
  current_page <- reactiveVal(0)
  
  # Filter wines basing on user inputs
  observe({
    current_page(0)
    temporary_filtered_wines <- wines %>% filter(price >= input$price_range[1],
                                    year >= input$year_range[1], year <= input$year_range[2],
                                    grepl(input$searchbar, name, ignore.case = T)
                                    )
    
    if (input$price_range[2] < 1000) {
      temporary_filtered_wines <- temporary_filtered_wines %>% filter(price <= input$price_range[2])
    }
    if (length(input$country_of_origin) > 0) {
      temporary_filtered_wines <- temporary_filtered_wines %>% filter(country %in% input$country_of_origin)
    }
    if (length(input$wine_type) > 0) {
      temporary_filtered_wines <- temporary_filtered_wines %>% filter(type %in% input$wine_type)
    }
    
    if (input$order == "Price (ascending)") {
      temporary_filtered_wines <- temporary_filtered_wines %>% arrange(price)
    } else if (input$order == "Price (descending)") {
      temporary_filtered_wines <- temporary_filtered_wines %>% arrange(desc(price))
    } else if (input$order == "Name (ascending)") {
      temporary_filtered_wines <- temporary_filtered_wines %>% arrange(name)
    } else if (input$order == "Name (descending)") {
      temporary_filtered_wines <- temporary_filtered_wines %>% arrange(desc(name))
    }
    
    if ("Sparkling" %in% input$additional_filters) {
      temporary_filtered_wines <- temporary_filtered_wines %>% filter(sparkling == "sparkling")
    }
    if ("Organic" %in% input$additional_filters) {
      temporary_filtered_wines <- temporary_filtered_wines %>% filter(organic == "organic")
    }
    if ("Vegan" %in% input$additional_filters) {
      temporary_filtered_wines <- temporary_filtered_wines %>% filter(vegan == "vegan")
    }
    
    if (input$filter_by_taste == "Yes") {
      temporary_filtered_wines <- temporary_filtered_wines %>% filter(weight >= input$filter_weight[1], weight <= input$filter_weight[2],
                                                                      sweet >= input$filter_sweet[1], sweet <= input$filter_sweet[2],
                                                                      acid >= input$filter_acid[1], acid <= input$filter_acid[2],
                                                                      salt >= input$filter_salt[1], salt <= input$filter_salt[2],
                                                                      piquant >= input$filter_piquant[1], piquant <= input$filter_piquant[2],
                                                                      fat >= input$filter_fat[1], fat <= input$filter_fat[2],
                                                                      bitter >= input$filter_bitter[1], bitter <= input$filter_bitter[2],
                                                                      )
    }
    
    filtered_wines(temporary_filtered_wines)
  })
  
  # Render filtered wines
  output$filtered_wines_list <- renderUI({
    if(nrow(filtered_wines()) != 0) {
      div(class = "matching-wines-container",
          apply(filtered_wines()[(current_page()*10 + 1):min((current_page()+1)*10, nrow(filtered_wines())), ], 1, function(x) {
            div(class = "wine-card",
                h2(x["name"]),
                h3("$", paste(x["price"], sep = "")),
                h4(paste(x["region"], x["country"], sep = ", ")),
                actionButton(paste("filter_button", x["id"], sep = ""), "See details")
            )
          })
      )
    } else {
      h4(class = "sorry_message", "We are sorry, there are no wines matching the filters") 
    }
  })
  
  # Create observe event linking to the wines detail page for every wine displayed 
  observe({
    apply(filtered_wines()[(current_page()*10 + 1):min((current_page()+1)*10, nrow(filtered_wines())), ], 1, function(x) {
      observeEvent(input[[paste("filter_button", x["id"], sep = "")]], {
        selected_wine(wines[wines$id == x["id"], ])
        updateNavbarPage(session=session, inputId="navbar", selected="selected_wine")
      })
    })
  })
  
  # Create observe event to move to next page
  observeEvent(input$next_filter, {
    current_page(min(current_page() + 1, floor(nrow(filtered_wines()) / 10)))
  })
  
  # Create observe event to move to previous pge
  observeEvent(input$prev_filter, {
    current_page(max(0, current_page() - 1)) 
  })
  
  # Render page number
  output$page_count <- renderUI({
    p(paste("Page ", current_page() + 1), "/", ceiling(nrow(filtered_wines()) / 10), sep = "")
  })
  
  # COMPARE WINES PAGE -----------------------
  wine_to_compare1_df <- reactive({wines[wines$name == input$wine_to_compare1, ]})     
  wine_to_compare2_df <- reactive({wines[wines$name == input$wine_to_compare2, ]}) 
  
  # Render UI to compare wines
  output$render_compared_wines <- renderUI({
    div(class = "compare-wines-page-subcontainer",       
        h1(wine_to_compare1_df()$name),  
        h1(wine_to_compare2_df()$name),
        h3(paste("$", wine_to_compare1_df()$price, sep = "")),
        h3(paste("$", wine_to_compare2_df()$price, sep = "")),
        plotOutput("boxplot1"),
        plotOutput("boxplot2"),
        a(href = wine_to_compare1_df()$link, "Purchase wine", class = "link-button"),
        a(href = wine_to_compare2_df()$link, "Purchase wine", class = "link-button"),
        div(class = "characteristics-container", 
            p(span(class = "bold", "Type: "), wine_to_compare1_df()$type),
            p(span(class = "bold", "Grape variety: "), wine_to_compare1_df()$grape_variety),
            p(span(class = "bold", "Region: "), paste(wine_to_compare1_df()$country, ", ", wine_to_compare1_df()$region, sep = "")),
            p(span(class = "bold", "Producer: "), wine_to_compare1_df()$producer),
            p(span(class = "bold", "Year: "), wine_to_compare1_df()$year),
            p(span(class = "bold", "Bottle size: "), paste(wine_to_compare1_df()$bottle_size, "ml")),
            p(class = "description", span(class = "bold", "Description: "), br(), wine_to_compare1_df()$description),
            p(class = "short-description", wine_to_compare1_df()$short_description)
        ),
        div(class = "characteristics-container", 
            p(span(class = "bold", "Type: "), wine_to_compare2_df()$type),
            p(span(class = "bold", "Grape variety: "), wine_to_compare2_df()$grape_variety),
            p(span(class = "bold", "Region: "), paste(wine_to_compare2_df()$country, ", ", wine_to_compare2_df()$region, sep = "")),
            p(span(class = "bold", "Producer: "), wine_to_compare2_df()$producer),
            p(span(class = "bold", "Year: "), wine_to_compare2_df()$year),
            p(span(class = "bold", "Bottle size: "), paste(wine_to_compare2_df()$bottle_size, "ml")),
            p(class = "description", span(class = "bold", "Description: "), br(), wine_to_compare2_df()$description),
            p(class = "short-description", wine_to_compare2_df()$short_description)
        ),
        div(class = "selected-wine-charts-container", 
            div(class = "selected-wine-taste-map",
                h2("Taste Profile Chart"),
                plotOutput("compared_wine_taste_chart1")
            ),
            div(
              h2("Geographical Origin"),
              leafletOutput("compared_wine_map1")
            )
        ),
        div(class = "selected-wine-charts-container", 
            div(class = "selected-wine-taste-map",
                h2("Taste Profile Chart"),
                plotOutput("compared_wine_taste_chart2")
            ),
            div(
              h2("Geographical Origin"),
              leafletOutput("compared_wine_map2")
            )
        )
    )
  })
  
  # Render boxplot for wine 1
  output$boxplot1 <- renderPlot(
    plot_with_anomalies_and_dot(wines, wine_to_compare1_df()$name)
  )
  
  # Render taste profile chart for wine 1
  output$compared_wine_taste_chart1 <- renderPlot(
    taste_chart(wine_to_compare1_df())
  )
  
  # Render map for wine 1
  output$compared_wine_map1 <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = wine_to_compare1_df()$longitude, lat = wine_to_compare1_df()$latitude, zoom = 3) %>% 
      addMarkers(lng = wine_to_compare1_df()$longitude, lat = wine_to_compare1_df()$latitude, 
                 popup = paste(wine_to_compare1_df()$country, wine_to_compare1_df()$region, sep = ", "))
  })
  
  # Render boxplot for wine 2
  output$boxplot2 <- renderPlot(
    plot_with_anomalies_and_dot(wines, wine_to_compare2_df()$name)
  )
  
  # Render taste profile chart for wine 2
  output$compared_wine_taste_chart2 <- renderPlot(
    taste_chart(wine_to_compare2_df())
  )
  
  # Render map for wine 2
  output$compared_wine_map2 <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = wine_to_compare2_df()$longitude, lat = wine_to_compare2_df()$latitude, zoom = 3) %>%
      addMarkers(lng = wine_to_compare2_df()$longitude, lat = wine_to_compare2_df()$latitude, 
                 popup = paste(wine_to_compare2_df()$country, wine_to_compare2_df()$region, sep = ", "))
  })
  
  # GEOGRAPHIC DISTRIBUTION PAGE -----------------
  filtered_data <- reactiveVal(wines %>% filter(!is.na(longitude) & !is.na(latitude)) %>%
                                 group_by(longitude, latitude, type, country, region) %>%
                                 summarise(count = n()))
  
  # Filter dataset based on selected variable
  observe({
    if (input$geographic_filter_by == "Wine Type") {
      filtered <- wines %>%
        filter(!is.na(longitude) & !is.na(latitude) & !is.na(type) & type %in% input$geographic_wine_types) %>%
        group_by(longitude, latitude, type, country, region) %>%
        summarise(count = n())
    } else if (input$geographic_filter_by == "Grape Variety") {
      filtered <- wines %>%
        filter(!is.na(longitude) & !is.na(latitude) & !is.na(grape_variety))
      
      if (!is.null(input$geographic_grape_type) && input$geographic_grape_type != "") {
        filtered <- filtered %>%
          filter(grape_variety == input$geographic_grape_type)
      }
      
      filtered <- filtered %>%
        group_by(longitude, latitude, grape_variety, country, region) %>%
        summarise(count = n())
    } else {
      # Return an empty data frame or NULL when neither "Wine Type" nor "Grape Variety" is selected
      filtered <- NULL
    }
    
    filtered_data(filtered)
  })
  
  # Render the world map with scatter plot
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) 
  })
  
  # Observe changes in radio button selection
  observe({
    if (input$geographic_filter_by == "Grape Variety") {
      leafletProxy("map", data = filtered_data()) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = ~count/10, # Adjust radius based on count
          color = "purple", # Set color for all markers
          group = "grape_markers", # Assign group for legend
          popup = ~paste(country, ", ", region, "<br>",
                         "Grape Variety: ", grape_variety, "<br>",
                         "Count: ", count, sep = "")
        ) 
    } else {
      leafletProxy("map", data = filtered_data()) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = ~count/10, # Adjust radius based on count
          color = ~pal(type), # Set color based on wine type
          group = "wine_markers", # Assign group for legend
          popup = ~paste(country, ", ", region, "<br>",
                         "Count: ", count, sep = "")
        ) %>%
        clearControls() %>% # Clear previous legend
        addLegend("topright", # Position legend on top right
                  pal = pal, # Color palette
                  values = ~type, # Values for legend
                  title = "Wine Type" # Legend title
        )
    }
  })      
  
}  

# Run the app
shinyApp(ui = ui, server = server)




