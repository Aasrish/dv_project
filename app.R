library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(caret)
library(plotly)
library(visNetwork)
library(plotly)
library(jsonlite)

# Read the data for both plots
data1 <- read.csv("plot1.csv")
data2 <- read.csv("plot3.csv")
data4 <- fromJSON("plot8.json")
data3 <- list(
  'Debdut' = c('OneRepublic', 'Selena Gomez', 'Charlie Puth', 'Taylor Swift', 'Jason Derulo', 'Martin Garrix', 'Maroon 5', 'Avicii', 'Ed Sheeran', 'Shawn Mendes', 'David Guetta', 'James Arthur', 'Imagine Dragons', 'Selena Gomez & The Scene', 'Miley Cyrus', 'Pritam', 'Em Beihold', 'Kygo', 'Dua Lipa', 'Pink Sweat$'),
  'Aasrish' = c('OneRepublic', 'Anirudh Ravichander', 'Arijit Singh', 'Armaan Malik', 'David Guetta', 'Dua Lipa', 'Ed Sheeran', 'Elley Duhé', 'Harrdy Sandhu', 'Joel Sunny', 'Justin Bieber', 'Neha Kakkar', 'Ramin Djawadi', 'Ruth B', 'Sid Sriram', 'Taylor Swift', 'Thaman S'),
  'Tejas' = c('The Local Train', 'The Weeknd', 'Tame Impala', 'The Chainsmokers', 'Samuel Kim', 'Pritam', 'Hans Zimmer', 'VALORANT', 'Imagine Dragons', 'Glass Animals', 'OneRepublic', 'Marcin Przybyłowicz', 'David Guetta', 'John Paesano', 'Maroon 5', 'AC/DC', 'Arctic Monkeys', 'Green Day', 'Tom Player', 'Salim–Sulaiman')
)# Assuming "plot3.csv" contains data for Duration vs Freq bar plot

# Define genre categories for the first plot
genre_categories <- list(
  'Rock' = c('piano rock', 'modern rock', 'rock', 'garage rock', 'celtic rock'),
  'Pop' = c('pop', 'post-teen pop', 'viral pop', 'dance pop', 'pop dance', 'uk pop', 'canadian pop', 'singer-songwriter pop', 'talent show', 'gen z singer-songwriter', 'pop soul', 'pop rock', 'australian pop', 'desi pop', 'barbadian pop', 'boy band'),
  'EDM/Dance' = c('dutch edm', 'edm', 'progressive house', 'tropical house', 'bedroom soul', 'permanent wave', 'sheffield indie', 'indie rockism', 'art pop', 'brostep', 'progressive electro house', 'dutch pop', 'australian dance', 'electro house', 'house', 'uk dance'),
  'Hip Hop/Urban' = c('latin hip hop', 'reggaeton', 'trap latino', 'urbano latino', 'urban contemporary', 'japanese old school hip hop', 'hindi indie', 'indian indie', 'indian rock', 'new delhi indie'),
  'Bollywood/Indian' = c('filmi', 'indian instrumental', 'modern bollywood'),
  'Soul/R&B' = c('british soul', 'chill r&b', 'canadian contemporary r&b'),
  'Other' = c('alt z', 'metropopolis', 'red dirt', 'indietronica', 'classic texas country')
)

# Group genres into broader categories for the first plot
data1$Group <- NA
for (category in names(genre_categories)) {
  data1[data1$Genres %in% genre_categories[[category]], "Group"] <- category
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Combined Apps"),
  tabsetPanel(
    tabPanel("Genre vs Popularity", 
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("groups", "Select Categories to Display:",
                                    choices = names(genre_categories),
                                    selected = names(genre_categories))
               ),
               mainPanel(
                 plotlyOutput("boxplot")
               )
             )
    ),
    tabPanel("Duration vs Freq",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("bin_size", "Bin Size", min = 1, max = 100, value = 10),
                 selectInput("color", "Bar Color", choices = c("Red", "Blue", "Green"), selected = "Blue")
               ),
               mainPanel(
                 plotlyOutput("histogram")
               )
             )
    ),
    tabPanel("Wordbubble",
             mainPanel(titlePanel("Top Genres and Artists Visualization"),
                       selectInput("selection", "Choose Visualization:",
                                   choices = c("Genres", "Artists")),
                       uiOutput("dynamicUI")
                       )
             ),
    tabPanel("Interactive Line Plot",
             sidebarLayout(
               sidebarPanel(
                 # Assuming the names are consistent across terms, if not, adjust accordingly
                 checkboxGroupInput("artists", "Choose artists to keep:", choices = data4$short_term$names, selected = data4$short_term$names),
                 selectInput("y_param", "Choose Y-axis parameter:", choices = c("followers", "popularities"))
               ),
               
               mainPanel(
                 plotlyOutput("line_plot")
               )
             )
             ),
    tabPanel("Mood & time analysis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("plot_data", "Upload Plot Data (CSV format)")
               ),
               mainPanel(
                 plotOutput("plot"),
                 plotlyOutput("song_popularity_plot")
               )
             )
      
    ),
    tabPanel("Graph Connections",
             mainPanel(
               titlePanel("Music Network"),
               visNetworkOutput("network")
             )
             )
  ),
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    categories <- input$groups
    data1 %>%
      filter(Group %in% categories)
  })
  output$boxplot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Group, y = ~Popularity, type = "box", fill = "#1f77b4") %>%
      layout(title = "Popularity Distribution by Genre Category",
             xaxis = list(title = "Category", fixedrange = FALSE),
             yaxis = list(title = "Popularity", fixedrange = FALSE),
             width = input$plotWidth, height = input$plotHeight)
  })
  
  # Render box plot for the first plot
  output$histogram <- renderPlotly({
    plot_ly(data = data2, x = ~duration, type = "histogram", marker = list(color = input$color), nbinsx = input$bin_size) %>%
      layout(title = "Duration vs Frequency",
             xaxis = list(title = "Duration (s)", fixedrange = FALSE),
             yaxis = list(title = "Frequency", fixedrange = FALSE),
             width = input$plotWidth, height = input$plotHeight)
  })
  # Render bar plot for the second plot
  data <- reactiveValues(train_data = NULL, plot_data = NULL)
  data$train_data <- read.csv("train_data.csv")
  
  # Load plot data
  observeEvent(input$plot_data, {
    data$plot_data <- read.csv(input$plot_data$datapath)
    print("Plot data uploaded")
  })
  
  # Perform prediction and create plot
  # Create plot using plotly
  output$plot <- renderPlot({
    req(data$train_data, data$plot_data)
    
    # Train a model
    # Train a model without 'key' variable
    model <- train(mood ~ ., data = data$train_data, method = "rf")
    print("Model Trained")
    # Predict mood using the trained model
    
    
    # Process plot data
    plot_data <- data$plot_data
    plot_data$timestamp <- as.POSIXct(plot_data$timestamp, format = "%Y-%m-%dT%H:%M:%S")
    plot_data$timestamp <- round_date(plot_data$timestamp, "hour")
    
    # Calculate average of features over one-hour intervals
    hourly_avg_data <- plot_data %>%
      group_by(timestamp) %>%
      summarize(across(c(danceability, acousticness, energy, instrumentalness, 
                         liveness, valence, loudness, speechiness, tempo), mean))
    
    hourly_avg_data$mood <- predict(model, newdata = hourly_avg_data)
    print("Mood predicted")
    mood_levels <- unique(hourly_avg_data$mood)
    
    # Choose a color palette with enough colors
    color_palette <- rainbow(length(mood_levels))
    
    # Create plot
    ggplot(hourly_avg_data, aes(x = timestamp, y = as.POSIXct(1, origin = "1970-01-01"), fill = mood)) +
      geom_tile() +
      scale_fill_manual(values = color_palette, limits = mood_levels) +  
      scale_x_datetime(date_breaks = "4 hours", date_labels = "%H:%M") +  # Set breaks every 4 hours
      theme_minimal() +
      labs(title = "Predicted Mood Over Time",
           x = "Time",
           y = " ",
           fill = "Mood")
    # Set the aspect ratio to 0.5 (you can adjust this value as needed)
    
  })
  output$song_popularity_plot <- renderPlotly({
    req(data$plot_data)
    
    # Process plot data for popular time analysis
    plot_data <- data$plot_data
    plot_data$timestamp <- as.POSIXct(plot_data$timestamp, format = "%Y-%m-%dT%H:%M:%S")
    
    # Determine popular time for listening to songs
    song_popularity <- plot_data %>%
      mutate(hour = hour(timestamp)) %>%
      count(hour) %>%
      arrange(desc(n))
    
    # Plot the popular time for listening to songs
    song_popularity_plot <- plot_ly(song_popularity, x = ~hour, y = ~n, type = 'bar', 
                                    marker = list(color = 'blue')) %>%
      layout(title = 'Popular Time for Listening to Songs',
             xaxis = list(title = 'Hour of Day'),
             yaxis = list(title = 'Number of Songs'))
    
    # Convert ggplot to plotly
    ggplotly(song_popularity_plot)
  })
  output$network <- renderVisNetwork({
    # Create a dataframe of nodes
    main_nodes <- unique(names(data3))
    musician_nodes <- unlist(data3)
    all_nodes <- unique(c(main_nodes, musician_nodes))
    nodes_df <- data.frame(id = all_nodes, 
                            label = all_nodes, 
                            group = ifelse(all_nodes %in% main_nodes, 1, 2))
    
    # Create a dataframe of edges
    edges_df <- data.frame(from = rep(names(data3), times = lengths(data3)), 
                            to = unlist(data3))
    
    # Plot the network
    visNetwork(nodes = nodes_df, edges = edges_df) %>%
      visGroups(groupname = "1", color = list(border = "blue"), shape = "box") %>%
      visGroups(groupname = "2", color = list(border = "green"), shape = "circle")
  })
  ##################################
  output$dynamicUI <- renderUI({
    if(input$selection == "Artists") {
      plotOutput("artistsBubbles")
    } else if(input$selection == "Genres") {
      plotOutput("genresBubbles")
    }
  })
  
  # Artists data and plot
  artists <- c('The Local Train', 'The Weeknd', 'Tame Impala', 'The Chainsmokers', 'Samuel Kim', 
               'Pritam', 'Hans Zimmer', 'VALORANT', 'Imagine Dragons', 'Glass Animals', 
               'OneRepublic', 'Marcin Przybyłowicz', 'David Guetta', 'John Paesano', 'Maroon 5', 
               'AC/DC', 'Arctic Monkeys', 'Green Day', 'Tom Player', 'Salim-Sulaiman')
  frequencies <- seq_along(artists)
  frequencies <- rev(frequencies)
  artists_data <- data.frame(artist = artists, freq = frequencies, x = runif(length(artists)), y = runif(length(artists)))
  
  output$artistsBubbles <- renderPlot({
    ggplot(artists_data, aes(x = x, y = y, size = freq, label = artist)) +
      geom_point(color = "blue", alpha = 1) +
      geom_text(vjust = 0, nudge_y = 0.05, size = 3.5) +
      scale_size(range = c(3, 10)) +
      theme_void() +
      theme(legend.position = "none") +
      xlim(0, 1) +
      ylim(0, 1) +
      ggtitle("Top Artists Bubble Chart")
  })
  
  # Genres data and plot
  genres_list <- list(
    c('canadian contemporary r&b', 'canadian pop', 'pop'),
    c('filmi', 'indian instrumental', 'modern bollywood'),
    c('hindi indie', 'indian indie', 'indian rock', 'new delhi indie'),
    c('german soundtrack', 'orchestral soundtrack', 'soundtrack'),
    c('anime lo-fi'),
    c('pop'),
    c('australian psych', 'modern rock', 'neo-psychedelic', 'rock'),
    c('epicore'),
    c('german soundtrack', 'orchestral soundtrack', 'scorecore', 'soundtrack'),
    c('pop'),
    c('gauze pop', 'indietronica', 'modern rock', 'pov: indie', 'shiver pop'),
    c('soundtrack', 'video game music'),
    c('chill pop', 'singer-songwriter pop'),
    c('garage rock', 'modern rock', 'permanent wave', 'rock', 'sheffield indie'),
    c('video game music'),
    c('big room', 'dance pop', 'edm', 'pop', 'pop dance'),
    c('modern rock', 'pop', 'rock'),
    c('orchestral soundtrack', 'video game music'),
    c('modern rock', 'permanent wave', 'punk', 'rock'),
    c('piano rock', 'pop')
    
  )
  
  # Flatten the list and calculate the frequency of each genre
  genres <- unlist(genres_list)
  genre_freq <- table(genres)
  
  # Create a data frame for plotting
  genres_data <- data.frame(genre = names(genre_freq), freq = as.integer(genre_freq))
  set.seed(123) # Set a seed for reproducibility
  genres_data$x <- runif(nrow(genres_data))
  genres_data$y <- runif(nrow(genres_data))
  
  output$genresBubbles <- renderPlot({
    ggplot(genres_data, aes(x = x, y = y, size = freq, label = genre)) +
      geom_point(color = "blue", alpha = 0.5) +
      geom_text(vjust = 0, nudge_y = 0.05, size = 3.5) +
      scale_size(range = c(5, 15)) +
      theme_void() +
      theme(legend.position = "none") +
      xlim(0, 1) +
      ylim(0, 1) +
      ggtitle("Top Genres Bubble Chart")
  })
  ####################################
  filtered_data1 <- reactive({
    artists_selected <- input$artists
    y_param <- input$y_param
    
    # Prepare a data frame to hold all the filtered data
    plot_data <- data.frame(Term = character(), Artist = character(), Value = numeric(), stringsAsFactors = FALSE)
    
    # Loop through each term to filter and collect data
    for (term in c("short_term", "medium_term", "long_term")) {
      term_data <- data4[[term]]
      term_artists <- term_data$names
      term_values <- if (y_param == "followers") term_data$followers else term_data$popularities
      
      # Filter based on selected artists
      filtered_indices1 <- which(term_artists %in% artists_selected)
      term_artists <- term_artists[filtered_indices1]
      term_values <- term_values[filtered_indices1]
      
      # Append to the plot_data data frame
      plot_data <- rbind(plot_data, data.frame(Term = term, Artist = term_artists, Value = term_values, stringsAsFactors = FALSE))
    }
    
    plot_data
  })
  
  # Render the interactive line plot
  output$line_plot <- renderPlotly({
    plot_data <- filtered_data1()
    
    plot_ly(data = plot_data, x = ~Term, y = ~Value, type = 'scatter', mode = 'lines+markers',
            color = ~Artist, colors = 'Paired') %>%
      layout(title = paste("Interactive Line Plot -", input$y_param))
  })
  # Render the interactive line plot

}

# Run the application
shinyApp(ui = ui, server = server)
