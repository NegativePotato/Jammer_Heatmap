# App for the Jammer Heatmap

# Load packages

library(shiny)
library(bslib)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(png)
library(googledrive)
library(googlesheets4)
library(DT)
library(RColorBrewer)

# Authetication
googledrive::drive_auth(email = "jammer.stats.app@gmail.com") 
gs4_auth(email = "jammer.stats.app@gmail.com") #scope = "https://www.googleapis.com/auth/drive")


# testttssss
# drive_mkdir("Shiny_Stats")
# gs4_create(name = "test_2", sheets = NULL)

# folder_id <- drive_find(n_max = 10, pattern = "Shiny_Stats")$id
# drive_mv(file = "~/test_2", path = as_id(folder_id))

# Load data and images
track_img <- readPNG("track_background.png")


# Define UI

ui <- page_sidebar(
  sidebar = sidebar(
    numericInput(
      inputId = "half_number",
      label = "Half number", 
      value = 1,
      step = 1,
      min = 1,
      max = 4
    ),
    numericInput(
      inputId = "jam_number",
      label = "Jam number", 
      value = 1,
      step = 1,
      min = 1
    ),
    selectInput(inputId = "n_blockers", 
                label = "Number of Blockers", 
                choices = c(4,3,2,1), 
                selected = 4
    ),
    textInput(
      inputId = "jammer_name",
      label = "Jammer Name",
      value = "MRD Jammer"
    )
    # textInput(
    #   inputId = "league_block",
    #   label = "League (blocking team): ",
    #   value = "League Block"
    # ),
    # textInput(
    #   inputId = "team_block",
    #   label = "Blocking Team:",
    #   value = "Team Block"
    # )
  ),
  textOutput(outputId = "page_title"),
  navset_card_underline(
    nav_panel("Instructions & Meta-Data", 
              card(
                textOutput(outputId = "card_1_title"),
                fluidRow(
                  column(6, style = "padding-right: 2px;", 
                         selectInput(inputId = "game_type", 
                                     label = "Game Type", 
                                     choices = c("Scrimmage - Block 1", 
                                                 "Scrimmage - Block 2",
                                                 "Scrimmage - Block 3",
                                                 "Bout - Regulation", 
                                                 "Bout - Sanctioned"))),
                  column(6, style = "padding-left: 2px;", 
                         textInput(
                           inputId = "location",
                           label = "Location",
                           value = "AEC")),
                ),
                fluidRow(
                  column(6, style = "padding-right: 2px;", 
                         textInput("team_1", "Team 1", value = "MRD")),
                  column(6, style = "padding-left: 2px;", 
                         textInput("team_2", "Team 2", value = "MRD"))
                )),
              card(card_header("Meta-Data Table"),
                   DT::DTOutput(outputId = "meta_data.df"))),
    nav_panel("Record Jams", 
              card(card_header("Tap the track where the jammer got out"),
                   plotOutput(outputId = "track_image",
                              click = "jammer_1_click", 
                              hover = hoverOpts(id = "jammer_1_hover", delay = 10)),
                   min_height = "500px"),
              card(card_header("Controls"),
                   fluidRow(
                     column(4, actionButton(inputId = "start_jam_btn",
                                            label = "Start Jam", width = "100%")),
                     column(4, actionButton(inputId = "end_jam_btn",
                                            label = "End Jam", width = "100%")),
                     column(4, div(
                       style = "font-size: 48px; font-weight: bold; text-align: center; margin-top: 20px;",
                       textOutput("timer_display")))))),
              # card(card_header("Jammer Out Heatmap Data"),
              #      DT::DTOutput(outputId = "jammer_out_data.df"))),
    nav_panel("Dataset Fullscreen", 
              card(card_header("Jammer Out Heatmap Data - Full Screen"),
                   DT::DTOutput(outputId = "heatmap_data.df")),
              card(downloadButton(outputId = "download_heatmap_data", 
                                  label = "Download Heatmap Data"))),
    nav_panel("Data Visualisation", 
              card(card_header("All data where the jammer got out so far"),
                   plotOutput(outputId = "where_jammer_got_out_all"),
                   downloadButton(outputId = "download_heatmap_dots", 
                                  label = "Download the map of jammer-out locations"),
                   min_height = "500px"),
              card(card_header("Times when the jammer got out so far"),
                   plotOutput(outputId = "time_jammer_got_out_all"),
                   downloadButton(outputId = "download_out_time_histogram", 
                                  label = "Download the histogram of jammer-out times"),
                   min_height = "500px")),
  )
)


# Define server

server <- function(input, output, session) {
  # --- Variables --- #
  metadata.df <- reactive(
    data.frame("Date" = Sys.Date(), 
           "Game Type" = input$game_type, 
           "Location" = input$location, 
           "Team 1" = input$team_1,  
           "Team 2" = input$team_2, 
           "Notes" = ""))
  
  heatmap.df <- reactiveVal(
    tibble(date = character(), 
           jam_id = character(),
           time_stamp_absolute = date(),
           jam_start_time = date(),
           time_stamp_in_game = numeric(),
           time_for_this_pass = numeric(),
           jammer = character(),
           half = numeric(),
           jam_number = numeric(),
           pass_number = numeric(),
           initial_or_scoring = character(),
           x_out = numeric(), 
           y_out = numeric(), 
           distance_form_start = numeric(),
           n_blockers_in_pack = numeric(),
           jam_duration = numeric(),
           note_taker = character(), 
           notes = character()))
  
  jammer_1_coordinates_history <- reactiveValues(x=NA,y=NA)
  pass_data <- reactiveValues(jam_nbr=NA,pass_nbr=NA,
                              jam_start_time=NULL,  
                              jam_elapsed_time=0)
  timer_running <- reactiveVal(FALSE)
  max_duration <- 120
  timer <- reactiveTimer(1000) # timer oberver
  
  coul <- brewer.pal(4, "PuOr") 
  color_scale_points_track <- colorRampPalette(coul)(25)
  
  jammer_out_location_all_dots.plot <- reactiveVal(ggplot())
  jammer_out_time_hist.plot <- reactiveVal(ggplot())
  
  # --- Functions --- #
  set_pass_data_at_jam_start <- function() {
    # Set the pass data at jam start
    if(is.na(pass_data$jam_nbr)) {
      pass_data$jam_nbr <- 1
    } else {
      pass_data$jam_nbr <- pass_data$jam_nbr + 1
    }
    pass_data$pass_nbr <- 1
  }
  
  add_row_to_heatmap_df <- function() {
    # Storage of click coordinates
    existing <- heatmap.df()
    
    jam_id <- sprintf("Jam_%i_%02i_%02i", input$half_number, 
                      pass_data$jam_nbr, pass_data$pass_nbr)
    timestamp_last_jam <- ifelse(pass_data$pass_nbr == 1, 0, 
                                 tail(existing$time_stamp_in_game,1))
    
    new_row.df <- tibble(date = Sys.Date(), 
                         jam_id = jam_id,
                         time_stamp_absolute = Sys.time(),
                         jam_start_time = ifelse(is.null(pass_data$jam_start_time), 
                                                 Sys.time(), pass_data$jam_start_time),
                         time_stamp_in_game = pass_data$jam_elapsed_time,
                         time_for_this_pass = 
                           pass_data$jam_elapsed_time - 
                           timestamp_last_jam,
                         jammer = input$jammer_name,
                         half = input$half_number,
                         jam_number = pass_data$jam_nbr,
                         pass_number = pass_data$pass_nbr,
                         initial_or_scoring = ifelse(pass_number == 1, "Initial", "Scoring"),
                         x_out = input$jammer_1_click$x, 
                         y_out = input$jammer_1_click$y, 
                         distance_form_start = "15",
                         n_blockers_in_pack = input$n_blockers,
                         jam_duration = 120,
                         note_taker = "Oui", 
                         notes = "None")
    
    new_data <- rbind(existing, new_row.df)
    heatmap.df(new_data)
    print(heatmap.df())
  }
  
  # --- UI Events --- #
  # Click on map to add a new pass
  observeEvent(input$jammer_1_click, {
    jammer_1_coordinates_history$x <- 
      c(jammer_1_coordinates_history$x,input$jammer_1_click$x)
    jammer_1_coordinates_history$y <- 
      c(jammer_1_coordinates_history$y,input$jammer_1_click$y)
    
    # Add the row to the heatmap dt
    add_row_to_heatmap_df()
    
    # Update the pass number
    pass_data$pass_nbr <- pass_data$pass_nbr + 1
  })
  
  observeEvent(input$start_jam_btn, {
    set_pass_data_at_jam_start()
    
    if (!timer_running()) {
      timer_running(TRUE)
      pass_data$jam_start_time <- Sys.time()
      print(pass_data$jam_start_time)
    }
  })
  
  observeEvent(input$end_jam_btn, {
    timer_running(FALSE)
    pass_data$jam_elapsed_time <- 0  # Reset the timer
    pass_data$jam_start_time <- NULL
    updateTextInput(inputId = "jam_number", value = pass_data$jam_nbr + 1)
    pass_data$pass_nbr <- NA 
  })
  
  # Timer logic
  observe({
    timer()
    if (timer_running()) {
      # Calculate elapsed time
      current_time <- Sys.time()
      start <- pass_data$jam_start_time
      
      if (!is.null(start)) {
        elapsed <- as.numeric(current_time - start)
        
        # Check if the timer has reached the maximum duration
        if (elapsed >= max_duration) {
          timer_running(FALSE)
          elapsed_time(0)  # Reset the timer
          pass_data$jam_start_time <- NULL # Reset the start time
        } else {
          pass_data$jam_elapsed_time <- elapsed
        }
      }
    }
  })
  
  
  # --- UI Outputs --- #
  # Instruction tab
  output$card_1_title <- renderText({
    # paste(input$jammer_1_name, input$jammer_1_number, sep = ", ")
    sprintf("Here be instructions")
  })
  
  output$meta_data.df <- renderDT({
    metadata.df()
  })
  
  output$jammer_1_coordinates <- renderText({
    paste(input$jammer_1_hover$x, input$jammer_1_hover$y, sep = ", ")
  })
  
  output$card_2_title <- renderText({
    paste(input$jammer_2_name, input$jammer_2_number, sep = ", ")
  })
  
  output$page_title <- renderText({
    sprintf("%s, %s (Jamming) vs %s, %s (Blocking)",
            input$team_jam, input$league_jam,
            input$team_block, input$league_block)
  })
  
  output$jammer_1_data_str <- renderUI({
    HTML(paste("(", jammer_1_coordinates_history$x, ", ", jammer_1_coordinates_history$y, ")", 
               sep = "", collapse = "<br/>" ))
  })
  
  
  # Track Tab
  output$track_image <- renderPlot({
    existing <- heatmap.df()
    current_passes <- existing %>% 
      filter(jam_number == pass_data$jam_nbr) %>% 
      mutate()
    
    img_dims <- dim(track_img)
    img_height <- img_dims[1]
    img_width <- img_dims[2]
    
    track.plot <- 
      # ggplot(data.frame(x = c(0,img_width), y = c(0,img_height))) +
      ggplot() +
        background_image(track_img) +
        lims(x = c(0,img_width), y = c(0,img_height))
    
    if(nrow(current_passes) > 0 & !is.null(pass_data$jam_start_time)) {
      track.plot <- track.plot + 
        geom_point(data = current_passes,
                   mapping = aes(x = x_out, 
                                 y = y_out, 
                                 fill = factor(pass_number)),
                   shape = 21,
                   size = 4,
                   color = "black")+ 
        scale_fill_manual(values = color_scale_points_track)
    }
    
    if(is.null(pass_data$jam_start_time)) {
      track.plot <- track.plot + 
        geom_text(aes(x = img_width/2, y = img_height/2, 
                      label = "JAM TIMER STOPPED\nSTART TIMER TO START RECORDING"), 
                  fontface = "bold", 
                  color = "red", 
                  size = 10)
    }
    
    track.plot + 
      theme_void() + 
      theme(axis.title = element_blank(),
            legend.position="none") +
      ggtitle('Ze Track')
  })
  
  output$timer_display <- renderText({
    seconds <- pass_data$jam_elapsed_time
    minutes <- seconds %/% 60
    remaining_seconds <- seconds %% 60
    sprintf("%02.0f:%02.0f", minutes, remaining_seconds)
  })
    
  # Dataset Tab
  output$heatmap_data.df <- renderDT({
    heatmap.df()
  })
  
  output$download_heatmap_data <- downloadHandler(
    filename = function(){
      sprintf("%s_%s_%s_%s.csv", 
              format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), 
              input$game_type, 
              input$team_1, 
              input$team_2)},
    content = function(file){
      write.csv(heatmap.df(), file, row.names = FALSE)
  })
  
  # Show data tab
  output$where_jammer_got_out_all <- renderPlot({
    existing <- heatmap.df()
    
    img_dims <- dim(track_img)
    img_height <- img_dims[1]
    img_width <- img_dims[2]
    
    track.plot <- 
      # ggplot(data.frame(x = c(0,img_width), y = c(0,img_height))) +
      ggplot() +
      background_image(track_img) +
      lims(x = c(0,img_width), y = c(0,img_height)) + 
      geom_point(data = existing,
                 mapping = aes(x = x_out, 
                               y = y_out, 
                               shape = initial_or_scoring),
                 size = 4,
                 fill = "red",
                 color = "black")+ 
      scale_shape_manual(values = c("Initial" = 21, "Scoring" = 8)) + 
      theme_void() + 
      theme(axis.title = element_blank(),
            legend.position="none") +
      ggtitle('Where the jammer gets out during this Session')
    
    jammer_out_location_all_dots.plot(track.plot)
    
    track.plot
  })
  
  output$download_heatmap_dots <- downloadHandler(
    filename = function(){
      sprintf("Jammer-Out-Location_%s_%s_%s_%s.png", 
              format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), 
              input$game_type, 
              input$team_1, 
              input$team_2)},
    content = function(file){
      ggsave(file, 
             plot = jammer_out_location_all_dots.plot(), 
             width = 1.05*input$plot_height * uwm_bb.width/uwm_bb.height, # Need to update that 
             height = input$plot_height, 
             dpi = 300)
    }
  )
  
  output$time_jammer_got_out_all <- renderPlot({
    existing <- heatmap.df()
    
    img_dims <- dim(track_img)
    img_height <- img_dims[1]
    img_width <- img_dims[2]
    
    jammer_out_time.plot <- 
      # ggplot(data.frame(x = c(0,img_width), y = c(0,img_height))) +
      ggplot() +
      geom_histogram(data = existing,
                     mapping = aes(x = time_for_this_pass, 
                                   fill = initial_or_scoring),
                 color = "black")+
      facet_grid(~ initial_or_scoring) +
      scale_fill_manual(values = c("Initial" = "red", "Scoring" = "black")) + 
      theme_bw() + 
      theme(axis.title = element_blank(),
            legend.position="none") +
      ggtitle('Jammer Data This Session')
    
    jammer_out_time_hist.plot(jammer_out_time.plot)
    
    jammer_out_time.plot
  })
  
  output$download_out_time_histogram <- downloadHandler(
    filename = function(){
      sprintf("Jammer-Out-Time_%s_%s_%s_%s.png", 
              format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), 
              input$game_type, 
              input$team_1, 
              input$team_2)},
    content = function(file){
      ggsave(file, 
             plot = time_jammer_got_out_all_hist.plot(), 
             width = 1.05*input$plot_height * uwm_bb.width/uwm_bb.height, # Need to update that 
             height = input$plot_height, 
             dpi = 300)
    })
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)


# Todo 
# - Aspect Ratio constant for the track. 
# - Put the info for jammers in the ggtitle
# - Create a google account for this app
# - Save data to a google sheet
# - Dynamically add/remove Jammer info lines
# - Have different colors for each jammer 
# - Add a tab for the blocking team
# - Save information for jamming AND blocking team. 
# 