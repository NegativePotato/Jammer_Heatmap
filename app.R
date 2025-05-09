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
library(colourpicker)

# Todo 
# - Add autosave to online database (protect authetification tokens)
# - Add possibility to upload custom list of jammers 
# - Compute lane out + Distance out/Location
# - Left click is one team, right click is another team


# Authetication
# googledrive::drive_auth(email = "jammer.stats.app@gmail.com") 
# gs4_auth(email = "jammer.stats.app@gmail.com") #scope = "https://www.googleapis.com/auth/drive")


# testttssss
# drive_mkdir("Shiny_Stats")
# gs4_create(name = "test_2", sheets = NULL)

# folder_id <- drive_find(n_max = 10, pattern = "Shiny_Stats")$id
# drive_mv(file = "~/test_2", path = as_id(folder_id))

# Load data and images
track_img <- readPNG("track_background.png")
img_dims <- dim(track_img)
img_height <- img_dims[1]
img_width <- img_dims[2]

# List of jammers
mrd_jammer_list <- c("Amperslam - Amps",
                     "Beryl Roller - Beryl",
                     "Black Magick - Magick",
                     "Dalek - Dalek",
                     "Damnit Janet - DJ",
                     "Darkhorse - Neigh",
                     "Dil-Emma - Dil",
                     "Get Bucky - Bucky",
                     "Hawai'i KO - KO",
                     "Kat Scratch Fever - Fever",
                     "LaMarche Madness - Marche",
                     "Oui-Plash - Oui",
                     "Radiomacktive - Dio",
                     "Seam Ripper - Ripper", 
                     "Zooma Thurman - Zooma",
                     "Other")

# Define UI
ui <- page_sidebar(
  sidebar = sidebar(
    fluidRow(
      column(6, style = "padding-right: 2px;", 
             numericInput(
               inputId = "half_number",
               label = "Half #", 
               value = 1,
               step = 1,
               min = 1,
               max = 4
             )),
      column(6, style = "padding-left: 2px;", 
             numericInput(
               inputId = "jam_number",
               label = "Jam #", 
               value = 1,
               step = 1,
               min = 1
             )),
      style = "margin-top: 0px; margin-bottom: 0px, padding-bottom: 0px;"
    ),
    hr(style = "border-top: 1px solid #000000; margin-top: 0px; margin-bottom: 0px"),
    selectInput(inputId = "n_blockers_1", 
                label = "Number of Blockers", 
                choices = c(4,3,2,1), 
                selected = 4
    ),
    selectInput(
      inputId = "jammer_name_1",
      label = "Jammer Name 1",
      choices = mrd_jammer_list[-2],
      selected = mrd_jammer_list[1]
    ),
    textInput(
      inputId = "jammer_name_other_1",
      label = "If \"Other\", enter jammer name here",
      value = "MRD Jammer"
    ),
    hr(style = "border-top: 1px solid #000000; margin-top: 0px; margin-bottom: 0px"),
    selectInput(inputId = "n_blockers_2", 
                label = "Number of Blockers", 
                choices = c(4,3,2,1), 
                selected = 4
    ),
    selectInput(
      inputId = "jammer_name_2",
      label = "Jammer Name",
      choices = mrd_jammer_list[-1],
      selected = mrd_jammer_list[2]
    ),
    textInput(
      inputId = "jammer_name_other_2",
      label = "If \"Other\", enter jammer name here",
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
  # textOutput(outputId = "page_title"),
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
                ),
                fluidRow(
                  column(6, style = "padding-right: 2px;", 
                         colourInput(inputId = "team_color_1", label = "Team 1 Color", value = "#187BBE")),
                  column(6, style = "padding-left: 2px;", 
                         colourInput(inputId = "team_color_2", label = "Team 2 Color", value = "white"))
                )),
              card(card_header("Meta-Data Table"),
                   DT::DTOutput(outputId = "meta_data.df"))),
    nav_panel("Record Jams", 
              layout_columns(
                card(card_header("Tap the track where jammer 1 got out"),
                     plotOutput(outputId = "track_image_jammer_1",
                                click = "jammer_1_click", 
                                hover = hoverOpts(id = "jammer_1_hover", delay = 10)),
                     min_height = "500px"),
                card(card_header("Tap the track where jammer 2 got out"),
                     plotOutput(outputId = "track_image_jammer_2",
                                click = "jammer_2_click", 
                                hover = hoverOpts(id = "jammer_1_hover", delay = 10)),
                     min_height = "500px")
              ),
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
  
  jammer_data_1 <- reactiveValues(pass_nbr=NA, name="MRD Jammer 1", n_blockers=4)
  jammer_data_2 <- reactiveValues(pass_nbr=NA, name="MRD Jammer 2", n_blockers=4)
  
  jammer_1_coordinates_history <- reactiveValues(x=NA,y=NA)
  pass_data <- reactiveValues(jam_nbr=NA,
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
    jammer_data_1$pass_nbr <- 1
    jammer_data_2$pass_nbr <- 1
    jammer_data_1$name <- input$jammer_name_1
    jammer_data_2$name <- input$jammer_name_2
  }
  
  get_jammer_name <- function() {
    ifelse(input$jammer_name == "Other", input$jammer_name_other, input$jammer_name)
  }
  
  add_row_to_heatmap_df <- function(jammer_data, click_coords) {
    # Storage of click coordinates
    existing <- heatmap.df()
    
    jam_id <- sprintf("Jam_%i_%02i", input$half_number, 
                      pass_data$jam_nbr)
    timestamp_last_pass <- ifelse(jammer_data$pass_nbr == 1, 0, 
                                 tail(existing$time_stamp_in_game,1))
    
    new_row.df <- tibble(date = Sys.Date(), 
                         jam_id = jam_id,
                         time_stamp_absolute = Sys.time(),
                         jam_start_time = ifelse(is.null(pass_data$jam_start_time), 
                                                 Sys.time(), pass_data$jam_start_time),
                         time_stamp_in_game = pass_data$jam_elapsed_time,
                         time_for_this_pass = 
                           pass_data$jam_elapsed_time - 
                           timestamp_last_pass,
                         jammer = jammer_data$name,
                         half = input$half_number,
                         jam_number = pass_data$jam_nbr,
                         pass_number = jammer_data$pass_nbr,
                         initial_or_scoring = ifelse(pass_number == 1, "Initial", "Scoring"),
                         x_out = click_coords$x, 
                         y_out = click_coords$y, 
                         distance_form_start = "15",
                         n_blockers_in_pack = jammer_data$n_blockers,
                         jam_duration = 120,
                         note_taker = "Oui", 
                         notes = "None")
    
    print(heatmap.df())
    print(new_row.df)
    new_data <- rbind(existing, new_row.df)
    heatmap.df(new_data)
  }
  
  # --- UI Events --- #
  # Click on map to add a new pass
  observeEvent(input$jammer_1_click, {
    # Add the row to the heatmap dt
    jammer_data_1$n_blockers <- input$n_blockers_1
    add_row_to_heatmap_df(jammer_data_1, input$jammer_1_click)
    
    # Update the pass number
    jammer_data_1$pass_nbr <- jammer_data_1$pass_nbr + 1
  })
  
  observeEvent(input$jammer_2_click, {
    # Add the row to the heatmap dt
    jammer_data_2$n_blockers <- input$n_blockers_2
    add_row_to_heatmap_df(jammer_data_2, input$jammer_2_click)
    
    # Update the pass number
    jammer_data_2$pass_nbr <- jammer_data_2$pass_nbr + 1
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
    jammer_data_1$pass_nbr <- NA 
    jammer_data_2$pass_nbr <- NA 
  })
  
  observeEvent(input$jammer_name_1, {
    if(input$jammer_name_1 == "Other") {
      dropdown_list <- mrd_jammer_list
    } else {
      dropdown_list <- mrd_jammer_list[mrd_jammer_list != input$jammer_name_1]
    }
    
    updateSelectInput(inputId = "jammer_name_2", selected = input$jammer_name_2,
                      choices = dropdown_list)
  })
  
  observeEvent(input$jammer_name_2, {
      if(input$jammer_name_2 == "Other") {
        dropdown_list <- mrd_jammer_list
      } else {
        dropdown_list <- mrd_jammer_list[mrd_jammer_list != input$jammer_name_2]
      }
           
    updateSelectInput(inputId = "jammer_name_1", selected = input$jammer_name_1,
                      choices = dropdown_list)
  })
  
  # Timer logic
  observe({
    timer()
    if (timer_running()) {
      # Calculate elapsed time
      current_time <- Sys.time()
      start <- pass_data$jam_start_time
      
      if (!is.null(start)) {
        elapsed <- as.numeric(difftime(current_time, start, units = "secs"))
        
        # Check if the timer has reached the maximum duration
        if (elapsed >= max_duration) {
          timer_running(FALSE)
          pass_data$jam_elapsed_time <- 0  # Reset the timer
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
  output$track_image_jammer_1 <- renderPlot({
    existing <- heatmap.df()
    current_passes <- existing %>% 
      filter(jam_number == pass_data$jam_nbr & 
               jammer == jammer_data_1$name) %>% 
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
                                 y = y_out), 
                                 # fill = factor(pass_number)),
                   shape = 21,
                   size = 4,
                   fill = input$team_color_1,
                   color = "black")
        # scale_fill_manual(values = color_scale_points_track)
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
            legend.position="none", 
            plot.background = element_rect(fill = input$team_color_1)) +
      ggtitle('Ze Track')
  })
  
  output$track_image_jammer_2 <- renderPlot({
    existing <- heatmap.df()
    current_passes <- existing %>% 
      filter(jam_number == pass_data$jam_nbr & 
              jammer == jammer_data_2$name) %>% 
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
                                 y = y_out), 
                   # fill = factor(pass_number)),
                   shape = 21,
                   size = 4,
                   fill = input$team_color_2,
                   color = "black")
      # scale_fill_manual(values = color_scale_points_track)
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
            legend.position="none",
            plot.background = element_rect(fill = input$team_color_2)) +
      ggtitle('Ze Track')
  })
  
  output$timer_display <- renderText({
    seconds <- pass_data$jam_elapsed_time
    minutes <- seconds %/% 60
    remaining_seconds <- round(seconds %% 60)
    if(remaining_seconds == 60) {
      remaining_seconds <- 00
      minutes <- 2
    }
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
      write.csv(heatmap.df(), file, row.names = FALSE)}, 
    contentType = "application/octet-stream"
  )
  
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
             width = 1.05 * 7 * img_width/img_height, # Need to update that 
             height = 7, 
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
             plot = jammer_out_time_hist.plot(), 
             width = 6, # Need to update that 
             height = 3, 
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