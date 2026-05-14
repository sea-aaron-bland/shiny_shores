
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)

#load input error sets
load('Inputs/HudError.rda')
load('Inputs/RTKError.rda')
load('Inputs/TransectError.rda')
load('Inputs/GBseasonalError.rda')

#Error Functions ----

#for a position in meters (x), simulate the measurement of that position with HUD from imagery
getHUDError <- function(x, resolut, georect, seasonal = FALSE) {
  HUD <- sample(Hud.Error, 1)
  SEAS <- ifelse(seasonal, sample(GB.seasonal.error, 1), 0)
  R <- runif(1, min = resolut/-2, max = resolut/2)
  G <- rnorm(1, mean = 0, sd = georect)
  
  return(x + HUD + SEAS + R + G)
  
}

getHUDError.UAS.good <- function(x, seasonal = FALSE) {
  #0.025 m resolution, 0.25 m RMSE/Std Dev positioning error
  return(getHUDError(x, 0.025, 0.25, seasonal))
}

getHUDError.UAS.poor <- function(x, seasonal = FALSE) {
  #0.050 m resolution, 1.5 m RMSE/Std Dev positioning error
  return(getHUDError(x, 0.05, 1.5, seasonal))
}

#NAIP
#https://www.usgs.gov/centers/eros/science/usgs-eros-archive-aerial-photography-national-agriculture-imagery-program-naip

getHUDError.aerial.good <- function(x, seasonal = FALSE) {
  #0.5 m resolution
  #+/- 4 m, assuming 90% confidence
  z <- qnorm(0.1/2, lower.tail = FALSE) #get two-sided z-score for 90% confidence
  
  return(getHUDError(x, 0.5, 4/z, seasonal))
  
}

getHUDError.aerial.poor <- function(x, seasonal = FALSE) {
  #1 m resolution
  #+/- 5 m, assuming 90% confidence
  z <- qnorm(0.1/2, lower.tail = FALSE) #get two-sided z-score for 90% confidence
  
  return(getHUDError(x, 1, 5/z, seasonal))
  
}

#for a position in meters (x), simulate the measurement of that position with an RTK
getRTKError <- function(x, precision = 0.015, seasonal = FALSE) {
  RTK <- sample(RTK.Error, 1)
  SEAS <- ifelse(seasonal, sample(GB.seasonal.error, 1), 0)
  P <- rnorm(1, mean = 0, sd = precision)
  
  return(x + RTK + SEAS + P)
}

#Modelling Functions (used in publication, not used in the shiny app) ----

#for a given rate of shoreline change, number of years monitored, monitoring interval,
#create a number of estimates (reps) for the shoreline change rate
#using the provided function

#if transects is greater than 1, modify each slope by along-transect variability
#and average across all transects
scaModel <- function(rate, years = 10, inter = 2, reps = 1000, func = getRTKError, transects = 1, s = FALSE){
  
  date = seq(0, by = inter, to = years)
  pos = seq(from = 0, by = rate*inter, along.with = date)
  slopes <- numeric(reps)
  for(i in 1:reps){
    simSlopes <- numeric(transects)
    
    for(j in 1:transects){
      
      meas <- sapply(pos, func, seasonal = s)
      model <- lm(meas ~ date)
      
      #if using transect errors (transects > 1), sample a transect error from loaded distribution
      tranErr <- ifelse(transects == 1, 0, sample(transect.Error, 1))
      
      simSlopes[j] <- model$coefficients[2] + tranErr
      
    }
    slopes[i] <- mean(simSlopes) #average of slopes from each transect
  }
  
  return(slopes)
  
}


addModelResults <- function(rate, years, interval, reps = 1000, func = getRTKError, transects = 1, method, seasonal = FALSE){
  
  ModelSlopes <- scaModel(rate, years, interval, reps, func, transects, seasonal)
  
  out <- data.frame(Rate = rate, Years = years, Interval = interval, Method = method, Transects = transects,
                    Est = mean(ModelSlopes), 
                    Lower = quantile(ModelSlopes, 0.025), Upper = quantile(ModelSlopes, 0.975),
                    Range = quantile(ModelSlopes, 0.975) - quantile(ModelSlopes, 0.025),
                    Replicates = reps,
                    Seasonal = seasonal)
  
  return(out)
}

#App controls ----

help_label <- function(text, info) {
  tags$div(
    style = "display: flex; align-items: center; gap: 5px; margin-bottom: 2px;",
    tags$strong(text),
    popover(icon("question-circle"), info, placement = "right")
  )
}

#override the downloadButton function to handle Chrome browsers
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

## Define UI ----
ui <- page_sidebar(
  
  theme = bs_theme(version = 5) %>% 
    bs_add_rules(".popover-body { max-width: 800px; text-align: left;}"), # Sets width and justification for all tips
  
  # Application title
  title = "Shiny Shores: Shoreline Change Analysis Simulations",
  
  ### Sidebar to control the model runs ----
  sidebar = sidebar(
    id = "my_sidebar",
    width = 500,
    
    help_label("Shoreline data type:", 
               "Type of shoreline data used in the simulations.
               Depending on the selection (RTK/GPS, or the various imagery types),
               relevant sampling errors will be included based on repeated measurements
               of in-field RTK surveys or digitizations of shoreline imagery.
               This selection also toggles the display of relevant parameters below,
               as well as relevant defaults."),
    selectInput("selected_type",
                label = NULL,
                choices = c("RTK/GPS" = "rtk",
                            "UAV (poor)" = "uav_poor",
                            "UAV (good)" = "uav_good",
                            "Airplane (poor)" = "air_poor",
                            "Airplane (good)" = "air_good")
    ),
    
    # Only shows if 'type' is 'rtk'
    
    conditionalPanel(
      condition = "input.selected_type == 'rtk'",
      help_label("GPS precision (RMSE or std dev, meters)", 
                 "Horizontal precision of the GPS instrument, measured
                       as a standard deviation of repeated measurements, or
                       equivalently, the RMSE.
                       Default value of 0.15 m corresponds to reported
                       precision of the Emlid Reach RS2 RTK receiver."),
      numericInput("gps_precision",
                   label = NULL, 
                   value = 0.015)
    ),
    
    # Only shows if 'type' is not 'rtk'
    conditionalPanel(
      condition = "input.selected_type != 'rtk'",
      help_label("Image resolution (meters/pixel)", 
                 "Image resolution or pixel size."),
      numericInput("image_resolution", 
                   label = NULL, 
                   value = 0.5)
    ),
    conditionalPanel(
      condition = "input.selected_type != 'rtk'",
      help_label("Image georectification error (RMSE or std dev, meters)", 
                 "Horizontal precision of the image georectification, measured
                       as a standard deviation of repeated measurements, or
                       equivalently, the RMSE.
                       Some imagery surveys instead report a confidence for a given
                       percentage. You can estimate the RMSE by using the Z-score for
                       the given percentage. E.g., for a survey with positioning
                       within 5 meters at 90% confidence, the RMSE is
                       5 / qnorm((1-0.9)/2, lower.tail = FALSE)"),
      numericInput("image_georectification", 
                   label = NULL, 
                   value = 2.43)
    ),
    
    help_label("Include seasonal errors?", 
               "Should simulations include sampling across different seasons?
               An additional error term is added to each sampling event
               to represent the magnitude of marsh shoreline movement among
                seasons based on a dataset from the Grand Bay NERR, MS."),
    checkboxInput("seasonal", 
                  label = NULL, 
                  value = FALSE),
    
    help_label("Shoreline movement rate (m/yr)", 
               "The actual linear rate of shoreline movement being simulated.
               For exploratory analyses, consider obtaining an estimate of
               shoreline movement rates based on nearby systems or by taking a
               few measurements using a timeseries of publicly available imagery,
               e.g., using Google Earth."),
    numericInput("moverate", label = NULL, value = 2),
    
    help_label("Total duration of timeseries (years)", 
               "The maximum total duration of the shoreline measurement timeseries.
               Depending on the input interval, fewer than the maximum number of
               years may be simulated. E.g., a max of 10 years with a 3 year
               interval will result in a dataset that spans 9 years."),
    numericInput("yearspan", label = NULL, value = 6),
    
    help_label("Sampling interval (years)", 
               "The sampling interval between successive shoreline measurements,
               i.e., sampling occurs every X years.
               Depending on the input interval, fewer than the maximum number of
               years may be simulated. E.g., a max of 10 years with a 3 year
               interval will result in a dataset that spans 9 years."),
    numericInput("interval", label = NULL, value = 2),
    
    help_label("Transects monitored", 
               "Integer number of synthetic cross-shore transects used to track marsh movement,
               across a single site. When using more than 1 transect, an additional
               error term is included to represent among-transect variation in
               shoreline movement rates, based on a dataset of marsh shoreline movement
               rates from coastal Alabama.
               Increasing the number of transects tends to improve the precision
               of change rate estimates. I recommend using a regular transect
               spacing interval (e.g., every 10-20 meters along-shore) to determine
               the number of transects for a particular site, with the total number
               depending on the site size. I discourage increasing the number of
               transects by reducing the spacing interval, as this risks
               pseudoreplication of shoreline measurements."),
    numericInput("transects", label = NULL, value = 30),
    
    help_label("Percentile range (0-100)", 
               "The middle percentile of observations used to define the expected
               range of shoreline change estimates (lower to upper). This value
               is also used to define which scenarios provide suitable estimates.
               Suitable estimates are defined as having the lower bound
               greater than 0. E.g., for a 95% percentile range, if the bottom
               2.5% of estimates includes values less than 0 m/yr, then too many 
               simulated shoreline change rates estimate that the shoreline is moving in
               the wrong direction, therefore, the provided data are not suitable."),
    numericInput("success", label = NULL, value = 95),
    
    help_label("Model replicates", 
               "Number of times the simulation is repeated to generate a range of
               estimates, i.e., the number of observations in the plotted histogram. 
               This value and the number of transects will determine
               how long it will take to run the simulations. Avoid values greater
               than 1000, particularly for a large number of transects (greater than 100) "),
    numericInput("reps", label = NULL, value = 100),
    
    # button to run the simulation
    actionButton("run_sim", label = "Run Simulation", class = "btn-primary")
  ),
  
  ### main panel to display results ----
  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Shoreline Analysis Results",
      popover(
        icon("question-circle"),
        "This histogram shows the distribution of simulated slopes for the latest
        model run. The green line represents your input move rate, and the lower
        and upper bounds of the distribution based on your percentile range
        are drawn as blue lines. The suitability of this model is indicated as
        text in the top right based on the position of the lower bound in relation
         to 0.",
        placement = "left"
      )
    ),
    plotOutput("slope_histogram"),
    hr(),
    tableOutput("summary_table"),
    layout_column_wrap(
      width = 1/2,
      downloadButton("export_csv", "Save CSV"),
      actionButton("show_sidebar", "Modify Inputs", icon = icon("sliders"), class = "btn-secondary")
    )
  )
  
)
  

## Define server logic ----
server <- function(input, output, session) {
  
  ### Initialize the reactive variables ----
  current_reps <- reactiveVal(NULL)
  current_slopes <- reactiveVal(NULL)
  current_success <- reactiveVal(NULL)
  current_lower <- reactiveVal(NULL)
  current_upper <- reactiveVal(NULL)
  
  history_table <- reactiveVal(
    data.frame(GPS_Precision = numeric(0), Image_Resol = numeric(0), Image_Georect = numeric(0), 
               Rate = numeric(0), Years = numeric(0), Interval = numeric(0), Method = character(0), Seasonal = vector(mode = "logical", length = 0), 
               Transects = numeric(0),  Replicates = numeric(0), 
               Threshold = numeric(0), Suitable = vector(mode = "logical", length = 0),
               Lower = numeric(0), Upper = numeric(0)) )
  
  ### Watch for changes to change the 'image_resolution' default ----
  observeEvent(input$selected_type, {
    # Determine the new default based on selection
    new_val <- switch(input$selected_type,
                      "air_good" = 0.5,
                      "air_poor" = 1.0,
                      "uav_good" = 0.025,
                      "uav_poor" = 0.05,
                      0.5) # Fallback default
    
    # Update the UI element from the server
    updateNumericInput(session, "image_resolution", value = new_val) } )
  
  
  observeEvent(input$selected_type, {
    # Determine the new default based on selection
    new_val <- switch(input$selected_type,
                      "air_good" = 2.43,
                      "air_poor" = 3.04,
                      "uav_good" = 0.25,
                      "uav_poor" = 1.5,
                      2.43) # Fallback default
    
    # Update the UI element from the server
    updateNumericInput(session, "image_georectification", value = new_val) 
  })
  
  ### Run the simulation ----
  observeEvent(input$run_sim, {
    
    #initialize static values
    rate = input$moverate
    years = input$yearspan
    inter = input$interval
    reps = input$reps
    transects = input$transects
    seasonalVal = input$seasonal
    succ = input$success
    
    date = seq(0, by = inter, to = years)
    pos = seq(from = 0, by = rate*inter, along.with = date)
    slopes <- numeric(reps)
    
    type <- input$selected_type
    
    # start the progress bar
    withProgress(message = 'Running Simulation...', value = 0, {
      
      for(i in 1:reps){
        
        # 2. Update progress incrementally
        # This calculates the step size (1 / total reps)
        if (i %% 100 == 0) {
          incProgress(amount = 100/reps, detail = paste("Replicate", i, "of", reps)) }
        
        simSlopes <- numeric(transects)
        
        for(j in 1:transects){
          
          meas <- sapply(pos, 
                         function(x){
                           if(type == 'rtk'){
                             getRTKError(x, precision = input$gps_precision, seasonal = seasonalVal)
                           } else{
                             getHUDError(x, 
                                         resolut = input$image_resolution, 
                                         georect = input$image_georectification, 
                                         seasonal = seasonalVal)
                           }
                           
                         })
          model <- lm(meas ~ date)
          
          #if using transect errors (transects > 1), sample a transect error from loaded distribution
          tranErr <- ifelse(transects == 1, 0, sample(transect.Error, 1))
          
          simSlopes[j] <- model$coefficients[2] + tranErr
          
        }
        
        slopes[i] <- mean(simSlopes) #average of slopes from each transect
        
      } 
      
    })
    
    current_reps(reps)
    current_slopes(slopes)
    current_success(quantile(slopes, (1-(succ/100))/2)>=0)
    current_lower(quantile(slopes, (1-(succ/100))/2))
    current_upper(quantile(slopes, (1+(succ/100))/2))
    
    # Create a new row of results
    
    
    new_row <- data.frame(
      GPS_Precision = ifelse(type == 'rtk', input$gps_precision, NA), 
      Image_Resol = ifelse(type == 'rtk', NA, input$image_resolution), 
      Image_Georect = ifelse(type == 'rtk', NA, input$image_georectification),
      Rate = rate, Years = years, Interval = inter, Method = type, Seasonal = seasonalVal, 
      Transects = transects,  Replicates = reps, 
      Threshold = succ, Suitable = current_success(),
      Lower = current_lower(), Upper = current_upper()
    )
    
    # Get the current table, bind the new row, and save it back
    old_data <- history_table()
    updated_data <- rbind(old_data, new_row)
    history_table(updated_data)
    
    sidebar_toggle("my_sidebar", open = FALSE)
    
    
  })
  
  ### Render the Histogram ----
  output$slope_histogram <- renderPlot({
    req(current_slopes()) # Only plot if data exists
    
    # Calculate range and include 0 and a small buffer
    s_range <- range(current_slopes())
    plot_min <- min(s_range[1] - 0.2, -0.2)
    plot_max <- max(s_range[2] + 0.2, 0.2) # Ensure positive side also has room
    
    par(mar = c(5, 4, 7, 2)) 
    
    hist(current_slopes(), 
         main = paste("Slopes for latest model setup (", current_reps(), " runs)"),
         xlab = "Shoreline Change Rate (m/yr)",
         col = "skyblue", border = "white",
         xlim =  c(plot_min, plot_max) )
    abline(v = 0, col = "red", lwd = 2, lty = 2)
    abline(v = current_lower(), col = "blue", lwd = 2, lty = 2)
    abline(v = current_upper(), col = "blue", lwd = 2, lty = 2)
    abline(v = input$moverate, col = "darkgreen", lwd = 2, lty = 2)
    
    # 3. Add text near the upper right (e.g., 95% of max X and Y)
    coords <- par("usr")
    text(x = coords[2] * 0.95, y = coords[4] * 0.95, 
         labels = ifelse(current_success(),
                         "Suitable", "Not suitable"), 
         adj = c(1, 1),  # Right and top aligned relative to (x,y)
         col = ifelse(current_success(),
                      "blue", "red"),
         font = 2)
    text(x = input$moverate, 
         y = coords[4] + (coords[4] - coords[3]) * 0.08, # 2% above the top limit
         labels = paste("Input movement rate:\n", input$moverate, "m/yr"), 
         adj = c(0.5, 0), # Centered horizontally, bottom-aligned vertically
         col = "darkgreen",
         font = 2,
         xpd = TRUE) # CRITICAL: Allows drawing outside the plot area
    text(x = current_lower(), 
         y = coords[4] + (coords[4] - coords[3]) * 0.02, # 2% above the top limit
         labels = "Lower", 
         adj = c(0.5, 0), # Centered horizontally, bottom-aligned vertically
         col = "blue",
         font = 2,
         xpd = TRUE) # CRITICAL: Allows drawing outside the plot area
    text(x = current_upper(), 
         y = coords[4] + (coords[4] - coords[3]) * 0.02, # 2% above the top limit
         labels = "Upper", 
         adj = c(0.5, 0), # Centered horizontally, bottom-aligned vertically
         col = "blue",
         font = 2,
         xpd = TRUE) # CRITICAL: Allows drawing outside the plot area
  })
  
  ### display the table of model runs ----
  output$summary_table <- renderTable({
    history_table()
  }, digits = 3)
  
  
  ### download the table of model runs ----
  output$export_csv <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(history_table(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### toggle the sidebar (model parameters) ----
  observeEvent(input$show_sidebar, {
    sidebar_toggle("my_sidebar", open = TRUE)
  })
}

# Run the application 
shinyApp(ui, server)

shinylive::export(
  appdir = "D:/.shortcut-targets-by-id/1avidn3l8UPCyW6_A-M00EH-A2Q3J-f3P/BakerLabDrive/LabProjects/2757RB_RESTORE_LivingShorelines/Data/Shoreline_Data/Shoreline Methods Analyses/SCA_Sim/sca_shiny",
  destdir = "C:/Users/bland/Desktop/shiny_shores/docs"
)
