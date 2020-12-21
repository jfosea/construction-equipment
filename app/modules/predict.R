predictUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    tagList(h2("New  Prediction"),
            h5("Enter a new information. Then we can predict it's current condition
             and it's approximate time to malfunction."),
            box(selectInput(ns("machineName"), label="Choose a Machine", character(0)),
                textInput(ns("hoursUsage"), "Hours Used",NULL),
                textInput(ns("type1"), "Filter",NULL),
                textInput(ns("type2"), "Thickness",NULL),
                textInput(ns("type3"), "Pressure",NULL)
            ),
            box(h2("Prediction"),
                textOutput(ns("timeToMalf")),
                textOutput(ns("cond"))),
            actionButton(ns("predict"), label="Predict", class="pull-right btn-info")
    ))
}


predict <-function(input, output, session, pool, reqColInTable) {
  
  observe({
    reqColInTable("Machine", "name")
    df <- as_data_frame(pool %>% tbl("Machine") %>% select("name"))
    allUniqueVals <- unique(df[["name"]])
    updateSelectInput(session, "machineName",
                      choices = allUniqueVals)
  })
  
  observeEvent(input$predict,{
    values <- reactiveValuesToList(input)
    
    for (i in values) {
      # check if all entries are filled in
      if (i == ""){
        showModal(modalDialog(
          title = "NULL value",
          "NULL values are not allowed for new entries",
          easyClose = TRUE, footer = NULL
        ))
        return()
      }
    }
    
    machine_id <- 9999
    filter <- as.numeric(values$type1)
    thickness <- as.numeric(values$type2)
    pressure <- as.numeric(values$type3)
    name <- values$machineName
    hours_usage <- as.numeric(values$hoursUsage)
    time_to_malfunction <- NA
    condition <- NA
    
    valuesF <- data.frame(machine_id, filter, thickness, pressure, name, hours_usage, time_to_malfunction, condition)
    pred <- predictBoth(pool, valuesF)
    a <- reactiveVal(pred[[1]])
    b <- pred[[2]]
    if (b == 1) {
      b = "Bad"
    } else if (b == 2) {
      b = "Moderate"
    } else {
      b = "Excellent"
    }
    
    output$timeToMalf <- renderText({paste("Expected time to malfunction \n in hours: ", a())})
    output$cond <- renderText({paste("Estimated condition: ", b)})
  })
  
  
}