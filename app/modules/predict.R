predictUI <- function(id) {
  ns <- NS(id)
  fluidPage(
  useShinyjs(),
  tagList(h2("New  Prediction"),
          h5("Enter a new information. Then we can predict it's current condition
             and it's approximate time to malfunction."),
          box(textInput(ns("machineName"), "Machine Name"),
              numericInput(ns("hoursUsage"), "Hours Used",NULL)
          ),
          box(textInput(ns("sensorType1"), "Sensor Type"),
              numericInput(ns("value1"), "Value",NULL),
              actionButton(ns("addSensor1"), "Add Another Sensor")
          ),
          uiOutput(ns("anotherSensor2")),
          uiOutput(ns("anotherSensor3")),
          actionButton(ns("predict"), label="Predict", class="pull-right btn-info")
  ))
}


predict <-function(input, output, session, pool) {

  track <- reactiveVal(0)
  
  observeEvent(input$addSensor1, {
    output$anotherSensor2 <- renderUI({
      tagList(box(textInput(session$ns("sensorType2"), "Sensor Type"),
                  numericInput(session$ns("value2"), "Value",NULL),
                  actionButton(session$ns("addSensor2"), "Add Another Sensor")
                 ))
    })
    hide('addSensor1')
  })
  
  observeEvent(input$addSensor2, {
    output$anotherSensor3 <- renderUI({
      tagList(box(textInput(session$ns("sensorType3"), "Sensor Type"),
                  numericInput(session$ns("value3"), "Value",NULL)
      ))
    })
    observeEvent(input$addSensor2, {hide('addSensor2')})  
  })
  
}