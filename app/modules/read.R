# read UI and Server

readUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    box(title = "Table and columns", width = 6, solidHeader = TRUE, status = "primary",
        selectInput(ns("tableName"), "Choose a table", character(0)),
        checkboxGroupInput(ns("select"), "Choose columns to read")
  ),
  box(title = "Rows (optional)", width = 6, solidHeader = TRUE, status = "info",
      selectInput(ns("filter"), "Choose column to filter on", NULL),
      checkboxGroupInput(ns("vals"), "Choose values to include")
  ),
  box(tableOutput(ns("res")), width = 12)
  )
  
}

read <- function(input, output, session, pool, reqTable) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observe({
    reqTable(input$tableName)
    cols <- db_query_fields(pool, input$tableName)
    updateCheckboxGroupInput(session, "select",
                             choices = cols, selected = cols, inline = TRUE)
  })
}