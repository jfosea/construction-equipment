# createEntry UI and Server

createEntryUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("create"), label="Create entry", class="pull-right btn-info"),
    selectInput(ns("tableName"), label="Choose a table", character(0)),
    uiOutput(ns("fields"))
  )
}


createEntry <- function(input, output, session, pool) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  fields <- reactive({
    pool %>% tbl(input$tableName) %>% head %>% collect %>% 
      lapply(type_sum) %>% unlist
  })
  
  output$fields <- renderUI({
    fieldNames <- names(fields())
    fieldTypes <- unname(fields())
    selections <- vector("list", length(fieldNames))
    for (i in seq_len(length(fieldNames))) {
      nm <- fieldNames[i]
      id <- paste0("field", nm)
      selections[[i]] <- box(width = 4,
                             switch(fieldTypes[i],
                                    int = numericInput(session$ns(id), nm, NULL),
                                    dbl = numericInput(session$ns(id), nm, NULL),
                                    chr = textInput(session$ns(id), nm, NULL)
                             )
      )
    }
    selections
  })
  
  observeEvent(input$create, {
    entryValues <- data.frame(stringsAsFactors = FALSE,
                              lapply(fields(), type.convert)
    )
    
    for (name in names(entryValues)) {
      id <- paste0("field", name)
      
      # check if all entries are filled in
      if (!isTruthy(input[[id]])) {
        showModal(modalDialog(
          title = "NULL value",
          "NULL values are not allowed for new entries",
          easyClose = TRUE, footer = NULL
        ))
        return()
      }
      
      entryValues[name] <- input[[id]]
      
      # check if machine id in senser input is an actual machine in the DB
      machineIds <- db %>% tbl("Machine") %>% select("id") %>% collect() %>% unlist() 
      if (name == "machine_id" & !is.element(entryValues[name], machineIds)) {
        showModal(modalDialog(
          title = "Can't find corresponding machine",
          "Senser entry must have an existing Machine ID",
          easyClose = TRUE, footer = NULL
        ))
      }
    }
    
    # check if UNIQUE constraint in entry is met
    tryCatch(
      {
        dbAppendTable(pool, input$tableName, entryValues)      
      },
      error=function(e) {
        showModal(modalDialog(
          title = "UNIQUE contraint failed",
          "id already in the database. Please enter a new id.",
          easyClose = TRUE, footer = NULL
        ))
      })
    
    
  })
  
  
}