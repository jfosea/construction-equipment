# welcomePage UI and Server

welcomePageUI <- function(id) {
  ns <- NS(id)
  tagList(h2("Welcome!"), 
          h5("This application stores information of different machinery 
          data and their corresponding senser data. You can create new entries
          for a new machinery or a new senser for an existing machinery.
          We can also predict the condition of a machinery equipment 
          provided its senser data. Here are the tables in the database."),
          uiOutput(ns("tables")))
}

welcomePage <- function(input, output, session, pool) {
  output$tables <- renderUI({
    allTables <- tbls()
    bullets <- list()
    for (i in seq_len(length(allTables))) {
      tblName <- allTables[[i]]
      fieldNames <- db_query_fields(pool, tblName)
      nRows <- pool %>% tbl(tblName) %>% collect() %>% nrow()
      bullets[[i]] <- tags$li(paste0(
        allTables[[i]], ": ", nRows, " rows. Field names: ",
        paste(fieldNames, collapse = ", "), "."
      ))
    }
    tags$ul(bullets)
  })
}