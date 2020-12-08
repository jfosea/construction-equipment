library(shiny)
library(shinydashboard)
library(dplyr)
library(dbplyr)
library(pool)
library(DBI)
library(rlang)

source("modules/createEntry.R")
source("modules/welcomePage.R")
source("modules/read.R")

db <- dbPool(RSQLite::SQLite(), dbname = "project.db")


tbls <- reactiveFileReader(500, NULL, "project.db",
                           function(x) dbListTables(db))


header <- dashboardHeader(title="Actions")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome Page",tabName= "welcome"),
    menuItem("Creat Entry",tabName= "createEntry"),
    menuItem("Read",tabName= "read")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome", welcomePageUI("welcomePage-module")),
    tabItem(tabName = "createEntry", createEntryUI("createEntry-module")),
    tabItem(tabName = "read", readUI("read-module"))
  )
)


server <- function(input, output, session) {
  
  reqTable <- function(tableName) {
    tbls()
    req(tableName)
    req(tableName %in% dbListTables(db))
  }
  
  callModule(welcomePage, "welcomePage-module", db)
  callModule(createEntry, "createEntry-module", db)
  callModule(read, "read-module", db, reqTable)
}


ui <- dashboardPage(header, sidebar, body)
shinyApp(ui = ui, server = server)
 