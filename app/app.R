library(shiny)
library(shinydashboard)
library(dplyr)
library(dbplyr)
library(pool)
library(DBI)
library(rlang)
library(shinyjs)
library(tidyr)

source("modules/createEntry.R")
source("modules/welcomePage.R")
source("modules/read.R")
source("modules/predict.R")
source("modules/classifier.R")

db <- dbPool(RSQLite::SQLite(), dbname = "project.db")

tbls <- reactiveFileReader(500, NULL, "project.db",
                           function(x) dbListTables(db))

header <- dashboardHeader(title="Actions")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome Page",tabName= "welcome"),
    menuItem("Creat Entry",tabName= "createEntry"),
    menuItem("Read",tabName= "read"),
    menuItem("Predict", tabName="predict")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome", welcomePageUI("welcomePage-module")),
    tabItem(tabName = "createEntry", createEntryUI("createEntry-module")),
    tabItem(tabName = "read", readUI("read-module")),
    tabItem(tabName = "predict", predictUI("predict-module"))
  )
)

server <- function(input, output, session) {
  
  reqTable <- function(tableName) {
    tbls()
    req(tableName)
    req(tableName %in% dbListTables(db))
  }

  reqColInTable <- function(tableName, colName) {
    reqTable(tableName)
    req(colName)
    req(colName %in% db_query_fields(db, tableName))
  }

  callModule(welcomePage, "welcomePage-module", db)
  callModule(read, "read-module", db, reqTable,reqColInTable)
  callModule(createEntry, "createEntry-module", db)
  callModule(predict, "predict-module",db, reqColInTable)
}

ui <- dashboardPage(header, sidebar, body)
shinyApp(ui = ui, server = server)
