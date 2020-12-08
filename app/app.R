library(shiny)
library(shinydashboard)
library(dplyr)
library(dbplyr)
library(pool)
library(DBI)
library(rlang)

source("modules/createEntry.R")
source("modules/welcomePage.R")

db <- dbPool(RSQLite::SQLite(), dbname = "project.db")

tbls <- reactiveFileReader(500, NULL, "project.db",
                           function(x) dbListTables(db)
)

tblsheader <- dashboardHeader(title="Construction Equipment")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome Page",tabName= "welcome"),
    menuItem("Creat Entry",tabName= "createEntry")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome", welcomePageUI("welcomePage-module")),
    tabItem(tabName = "createEntry", createEntryUI("createEntry-module"))
  )
)


server <- function(input, output, session) {
  callModule(welcomePage, "welcomePage-module")
  callModule(createEntry, "createEntry-module", db)
}


ui <- dashboardPage(header, sidebar, body)
shinyApp(ui = ui, server = server)
 