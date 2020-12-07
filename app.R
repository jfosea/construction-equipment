library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tibble)
library(pool)
library(rlang)

db <- dbPool(RSQLite::SQLite(), dbname = "project.db")

header <- dashboardHeader(title="Construction Equipment")

sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem("Welcome Page",tabName= "welcome"),
  menuItem("Add Entry",tabName= "add_entry"),
  menuItem("Update Entry", tabName = "update_entry")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome",h2("Welcome to the Construction Equipment Website!")),
    tabItem(tabName = "add_entry"),
    tabItem(tabName = "update_entry",h2("Update Entry"))
  )
)


server <- function(input, output, session) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item")
    )
  })
}


ui <- dashboardPage(header, sidebar, body)
shinyApp(ui = ui, server = server)

