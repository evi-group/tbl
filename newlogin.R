library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(shinyjs)
library(glue)
library(sodium)

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: John  Password: pass1"),
                     br(),
                     tags$code("Username: Steven  Password: pass2"),
                     br(),
                     tags$code("Username: Lynn  Password: pass3")
                   ))
)
credentials = data.frame(
  username_id = c("John", "Steven", "Lynn"),
  passod   = sapply(c("pass1", "pass2", "pass3"),password_store),
  permission  = c("advanced", "basic", "basic"), 
  stringsAsFactors = F
)
header <- dashboardHeader( title = "TBL", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(
          menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("About Page", tabName = "About", icon = icon("th"))
        )
      }
      else{
        sidebarMenu(
          menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
        )
      }
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            fluidRow(
              box(width = 12, dataTableOutput('results'))
            )),
          tabItem(
            tabName ="About",
            h2("This is second tab")
          )
        )
      } 
      else {
        tabItem(
          tabName ="dashboard", class = "active",
          fluidRow(
            box(width = 4,
                h3(paste0("Welcome :"),input$userName))),
          fluidRow(
            box(width = 4,
                textInput(inputId = "test_name", label = "Test Name")),
            box(width = 2,
                actionButton(inputId = "load_test", label = "Load"))
          ),
          fluidRow( 
            box(width = 12, uiOutput("test_questions"))
          ))
      }
    }
    else {
      loginpage
    }
  })
  
  observeEvent(input$load_test, {
    test_temp <- paste0(input$test_name,".csv")
    print(test_temp)
    if(file.exists(test_temp)) {
      test <- read.csv(test_temp, stringsAsFactors = F)
      str(test)} else {
        print("Bad")
      }
    
  })
  
  output$test_questions <- renderUI({
    
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
}

shinyApp(ui, server)