# TBL app 01/06/2020
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(shinyjs)
library(sodium)
library(shinyalert)

user_answer_list <- data.frame(user_test = "Steven Romriell",stringsAsFactors = FALSE)
master_answer_list <- data.frame(master = "Master Answers", stringsAsFactors = FALSE)
question_number <- 0

credentials = data.frame(
  unique_id = c(1, 2),
  full_name = c("TBL", "user"),
  username_id = c("TBL", "user"),
  passod   = sapply(c("pass1", "pass2"), sodium::password_store),
  permission  = c("advanced", "basic"),
  stringsAsFactors = F, row.names = NULL)


header <- dashboardHeaderPlus ( title = "TBL", uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))  
body <- dashboardBody(
  fluidPage(tabItems(
    tabItem(tabName = "login",
            shinyjs::useShinyjs(),
            div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                wellPanel(
                  tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#808080; font-weight:600;"),
                  textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                  passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                  br(),
                  div(
                    style = "text-align: center;",
                    actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                    br(),
                    br(),
                    br()
                  ),
                  div(
                    style = "text-align: center;",
                    actionButton("add_user", "Create Account", style = "color: white; background-color:#808080;
                                 padding: 10px 15px; width: 200px; cursor: pointer;
                                 font-size: 16px; font-weight: 600;"),
                    br(),
                    includeScript("returnClick.js"),
                    useShinyalert(),
                    useShinyjs()
                  )
                )
            )),
    tabItem(tabName = "instructor",
            fluidPage(
              
            )
    ),
    tabItem(tabName = "welcome", class = "active",
            fluidPage(
              h3("Welcome"),
              column(width = 6,
                     fluidRow(
                       widgetUserBox(
                         title = "Steven Romriell",
                         width = 12,
                         type = 2,
                         color = "green")
                     ),
                     fluidRow(
                       box(width = 12, height = 200,
                           verbatimTextOutput(outputId = "question_num"),
                           textOutput(outputId = "indiv_question"))
                     ),
                     fluidRow(
                       box(width = 12,
                           radioButtons(inputId = "answer",
                                        label = "Answers", 
                                        selected = "E",
                                        choices = c("A", "B", "C", "D", "E"))            
                       )
                     ),
                     fluidRow(
                       box(width = 4,
                           actionButton(inputId = "indiv_next_bttn", label = "Next")),
                       box(width = 4, 
                           actionButton(inputId = "indiv_Submit_bttn", label = "Submit"))
                     )
              ),
              column(width = 4, offset = 1,
                     box(width = 8,
                         selectInput(inputId = "indiv_test", label = "Tests Available",
                                     choices = c("test1", "test2"), selected = NULL),
                         align = "center"),
                     box(width = 4, height = '100px',
                         actionButton(inputId = "indiv_load_test", label = "Load Test",
                                      width = '100%', style='height:80px'))
              )
            )
            
    )
  )
  
  )
)

ui <- dashboardPagePlus (header, sidebar, body)

server <- function(input, output, session) {
  login <- FALSE
  USER <- reactiveValues(login = login)
  
  observeEvent(input$login,{
    if (USER$login == FALSE) {
      Username <- isolate(input$userName)
      Password <- isolate(input$passwd)
      if(length(which(credentials$username_id==Username))==1) { 
        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
        pasverify <- password_verify(pasmatch, Password)
        if(pasverify) {
          USER$login <- TRUE
          unique_index <<- filter(credentials, username_id %in% Username, passod %in% pasmatch) %>% select(unique_id)
        } else {
          shinyalert(title = "Username or Password incorrect", type = "error")
        }
      } else {
        shinyalert(title = "Username or Password incorrect", type = "error")
      }
    } 
  })
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color:#D0D0D0 !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(id = "tabs",
                    menuItem("Test Page", tabName = "welcome"),
                    menuItem("Instructor", tabName = "instructor"))
      } else {
        sidebarMenu(id = "tabs",
                    menuItem("Test Page", tabName = "welcome"))
      }} else {
        sidebarMenu(id = "log_tabs",
                    menuItem("Login", tabName = "login"))
      }
  })
  observe({
    if (USER$login == TRUE ){
      updateTabItems(session, "tabs", "welcome") } else {
        updateTabItems(session, "log_tabs", "login")
      }
  })
  
  update_test <- function() {
    output$question_num <- renderText({paste0("Question #", question_number)})
    output$indiv_question <- renderText({test1$Question[question_number]})
    updateRadioButtons(session, inputId = "answer",
                       label = "Answers",
                       selected = "E",
                       choiceValues = c("A","B","C","D","E"),
                       choiceNames = c(paste0("A: ",test1$Option.A[question_number]),
                                       paste0("B: ",test1$Option.B[question_number]),
                                       paste0("C: ",test1$Option.C[question_number]),
                                       paste0("D: ",test1$Option.D[question_number]),
                                       paste0("E: Don't know")))
  }
  
  observeEvent(input$indiv_next_bttn, {
    
    if(question_number > 0 && question_number <= nrow(test1)) {
      update_test()
      master_answer_list <<- rbind(master_answer_list, test1$Correct.Answer[question_number])
      user_answer_list <<- rbind(user_answer_list, input$answer)
      question_number <<- question_number + 1
    }
  })
  
  observeEvent(input$indiv_load_test, {
    test_temp <- paste0(input$indiv_test,".csv")
    if(file.exists(test_temp)) {
      test1 <<- read.csv(test_temp, stringsAsFactors = F)
      question_number <<- 1
      update_test()
    } else {
      print("Bad")
    }
  }) 
  
}

shinyApp(ui, server)