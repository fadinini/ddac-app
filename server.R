### Libraries
library(shiny)
options(shiny.maxRequestSize = 500*1024^2)
library(shinydashboard)
library(plyr)
library(vroom)
library(caret)
library("readxl")
library(esquisse)
library(tidyverse)
library(DT)
library(data.table)
library(shinymanager)



# define some credentials
credentials <- data.frame(
  user = c("ddac"),
  password = c("ddac"),
  stringsAsFactors = FALSE
)


### Import table 
c <- read_excel("table_def_1.xlsx")

### Import Model
a <-read.csv("mdl_dat.csv")
mdl_glm <- readRDS("mdl_glm.rds")
a[sapply(a, is.character)] <- lapply(a[sapply(a, is.character)], as.factor)




####################################### Server ################################

server <- function(input, output){ 
  
  
  ####################### Password ###################
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  
  ####################### Acceuil ####################
  
  output$table1 <- renderTable({
    
    c
    
  })
  
  ##################### Fin Acceuil ###################
  
  
  
  
  
  ##################### Visualisation ####################
  
  
  ### tab_base
  
  b <- reactive({
    
    if(stringr::str_ends(input$explore$datapath, "csv")) {
      fread(input$explore$datapath)
    } else if (stringr::str_ends(input$explore$datapath, "(xlsx|xls)")) {
      readxl::read_excel(input$explore$datapath)
    }
    
  })
  
  observeEvent(input$explore, 
               isolate(output$table <- DT::renderDT({
                 b()
               }, options = list(pageLength = 8, scrollX = TRUE, length))
               )
  )
  
  
  ### Fin tab_base
  
  
  ### tab_visual
  
  data_r <- reactiveValues(data = iris, name = "iris")
  
  observeEvent(input$explore, {
    
    data_r$data <- b()
    esquisse_server(
      
      id = "esquisse",
      data_rv = data_r
      
    )
  })
  
  
  ### Fin tab_visual
  
  
  
  #################### Fin Visualisation ###################
  
  
  
  
  ##################### Prediction ####################
  
  
  ###################### tab base
  
  ### user Data cleaning and predicting
  
  b2 <- reactive({
    
    
    if(stringr::str_ends(input$predire$datapath, "csv")) {
      b <- fread(input$predire$datapath)
    } else if (stringr::str_ends(input$predire$datapath, "(xlsx|xls)")) {
      b <- readxl::read_excel(input$predire$datapath)
    }
    
    ### data cleaning
    
    b <- as.data.frame(unclass(b),stringsAsFactors=TRUE)
    
    # unk_AID_FIN <- which(!(b$AID_FIN %in% levels(a$AID_FIN)))
    # b$AID_FIN[unk_AID_FIN] <- NA
    unk_SITUPRE <- which(!(b$SITUPRE %in% levels(a$SITUPRE)))
    b$SITUPRE[unk_SITUPRE] <- NA
    unk_DIPDER <- which(!(b$DIPDER %in% levels(a$DIPDER)))
    b$DIPDER[unk_DIPDER] <- NA
    unk_CURSUS_LMD <- which(!(b$CURSUS_LMD %in% levels(a$CURSUS_LMD)))
    b$CURSUS_LMD[unk_CURSUS_LMD] <- NA
    unk_SEXE <- which(!(b$SEXE %in% levels(a$SEXE)))
    b$SEXE[unk_SEXE] <- NA
    
    
    b <- na.omit(b)
    
    
    # Predict
    
    pred_glm <- predict(mdl_glm, b)
    
    
    # Export Base
    
    b$RES_Pred <- pred_glm
    b
    
  })### Fin Prediction
  
  observeEvent(input$predire, 
               output$table2 <- DT::renderDT({
                 b2()
               }, options = list(pageLength = 8, scrollX = TRUE, length))
  )
  
  
  ##################### tab_visual
  
  data_r <- reactiveValues(data = iris, name = "iris")
  
  observeEvent(input$predire, {
    
    data_r$data <- b2()
    esquisse_server(
      
      id = "esquisse2",
      data_rv = data_r
      
    )
  })
  
  
  ##################### tab_download
  
  observeEvent(input$predire, 
               output$summ <- renderPrint({
                 summary(b2())
               })
  )
  
  output$dbase <- downloadHandler(
    
    filename = function(){
      
      paste("pred","csv", sep = ".")
    },
    
    content = function(file){
      
      vroom::vroom_write(b2(),file)
    })
  
  
  
  
  
}### Fin Server