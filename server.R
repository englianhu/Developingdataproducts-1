# The original version of this app run too slowly on the shiny server so some features were removed
# Some references relating to these features may still remain in this code.

library(shiny)
library(data.table)
library(reshape2)
options(shiny.reactlog=TRUE); 
#------------------------------------------------

load("erp.RData")

#------------------------------------------------

regular1 <- matrix( c(18,24,25,34,35,44,45,54,55,64,65,100), ncol = 2, byrow = TRUE)
regular2 <- matrix( c(18,34,35,49,50,64,65,100), ncol = 2, byrow = TRUE)
regular3 <- matrix( c(18,34,35,44,45,54,55,100), ncol = 2, byrow = TRUE)

fy1 <- c(seq(from = 0, to = 90, by = 5))
fy2 <- c(seq(from = 4, to = 94, by = 5))
fy3 <- c(95,100)
five_year <- cbind(fy1,fy2)
five_year <- rbind(five_year,fy3)

ty1 <- c(seq(from = 0, to = 80, by = 10))
ty2 <- c(seq(from = 9, to = 89, by = 10))
ty3 <- c(90,100)
ten_year <- cbind(ty1,ty2)
ten_year <- rbind(ten_year, ty3)

r1 <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65 and over")
r2 <- c("18-34", "35-49", "50-64", "65 and over", NA, NA)
r3 <- c("18-34", "35-44", "45-54", "55 and over", NA, NA)
age_group_table <- data.frame(r1, r2, r3, row.names = NULL)
colnames(age_group_table) = c("Regular 1", "Regular 2", "Regular 3")

showShinyAlert <- function(id,HTMLtext,session) {
  session$sendCustomMessage(id,HTMLtext)
} 

#--------------------------------------------------

shinyServer(function(input, output,session) {
  
  # You can access the values of the widget (as a vector)
  # with input$radio, e.g.
  # choose table style
  output$table_format <- renderPrint({ input$table_format })
  
  # You can access the values of the widget (as a vector)
  # with input$checkGroup, e.g.
  output$table_variables <- renderPrint({ input$table_variables })
  
  
  ###age_range slider
  # You can access the values of the second widget with input$slider2, e.g.
  output$age_range <- renderPrint({ input$age_range }) 
  
  
  #Dynamic output - customised age sliders
  output$Dynamic <- renderUI({
    #   
    #   browser()
    if (! ("Age") %in% input$table_variables){
      return(NULL)
    }
    
    if (! ("age_custom") %in% input$age_type){
      return(NULL)
    }
        
    if (input$age_type == "age_custom" & !(as.numeric(input$customised_age) %in% 1:20)) {
      stop("Invalid input age")
    }
    
    
  })   
  
    
#----------------------------------
  
  output$display <- renderDataTable(
    #head(RAWERP,10)})
    showtable(),
    options = list(
      iDisplayLength = 10),
    searchDelay = 500
  )
 
  
output$out_age_group_table <- renderTable({

  age_group_table
  },
  include.rownames=FALSE
  )
  
  
  #--------------------
  #Functional stuff
  #--------------------
    
  showtable <- reactive ({
    
    if (input$age_type == "age_custom" & !(as.numeric(input$customised_age) %in% 1:20)) {
      return(NULL)
    }
    
    if (! ("Age") %in% input$table_variables){
      age_matrix <- matrix(data = c(18,100), nrow = 1)
    }
    else if (input$age_type == "age_regular1"){
      age_matrix <- matrix(data = regular1, ncol =2)
    } 
    else if (input$age_type == "age_regular2"){
      age_matrix <- matrix(data = regular2, ncol =2)
    } 
    else if (input$age_type == "age_regular3"){
      age_matrix <- matrix(data = regular3, ncol =2)
    } 
    else if (input$age_type == "age_five"){
      age_matrix <- matrix(data = five_year, ncol =2)
    } 
    else if (input$age_type == "age_ten"){
      age_matrix <- matrix(data = ten_year, ncol =2) 
    } 
    else if (input$age_type == "age_custom"){
      if (is.null(eval(parse(text=paste0("input$group", as.numeric(input$customised_age)))))) {
        return(NULL)
      }
      
      age_matrix <- matrix(nrow=as.numeric(input$customised_age), ncol=2)
      
      for (i in 1:as.numeric(input$customised_age)) {
        age_matrix[i, 1] <- eval(parse(text=paste0("input$group", i, "[1]")))
        age_matrix[i, 2] <- eval(parse(text=paste0("input$group", i, "[2]")))
      }
    }
    
    else {
      age_matrix <- matrix(data = c(input$age_range[1], input$age_range[2]), nrow = 1)
    }
    
    byvars <- input$table_variables[input$table_variables != "Age"]
    #   browser()
    state_flt <- input$state_variables
    if (!("State" %in% byvars)) {
      state_flt <- "Australia"
    }
    if (is.null(input$state_variables)){
      state_flt <- "Australia"
    }
    
  
    merg <- data.table()
    for (i in 1:nrow(age_matrix)){
      # browser()
      #agg <- data.table()
      min_age <- age_matrix[i, 1]
      max_age <- age_matrix[i, 2]
      
      if (max_age == 100){
        agelabel <- paste0(min_age,"-","100+")
      } 
      else {agelabel <- paste0(min_age,"-",max_age)}
      
      pop <- RAWERP[Age >= min_age & Age <= max_age & State %in% state_flt, sum(Population, na.rm = TRUE), by=byvars]
      
      pop <- cbind(agelabel, pop)
      merg <- rbind(merg, pop)
      
      }
    
    setnames(merg, "V1", "Population")
    setnames(merg, "agelabel", "Age")
    downloadtable <<- merg
    merg
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('ERP-Table-', Sys.Date(), '.csv', sep = "")
    },
    content = function(file) {
      write.csv(downloadtable, file, row.names= FALSE, col.names = TRUE)
    }
  )
  
  
})
