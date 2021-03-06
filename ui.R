

#For custom age groups panel, narrower for a better fit on page
#than standard shiny numericinput
textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}



shinyUI(
  
  
  
  fluidPage( theme = "default.css",
             
             tags$div(style="background-color:#F8F8F8;",
                      
                      fluidRow(
                        
                        column(12,
                               br(),
                               
                               strong(h1("Estimated Resident Population Tables", align = "center", style = " color: #1b3d93"),
                                      br(),
                                      h4("ABS release version - December 2013", align = "center",style = " color: #000000"),
                                      br()
                               )))),
             
             #can use vertical layout to seperate header from navbar            
             br(),
             
             verticalLayout( 
               navbarPage(
                 title = '',
                 
                 tabPanel('Read Me',  
                          
                          
                          helpText(h2("Read Me")),
                          
                          helpText("This app generates Australian population statistics based on data obtained from the Australian Bureau of Statistics (ABS)."),
                          br(),
                          helpText(h3("How to use this app")),
                      
                          
                          helpText("Click on the 'Table' tab above."),
                          helpText("The default population statistics are shown for all of Australia - ages 18 to 100+"),
                          helpText("You can break down the population statistics by Gender, State and Age"),
                          helpText("Selecting State will give the option to choose which states to show population figures"),
                          helpText("Selecting Age will give the option to breakdown age in different ways."),
                          helpText("'The about this release' tab provides more information on the data and links to the ABS webpage")),
                          
        

                          

                 tabPanel('Table',     
                          
                          fluidRow(tags$style("body {background-color: #F8F8F8;;}"
                          )),
                          
                          
                          column(2,
                                 
                                 # Group of checkboxes
                                 # Choose variables for table
                                 checkboxGroupInput("table_variables", 
                                                    label = h3("Variables"), 
                                                    choices = list("by Gender" = "Sex", "by State" = "State", "by Age" = "Age"),
                                                    selected = c()),
                                 
                                 
                                 #conditional panel for state selection
                                 #condition= is javascript
                                 #need to use . instead of $ for js eg. input.table instead of input$table
                                 conditionalPanel(
                                   condition = "input.table_variables.indexOf('State') > -1",
                                   br(),
                                   checkboxGroupInput("state_variables",
                                                      label = h4("Select States"),
                                                      choices = list("New South Wales" = "New South Wales",
                                                                     "Victoria" = "Victoria",
                                                                     "Queensland" = "Queensland",
                                                                     "South Australia" = "South Australia",
                                                                     "Western Australia" = "Western Australia",
                                                                     "Tasmania" = "Tasmania",
                                                                     "Northern Territory" = "Northern Territory",
                                                                     "Australian Capital Territory" = "Australian Capital Territory"),
                                                      selected = c("New South Wales", "Victoria", "Queensland", "South Australia", "Western Australia",
                                                                   "Tasmania", "Northern Territory", "Australian Capital Territory")))),
                          
                          column(2,
                                 
                                 
                                 
                                 #conditional panel for age selection
                                 conditionalPanel(
                                   condition = "input.table_variables.indexOf('Age') > -1",
                                   
                                   selectInput(
                                     "age_type", h4("Group age by"),
                                     c("Five year" = "age_five",
                                       "Ten year" = "age_ten",
                                       "Regular 1" = "age_regular1",
                                       "Regular 2" = "age_regular2",
                                       "Regular 3" = "age_regular3"
                             
                                     )),
                                   
                                   #conditional panel for age selection (min/max)
                                   conditionalPanel(
                                     condition = "input.age_type == 'age_custom'",
                                     
                                     sliderInput("age_range", label = h4("Age range"), min = 0, 
                                                 max = 100, value = c(18, 100))),
                                   
                                   # Only show this panel if custom age selected
                                   conditionalPanel(
                                     condition = "input.age_type == 'age_custom'",
                                     #     numericInput("customised_age", label = h4("Number of age groups"), max = 20, value = 1))),
                                     textInputRow(inputId="customised_age", label= h4("Number of age groups"), value = 1)))),
                                 
                                 
                  
                          
                          
                          column(7,
                                 
                                 
                                 downloadLink("downloadData","Right click here and 'Save Link As' (or equivalent) to download"), align = "right",
                                 br(),
                                 br(),
                                 #helpText("Table preview - First 10 cases --- (will be dynamic)"),
                                 dataTableOutput("display")
                                 
                                 #can display download button instead
                                 #                            downloadButton("downloadData", label = "Download",
                                 #                                           class = NULL)
                                 
                                 
                                 
                                 
                                 
                          )),
                 
                 
                 tabPanel('About this release',  
                          
                          
                          helpText(h2("3101.0 - Australian Demographic Statistics")),
                          helpText(h3("About this release")),
                          br(),
                          
                          
                          
                          helpText("Quarterly estimates of total population for states, territories and Australia. Includes the most recent estimates of the population in five-year age groups; numbers (and some rates) of births, deaths, infant deaths, interstate and overseas movements. Quarterly and/or annual time series tables throughout. Also includes projected resident populations, projected population in households, projected number of households and projected average household size for states, territories and Australia. 
                                      
                                      \n 
                                      Periodically, articles on specific demographic topics will be released on the ABS web site in conjunction with this publication."),
                          
                          br(),
                          
                          helpText(h3("ABS website links to this release")),
                          br(),
                          
                          tags$a(href="http://www.abs.gov.au/AUSSTATS/abs@.nsf/Latestproducts/3101.0Main%20Features9999Dec%202013?opendocument&tabname=Summary&prodno=3101.0&issue=Dec%202013&num=&view=", 
                                 target="_blank","ABS Website link - Information about this release"),
                          br(),
                          br(),
                          
                          
                          tags$a(href="http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3101.0Dec%202013?OpenDocument", 
                                 target="_blank","ABS Website link - Downloads"),
                          
                          br(),
                          br(),
                          
                          tags$a(href="http://www.abs.gov.au/AUSSTATS/abs@.nsf/second+level+view?ReadForm&prodno=3101.0&viewtitle=Australian%20Demographic%20Statistics~Dec%202013~Latest~19/06/2014&&tabname=Past%20Future%20Issues&prodno=3101.0&issue=Dec%202013&num=&view=&", 
                                 target="_blank","ABS Website link - Past and Future Releases")),
                 
                 
                 
                 tabPanel('Notes on the data',
                          
                          fluidRow(
                            column(5,
                                   helpText(h2("Notes on the data")),
                                   
                                   helpText(h3("Selecting custom age")),
                                   
                                   helpText("Age 100 includes ages 100 and over"),
                                   br(),
                                   helpText("The ABS data from this realease does not break down ages 
                                85-89, 90-94, 95-99 into indivual years."),
                                   helpText("In this in this table selecting ages 85, 90, or 95 will select the entire age group. 
                                E.g 85 encompases ages 85-89.")),
                            
                            
                            
                            
                            column(4,
                                   br(),
                                   helpText(h3("Default age groups")),
                                   
                                   tableOutput("out_age_group_table")
                                   
                            ))))
               
               
             )
  )
)





