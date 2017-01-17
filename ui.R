library("shiny")
library("plotly")
library("XLConnectJars")
library("XLConnect")

navbarPage("Q2 Summariser",
          
          tabPanel("Q2 Data",
            fluidRow(
              column(3,
                tags$h1("Upload Q2 Data File"),
                fileInput("xlsInput", "", accept = c(".xls",".xlsx")),
                textOutput("data_notice")  
              )

            ),
            fluidRow(
              tableOutput("Q2_Data")
            )),
          tabPanel("Q2 Metadata",
            fluidRow(
              column(3,
                tags$h1("Metadata File"),
                fileInput("metadata","", accept = c(".xls",".xlsx")),
                textOutput("meta_notice"),
                tags$h1("Q2 Conditions"),
                textInput("altitude","Altitude (m)"),
                textInput("sitePressure","Site Pressure (kpa)"),
                #textInput("airportPressure","Airport Pressure (kpa)"),
                textInput("tubeVol","Tube Vol (mL)"),
                textInput("temperature","Temperature (C)"),
                textInput("startTime", "Start Time (mins)"),
                textInput("duration", "Duration (mins)")
              ),
              column(6,
                tableOutput("Q2_Meta")
            )
            )),
            
          navbarMenu("Analysis",
            tabPanel("Fluorescence",
              
              textOutput("fluoro_notice"),
              plotlyOutput("fluoro")
                     
              ),
            tabPanel("Slopes",
              
              textOutput("slopes_notice"),
              tableOutput("slopes")
                     
              ),
            tabPanel("Respiration", 
              
              textOutput("respiration_notice"),
              tableOutput("respiration")
                            
              )
            ),
          tabPanel("Help")


  # *Output("...") functions
  #plotOutput("hist")
  #textOutput("text")
  )
