library("shiny")
library("plotly")
library("XLConnectJars")
library("XLConnect")



fluidPage(
  
  tags$h1("Q2 Analyser"),
  tags$br(),
  # uiOutput("download_type_spot"), - NB: For saveing as different types
  uiOutput("data_download"),
  tags$br(),
    
  tabsetPanel(id = "tabs",
    tabPanel("Q2 Data", value = "Q2_data_tab",
      fluidRow(
        column(3,
          tags$h1("Upload Q2 Data File"),
          fileInput("xlsInput", "", accept = c(".xls",".xlsx")),
          textOutput("data_notice"),  
          uiOutput("data_blanks")
        )
  
      ),
      fluidRow(
        tableOutput("Q2_Data")
      )),
    tabPanel("Q2 Metadata", value = "Q2_metadata_tab",
      fluidRow(
        column(3,
          tags$h1("Metadata File"),
          downloadButton("downloadTemplate", "Download Metadata Template (.xlsx)"),
          fileInput("metadata","", accept = c(".xls",".xlsx")),
          textOutput("meta_notice"),
          tags$h1("Q2 Conditions"),
          textInput("altitude","Altitude (m)"),
          textInput("sitePressure","Site Pressure (kpa)"),
          #textInput("airportPressure","Airport Pressure (kpa)"),
          textInput("tubeVol","Tube Vol (mL)"),
          textInput("temperature","Temperature (C)")
        ),
        column(6,
          tableOutput("Q2_Meta")
      )
      )),
      
    navbarMenu("Analysis",
      tabPanel("Fluorescence", value = "Q2_fluoro_tab",
        
        textOutput("fluoro_notice"),
        plotlyOutput("fluoro"),
        uiOutput("fluoro_slider"),
        uiOutput("fluoro_start_spot"),
        uiOutput("fluoro_end_spot"),
        textOutput("fluoro_or_spot"),
        uiOutput("fluoro_duration_spot")
        
        ),
     tabPanel("Respiration", value = "Q2_resp_tab",
        
      fluidPage(
        textOutput("respiration_notice"),
        # column(4,
        #   textOutput("slopes_notice"),
        #   tableOutput("slopes")
        #               
        # ),
        # column(8,
        #   tableOutput("respiration"),
        #   textOutput("respiration_notice")
        # )
          column(1,
            tableOutput("respiration")
          ),
          column(6,offset=5,
                 plotlyOutput("Resp_Area"),
                 tags$br(),
                 plotlyOutput("Resp_Fresh_Mass"),
                 tags$br(),
                 plotlyOutput("Resp_Dry_Mass")
          )
        )
      )
    ),
    tabPanel("Help", value = "Q2_help_tab",
            tags$h2("Help and instructions are coming soon. In the meantime, contact Darren Cullerne for help!"),
            tags$a(href="mailto:darren.cullerne@anu.edu.au", "darren.cullerne@anu.edu.au"),
            textOutput("temp1")
    ),
    
    tabPanel("Debug", value = "Q2_help_tab",
      tableOutput("tempTable1"),
      tableOutput("tempTable2"),
      textOutput("tempText1"),
      textOutput("tempText2")
    )
  )
)
