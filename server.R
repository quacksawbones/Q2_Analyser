library("shiny")
library("reshape2")
library("plotly")
library("ggplot2")
library("plyr")
library("XLConnectJars")
library("XLConnect")


options(shiny.sanitize.errors = FALSE)


pdf(NULL)



function(input,output,session){

  cbind.fill <- function(...){
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function (x) rbind(x, matrix(, n-nrow(x), ncol(x)))))
  }
  
  
  
  output$data_notice <- renderText("Choose a valid .xls(x) data file")
  output$meta_notice <- renderText("Download, complete and upload a metadata file")
  output$respiration_notice <- renderText("Please ensure valid Q2 data, Q2 metadata and experimental conditions are loaded")

  
  
  well_cols <- c("A","B","C","D","E","F")

  wells <- NULL
  for(i in 1:8){
    for(j in well_cols){
      wells <- c(wells,paste0(j,i))
    }
  }   

  

  Q2_data <- reactive({
  
    if (!is.null(input$xlsInput)){

      plate_data <- readWorksheetFromFile(input$xlsInput$datapath,sheet=1,header=FALSE)

      if (plate_data[1,1] != "Q2 RUN informaton"){
        
        output$data_notice <- renderText("Does not appear to be a Q2 data file")
        # plate_data <- NULL
        return(plate_data)
        
      } else {

        
        tryCatch({
          x <- as.POSIXct(plate_data[23,4],format="%m/%d/%Y %H:%M:%S")},
          warning = function(w) {},
          error = function(e) {
            print(e)
          })
        
        if (is.na(x)){
          interval <- as.integer(as.POSIXct(plate_data[24,4],format="%d/%m/%Y %H:%M:%S") - as.POSIXct(plate_data[23,4],format="%d/%m/%Y %H:%M:%S"))
          
        }else{
          interval <- as.integer(as.POSIXct(plate_data[24,4],format="%m/%d/%Y %H:%M:%S") - as.POSIXct(plate_data[23,4],format="%m/%d/%Y %H:%M:%S"))
        }
        
        plate_data <- plate_data[-(1:21),]
        names(plate_data) <- plate_data[1,]
        plate_data <- plate_data[-1,]

        for(X in 0:(length(plate_data$HRS)-1)) {
          plate_data$HRS[[X+1]] <- formatC(X*interval, width=3, flag="0")
        }
        
        plate_data$HRS <- as.integer(plate_data$HRS)
        colnames(plate_data)[which(names(plate_data) == "HRS")] <- paste("MINS")
        

        output$data_notice <- renderText("Q2 data file appears to be valid")

        return(plate_data)
      }
    }
  })
  
  
  
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      "Q2_metadata_template_48_wells.xlsx"
    },
    content = function(file) {
      myfile <- srcpath <- "./resources/Q2_metadata_template_48_wells.xlsx"
      file.copy(myfile, file)
    }
  )

  
      
  Q2_plate_metadata <- reactive({
    
    if (!is.null(input$metadata)){
    
      plate_metadata <- readWorksheetFromFile(input$metadata$datapath,sheet=1,header=TRUE)
      if (("Well" %in% colnames(plate_metadata)) && ("Plate" %in% colnames(plate_metadata))){
        
        output$meta_notice <- renderText("Q2 metadata file appears to be valid")
        return(plate_metadata)
          
      } else {
        
        output$meta_notice <- renderText("Does not appear to be a valid metadata file")
        return(plate_metadata)
        
      }
    }
  })
  
  

  observeEvent({input$tabs == "Q2_fluoro_tab"},
    
    if (is.null(input$xlsInput) || (is.null(Q2_data())) || (colnames(Q2_data()[1]) != "ID")){
      
      output$fluoro_notice <- renderText("Please ensure a valid Q2 data file is loaded") 
      
    } else if (colnames(Q2_data()[1]) == "ID"){

      output$fluoro_notice <- renderText("")
      output$fluoro <- renderPlotly(
        
      if (!is.null(input$xlsInput)){
        
        melting <- Q2_data()
        
        for (Q in Q2_blanks()){
          melting[[Q]] <- NULL
        }
        
        wells_and_blanks <- setdiff(wells, Q2_blanks())
        
        melting <- melt(melting, measure.vars = wells_and_blanks)
        melting$value <- as.numeric(melting$value)

        fluoro_plot <- ggplot(melting, aes(x=MINS, y=value, group=variable, color=variable)) + geom_line()
        fluoro_plot <- fluoro_plot + labs(x = "Time (min)", y = "Q2 Fluorescence", title = "Q2 Flourescence over Time")
        fluoro_plot <- fluoro_plot + scale_y_continuous(limits = c(-0.2, 1.2), breaks = seq(-0.2,1.2,by=0.2), expand=c(0,0))
        fluoro_plot <- fluoro_plot + scale_x_continuous(limits = c(min(Q2_data()$MINS, na.rm = TRUE),max(Q2_data()$MINS, na.rm = TRUE)), breaks = seq(min(Q2_data()$MINS, na.rm = TRUE),max(Q2_data()$MINS, na.rm = TRUE),by=100), expand=c(0,0))
        fluoro_plot <- fluoro_plot + theme(axis.text.x = element_text(hjust = 1))
        fluoro_plot <- fluoro_plot + theme(axis.title.x = element_text(vjust = -0.5))
        
      } 
      )

      if (is.null(input$UI_time_start) && is.null(input$UI_time_stop)){
        
        output$fluoro_start_spot <- renderUI(numericInput("UI_time_start",
                                                          "Start Time",
                                                          as.numeric(paste0(min(Q2_data()$MINS, na.rm = TRUE))),
                                                          min=as.numeric(paste0(min(Q2_data()$MINS, na.rm = TRUE))),
                                                          max=as.numeric(paste0(max(Q2_data()$MINS, na.rm = TRUE))),
                                                          step=Q2_data()$MINS[2]))
        output$fluoro_end_spot <- renderUI(numericInput("UI_time_stop",
                                                        "End Time",
                                                        value=as.numeric(paste0(max(Q2_data()$MINS, na.rm = TRUE))),
                                                        min=as.numeric(paste0(min(Q2_data()$MINS, na.rm = TRUE))),
                                                        max=as.numeric(paste0(max(Q2_data()$MINS, na.rm = TRUE))),
                                                        step=Q2_data()$MINS[2]))
      }
    }
  )

  
  
  observe({
    
    if (!is.null(input$UI_time_start) || (!is.null(input$UI_time_stop))){
      
      if (!is.na(as.numeric(input$UI_time_start)) && !is.na(as.numeric(input$UI_time_stop))){
      
        if (as.numeric(input$UI_time_start) > -1 || as.numeric(input$UI_time_stop) > -1) {
        
          if (as.numeric(input$UI_time_stop) - as.numeric(input$UI_time_start) > 0){
            
            if (as.numeric(input$UI_time_start) < as.numeric(paste0(min(Q2_data()$MINS, na.rm = TRUE))) || as.numeric(input$UI_time_stop) > as.numeric(paste0(max(Q2_data()$MINS, na.rm = TRUE)))){
             
              output$fluro_duration <- renderText(paste("Ensure start time is equal to or greater than ", paste0(min(Q2_data()$MINS, na.rm = TRUE)), " and end time is equal to or less than ",paste0(max(Q2_data()$MINS, na.rm = TRUE))))
               
            }
            
            else{
              multiplier <- as.numeric(Q2_data()$MINS[2])
              
              if (as.numeric(input$UI_time_start) %% multiplier != 0 || as.numeric(input$UI_time_stop) %% multiplier != 0){
                output$fluro_duration <- renderText(paste("Ensure start and stop times are a multiple of ", multiplier))
              }
            
              else
              {
                output$fluro_duration <- renderText(paste("Duration: ",as.numeric(input$UI_time_stop) - as.numeric(input$UI_time_start)," minutes"))
              }
            }
          }
          
          else{
           
            output$fluro_duration <- renderText(paste("Ensure start time is before end time"))
            
          }
        }
        
        else{
        
          output$fluro_duration <- renderText(paste("Ensure start and end times are positive"))
        
        }
      }
    
      else{
        output$fluro_duration <- renderText(paste("Please ensure the start and end times are numbers"))
      }
    }
    
  })

  
  
  observe({
    if (input$is_MSLP){
      updateTextInput(session,"altitude",value = paste(""), label = paste("Altitude not required"))
      shinyjs::disable("altitude")
    }
    else{
      updateTextInput(session,"altitude", label = paste("Altitude (m)"))
      shinyjs::enable("altitude")
    }
  })

  
  
  Q2_blanks <- reactive({
    
    blanks <- vector()

    if (!is.null(Q2_data()) && ((colnames(Q2_data()[1]) == "ID") && (colnames(Q2_data()[6]) == "TEMPERATURE"))){
      
      for (i in 7:54){
        
        print(Q2_data()[1,i])
        
        if (is.na(Q2_data()[,i])){
          
          blanks <- c(blanks,colnames(Q2_data()[i]))
          
        }
      }
    }
    
    return(blanks)
    
  })
  
  
  
  Q2_slopes <- reactive({
    
    req(input$UI_time_start)
    req(input$UI_time_stop)
    
    Q2_subset <- subset(Q2_data(),Q2_data()$MINS >= input$UI_time_start & Q2_data()$MINS <= input$UI_time_stop)
    
    wells_and_blanks <- setdiff(wells, Q2_blanks())

    Q2_subset_temp <- Q2_subset[,7:54]
    Q2_subset_temp <- Q2_subset_temp[,wells_and_blanks]
    Q2_subset[,7:54] <- Q2_subset_temp

    for (Q in Q2_blanks()){
      Q2_subset[[Q]] <- NULL
    }
    
    slopes <- NULL
    
    for (Y in wells_and_blanks){

      holder <- lm(Q2_subset[[paste(Y,sep="")]] ~ Q2_subset$MINS)
      slopes[[paste(Y,sep="")]] <- print(holder$coefficients[[2]])

    }

    slopes <- cbind(wells, slopes)
    
    slopes[,2] <- as.numeric(slopes[,2])*60

    return(slopes)

  })
  
  
  
  Q2_Respiration <- reactive({
    
    req(as.numeric(input$UI_time_start))
    req(as.numeric(input$UI_time_stop))
    
    temp_Q2_plate_metadata <- NULL
    
    meta <- GoodMeta()
    
    metafile <- input$metadata
    altitude <- as.numeric(input$altitude)
    sitePressure <- as.numeric(input$sitePressure)
    tubeVol <- as.numeric(input$tubeVol)
    temperature <- as.numeric(input$temperature)
    

    #NB: One atmosphere of pressure is 101.5 kPa, so the pressure should be a similar value to this

    if (input$is_MSLP){
      pressureCalc <- sitePressure
    }
    else{
      pressureCalc <- sitePressure-(101.35*(1-(1-(altitude/44307.69231))^5.25328))
    }
                                      
    if (!is.null(input$xlsInput) &&
        (!is.na(as.numeric(input$UI_time_start) || (!is.na(as.numeric(input$UI_time_stop))))) && 
        (as.numeric(input$UI_time_start) < as.numeric(input$UI_time_stop)) && (meta == "0") && 
        (!is.null(mean(c(as.numeric(Q2_slopes()[3:4,2])))))){
      
      #AFB- replace airSlope method with data selection method in future
      airSlope <- mean(c(as.numeric(Q2_slopes()[3:4,2])))
      
      temp_Q2_slopes <- Q2_slopes()[,2]
      
      temp_Q2_plate_metadata <- Q2_plate_metadata()
      temp_Q2_slopes <- lapply(temp_Q2_slopes, as.character)
      temp_Q2_slopes <- as.numeric(temp_Q2_slopes)
      
      temp_Q2_plate_metadata$Slopes <- as.numeric(Q2_slopes()[,2])
      
      temp_Q2_plate_metadata$Area <- as.numeric(temp_Q2_plate_metadata$Area)
      temp_Q2_plate_metadata$Fresh_Weight <- as.numeric(temp_Q2_plate_metadata$Fresh_Weight)
      temp_Q2_plate_metadata$Dry_Weight <- as.numeric(temp_Q2_plate_metadata$Dry_Weight)

      temp_Q2_plate_metadata$Area_Resp <- -(pressureCalc*((20.95/100)*tubeVol*(temp_Q2_slopes))/(8314*(273.15+temperature))/(60*60)*1000000*(10000/temp_Q2_plate_metadata$Area))
      temp_Q2_plate_metadata$Fresh_Resp <- -(pressureCalc*((20.95/100)*tubeVol*(temp_Q2_slopes))/(8314*(273.15+temperature))/(60*60)*1000000*(1/temp_Q2_plate_metadata$Fresh_Weight)*1000)
      temp_Q2_plate_metadata$Dry_Resp <- -(pressureCalc*((20.95/100)*tubeVol*(temp_Q2_slopes))/(8314*(273.15+temperature))/(60*60)*1000000*(1/temp_Q2_plate_metadata$Dry_Weight)*1000)

      temp_Q2_plate_metadata <- subset(temp_Q2_plate_metadata, !(temp_Q2_plate_metadata$Well %in% Q2_blanks()))
      
      return(temp_Q2_plate_metadata)
            
    }
    else if (!is.na(as.numeric(input$UI_time_start) || (!is.na(as.numeric(input$UI_time_stop))))){
      
      temp_Q2_plate_metadata <- data.frame("")
      return(temp_Q2_plate_metadata)
      
    }
  })
  
  
  
  observe({
    
    req(GoodMeta())
    req(Q2_metadata_table())
    
    if (!is.na(as.numeric(input$UI_time_start) || (!is.na(as.numeric(input$UI_time_stop))))){
      output$respiration <- renderTable(digits = 4,Q2_Respiration()[,-1])
      output$Resp_Meta <- renderTable(Q2_metadata_table(), colnames = FALSE)
    }
  })
  
  
  
  Q2_metadata_table <- reactive({
    
    req(GoodMeta())
    
    if ((!is.null(Q2_Respiration())) && (!is.na(as.numeric(input$UI_time_start) || (!is.na(as.numeric(input$UI_time_stop)))))){
      metadata_table <- data.frame(matrix(NA,nrow=0, ncol=2))
      
      if (GoodMeta() == 0){
        if (input$is_MSLP){
          metadata_table <-rbind(metadata_table, data.frame(X1="Altitude (m)", X2="Pressure already MSL"))
        }
        else{
          metadata_table <-rbind(metadata_table, data.frame(X1="Altitude (m)", X2=input$altitude))
        }
        
        metadata_table <- rbind(metadata_table, data.frame(X1="Pressure (kpa)", X2=input$sitePressure))
        metadata_table <- rbind(metadata_table, data.frame(X1="Tube Vol (mL)", X2=input$tubeVol))
        metadata_table <- rbind(metadata_table, data.frame(X1="Temperature (C)", X2=input$temperature))
        metadata_table <- rbind(metadata_table, data.frame(X1="Start Time (min)", X2=as.character(input$UI_time_start)))
        metadata_table <- rbind(metadata_table, data.frame(X1="Stop Time (min)", X2=as.character(input$UI_time_stop)))
        metadata_table <- rbind(metadata_table, data.frame(X1="Q2 Data File", X2=input$xlsInput$name))
        metadata_table <- rbind(metadata_table, data.frame(X1="Metadata File", X2=input$metadata$name))

      }
      
      return(metadata_table)
      
    }
  })
  
  
  
  
  observe({
    
    req(as.numeric(input$UI_time_start))
    req(as.numeric(input$UI_time_stop))

    #AFB - removing selection [(-1:4),] to restore all rows to the charting, remove standards with complete cases method.

    if ((!is.null(Q2_Respiration())) && (!is.na(as.numeric(input$UI_time_start) || (!is.na(as.numeric(input$UI_time_stop)))))){  
      output$Resp_Area <- renderPlotly({
        #resp_area_plot <- ggplot(Q2_Respiration()[-(1:4),], aes(x=Well, y=Area_Resp)) + geom_bar(stat="identity")
        resp_area_plot <- ggplot(Q2_Respiration()[complete.cases(Q2_Respiration()$Area_Resp),], 
                                 aes(x=Well, y=Area_Resp)) + geom_bar(stat="identity")
        resp_area_plot <- resp_area_plot + labs(x = "Well (Q2)",y = paste0("\u03BC","mol Oxygen /m*m/s"), title = "Respiration based on Leaf Area")
        resp_area_plot <- resp_area_plot + scale_y_continuous(expand=c(0,0))
        resp_area_plot <- resp_area_plot + theme(axis.title.x = element_text(vjust = -0.5),axis.title.y = element_text(hjust = -1),axis.text = element_text(size=5)) + theme_bw()
        resp_area_plot
      
      })
    }
  })
  
  
  
  observe({
    if ((!is.null(Q2_Respiration())) && (!is.na(as.numeric(input$UI_time_start) || (!is.na(as.numeric(input$UI_time_stop)))))){
      output$Resp_Fresh_Mass <- renderPlotly({
        #resp_area_plot <- ggplot(Q2_Respiration()[-(1:4),], aes(x=Well, y=Fresh_Resp)) + geom_bar(stat="identity")
        resp_area_plot <- ggplot(Q2_Respiration()[complete.cases(Q2_Respiration()$Fresh_Resp),],
                                 aes(x=Well, y=Fresh_Resp)) + geom_bar(stat="identity")
        resp_area_plot <- resp_area_plot + labs(x = "Well (Q2)",y = paste0("nmol Oxygen /g/s"), title = "Respiration based on Leaf Mass (Fresh)")
        resp_area_plot <- resp_area_plot + scale_y_continuous(expand=c(0,0))
        resp_area_plot <- resp_area_plot + theme(axis.title.x = element_text(vjust = -0.5),axis.title.y = element_text(hjust = -1),axis.text = element_text(size=5)) + theme_bw()
        resp_area_plot
      })
    }
  })

  
  
  observe({
    if ((!is.null(Q2_Respiration())) && (!is.na(as.numeric(input$UI_time_start) || (!is.na(as.numeric(input$UI_time_stop)))))){
      output$Resp_Dry_Mass <- renderPlotly({
        #resp_area_plot <- ggplot(Q2_Respiration()[-(1:4),], aes(x=Well, y=Dry_Resp)) + geom_bar(stat="identity")
        resp_area_plot <- ggplot(Q2_Respiration()[complete.cases(Q2_Respiration()$Dry_Resp),], aes(x=Well, y=Dry_Resp)) + geom_bar(stat="identity")
        resp_area_plot <- resp_area_plot + labs(x = "Well (Q2)",y = paste0("nmol Oxygen /g/s"), title = "Respiration based on Leaf Mass (Dry)")
        resp_area_plot <- resp_area_plot + scale_y_continuous(expand=c(0,0))
        resp_area_plot <- resp_area_plot + theme(axis.title.x = element_text(vjust = -0.5),axis.title.y = element_text(hjust = -1),axis.text = element_text(size=5)) + theme_bw()
        resp_area_plot
      })
    }
  })

  
  
  output$data_download <- renderUI(
    if (!is.null(Q2_Respiration()) && (!is.null(input$UI_time_start) || (!is.null(input$UI_time_stop)))){
      downloadButton("downloadBtn", "Download Respiration Data")
    }
  )
  
  

  output$downloadBtn <- downloadHandler(
    filename = function() {paste((strsplit(input$xlsInput$name, "\\.")[[1]][1]),"_respiration.csv")},
    content = function(file) {write.csv(cbind.fill(Q2_Respiration(),Q2_metadata_table()), file, row.names=FALSE)}
  )
  
  
  
  GoodMeta <- reactive({
     
    stuff <- NULL
    
    metafile <- input$metadata
    altitude <- input$altitude
    pressure <- input$sitePressure
    tubeVol <- input$tubeVol
    temperature <- input$temperature
    
    if (is.null(metafile)){
      stuff <- c(stuff,"metadata file")
    }

    if (((altitude == "") || (is.na(as.numeric(altitude)*1))) && !input$is_MSLP){
      stuff <- c(stuff,"altitude")
    }
    
    if ((pressure == "") || (is.na(as.numeric(pressure)*1))){
      stuff <- c(stuff,"pressure")
    }
    
    if ((tubeVol == "") || (is.na(as.numeric(tubeVol)*1))){
      stuff <- c(stuff,"tube volume")
    }
    
    if ((temperature == "") || (is.na(as.numeric(temperature)*1))){
      stuff <- c(stuff,"temperature")
    }
    
    if (is.null(input$UI_time_start) || (is.null(input$UI_time_stop))){
      if (!is.numeric(input$UI_time_start) || (!is.numeric(input$UI_time_stop))){
        stuff <- c(stuff, "respiration times")
      }
    }
    
    if (!is.null(stuff)){
      
      output$respiration_notice <- renderText(c(print("Please check the following metadata: "),paste(stuff,collapse=", ")))
      return("1")
      
    } else {
      
      output$respiration_notice <- renderText("")
      return("0")
        
    }
    
  })
  


  output$data_blanks <- renderText({
    
    if (length(Q2_blanks()) != 0){
    
      c(print("The following wells have been identified as blank: "),paste(Q2_blanks(),collapse=", "))
    
    }
    
    else{NULL}
  
  })
  
  
  
  output$Q2_Data <- renderTable({
    
    Q2_data()
  })
  

  
  output$Q2_Meta <- renderTable(
    
    Q2_plate_metadata()
  )

  session$onSessionEnded(function() {
    dev.off(which = dev.cur())
  })
  
  }
