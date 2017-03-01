library("shiny")
library("reshape2")
library("plotly")
library("ggplot2")
library("plyr")
library("XLConnectJars")
library("XLConnect")


pdf(NULL)


function(input,output,session){

  
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
        
        interval <- as.integer(plate_data[8,3])
  
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
      "Q2_metadata_template.xlsx"
    },
    content = function(file) {
      myfile <- srcpath <- "/home/darrenc/R/templates/Q2_metadata_template.xlsx"
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
  
  
  
  Q2_Air_Nitro <- reactive({
    
    airNitro <- NULL
    
    # airNitro_temp <- as.data.frame(subset(Q2_data()[,7:10],Q2_data()$MINS >= as.numeric(input$fluoro_start) & Q2_data()$MINS <= as.numeric(input$fluoro_end)), stringsAsFactors = FALSE)
    airNitro_temp <- as.data.frame(subset(Q2_data()[,7:10],Q2_data()$MINS >= as.numeric(input$UI_times[1]) & Q2_data()$MINS <= as.numeric(input$UI_times[2])), stringsAsFactors = FALSE)
    
    airNitro_temp[,1] <- as.numeric(unlist(airNitro_temp[,1]))
    airNitro_temp[,2] <- as.numeric(unlist(airNitro_temp[,2]))
    
    airNitro_temp[,3] <- as.numeric(unlist(airNitro_temp[,3]))
    airNitro_temp[,4] <- as.numeric(unlist(airNitro_temp[,4]))
    
    airNitro$nitroMean <- mean(c(airNitro_temp[,1],airNitro_temp[,2]))
    airNitro$airMean <- mean(c(airNitro_temp[,3],airNitro_temp[,4]))
    airNitro$ratio <- airNitro$airMean-airNitro$nitroMean

    return(airNitro)
    
  })
  

  observeEvent({input$tabs == "Q2_fluoro_tab"},
    
    if (is.null(input$xlsInput) || (is.null(Q2_data())) || (colnames(Q2_data()[1]) != "ID")){
      
      output$fluoro_notice <- renderText("Please ensure a valid Q2 data file is loaded") 
      
    } else if (colnames(Q2_data()[1]) == "ID"){

      output$fluoro_notice <- renderText("")
      output$fluoro <- renderPlotly(
        
      if (!is.null(input$xlsInput)){
        
        melting <- melt(Q2_data(), measure.vars = wells)
        melting$value <- as.numeric(melting$value)

        fluoro_plot <- ggplot(melting, aes(x=MINS, y=value, group=variable, color=variable)) + geom_line()
        fluoro_plot <- fluoro_plot + labs(x = "Time (min)", y = "Q2 Fluorescence", title = "Q2 Flourescence over Time")
        fluoro_plot <- fluoro_plot + scale_y_continuous(limits = c(-0.2, 1.2), breaks = seq(-0.2,1.2,by=0.2), expand=c(0,0))
        fluoro_plot <- fluoro_plot + scale_x_continuous(limits = c(min(Q2_data()$MINS, na.rm = TRUE),max(Q2_data()$MINS, na.rm = TRUE)), breaks = seq(min(Q2_data()$MINS, na.rm = TRUE),max(Q2_data()$MINS, na.rm = TRUE),by=100), expand=c(0,0))
        fluoro_plot <- fluoro_plot + theme(axis.text.x = element_text(hjust = 1))
        fluoro_plot <- fluoro_plot + theme(axis.title.x = element_text(vjust = -0.5))
        
      } 
      )
    
      if (is.null(input$UI_times)){
        output$fluoro_slider <- renderUI(sliderInput("UI_times","Start time and Duration",
                                min = as.numeric(paste0(min(Q2_data()$MINS, na.rm = TRUE))),
                                max = as.numeric(paste0(max(Q2_data()$MINS, na.rm = TRUE))),
                                value = c(as.numeric(paste0(min(Q2_data()$MINS, na.rm = TRUE))),
                                as.numeric(paste0(max(Q2_data()$MINS, na.rm = TRUE)))),
                                step = as.numeric(Q2_data()$MINS[2]),dragRange = TRUE, post = " min"))
        
        output$fluoro_start_spot <- renderUI(textInput("fluoro_start", "Start Time (min)",value = paste0(min(Q2_data()$MINS, na.rm = TRUE))))
        tags$br()
        output$fluoro_end_spot <- renderUI(textInput("fluoro_end", "End Time (min)",value = paste0(max(Q2_data()$MINS, na.rm = TRUE))))
        tags$h3(output$fluoro_or_spot <- renderText("or"))
        tags$br()
        output$fluoro_duration_spot <- renderUI(textInput("fluoro_duration", "Duration (min)",value = paste0(max(Q2_data()$MINS, na.rm = TRUE))))
      }
    }
  )
  
  # NB: Add function in here that checks the start, end and duration text boxes and updates them if they are NOT multiples of <INTERVAL>
  # What about something to do witg 

  Q2_slopes <- reactive({
    
    # Q2_subset <- subset(Q2_data(),Q2_data()$MINS >= as.numeric(input$fluoro_start) & Q2_data()$MINS <= as.numeric(input$fluoro_end))
    Q2_subset <- subset(Q2_data(),Q2_data()$MINS >= as.numeric(input$UI_times[1]) & Q2_data()$MINS <= as.numeric(input$UI_times[2]))
    
    Q2_subset_temp <- Q2_subset[,7:54]
    Q2_subset_temp <- Q2_subset_temp[,wells]
    Q2_subset[,7:54] <- Q2_subset_temp
    
    slopes <- NULL
    
    for (Y in wells){

      holder <- lm(Q2_subset[[paste(Y,sep="")]] ~ Q2_subset$MINS)
      slopes[[paste(Y,sep="")]] <- print(holder$coefficients[[2]])

      }
    # slopes <- cbind(wells, slopes)
    slopes <- cbind(wells, slopes)
    
    slopes[,2] <- as.numeric(slopes[,2])*60
    
    # slopes[-(1:4),2] <- as.numeric(slopes[-(1:4),2])-mean(c(as.numeric(slopes[3,2]),as.numeric(slopes[4,2])))
    
    return(slopes)

  })
  

  
  Q2_Respiration <- reactive({
    
    temp_Q2_plate_metadata <- NULL
    
    meta <- GoodMeta()
    
    metafile <- input$metadata
    altitude <- as.numeric(input$altitude)
    sitePressure <- as.numeric(input$sitePressure)
    tubeVol <- as.numeric(input$tubeVol)
    temperature <- as.numeric(input$temperature)
    
    pressureCalc <- sitePressure-(101.35*(1-(1-(altitude/44307.69231))^5.25328))
                                      
    if (!is.null(input$xlsInput) && (!is.null(input$UI_times)) && (meta == "0") && (!is.null(mean(c(as.numeric(Q2_slopes()[3:4,2])))))){
      
      # airN2Ratio <- as.numeric(Q2_Air_Nitro()$ratio)
      
      airSlope <- mean(c(as.numeric(Q2_slopes()[3:4,2])))
      
      # temp_Q2_slopes <- Q2_slopes()[-(1:4),2]
      temp_Q2_slopes <- Q2_slopes()[,2]
      
      # temp_Q2_plate_metadata <- Q2_plate_metadata()[-(1:4),]
      temp_Q2_plate_metadata <- Q2_plate_metadata()
      
      temp_Q2_slopes <- lapply(temp_Q2_slopes, as.character)
      temp_Q2_slopes <- as.numeric(temp_Q2_slopes)
      
      # temp_Q2_plate_metadata$Slopes <- as.numeric(Q2_slopes()[-(1:4),2])
      temp_Q2_plate_metadata$Slopes <- as.numeric(Q2_slopes()[,2])
      
      
      temp_Q2_plate_metadata$Area <- as.numeric(temp_Q2_plate_metadata$Area)
      temp_Q2_plate_metadata$Fresh_Weight <- as.numeric(temp_Q2_plate_metadata$Fresh_Weight)
      temp_Q2_plate_metadata$Dry_Weight <- as.numeric(temp_Q2_plate_metadata$Dry_Weight)
      
      # NB: Original calculations based on Clarissa's models. These are different to what is found in Scafaro et. al. (2016) (unpublished)
      # temp_Q2_plate_metadata$Area_Resp <- -(pressureCalc*((20.95/100)*tubeVol*temp_Q2_slopes/airN2Ratio)/(8314*(273.15+temperature))/(60*60)*1000000*(10000/temp_Q2_plate_metadata$Area))
      # temp_Q2_plate_metadata$Fresh_Resp <- -(pressureCalc*((20.95/100)*tubeVol*temp_Q2_slopes/airN2Ratio)/(8314*(273.15+temperature))/(60*60)*1000000*(1/temp_Q2_plate_metadata$Fresh_Weight)*1000)
      # temp_Q2_plate_metadata$Dry_Resp <- -(pressureCalc*((20.95/100)*tubeVol*temp_Q2_slopes/airN2Ratio)/(8314*(273.15+temperature))/(60*60)*1000000*(1/temp_Q2_plate_metadata$Dry_Weight)*1000)

      temp_Q2_plate_metadata$Area_Resp <- -(pressureCalc*((20.95/100)*tubeVol*(temp_Q2_slopes - airSlope))/(8314*(273.15+temperature))/(60*60)*1000000*(10000/temp_Q2_plate_metadata$Area))
      temp_Q2_plate_metadata$Fresh_Resp <- -(pressureCalc*((20.95/100)*tubeVol*(temp_Q2_slopes - airSlope))/(8314*(273.15+temperature))/(60*60)*1000000*(1/temp_Q2_plate_metadata$Fresh_Weight)*1000)
      temp_Q2_plate_metadata$Dry_Resp <- -(pressureCalc*((20.95/100)*tubeVol*(temp_Q2_slopes - airSlope))/(8314*(273.15+temperature))/(60*60)*1000000*(1/temp_Q2_plate_metadata$Dry_Weight)*1000)
      
    }
    
    
    
    # Clarissa
    # 
    # =-($D$4*(($D$1/100)*$D$5*B20/$D$8)/($D$6*(273.15+$D$7))/(60*60)*1000000*(10000/D20))
    # 
    # B20:<slope in time group>
    #   $D$8: <Air:Nitrogen Standard Range in time group> = 
    #   
    #   Lucy
    # 
    # =-($D$4*(($D$1/100)*$D$5*(K20+Q20)/P20))/($D$6*(273.15+$D$7))/(60*60)*1000000*(10000/D20)
    # 
    # K20:<slope>
    #   Q20:<signal slope> = <average air> - <average N2>
    #   P20:<signal range> = <air>(OFFSET?) - <N2>(OFFSET?)
    
    
    return(temp_Q2_plate_metadata)
    
  })
  
  
  
  output$respiration <- renderTable(digits = 4,Q2_Respiration()[,-1])

  
  
  observe({
    if ((!is.null(Q2_Respiration())) && (!is.null(input$UI_times))){  
      output$Resp_Area <- renderPlotly({
        resp_area_plot <- ggplot(Q2_Respiration()[-(1:4),], aes(x=Well, y=Area_Resp)) + geom_bar(stat="identity")
        resp_area_plot <- resp_area_plot + labs(x = "Well (Q2)",y = paste0("\u03BC","mol Oxygen /m*m/s"), title = "Respiration based on Leaf Area")
        resp_area_plot <- resp_area_plot + scale_y_continuous(expand=c(0,0))
        resp_area_plot <- resp_area_plot + theme(axis.title.x = element_text(vjust = -0.5),axis.title.y = element_text(hjust = -1),axis.text = element_text(size=5)) + theme_bw()
        resp_area_plot
      
      })
    }
  })
  
  
  
  observe({
    if ((!is.null(Q2_Respiration())) && (!is.null(input$UI_times))){
      output$Resp_Fresh_Mass <- renderPlotly({
        resp_area_plot <- ggplot(Q2_Respiration()[-(1:4),], aes(x=Well, y=Fresh_Resp)) + geom_bar(stat="identity")
        resp_area_plot <- resp_area_plot + labs(x = "Well (Q2)",y = paste0("nmol Oxygen /g/s"), title = "Respiration based on Leaf Mass (Fresh)")
        resp_area_plot <- resp_area_plot + scale_y_continuous(expand=c(0,0))
        resp_area_plot <- resp_area_plot + theme(axis.title.x = element_text(vjust = -0.5),axis.title.y = element_text(hjust = -1),axis.text = element_text(size=5)) + theme_bw()
        resp_area_plot
      })
    }
  })

  
  
  observe({
    if ((!is.null(Q2_Respiration())) && (!is.null(input$UI_times))){
      output$Resp_Dry_Mass <- renderPlotly({
        resp_area_plot <- ggplot(Q2_Respiration()[-(1:4),], aes(x=Well, y=Dry_Resp)) + geom_bar(stat="identity")
        resp_area_plot <- resp_area_plot + labs(x = "Well (Q2)",y = paste0("nmol Oxygen /g/s"), title = "Respiration based on Leaf Mass (Dry)")
        resp_area_plot <- resp_area_plot + scale_y_continuous(expand=c(0,0))
        resp_area_plot <- resp_area_plot + theme(axis.title.x = element_text(vjust = -0.5),axis.title.y = element_text(hjust = -1),axis.text = element_text(size=5)) + theme_bw()
        resp_area_plot
      })
    }
  })
  
  
  
  output$data_download <- renderUI(
    if (!is.null(Q2_Respiration()) && (!is.null(input$UI_times))){
      downloadButton("downloadBtn", "Download Respiration Data")
    }
  )
  
  # NB: this is for the future so you can save different types of data
  # 
  # output$download_type_spot <- renderUI(
  #   if (!is.null(Q2_Respiration()) && (!is.null(input$fluoro_start))){
  #     selectInput("download_type", "Download As:", c("Comma Separated" = "csv", "Tab Delimited" = "txt","Excel file" = "xls"))
  #   }
  # )
  # 
  # reactive({
  #   if (is.null(Q2_Respiration()) || (is.null(input$fluoro_start))){
  #   }
  #   else if (input$download_type == "txt"){
  #     output$downloadBtn <- downloadHandler(
  #       filename = function() {"Respiration_data.txt"},
  #       content = function(file) {write.table(Q2_Respiration(), file, row.names=FALSE, sep="\t")}
  #     )
  #   }
  #   else if (input$download_type == "csv"){
  #     output$downloadBtn <- downloadHandler(
  #       filename = function() {"Respiration_data.csv"},
  #       content = function(file) {write.csv(Q2_Respiration(), file, row.names=FALSE)}
  #     )
  #   }
  #   else if (input$download_type == "xlsx"){
  #     output$downloadBtn <- downloadHandler(
  #       filename = function() {"Respiration_data.xlsx"},
  #       content = function(file) {write.xlsx(Q2_Respiration(), file, row.names=FALSE)}
  #     )
  #   }
  # })
  
  

  

  output$downloadBtn <- downloadHandler(

      filename = function() {"Respiration_data.csv"},
      content = function(file) {write.csv(Q2_Respiration(), file, row.names=FALSE)}
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

    if ((altitude == "") || (is.na(as.numeric(altitude)*1))){
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
    
    if (!is.null(stuff)){
      
      output$respiration_notice <- renderText(c(print("Please check the following metadata: "),paste(stuff,collapse=", ")))
      return("1")
      
    } else {
      
      output$respiration_notice <- renderText("")
      return("0")
        
    }
    
  })
  

  
  output$Q2_Data <- renderTable({
    
    Q2_data()
 
  })
  

  
  output$Q2_Meta <- renderTable(
    
    Q2_plate_metadata()
      
  )
  
}
