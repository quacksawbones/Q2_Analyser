library("shiny")
library("reshape2")
library("plotly")
library("ggplot2")
library("XLConnectJars")
library("XLConnect")

function(input,output){

  
  output$data_notice <- renderText("Choose a valid xls(x) data file")
  output$meta_notice <- renderText("Choose a valid xls(x) data file")
  output$fluoro_notice <- renderText("Please ensure a valid Q2 data is loaded")
  output$slopes_notice <- renderText("Please ensure a valid Q2 data is loaded")
  output$respiration_notice <- renderText("Please ensure valid Q2 data, Q2 metadata and experimental conditions are loaded")
  
  
  # data <- reactive({rnorm(input$num)})
  # 
  # output$hist <- renderPlot({
  #   title <- paste(as.character(input$num),"Random Numbers")
  #   hist(data(), main = title)
  # })
  # 
  # output$stats <- renderPrint(summary(data()))

  Q2_data <- reactive({
  
    if (!is.null(input$xlsInput)){
      
      #NB: MUST put in here a way to catch an exception of trying to put in a non-XLS(x) file!
      plate_data <- readWorksheetFromFile(input$xlsInput$datapath,sheet=1,header=FALSE)
      
      if (plate_data[1,1] != "Q2 RUN informaton"){
        
        output$data_notice <- renderText("Does not appear to be a Q2 data file")
        plate_data <- NULL
        return(plate_data)
        
      } else {
        
        interval <- as.integer(plate_data[8,3])
  
        plate_data <- plate_data[-(1:21),]
        names(plate_data) <- plate_data[1,]
        plate_data <- plate_data[-1,]
             
        for(X in 0:(length(plate_data$HRS)-1)) {
          plate_data$HRS[[X+1]] <- formatC(X*interval, width=3, flag="0")
        }
        
        output$data_notice <- renderText("Q2 data file appears to be valid")
        return(plate_data)
      }
    }  
  })
  
  
  
  Q2_melted <- reactive({
    
    if (!is.null(Q2_data)){
    
      wells <- c("A","B","C","D","E","F")
      
      y_val <- NULL
      for(i in 1:8){
        for(j in wells){
          y_val <- c(y_val,paste0(j,i))
        }
      }    
      
      melting <- melt(Q2_data(), measure.vars = y_val)
      melting$value <- as.numeric(melting$value)
    }
    return(melting)
  })
  
  
      
  Q2_metadata <- reactive({
    
    if (!is.null(input$metadata)){
    
      plate_metadata <- readWorksheetFromFile(input$metadata$datapath,sheet=1,header=FALSE)
      if ((plate_metadata[2,1] != "Well") && (plate_metadata[1,1] != "Plate")){
        
        output$meta_notice <- renderText("Does not appear to be a valid metadata file")
        return(plate_metadata)
          
      } else {
        
        output$meta_notice <- renderText("Q2 metadata file appears to be valid")
        return(plate_metadata)
        
      }
      
    }
  })
    
    
  output$fluoro <- renderPlotly(
    
    if (!is.null(input$xlsInput)){
      
      # Q2_plot <- plot_ly(x = ~Q2_data()$HRS, y = ~Q2_data()$A1, type = 'scatter', mode = 'lines', yaxis = list(range = c(-0.2, 1.2)), title = "Q2 Flourescence", evaluate = TRUE)
      
      fluoro_plot <- ggplot(Q2_melted(), aes(x=HRS, y=value, group=variable, color=variable)) +
        geom_line() +
        labs(x = "Time (min)", y = "Q2 Fluorescence", title = "Q2 Flourescence over Time") +
        scale_y_continuous(limits = c(-0.2, 1.2), breaks = seq(-0.2,1.2,by=0.2), expand=c(0,0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      if (!is.null(input$startTime) && !is.null(input$duration)){

        fluoro_plot <- fluoro_plot + geom_vline(aes(xintercept = as.integer(input$startTime))) + geom_vline(aes(xintercept = as.integer(input$duration)))

      }
      else if (!is.null(input$startTime) && is.null(input$duration)){

        fluoro_plot <- fluoro_plot + geom_vline(aes(xintercept = as.integer(input$startTime)))

      }
      else if (is.null(input$startTime) && !is.null(input$duration)){

        fluoro_plot <- fluoro_plot + geom_vline(aes(xintercept = as.integer(input$duration)))

      }
    
      # for(q in 1:8){
      #   for(r in wells){
      #     y_val <- paste0(r,q)
      # 
      #     add_trace(Q2_plot, x = ~HRS, y = ~y_val, type = 'scatter', mode = 'lines', yaxis = list(range = c(-0.2, 1.2)), title = "Q2 Flourescence", evaluate = TRUE)
      #   }
      # }
      
      # for(i in 1:8){
      #   for(j in wells){
      #     y_val <- paste0(j,i)
      #     
      #     calc_plot(Q2_plot,y_var)
      #   }
      # }
      
      # add_trace(Q2_plot, x = ~HRS, y = ~B1, type = 'scatter', mode = 'lines', yaxis = list(range = c(-0.2, 1.2)), title = "Q2 Flourescence")
      
      # Q2_plot
    }
    
  )  


  
          
  

    
  output$respiration <- reactive(
    
    if ((!is.null(input$xlsInput) && !is.null(input$metadata))){
      return(NULL)
      
      
    }
    
  )  
  
    
  
    # for(X in 1:length(plate_03)){
    #   for(Y in 1:length(plate_03[[X]])){
    #     print(plate_03[[X]][[Y]])
    #     if(is.na(as.integer(plate_03[[X]][[Y]]))) {
    #       next
    #     }
    #     else if(as.double(plate_03[[X]][[Y]]) == floor(as.double(plate_03[[X]][[Y]]))){
    #       plate_03[[X]][[Y]] <- as.integer(plate_03[[X]][[Y]])
    #     }
    #     else {
    #       plate_03[[X]][[Y]] <- as.double(plate_03[[X]][[Y]])
    #     }
    #   }
    # }
    # 
    # 
    # for(X in 0:(length(plate_03$data$HRS)-1)) {
    #   plate_03$data$HRS[[X+1]] <- as.integer(X*plate_03$run$interval)
    # }
    
    
    # if (is.null(input$xlsInput)) {
    #   
    #   output$Notice <- renderText({print("Not a Q2 Data File")})
    #   
    # }  
    # } else if (plate_data[1,1] == "Q2 RUN informaton") {
    #   
    #   return(plate_data)
    #   
    # } 
    
    
  output$Q2_Data <- renderTable({
    
    if (is.null(Q2_data()) || colnames(Q2_data()[1]) != "ID"){
      
      Q2_data()

    } else if (colnames(Q2_data()[1]) == "ID") {

      Q2_data()
      
    }
  })
  
  
  output$Q2_Meta <- renderTable({
    
    if (is.null(Q2_metadata())){
      
      Q2_metadata()
      
    } else {
      
      Q2_metadata()
      
    }
  })
  
  
  # output$Q2_Meta <- renderTable({
  #   
  #   if (!is.null(Q2_data()) && colnames(Q2_data()[1]) == "ID"){
  #     Q2_data()
  #   }
  # })
    # output$data_notice <-  renderText(
    #   if (is.null(Q2_data())){
    #     print("Please ensure data is like good")
    # }
    # )


  
  # observe(inXLSFile <- input$xlsInput,
  # 
  #   
  #   if (inXLSFile){
  # 
  #     output$Notice <- renderText({print("Please ensure files are uploaded and fields are complete")})
  # 
  #   }
  #   else{
  # 
  #     output$contents <- renderTable({
  # 
  #       readWorksheetFromFile(inXLSFile$datapath,sheet=1)
  # 
  #   })
  # }
  # 
  # )
  # Q2_plate <- list()
  # 
  # reactive({ input$upload
  #   output$notice <- renderText({print("Button pushed")})
  # })
  #   # inMeta <- list()
    # # input$file1 will be NULL initially. After the user selects
    # # and uploads a file, it will be a data frame with 'name',
    # # 'size', 'type', and 'datapath' columns. The 'datapath'
    # # column will contain the local filenames where the data can
    # # be found.
    # inXLSFile <- input$xlsInput
    # inMetaFile <- input$metadata
    # inMeta$altitude <- input$altitude
    # inMeta$sitePressure <- input$sitePressure
    # # inMeta$airportPressure <- input$airportPressure
    # inMeta$tubeVol <- input$tubeVol
    # inMeta$temperature <- input$temperature
    # 
    # if (is.null(inXLSFile)
    #   || is.null(inMetaFile)
    #   || is.null(inMeta$altitude)
    #   || is.null(inMeta$sitePressure)
    #   || is.null(inMeta$tubeVol)
    #   || is.null(inMeta$temperature)
    #   ){
    #   output$Notice <- renderText({print("Please ensure files are uploaded and fields are complete")})
    #   #return(NULL)
    # }
    # else{
    #   # assign(paste0("Q2_plates$plate_",sprintf("%02d",length(Q2_plate)+1),"$data"), readWorksheetFromFile(inXLSFile$datapath,sheet=1))
    #   # assign(paste0("Q2_plates$plate_",sprintf("%02d",length(Q2_plate)+1),"$meta"), readWorksheetFromFile(inMetaFile$datapath,sheet=1))
    #   # assign(paste0("Q2_plates$plate_",sprintf("%02d",length(Q2_plate)+1),"$conds"), inMeta)
    #   }
    
  

}