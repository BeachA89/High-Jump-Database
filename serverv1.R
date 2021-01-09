server <- function(input, output) {
  
  
  
  tab_Collateddata_1 <-  reactive({
    
    Collateddata%>%
      dplyr::filter(Name == input$Name) %>% 
      dplyr::filter(Competition == input$Competition)
    
  })  
  
  output$select_Name <-  renderUI({
    selectizeInput('Name', 'Select Name', choices = c("select" = "", unique(Collateddata$Name)))  
  }) 
  
  output$select_Competition <-  renderUI({
    inputName = as.character(input$Name)
    choice_Competition <- reactive({
      Collateddata %>% 
        dplyr::filter(Name == inputName) %>% 
        pull(Competition) %>% 
        as.character()
      
      
    })
    
    
    
    
    selectizeInput('Competition', 'Select Competition', choices = c("select" = "", choice_Competition()))  
  })        
  
  
  datafiltered <- Collateddata %>% filter(Name == "Nicola McDermott", Competition == "SydneyTrackClassic2020")
  
  output$datatable_Jumps <-  DT::renderDataTable({
    
    datatable_Jumps <- tab_Collateddata_1() %>% dplyr::select(Jump, `Height cleared`)
    datatable_Jumps <- column_to_rownames(datatable_Jumps, var = 'Jump')
    datatable_Jumps <-  t(datatable_Jumps)

    
    datatable_Jumps[is.na(datatable_Jumps)] <- "X"
    

        ({datatable(datatable_Jumps, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")), dom='t', ordering=F, scrollX = TRUE))})
    
  })
  
  ###########
  
  output$datatable_FootPlants <-  DT::renderDataTable({
    datatable_FootPlants <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Jump, `Height cleared`,
                                                           `Foot Plant Step 1`, `Foot Plant Step 2`, `Foot Plant Step 3`, `Foot Plant Step 4`, 
                                                           `Foot Plant Step 5`, `Foot Plant Step 6`, `Foot Plant Step 7`, `Foot Plant Step 8`,
                                                           `Foot Plant Step 9`, `Foot Plant Step 10`, `Foot Plant Step 11`, `Foot Plant Step 12`,
                                                           `Foot Plant Step 13`, `Foot Plant Take-Off`)
    
    
    all_na <- function(x) any(!is.na(x))
    datatable_FootPlants <- datatable_FootPlants %>% select_if(all_na)
    
    ({datatable(datatable_FootPlants, rownames= FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")), dom='t', ordering=F, scrollX = TRUE))})
    
  })
  
  
  
  
  
  output$datatable_StepTimes <-  DT::renderDataTable({
    datatable_StepTimes <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Jump, `Height cleared`,
                                                          `Step Time Step 1`, `Step Time Step 2`, `Step Time Step 3`, `Step Time Step 4`, 
                                                          `Step Time Step 5`, `Step Time Step 6`, `Step Time Step 7`, `Step Time Step 8`,
                                                          `Step Time Step 9`, `Step Time Step 10`, `Step Time Step 11`, `Step Time Step 12`,
                                                          `Step Time Step 13`)
    
    
    all_na <- function(x) any(!is.na(x))
    datatable_StepTimes <- datatable_StepTimes %>% select_if(all_na)
    datatable_StepTimes[is.na(datatable_StepTimes)] <- "X"
    ({datatable(datatable_StepTimes, rownames= FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")), dom='t', ordering=F, scrollX = TRUE))})
    
  })
  
  output$datatable_ContactTimes <-  DT::renderDataTable({
    datatable_ContactTimes <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Jump, `Height cleared`,
                                                             `Contact Time 3rd last`, `Contact Time 2nd last`, `Contact Time last`)
    
    
    all_na <- function(x) any(!is.na(x))
    datatable_ContactTimes <- datatable_ContactTimes %>% select_if(all_na)
    datatable_ContactTimes[is.na(datatable_ContactTimes)] <- "X"
    ({datatable(datatable_ContactTimes, rownames= FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")), dom='t', ordering=F, scrollX = TRUE))})
    
  })
  
  
  ############################# Plots #############################################
    
    
    
    
  output$Plot_StepTimes1 <-  renderPlot({
    Plot_StepTimes1 <-  tab_Collateddata_1() %>% dplyr::select(`Height cleared`, `Step Time Step 1`, `Step Time Step 2`, `Step Time Step 3`, `Step Time Step 4`, 
                                                       `Step Time Step 5`, `Step Time Step 6`, `Step Time Step 7`, `Step Time Step 8`,
                                                       `Step Time Step 9`, `Step Time Step 10`, `Step Time Step 11`, `Step Time Step 12`,
                                                       `Step Time Step 13`)
    
    all_na <- function(x) any(!is.na(x))
    Plot_StepTimes1 <- Plot_StepTimes1 %>% select_if(all_na)
    
    Plot_StepTimes1 <-  Plot_StepTimes1 %>% reshape2::melt(id = c("Height cleared"))
         
    nb.cols <- n_distinct(Plot_StepTimes1$`variable`)
    mycolors <- colorRampPalette(brewer.pal(8, "Greens"))(nb.cols)
    

    ggplot(data=Plot_StepTimes1, aes(x=`Height cleared`, y=value, fill=variable)) + geom_bar(stat="identity") +
      scale_fill_manual(name = "", values = mycolors, labels = c("Step 1", "Step 2", "Step 3", "Step 4","Step 5", "Step 6","Step 7", "Step 8", "Step 9", "Step 10", "Step 11", "Step 12", "Step 13")) + 
      scale_x_discrete(name = "Height") + scale_y_continuous("time (s)") + geom_text(aes(label = value), size = 3, position = position_stack(vjust = 0.5)) + coord_flip()

    
  })
  
  
  
  
  output$Plot_ContactTimes1 <-  renderPlot({
    Plot_ContactTimes1 <-  tab_Collateddata_1() %>% dplyr::select(`Height cleared`, `Contact Time last`,  `Contact Time 2nd last`, `Contact Time 3rd last`,) 
    
    all_na <- function(x) any(!is.na(x))
    Plot_ContactTimes1 <- Plot_ContactTimes1 %>% select_if(all_na) %>% dplyr::rename(Height = "Height cleared")%>% reshape2::melt(id = c("Height"))
    
    ggplot(data=Plot_ContactTimes1, aes(x=Height, y=value, fill=variable)) + geom_col() + 
      geom_text(aes(label = value), size = 3, position = position_stack(vjust = 0.5)) + scale_x_discrete(name = "Height") + scale_y_continuous("time (s)") +
      scale_fill_manual(name = "",values=c("orange","green4", "red")) + coord_flip()
  })
  
  
  output$Plot_Cadencev11 <-  renderPlot({
    Plot_Cadencev11 <-  tab_Collateddata_1() %>% dplyr::select(`Height cleared`, `Cadencev1 Step 1`, `Cadencev1 Step 2`, `Cadencev1 Step 3`, `Cadencev1 Step 4`, `Cadencev1 Step 5`, `Cadencev1 Step 6`, `Cadencev1 Step 7`, `Cadencev1 Step 8`, `Cadencev1 Step 9`, `Cadencev1 Step 10`, `Cadencev1 Step 11`, `Cadencev1 Step 12`, `Cadencev1 Step 13`) 
    
    all_na <- function(x) any(!is.na(x))
    Plot_Cadencev11 <- Plot_Cadencev11 %>% select_if(all_na) %>% dplyr::rename(Height = "Height cleared")%>% reshape2::melt(id = c("Height"))

    
    ggplot(data=Plot_Cadencev11, aes(x=variable, y=value, color=`Height`), group=1) + geom_line(aes(group=`Height`)) +
      scale_x_discrete(name = "", labels = c("Step 1", "Step 2", "Step 3", "Step 4","Step 5", "Step 6","Step 7", "Step 8", "Step 9", "Step 10", "Step 11", "Step 12", "Step 13")) + scale_y_continuous("time (s)") #+
  })
  
  output$Plot_Cadencev21 <-  renderPlot({
    Plot_Cadencev21 <-  tab_Collateddata_1() %>% dplyr::select(`Height cleared`, `Cadencev2 Step 1`, `Cadencev2 Step 2`, `Cadencev2 Step 3`, `Cadencev2 Step 4`, `Cadencev2 Step 5`, `Cadencev2 Step 6`, `Cadencev2 Step 7`, `Cadencev2 Step 8`, `Cadencev2 Step 9`, `Cadencev2 Step 10`, `Cadencev2 Step 11`, `Cadencev2 Step 12`, `Cadencev2 Step 13`) 
    
    all_na <- function(x) any(!is.na(x))
    Plot_Cadencev21 <- Plot_Cadencev21 %>% select_if(all_na)%>% dplyr::rename(Height = "Height cleared") %>% reshape2::melt(id = c("Height"))

    
    ggplot(data=Plot_Cadencev21, aes(x=variable, y=value, color=`Height`), group=1) + geom_line(aes(group=`Height`)) +
      scale_x_discrete(name = "", labels = c("Step 1", "Step 2", "Step 3", "Step 4","Step 5", "Step 6","Step 7", "Step 8", "Step 9", "Step 10", "Step 11", "Step 12", "Step 13")) + scale_y_continuous("time (s)")#+
  })


  }
  