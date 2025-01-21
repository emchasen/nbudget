server <- function(input, output, session) {
  
  #-nitrogen rate recommendation table----------------
  ##TODO is this still in the UI? put this somewhere
  output$table1 <- DT::renderDataTable({
    nitrogenrater_data
    })
  
  #Kevin's spreadsheet table-----------------
  output$table2 = DT::renderDataTable({
    budget
  })
  
  # reactive vals----------------
  vals <- reactiveValues(
    
    RTN = NULL,
    leachable = NULL,
    count = 0
    
  )
  leach <- reactiveValues()
  leach$df <- data.frame(Scenario = numeric(0), leachN = numeric(0))
  return <- reactiveValues()
  return$df <- data.frame(Scenario = numeric(0), RTN = numeric(0))
  scenario <- reactiveValues()
  scenario$df <- data.frame()
  
  # change counties based on crop----------------
  output$countyName <- renderUI({
    
    if(input$cropID == "Corn") {
      counties = NASS_Corn$County
    } else {
      counties = NASS_Potato$County
    }
    
    selectInput(inputId = "County", label = "County", choices = c(counties), selected = "ADAMS")
    
  })
  
  # make changes to UI and reactive vals based on cropID---------
  #observeEvent(input$cropID, {
  # fill in reactive vals--------------
  # observe({
  # 
  #   cropID <- input$cropID
  #   County_Selected_Corn <- subset(NASS_Corn, County == input$County)
  #   County_Selected_Potato <- subset(NASS_Potato, County == input$County)
  # 
  #   # updates yield range if corn is selected--------
  #   if (cropID == "corn") {
  #     updateSliderInput(session, "Yield", value = County_Selected_Corn$Avg,
  #                       min = 80, max = 300)
  #     # updates yield range if potato is selected--------
  #   } else if (cropID == "pota"){
  #     print("49")
  #     print(County_Selected_Potato)
  #     updateSliderInput(session, "Yield", value = County_Selected_Potato$Avg,
  #                       min = 100, max = 675)
  #   }
  # 
  # })

  # average yield text and yield slider update---------------------
  #bserve({
  observeEvent({
    input$cropID
    input$County}, {
    cropID <- input$cropID
    #print(cropID)
    County_Selected_Corn <- subset(NASS_Corn, County == input$County)
    #print(County_Selected_Corn)
    County_Selected_Potato <- subset(NASS_Potato, County == input$County)
    #print(County_Selected_Potato)
    
    if(cropID == "corn") {
      updateSliderInput(session, "Yield", value = County_Selected_Corn$Avg,
                        min = 80, max = 300)
      output$County_Avg <- renderText({
        paste("Average Corn Yield for", input$County, "is", County_Selected_Corn$Avg,"bu/ac")
        })
      } else if (cropID == "pota") {
        print("49")
        print(County_Selected_Potato)
        updateSliderInput(session, "Yield", value = County_Selected_Potato$Avg,
                          min = 100, max = 675) ##TODO slider doesn't update on first switch to potato (if it is the first user input change)
        output$County_Avg <- renderText({
          paste("Average Potato Yield for", input$County, "is", County_Selected_Potato$Avg,"CWT/ac")
        })
        }
    })

  # Update fertilizer bar --------------
  observe({
    HundredWeight <- as.character(Potato_N_Rate$HundredWeight)
    cropID <- input$cropID
    if (cropID == "pota" && input$Yield <= 350) {
      HundredWt = "250-350"
    } else if (cropID == "pota" && (input$Yield >= 351 && input$Yield <= 450)) {
      HundredWt = "351-450"
    } else if (cropID == "pota" && input$Yield >= 451 && input$Yield <= 550) {
      HundredWt = "451-550"
    } else {
      HundredWt = "551-650"
    }

    Previous_Updated <- subset(Corn_Loamy, Yield == input$Yield_Size)
    N_C_Updated <- subset(Previous_Updated, Previous == input$Previous)
    Fert_Updated <- subset(N_C_Updated, N_C == input$N_C)
    Fert_Updated_Sandy_Irr_Y <- subset(Corn_Sandy_Irr_Y, N_C == input$N_C_Y)
    Fert_Updated_Sandy_Irr_N <- subset(Corn_Sandy_Irr_N, N_C == input$N_C_N)
    #Potato_HW <- subset(Potato_N_Rate, HundredWeight == input$Yield_Range)
    Potato_HW <- filter(Potato_N_Rate, HundredWeight == HundredWt)
    Potato_SOM <- filter(Potato_HW, SOM == input$SOM)
  
    # fertilizer rec text-------------
    #"Sandy" = 1, "Loamy" = 2, "Clay" = 3
    if (cropID == "corn" && input$Soil != 1) { # corn and not sandy
      updateSliderInput(session, "Fert", value = Fert_Updated$Avg,
                        min = Fert_Updated$Min, max = Fert_Updated$Max)
      output$N_Rate<- renderText({
        paste("Based on above conditions, recommended N rate is", Fert_Updated$Avg,"lb/ac")})

    } else if (cropID == "corn" && input$Soil == 1 && input$Binary == 1){ # corn and sand and irrigated
      updateSliderInput(session, "Fert", value = Fert_Updated_Sandy_Irr_Y$Avg,
                        min = Fert_Updated_Sandy_Irr_Y$Min, max = Fert_Updated_Sandy_Irr_Y$Max)
      output$N_Rate<- renderText({
        paste("Based on above conditions, recommended N rate is", Fert_Updated_Sandy_Irr_Y$Avg,"lb/ac")})

    } else if (cropID == "corn" && input$Soil == 1 && input$Binary == 2){ # corn and sandy and not irrigated
      updateSliderInput(session, "Fert", value = Fert_Updated_Sandy_Irr_N$Avg,
                        min = Fert_Updated_Sandy_Irr_N$Min, max = Fert_Updated_Sandy_Irr_N$Max)
      output$N_Rate<- renderText({
        paste("Based on above conditions, recommended N rate is", Fert_Updated_Sandy_Irr_N$Avg,"lb/ac")})

    } else if (cropID == "pota") { # potato
      updateSliderInput(session, "Fert", value = Potato_SOM$Avg,
                        min = 50, max = 300)
      output$N_Rate<- renderText({
        paste("Based on above conditions, recommended N rate is", Potato_SOM$Avg,"lb/ac")})
    }

  })

  # action: display leaching and RTN--------------
  observeEvent(input$action,{

    # count number of clicks-------------
    vals$count <- vals$count + 1
    # set dentri-----------------
    if (input$Soil == 1){
      denitri = .04
    } else if (input$Soil == 2){
      denitri = .20
    } else if (input$Soil == 3){
      denitri = .40
    }

    # set ncontent and cropPrice------------
    if (input$cropID == "corn") {
      nContent = .73
      cropPrice = input$Corn_Price
    } else if (input$cropID == "pota"){
      nContent = .4
      cropPrice = input$Pota_Price
    }

    #define the rest of the variables---------------
    binary = input$Binary
    yield = input$Yield
    soil = input$Soil
    inchesApplied = input$Inches_Applied
    fert = input$Fert
    ammonia = fert*.15
    cornPrice = input$Corn_Price
    nPrice = input$N_Price
    
    #Return to N has to be normalized/account for amount of yield
    #that can occur with 0 N fertilizer applied. This is assumed to be
    #50% of the maximum attained.
    # calc RTN-------------------
    yieldUnfert=0.5*yield
    vals$RTN = ((yield-yieldUnfert)*cropPrice)-(fert*nPrice)
    #vals$RTN = (vals$Yield*vals$Crop_Price)-(vals$Fert*vals$N_Price) - incorrect formula
    Irr_Categorical = input$Irr_Categorical

    # N concentration to use for Irrigation Water-----------
    if (Irr_Categorical == "18" && soil == "1"){ #unknown irrigation
      Irr_N = 18
    } else if (Irr_Categorical == "18" && soil == "2"){ #unknown irrigation
      Irr_N = 4
    } else if (Irr_Categorical == "18" && soil == "3"){ #unknown irrigation
      Irr_N = 4
    } else if (Irr_Categorical == "0"){
      Irr_N = 0
    } else if (Irr_Categorical == "5"){
      Irr_N = 5
    } else if (Irr_Categorical == "10"){
      Irr_N = 10
    } else if (Irr_Categorical == "15"){
      Irr_N = 15
    } else if (Irr_Categorical == "20"){
      Irr_N = 20
    } else if (Irr_Categorical == "25"){
      Irr_N = 25
    } else if (Irr_Categorical == "30"){
      Irr_N = 30
    } else if (Irr_Categorical == "35"){
      Irr_N = 35
    } else {
      Irr_N = 40}
    
    Irr = Irr_N*inchesApplied*.226
    # calc totalInputs-------------------
    if (binary == "2") {
      totalInputs = fert + Man + Symbiotic_N_Fixation + Precip + Dry_Deposition + Crop_Seed + Nonsymbiotic_Fixation
    } else if (binary == "1") {
      totalInputs = fert + Man + Symbiotic_N_Fixation + Irr + Precip + Dry_Deposition + Crop_Seed + Nonsymbiotic_Fixation}
    # end of if statement for irrigation

    # calc output and leachable---------------
    harvestedN = yield*nContent
    totalOutput =  harvestedN + ammonia + Erosion + Runoff + Misc + Ammonia_sen + denitri
    vals$leachable = round(totalInputs - totalOutput - Total_Storage_Change, digits=2)
    #Adjust RTN for amount of lost fertilizer to leaching (e.g. lost money) - assume that nitrogen
    #recovery efficiency is 50% and therefore 50% of the N leaching is attibuted to fertilizer
    #vals$RTNadj = vals$RTN - 0.5*(vals$Leachable*vals$N_Price)

    #Makes all negative leachable N equal to 0
    if(vals$leachable <=0) {
      vals$leachable=0
    } else{
      vals$leachable=vals$leachable
    }

    # create plot-----------
    newLeachN <- c(Scenario = vals$count, leachN = vals$leachable)
    print(newLeachN)
    leach$df <- bind_rows(leach$df, newLeachN)
    print(leach$df)

    output$NleachPlot <- renderPlotly({
      validate(
        need(is.data.frame(leach$df), "add scenarios")
      )

      y <- list(
        title = " ",
        #range = c(0, max(pred_table$df$Erosion)),
        side = "top"
      )
      x <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )

      plot_ly(leach$df,
              y = ~leachN, x = ~Scenario,
              marker = list(color = '#D7BC3D'),
              type = "bar",
              hovertext = ~paste0("Scenario", Scenario, ": N leach ", leachN),
              hoverinfo = "text") %>%
        layout(title = "Predicted N loss <br> (lbs/acre)",
               xaxis = x, yaxis = y, barmode = 'group',
               margin = list(t=100))

      })
    
    newRTN <- c(Scenario = vals$count, RTN = vals$RTN)
    print(newRTN)
    return$df <- bind_rows(return$df, newRTN)
    print(return$df)

    output$MRTNplot <- renderPlotly({
      
      validate(
        need(is.data.frame(return$df), "add scenarios")
      )

      y <- list(
        title = " ",
        #range = c(0, max(pred_table$df$Erosion)),
        side = "top"
      )
      x <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )

      plot_ly(return$df,
              y = ~RTN, x = ~Scenario,
              marker = list(color = '#4A5D23'),
              type = "bar",
              hovertext = ~paste0("Scenario", Scenario, ": RTN ", RTN),
              hoverinfo = "text") %>%
        layout(title = "Return to N ($/acre)",
               xaxis = x, yaxis = y, barmode = 'group',
               margin = list(t=100))

    })
    
    newScenario <- c(Scenario = vals$count, 'Crop' = input$cropID, 'Fertilizer' = input$Fert,
                     'N leach' = vals$leachable, 'Return to N' = vals$RTN)
    
    scenario$df <- bind_rows(scenario$df, newScenario)
    
    # scenario dataframe---------------
    output$scenarios <- render_gt({
      
      validate(
        need(is.data.frame(scenario$df), "add scenarios")
      )
      
      scenario$df %>%
        as_tibble() %>%
        gt()
    })
    
    

    })

  #clear-----------
  observeEvent(input$reset_button, {
                 vals = NULL
                 return$df = NULL
                 leach$df = NULL
                 scenario$df = NULL
                 })

}