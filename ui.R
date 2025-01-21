ui <- fluidPage(
  theme = shinytheme("simplex"), 
  dashboardPage(skin = "red",
                dashboardHeader(title="Nitrogen Budget"),
                # sidebar-------------
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Calculator", tabName = "calc"),
                    menuItem("Main", tabName = "main"),
                    menuItem("Background", tabName = "back"),
                    menuItem("Nitrogen Guidelines", tabName = "nitro"),
                    menuItem("Nitrogen Cycle", tabName = "cycle"),
                    menuItem("References and Assumptions", tabName = "ref")
                    )
                  ),
                # body-----------
                dashboardBody(
                  tabItems(
                    #calc--------------------
                    tabItem(tabName = "calc",
                            h2("Nitrogen Decision Support Tool for Wisconsin"),
                            fluidRow(
                              column(width=6,
                                     ## select crop----------
                                     box(width=NULL,selectInput("cropID", "Crop Type",
                                                                c("Corn [bu/ac]" = "corn",
                                                                  "Potato [cwt/ac]" = "pota")),
                                         ## select county-------------
                                         uiOutput("countyName"),
                                         sliderInput(inputId = "Yield", label = "Yield",
                                                     value = 150, min = 80, max = 200),
                                         #Shows the avg yield for the county selected
                                         ## county avg--------------------
                                         span(textOutput("County_Avg"), style = "text-align:center;"),
                                         br(),
                                         selectInput("Soil", label = "Select the predominant soil texture", 
                                                     choices = list("Sandy" = 1, "Loamy" = 2, "Clay" = 3), selected = 1),
                                         # conditional panels--------------
                                         ## if corn and not sandy soil-----------------
                                         conditionalPanel(
                                           condition = "input.cropID == 'corn' && input.Soil != 1",     
                                           selectInput("Yield_Size", label = "Select if soil is high or medium yielding", 
                                                       choices = c(Corn_Loamy$Yield), selected = 2),
                                           selectInput("Previous", label = "Select previous crop",
                                                       choices = c(Corn_Loamy$Previous),selected = 2),
                                           selectInput('N_C', 'Select N fertilizer to Crop price ratio',
                                                       choices = seq(from = 0.05, to = 0.2, by = 0.05)),
                                           ),
                                         radioButtons("Binary", label = "Is irrigation applied?",
                                                      choices = list("Yes" = 1, "No" = 2), selected = 2),
                                         ## if irrigated-----------------
                                         conditionalPanel(condition = "input.Binary == '1'",
                                                           selectInput("Irr_Categorical", label = "Nitrate concentration [mg/L] in irrigation water",
                                                                       c("unknown" = 18,"0 [mg/L]" = 0, "5 [mg/L]" = 5, "10 [mg/L]" = 10, 
                                                                         "15 [mg/L]" = 15, "35 [mg/L]" = 35, "40 [mg/L]" = 40),selected = "un"),
                                                          numericInput("Inches_Applied", "Inches of irrigation applied throughout growing season",10)),
                                         ## if corn and sandy and irrigated------------
                                         conditionalPanel(condition = "input.cropID == 'corn' && input.Soil==1 && input.Binary==1",  
                                                          selectInput("N_C_Y", label = "Select N to Crop price ratio", 
                                                                      choices = c(Corn_Sandy_Irr_Y$N_C), selected = 2)), 
                                         ## if corn and sandy and non irrigated------------
                                         ##TODO input$N_C_Y and N_C_N could be the same? look at Corn_Sany_Irr
                                         conditionalPanel(condition = "input.cropID == 'corn' && input.Soil==1 && input.Binary==2",
                                                          selectInput("N_C_N", label = "Select N to Crop price ratio", 
                                                                     choices = c(Corn_Sandy_Irr_N$N_C), 
                                                                     selected = 2)),
                                         ## if potato-----------------------
                                         ##TODO Want to delete 
                                         conditionalPanel(condition = "input.cropID == 'pota' ",            
                                                         # selectInput("Yield_Range", label = "Select Yield Range", 
                                                         #             choices = c(Potato_N_Rate$HundredWeight), 
                                                         #             selected = 2),
                                                         selectInput("SOM", label = "Select Soil Organic Matter (%)", 
                                                                     choices = c(Potato_N_Rate$SOM), 
                                                                     selected = 2)),
                                         
                                         ## fertilizer values------------
                                         sliderInput(inputId = "Fert",label = "Fertilizer [lb/N per ac]",
                                                     value = 150, min = 0, max = 300),
                                         span(textOutput("N_Rate"), style = "text-align:center;"),
                                         br(),
                                         ## MRTN calculations - corn ---------------
                                         conditionalPanel(condition = "input.cropID == 'corn' ",
                                                         # user selects corn price, auto-selected as $3.20/bu
                                                         numericInput("Corn_Price", "Corn Price [$/bu]",3.20)),
                                         ## MRTN calculations - potato -------------------
                                         conditionalPanel(condition = "input.cropID == 'pota' ",
                                                         # user selects Potato price, auto-selected as $9.10/cwt
                                                         numericInput("Pota_Price", "Potato Price [$/cwt]",9.10)),
                                         numericInput("N_Price", "N Price [$/lb N]",0.40),
                                         box(width=NULL, actionButton("action", "Calculate")))),
                              ## calculation displays--------------
                              column(width = 6,
                                    box(width = NULL,
                                        h4("Leaching output"),
                                        plotlyOutput(outputId="NleachPlot")),
                                    box(width = NULL,
                                        h4("Return to N output"),
                                        plotlyOutput("MRTNplot")),
                                    box(width = NULL,
                                        h4("Scenario description table"),
                                        gt_output("scenarios")),
                                    #Creates a reset button
                                    box(width=NULL,
                                        actionButton("reset_button", "Clear Page"))
                                     )
                            )),
                    # main tab-------------------
                    tabItem(tabName = "main",
                            h3("Purpose"),
                            p("This app was developed by the Kucharik lab to assess the impacts of varied agricultural land management practices across Wisconsin on nitrogen losses to the environment. By defining a farming system, users can calculate nitrogen leaching and return to nitrogen estimates. In addition, this app digitizes and contextualizes key state and university findings surrounding nutrient management and groundwater quality."),
                            h3("How to"),
                            p("This app allows for extensive user flexibility in selecting the characteristics of an agricultural system. The nitrogen leaching value will be more accurate with a more realistic system. Using actual agricultural system values is recommended, but guidance is provided below and within the input selections if users want to experiment with how different assumptions and different systems change nitrogen leaching values."),
                            p("First, select the crop you want to calculate nitrogen leaching and return to nitrogen for."),
                            p("Next, select the county that the system is in. By selecting a county, the average corn yield in that county will appear below the yield slider bar. Use this number to select a reasonable yield for your system."),
                            p("After choosing a value for yield, select a soil type. If sandy is selected, you will only be prompted to select whether or not irrigation is applied. If irrigation water has been applied, select the nitrate concentration in the irrigation water as well as the amount applied throughout the growing season. For reference, the average nitrate concentration in Wisconsin well water is 18 mg/L. If unknown is selected, this is what the default value will be."),
                            p("If loamy or clay soil is selected, you will also be prompted to indicate if the soil is medium or high yielding, as well as the previous crop grown in the field."),
                            p("Regardless of soil type, you will then be asked to select the N to Crop price ratio. Then, choose a reasonable amount of fertilizer to achieve the selected yield. Given input selections, a recommended fertilizer application rate will appear below the fertilizer slider bar. Finally, enter the corn price in $/bushel and the nitrogen price in $/pound N."),
                            p("Click the Calculate button to compute estimated leachable nitrogen and the return to nitrogen in your system."),
                            p("To reset inputs, click the 'Clear Page button'")),
                    # nutrient management tab----------------
                    tabItem(tabName = "back",
                            h3("Importance of Nitrogen Management"),
                            p("Nitrogen is a key nutrient for plant growth and is one of the main determinants of yield. However, nitrogen is highly mobile, and nitrogen from agricultural management is a main source of water contamination. Nitrogen will leach, typically in the form of nitrate [NO3-] when soil water content is greater than the soil's maximum water holding capacity. "),
                            p("From there, nitrate enters the groundwater and affects drinking water quality. Additionally, the movement of affected groundwater to freshwater streams and lakes produces major environmental concerns. When these bodies of water become too nutrient rich, water oxygen levels are reduced and can result in algae blooms, fish kills, and toxin production."),
                            h3("Management Practices that Reduce Leachable N"),
                            p("Improved timing of N application at appropriate rates, soil tests and plant monitoring, diversifying crop rotations, using cover crops, reducing tillage, optimizing N application techniques, nitrification inhibitors."),
                            ),
                    # current nitrogen management standards-----------------
                    tabItem(tabName = "nitro",
                            h2("Current Nutrient Management Standards"),  
                            p("Due to the importance of nitrogen and other key nutrient management, Wisconsin legislature has adopted the following requirements for all Wisconsin farms:"),
                            p(strong("1. Conduct soil tests to determine existing nutrient levels")),
                            p(strong("2. Determine cropping plans and relevant crop nutrient needs")),
                            p(strong("3. Count nutrient contributions from all sources, including existing soil nutrients, nutrients supplied by crops (such as N from alfalfa and soybeans), and nutrients supplied by fertilizer and manure.")),
                            p(strong ("4. Develop and implement a NM plan that considers multi-year cropping plans, crop nutrient needs, and nutrient contributions from all sources.")),
                            p(strong("5. Limit nutrient applications to UW agronomic recommendations for relevant crops (considering nutrient contributions from all sources).")),
                            p(strong("6. Use appropriate conservation practices (such as conservation tillage, cover crops, grass waterways, buffer strips, and sound manure management practices) to limit nutrient and pathogen movement to surface water and groundwater.")),
                            p(em("With recent increases in nitrate concentrations, however, it should be noted that following current standards will not be enough to address Wisconsin's nitrate issue. In fact, only 32% of Wisconsin farms currently have nutrient management plans.")),
                            ),
                    # cycle tab--------------------
                    tabItem(tabName = "cycle",
                            h3("Potential N Leaching = N Inputs - N Outputs (excluding leaching) - Change in N Storage"),
                            br(),
                            h4("The variables used are in the tables below"),
                            #6tags$img(src='N_Inputs_Outputs.png'),
                            #outputs Kevin's excel spreadsheet in a table
                            DT::dataTableOutput("table2"),
                            
                            ),
                    # references--------------
                    tabItem(tabName = "ref",
                            h3("References used"),
                            p("1. Managing Nitrogen for Groundwater Quality and Farm Profitability"),
                            p("2. NASS Quick Stats"),
                            p("3. Nutrient Application Guidelines for Field, Vegetable, and Fruit Crops in Wisconsin"),
                            h3("Assumptions"),
                            h4("N fertilizer recommendations based on soil and fertilizer N price:corn price"),
                            DT::dataTableOutput("table1"))
                    )
                  )
                )
  )



