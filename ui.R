

ui <- dashboardPage(
  dashboardHeader (title = "High Jump Database", titleWidth = 450),
  dashboardSidebar(
    # fileInput("file1", "Choose files", multiple = TRUE,
    #           accept = c("text/csv",
    #                      "text/comma-separated-values,text/plain",
    #                      ".csv")),
    # selectInput("distance", "Distance:",
    #             choices = c("500" = "Labelled_data_500",
    #                         "1000" = "Labelled_data_1000")),
    # selectInput("Report_Type", "Report Type:",
    #             c("Single Race" = "Single Race",
    #               "Two Races" = "Two Races",
    #               "vs Top 10" = "vs Top 10")),    
    selectInput("Report_Type", "Report Type:",
                c("Single Competition" = "Single Competition",
                  "Comparison" = "Comparison")),
    uiOutput("select_Name"),

    
     uiOutput("select_Competition"),
    uiOutput("select_Height"),
    

    actionButton("goButton", "Go!")
    
  ),

  dashboardBody(
    
    fluidRow(
      h3(textOutput("Summaryhead"))
    ),
    fluidRow(
      box(title = "Jumps", status = "primary", solidHeader = TRUE, width = 12,
          collapsible = TRUE,
          DT::dataTableOutput("datatable_Jumps"))
    ),
    
    fluidRow(
      box(title = "Foot Contact Time (s) - the absolute time of each foor fall during run up", status = "primary", solidHeader = TRUE, width = 12,
          collapsible = TRUE,
          DT::dataTableOutput("datatable_FootPlants"))
    ),
    fluidRow(
      box(title = "Step Times (s) - the time between each step of run up", status = "primary", solidHeader = TRUE, width = 12,
          collapsible = TRUE,
          DT::dataTableOutput("datatable_StepTimes"))
    ),
    fluidRow(
      box(title = "Foot Contact Time (s) - the total time the foot is in contact with the ground in the last 3 steps", status = "primary", solidHeader = TRUE, width = 12,
          collapsible = TRUE,
          DT::dataTableOutput("datatable_ContactTimes"))
    ),
    fluidRow(
      box(title = "Step Times", status = "primary", solidHeader = TRUE,width = 12,
          collapsible = TRUE,plotOutput("Plot_StepTimes1"))
    ),
    
    fluidRow(
      box(title = "Contact Times", status = "primary", solidHeader = TRUE,width = 12,
          collapsible = TRUE,plotOutput("Plot_ContactTimes1"))
    ),
    
    # fluidRow(
    #   box(title = "Cadence v1", status = "primary", solidHeader = TRUE,width = 12,
    #       collapsible = TRUE,plotOutput("Plot_Cadencev11"))
    # ),
    fluidRow(
      box(title = "Cadence v2", status = "primary", solidHeader = TRUE,width = 12,
          collapsible = TRUE,plotOutput("Plot_Cadencev21"))
    )
    
    #fluidRow(
    #    box(title = "Distance Comparison", status = "primary", solidHeader = TRUE, width = 12,
    #        collapsible = TRUE,
    #        DT::dataTableOutput("datatable_comparison"))
    #)
    
    #    fluidRow(
    #      box(title = "Time vs Top 10 average", status = "primary", solidHeader = TRUE,width = 12,
    #          collapsible = TRUE,plotOutput("ggplot"))
    #    )
    
    
    
  )
  
)










# Create Shiny app ----
# shinyApp(ui, server)