library(shiny)
library(DT)

# Define UI for CombatSim app ----
ui <- pageWithSidebar(
    
    # App title ----
    headerPanel("D&D 5e Combat Simulator"),
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      tabsetPanel(
        tabPanel(title = "Home", 
                 br(),
                 h4(strong("Welcome to the D&D 5e Combat Simulator!")),
                 br(),
                 p("While still very much a work in progress, in its current state,
                   it's an effective means of simulating rudimentary melee combat between two parties on any scale, from small to large.
                   This simulation uses a suite of custom R functions I have made publicly available on my Github."),
                 br(),
                 p("In order to use this app, please navigate the various tabs in this side panel and adjust the settings 
                   according to your liking. All adjustments will yield changes in the main panel, which will either display 
                   turn-by-turn summaries of the actions of characters or the statistics of the combatants in table format."),
                 br(),
                 p("While this app offers a convenient, accessible, and no-cost means of simulating combat, running the R functions 
                   behind this app locally on your computer offers greater control to shape the battle as you'd like. Additionally, 
                   usage for the online app is capped at 25 hrs (collectively) per month; as such, it may not always be available. 
                   However, if you were to run the app locally on your computer, usage is unlimited."),
                 br(),
                 p("For more information on how to use the simulation most effectively or the planned development timeline, please visit the Github. If you encounter any issues,
                   please feel free to reach out using the links below."), 
                 h3(a('Github', href='https://github.com/Wjpmitchell3/Tabletop-RPG-Tools'), align = 'center'),
                 h3(a('About Me', href='https://sites.google.com/view/wjmitchell/home'), align = 'center'),
                 h3(a('Contact', href='mailto:wjpmitchell3@gmail.com'), align = 'center')),
        tabPanel(title = "Global Options", 
                 p("Use this panel to set a seed to replicate past or future simulations"),                 
                 br(),
                 # Input: Text entry to seed simulation ----
                 numericInput(inputId = "Seed", 
                              label = "Seed - An Optional Code To Replicate Simulations:",
                              value = sample(0:999999,1),
                              step = 1,
                              min = 1, 
                              width = '75%')),
        
        tabPanel(title = "PartySim", 
                 p("Use this panel to specify the details of your fighters"),                 
                 br(),
                 # Input: Text entry to specify group names ----
                 textInput(inputId = "group1",
                           label = "Name of First Group:",
                           value = "Nez's Oblates",
                           placeholder = "Nez's Oblates", 
                           width = '75%'),
                 textInput(inputId = "group2",
                           label = "Name of Second Group:",
                           value = "the Fiends",
                           placeholder = "Fiends", 
                           width = '75%'),
                 # Input: Numeric entry to specify group sizes ----                 
                 numericInput(inputId = "nPeople1",
                              label = "First Group Size:",
                              min = 1,
                              step = 1,
                              value = 10, 
                              width = '75%'),
                 numericInput(inputId = "nPeople2",
                              label = "Second Group Size:",
                              min = 1,
                              step = 1,
                              value = 150, 
                              width = '75%'),
                 # Input: Checkbox to indicate uniformity in simulated groups ----
                 checkboxInput(inputId = "Uniform",
                               label = "All Members of Each Group Should Have The Same Stats",
                               value = TRUE),
                 # Input: Numeric entry to simulate attack bonuses ----
                 numericInput(inputId = "ATK1",
                              label = "First Group ATK Bonus:",
                              min = 0,
                              step = 1,
                              value = 13, 
                              width = '75%'),
                 numericInput(inputId = "ATK2",
                              label = "Second Group ATK Bonus:",
                              min = 0,
                              step = 1,
                              value = 4, 
                              width = '75%'),
                 textInput(inputId = "DMG1",
                           label = "First Group Damage Dice:",
                           placeholder = "3d8 + 8",
                           value = "3d8 + 8", 
                           width = '75%'),
                 textInput(inputId = "DMG2",
                           label = "Second Group Damage Dice:",
                           placeholder = "1d6 + 2",
                           value = "1d6 + 2", 
                           width = '75%'),
                 numericInput(inputId = "HPmax1",
                              label = "First Group HP max:",
                              min = 1,
                              step = 1,
                              value = 178, 
                              width = '75%'),
                 numericInput(inputId = "HPmax2",
                              label = "Second Group HP max:",
                              min = 1,
                              step = 1,
                              value = 30, 
                              width = '75%'),
                 numericInput(inputId = "AC1",
                              label = "First Group AC:",
                              min = 1,
                              step = 1,
                              value = 24, 
                              width = '75%'),
                 numericInput(inputId = "AC2",
                              label = "Second Group AC:",
                              min = 1,
                              step = 1,
                              value = 12, 
                              width = '75%')),
    
        tabPanel(title = "CombatSim", 
                 p("Use this panel to specify the details of the battle"),
                 br(),
                 # Input: Selector to determine how long simulation should run ----
                 selectInput(inputId = "Ending",
                             label = "How Long Should The Simulation Run:",
                             choices =  c("Until One Side Dies (Deathmatch)" = "Death",
                                          "A Certain Number of Rounds (Timed)" = "Timed"),
                             selected = "Timed",
                             multiple = FALSE, 
                             width = '75%'),
                 # Input: Text entry to determine how many rounds simulation should run, if not death match ----
                 numericInput(inputId = "nRounds",
                              label = "Number of Rounds To Run Simulation:",
                              value = 3,
                              min = 1,
                              step = 1, 
                              width = '75%'),
                 
                 # Input: Selector to determine what turn-by-turn output the users would like ----
                 selectInput(inputId = "PlayByPlay",
                             label =  "Turn-By-Turn Output:",
                             choices = c("All Actions" = "Everything",
                                         "Attacks & Kills Only" = "Attacks",
                                         "Kills Only" = "Kills",
                                         "None" = "Off"),
                             selected = "Off",
                             multiple = FALSE, 
                             width = '75%')
        
                 )
        )
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h2("Simulation Results", align='center'),
      tabsetPanel(
        tabPanel(title = "Turn-By-Turn Summary", 
                 p("This is a report of all of the events that occurred during the simulation. 
                   The output contained here can be modified using the", strong("Turn-By-Turn Output"), 
                   "indicator on the side panel."),                 
                 br(), 
                 p(strong("Coming Soon:"), "Output downloads in .csv format"),
                 downloadButton('downloadSummary',"Download the summary"),
                 br(),
                 br(),
                 # Output: Table of the details of the fighters after the simulation ----
                 tableOutput(outputId = "summary")
                 ),
        tabPanel(title = "Combatant Stats", 
                 p("This is a table detailing the status of each fighter at the conclusion of the battle. 
                   The table is interactive and can be searched or sorted."),
                 br(), 
                 p(strong("Coming Soon:"), "Output downloads in .csv format"),
                 downloadButton('downloadTable',"Download the data"),
                 br(),
                 br(),
                 # Output: Table of the details of the fighters after the simulation ----
                 dataTableOutput(outputId = "table")
                 )
        ),
      )
    )


# Source: The primary simulation function
source('PartySim.R')
source('CombatSim.R')

# Define server ----
server <- function(input, output) {
  
    df <- reactive({

        (table <- PartySim(nGroups = 2,
                            GroupName = c(input$group1, input$group2),
                            nPeople = c(input$nPeople1,input$nPeople2),
                            Uniform = input$Uniform,
                            ATK = c(input$ATK1,input$ATK2),
                            DMG = c(input$DMG1, input$DMG2),
                            HPmax = c(input$HPmax1, input$HPmax2),
                            AC = c(input$AC1, input$AC2),
                            Seed = input$Seed))
    })

      # Rendering: Table output of the fighters ----
      output$table <- DT::renderDataTable({
        
          (table <- CombatSim(df = df(),
                              Output = "Table",
                              Ending = input$Ending,
                              nRounds = input$nRounds,
                              PlayByPlay = input$PlayByPlay,
                              Seed = input$Seed))
      })
      output$downloadTable <- downloadHandler(
                                         filename = function(){"df.csv"}, 
                                         content = function(fname){
                                         write.csv(df(), fname)
    }
  )
      
      # Rendering: Text output of the battle summary ----     
      output$summary <- renderTable({
        
          (summary <- CombatSim(df = df(),
                              Output = "Text",
                              Ending = input$Ending,
                              nRounds = input$nRounds,
                              PlayByPlay = input$PlayByPlay,
                              Seed = input$Seed))
      })
}

# Run the application 
shinyApp(ui = ui, server = server)