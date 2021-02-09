#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/



################################################################################
#
#   Okay also ich habe jetzt eine "Shiny Webapp" geoeffnet und die Datei 
#   "shinytest.csv" liegt in dem Ordner der Webapp "Beispiel"
#
#
################################################################################

library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(DT)
library(RCurl)
library(janitor)
options(digits = 2,scipen = 9999)

light <- bs_theme(version = 4)
dark <- bs_theme(
  version = 4,
  bg = "#222222",
  fg = "#FFFFFF",
  primary = "#375A7F",
  base_font = font_google("Roboto")
)

# bs_theme_preview(dark)

###################################
# Load Data: 
# Interner Link: Z:/GitHub/MBSL/
players_fpts <- readRDS("data/players_shiny.rds") 
players_fpts$GM <- as.factor(players_fpts$GM)
players_fpts$Player <- as.factor(players_fpts$Player)
players_fpts$extension <- as.factor(players_fpts$extension)
players_fpts$Spot <- as.factor(players_fpts$Spot)


###################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = light, 
  div(
    class = "custom-control custom-switch", 
    tags$input(
      id = "dark_mode", type = "checkbox", class = "custom-control-input",
      onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")
    ),
    tags$label(
      "Dark mode", `for` = "dark_mode", class = "custom-control-label"
    )
  ),
  # Application title
  #titlePanel(title = div(img(src="Upside Bowl_II.png"), "Upsidebowl")),
  titlePanel(title = div(img(src="logo.jpg", height = "200px")), "MBSL Fantasy"),
  uiOutput("sleeper"),
  # fluidRow(column(width = 3, offset = 0,
  #                 div(style = "height:20px;width:100%;background-color: #ffffff;border-style: solid;border-color: #ffffff",
  #                     tags$h3("")))),
  # Navigation Bar:
  navbarPage(
    title = "MBSL Roster",
  
  # Reiter f�r die Liga
    tabPanel("League",
             fluidRow(
               pickerInput("league_roster",
                                         label = "Choose a Spot",
                                         choices = levels(players_fpts$Spot),
                                         selected = levels(players_fpts$Spot),
                                         options = list(`actions-box` = TRUE),
                                         multiple = T)
             ),
             DT::dataTableOutput("League")
    ),
    
    
  # Reiter f�r die Roster
  tabPanel("Roster",
    fluidRow(column(4,
            pickerInput("roster_gm",
                        label = "Choose a Team",
                        choices = levels(players_fpts$GM),
                        selected = levels(players_fpts$GM),
                        options = list(`actions-box` = TRUE,
                                       `live-search` = TRUE),
                        multiple = T)),
            column(4,
            pickerInput("roster_roster",
                        label = "Choose a Spot",
                        choices = levels(players_fpts$Spot),
                        selected = levels(players_fpts$Spot),
                        options = list(`actions-box` = TRUE),
                        multiple = T)),
            column(4,
                   pickerInput("roster_player",
                               label = "Choose a Player",
                               choices = levels(players_fpts$Player),
                               selected = levels(players_fpts$Player),
                               options = list(`actions-box` = TRUE),
                               multiple = T))
            ), 
  DT::dataTableOutput("Roster")
  ),
 
  # Reiter f�r die Vertragsverl�ngerung
  tabPanel("Contract",
           fluidRow(column(4,
                           pickerInput("contract_gm",
                                       label = "Choose a Team",
                                       choices = levels(players_fpts$GM),
                                       selected = levels(players_fpts$GM),
                                       options = list(`actions-box` = TRUE,
                                                      `live-search` = TRUE),
                                       multiple = T)),
                    column(4,
                           pickerInput("contract_roster",
                                       label = "Choose a Spot",
                                       choices = levels(players_fpts$Spot),
                                       selected = levels(players_fpts$Spot),
                                       options = list(`actions-box` = TRUE),
                                       multiple = T)),
                    column(4,
                           pickerInput("contract_player",
                                       label = "Choose a Player",
                                       choices = levels(players_fpts$extension),
                                       selected = levels(players_fpts$extension),
                                       options = list(`actions-box` = TRUE,
                                                      `live-search` = TRUE),
                                       multiple = T))
           ), 
           DT::dataTableOutput("Contract")
  ), 
    # Text-Beispiel Fuer Christian:
  tabPanel("FAQ - WIP", 
           fluidRow(column(width=12,
                           # h1 ist eine gro?e Ueberschrift
                           h1("Frequently Asked Questions"),
                           # br() fuegt eine Leerzeile ein
                           br(),
                           strong("Achtung - Hier wird noch getestet!"),
                           br(),
                           # strong() ist quasi bold
                           # p() macht einen neuen Absatz (paragraph)
                           # em() generiert italic text
                           # code() erzeugt Text im "code-look"
                           tags$strong("Vertragsverlangerung und Free Agency"),
                           tags$div(
                             tags$ul(
                               tags$li("Fur Vertragsverlangerungen und Free Agents stehen jeden GM 15 Vertragsjahre zur Verfugung"),
                               tags$ul(
                                 tags$li("Die Summe aller Vertrage durfen 15 Vertragsjahre nicht ubersteigen"),
                                 tags$li("Jahresvertrage sind bei dieser Addition nicht zu berucksichtigen"),
                                 tags$li("Es konnen keine Vertragsjahre in die Zukunft ubernommen werden"),
                                 tags$li("Vertrage die aus dem Rookie Draft werden automatisch festgelegt (siehe Punkt 11)")
                             ),
                               tags$li("Vertragsverlangerungen"),
                               tags$ul(
                                 tags$li("Es durfen nur Vertrage mit Spielern verlangert werden, deren Restlaufzeit 2 Jahre oder weniger betragen."),
                                 tags$li("Spieler aus der MBSL Free Agency zur Verfugung."),
                                 tags$li("Alle Spieler, die nach dem Rookie Draft verpflichtet werden, bekommen automatisch einen 1-Jahresvertrag")
                           ),
                               tags$li("Bei einer Vertragsverlangerung wird der Spielerwert anhand der vergangenen Fantasy Leistung bewertet."),
                               tags$li("Es werden dafur die Werte der letzten drei Fantasy Jahre (Week 1-16) als Grundlage benutzt"),
                               tags$ul(
                                 tags$li("60% fur die abgelaufenen Saison"),
                                 tags$li("30% fur die Saison vor der o.g."),
                                 tags$li("10% fur die Saison vor der o.g."),
                                 tags$li("Sollte eine der drei Saison nicht zur Verfugung stehen, wird dieser Prozentsatz im gleichen
                                 Verhaltnis auf die anderen beiden aufgeteilt")
                           )
                           ))
                           
           )
           )
  )))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) dark else light
    )
  })
  
  # Link zur Liga in Sleeper:
  
  url1 <- a("MBSL", href="https://sleeper.app/leagues/649920742707777536")
  output$sleeper <- renderUI({
    tagList("Sleeper:", url1)
  })
  
  ##### League Table per GM
  output$League <- DT::renderDataTable({
    DT::datatable(
      players_fpts %>% 
        filter(Spot %in% input$league_roster) %>% 
        group_by(GM) %>% 
        summarise(
          under_contract = n(),
          base_2021 = sum(base_2021, na.rm = T),
          guarantee_2021 = sum(guarantee_2021, na.rm = T),
          Hit_2021 = base_2021+guarantee_2021,
          base_2022 = sum(base_2022, na.rm = T),
          guarantee_2022 = sum(guarantee_2022, na.rm = T),
          Hit_2022 = base_2022+guarantee_2022,
          base_2023 = sum(base_2023, na.rm = T),
          guarantee_2023 = sum(guarantee_2023, na.rm = T),
          Hit_2023 = base_2023+guarantee_2023,
          base_2024 = sum(base_2024, na.rm = T),
          guarantee_2024 = sum(guarantee_2024, na.rm = T),
          Hit_2024 = base_2024+guarantee_2024,
          cap_per_player = 
            (base_2021 +
               base_2022 +
               base_2023 +
               base_2024 +
               guarantee_2021 +
               guarantee_2022 +
               guarantee_2023 +
               guarantee_2024) / under_contract) %>% 
        select(-tidyselect::starts_with(c("base","guarantee"))),
      colnames = c(
        "ID",
        "GM",
        "Players",
        "Cap Hit '21",
        "Cap Hit '22",
        "Cap Hit '23",
        "Cap Hit '24",
        "Cap Hit per Player"
      ),
      options =  list(pageLength = 20),
              style = "bootstrap") %>% 
      formatRound(columns = c(3:7), digits = 2)
  })
  
  ##### Roster Table per GM
  output$Roster <- DT::renderDataTable({
    DT::datatable(
      players_fpts %>%
        filter(
          GM %in% input$roster_gm &
            Spot %in% input$roster_roster &
            Player %in% input$roster_player
        ) %>%
        select(
          Player,
          base_2021,
          guarantee_2021,
          base_2022,
          guarantee_2022,
          base_2023,
          guarantee_2023,
          base_2024,
          guarantee_2024,
          ppm
        ) %>%
        adorn_totals(name = input$roster_gm),
      colnames = c(
        "ID",
        "Player",
        "Base_21",
        "Guarantee_21",
        "Base_22",
        "Guarantee_22",
        "Base_23",
        "Guarantee_23",
        "Base_24",
        "Guarantee_24",
        "Points per Mio. $"
      ),
      options = list(pageLength = 20),
      filter = "top",
      style = "bootstrap"
    ) %>% 
      formatRound(columns = c(2:10), digits = 2)
  })

  ##### Contract Table per GM
  output$Contract <- DT::renderDataTable({
    DT::datatable(
      players_fpts %>%
        filter(
          !(is.na(extension)) &
          GM %in% input$contract_gm &
            Spot %in% input$contract_roster &
            Player %in% input$contract_player
        ) %>%
        mutate(delta = extension_value-base_2021,
               value = ppm*(-delta)) %>%
        select(
          Player,
          contract,
          base_2021,
          extension_value,
          value
        ) %>% 
        adorn_totals(name = input$contract_gm),
      colnames = c(
        "ID",
        "Player",
        "Restlaufzeit",
        "Base_21",
        "Base_extension",
        "Value"
      ),
      options = list(pageLength = 20),
      filter = "top",
      style = "bootstrap"
    ) %>% 
      formatRound(columns = c(3:5), digits = 2)
  })  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
