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
#library(shinyMobile)
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
players_fpts <- readRDS("Z:/GitHub/MBSL/data/players_shiny.rds") 
players_fpts$GM <- as.factor(players_fpts$GM)
players_fpts$position <- as.factor(players_fpts$position)
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
  
  # Reiter für die Liga
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
    
    
  # Reiter für die Roster
  tabPanel("Roster",
    fluidRow(column(3,
            pickerInput("roster_gm",
                        label = "Choose a Team",
                        choices = levels(players_fpts$GM),
                        selected = levels(players_fpts$GM),
                        options = list(`actions-box` = TRUE,
                                       `live-search` = TRUE,
                                       `multiple-separator` = " | "),
                        multiple = T)),
            column(3,
            pickerInput("roster_roster",
                        label = "Choose a Spot",
                        choices = levels(players_fpts$Spot),
                        selected = levels(players_fpts$Spot),
                        options = list(`actions-box` = TRUE),
                        multiple = T)),
            column(3,
                   pickerInput("roster_player",
                               label = "Choose a Player",
                               choices = levels(players_fpts$Player),
                               selected = levels(players_fpts$Player),
                               options = list(`actions-box` = TRUE,
                                              `live-search` = TRUE,
                                              `multiple-separator` = " | "),
                               multiple = T)),
            column(3,
                   pickerInput("roster_position",
                               label = "Choose a Position",
                               choices = levels(players_fpts$position),
                               selected = levels(players_fpts$position),
                               options = list(`actions-box` = TRUE,
                                              `live-search` = TRUE),
                               multiple = T))
            ), 
  DT::dataTableOutput("Roster")
  ),
 
  # Reiter für die Vertragsverlängerung
  tabPanel("Extensions",
           fluidRow(column(3,
                           pickerInput("contract_gm",
                                       label = "Choose a Team",
                                       choices = levels(players_fpts$GM),
                                       selected = levels(players_fpts$GM),
                                       options = list(`actions-box` = TRUE,
                                                      `live-search` = TRUE),
                                       multiple = T)),
                    column(3,
                           pickerInput("contract_roster",
                                       label = "Choose a Spot",
                                       choices = levels(players_fpts$Spot),
                                       selected = "Roster",
                                       options = list(`actions-box` = TRUE),
                                       multiple = T)),
                    column(3,
                           pickerInput("contract_player",
                                       label = "Choose a Player",
                                       choices = levels(players_fpts$extension),
                                       selected = levels(players_fpts$extension),
                                       options = list(`actions-box` = TRUE,
                                                      `live-search` = TRUE),
                                       multiple = T)),
                    column(3,
                           pickerInput("contract_position",
                                       label = "Choose a Position",
                                       choices = levels(players_fpts$position),
                                       selected = levels(players_fpts$position),
                                       options = list(`actions-box` = TRUE,
                                                      `live-search` = TRUE),
                                       multiple = T))
           ), 
           DT::dataTableOutput("Extensions")
  ), 
  
### Reiter für die Vertragsverlängerung - Spielerauswahl
  tabPanel("Extensions2",
           fluidRow(column(12,
                           strong("Vorsicht: In den Eingabefeldern zur 'Load' erfolgt keine Prufung auf 100%."),
                           br())),
           fluidRow(column(3,
                           pickerInput("extension_player_1",
                                       label = "Choose a Player",
                                       choices = levels(players_fpts$extension),
                                       selected = NULL,
                                       multiple = TRUE,
                                       options = list(`live-search` = TRUE,
                                                      pickerOptions(maxOptions = 1))
                                       )),
                    column(3,
                           numericInput("extension_length_1",
                                       label = "Choose a Contract Length",
                                       min = 1,
                                       max = 5,
                                       step = 1,
                                       value = 1
                                       )),
                    column(1,
                           numericInput("extension_year1_1",
                                       label = "Year 1 Load",
                                       min = 0,
                                       max = 100,
                                       step = 5,
                                       value = 20
                           )),
                    column(1,
                           numericInput("extension_year2_1",
                                        label = "Year 2 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year3_1",
                                        label = "Year 3 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year4_1",
                                        label = "Year 4 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year5_1",
                                        label = "Year 5 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           ))
           ),
           fluidRow(column(3,
                           pickerInput("extension_player_2",
                                       label = "Choose a Player",
                                       choices = levels(players_fpts$extension),
                                       selected = NULL,
                                       multiple = TRUE,
                                       options = list(`live-search` = TRUE,
                                                      pickerOptions(maxOptions = 1))
                           )),
                    column(3,
                           numericInput("extension_length_2",
                                       label = "Choose a Contract Length",
                                       min = 1,
                                       max = 5,
                                       step = 1,
                                       value = 1
                           )),
                    column(1,
                           numericInput("extension_year1_2",
                                        label = "Year 1 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year2_2",
                                        label = "Year 2 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year3_2",
                                        label = "Year 3 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year4_2",
                                        label = "Year 4 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year5_2",
                                        label = "Year 5 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           ))
           ),
           fluidRow(column(3,
                           pickerInput("extension_player_3",
                                       label = "Choose a Player",
                                       choices = levels(players_fpts$extension),
                                       selected = NULL,
                                       multiple = TRUE,
                                       options = list(`live-search` = TRUE,
                                                      pickerOptions(maxOptions = 1))
                           )),
                    column(3,
                           numericInput("extension_length_3",
                                       label = "Choose a Contract Length",
                                       min = 1,
                                       max = 5,
                                       step = 1,
                                       value = 1
                           )),
                    column(1,
                           numericInput("extension_year1_3",
                                        label = "Year 1 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year2_3",
                                        label = "Year 2 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year3_3",
                                        label = "Year 3 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year4_3",
                                        label = "Year 4 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           )),
                    column(1,
                           numericInput("extension_year5_3",
                                        label = "Year 5 Load",
                                        min = 0,
                                        max = 100,
                                        step = 5,
                                        value = 20
                           ))
           ),
           DT::dataTableOutput("Extensions2")
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
          !(is.na(base_2021)) &
          GM %in% input$roster_gm &
            position %in% input$roster_position &
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
  output$Extensions <- DT::renderDataTable({
    DT::datatable(
      players_fpts %>%
        filter(
          !(is.na(extension)) &
          GM %in% input$contract_gm &
            position %in% input$contract_position &
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

  ##### Contract Table per Player
  
  
  
  output$Extensions2 <- DT::renderDataTable({
    DT::datatable(bind_rows(
      players_fpts %>%
        filter(
          Player %in% c(input$extension_player_1,input$extension_player_2,input$extension_player_3) &
            Spot!="Off-Roster"
        ) %>%
        select(Player,
               base_2021,
               guarantee_2021,
               base_2022,
               guarantee_2022,
               base_2023,
               guarantee_2023,
               base_2024,
               guarantee_2024,
               contract) %>%
        mutate(base_2025 = NA_real_,
               guarantee_2025 = NA_real_,
               contract_value = rowSums(data.frame(
                 base_2021,
                 guarantee_2021,
                 base_2022,
                 guarantee_2022,
                 base_2023,
                 guarantee_2023,
                 base_2024,
                 guarantee_2024)
                 , na.rm = T),
        contract_value_year = contract_value/contract) %>% 
        select(-contract),
      ### output player 1 with extension length
      players_fpts %>%
        filter(Player %in% c(input$extension_player_1)&
                 Spot!="Off-Roster") %>%
        mutate(guarantee = case_when(input$extension_length_1 == 1 ~ extension_value*0.3,
                                     input$extension_length_1 == 2 ~ extension_value*0.4,
                                     input$extension_length_1 == 3 ~ extension_value*0.5,
                                     input$extension_length_1 == 4 ~ extension_value*0.6,
                                     input$extension_length_1 == 5 ~ extension_value*0.7,
                                     T ~ 0) * input$extension_length_1,
               base_2021_new = extension_value,
               guarantee_2021_new = guarantee*(input$extension_year1_1/100),
               base_2022_new = case_when(input$extension_length_1==1 ~ 0,
                                         T ~ extension_value),
               guarantee_2022_new = case_when(input$extension_length_1==1 ~ 0,
                                              T ~ guarantee*(input$extension_year2_1/100)),
               base_2023_new = case_when(input$extension_length_1 %in% c(1,2) ~ 0,
                                         T ~ extension_value),
               guarantee_2023_new = case_when(input$extension_length_1 %in% c(1,2) ~ 0,
                                              T ~ guarantee*(input$extension_year3_1/100)),
               base_2024_new = case_when(input$extension_length_1 %in% c(1,2,3) ~ 0,
                                         T ~ extension_value),
               guarantee_2024_new = case_when(input$extension_length_1 %in% c(1,2,3) ~ 0,
                                              T ~ guarantee*(input$extension_year4_1/100)),
               base_2025_new = case_when(input$extension_length_1 %in% c(1,2,3,4) ~ 0,
                                         T ~ extension_value),
               guarantee_2025_new = case_when(input$extension_length_1 %in% c(1,2,3,4) ~ 0,
                                              T ~ guarantee*(input$extension_year5_1/100)),
               contract_value = sum(
                 base_2021_new,
                 guarantee_2021_new,
                 base_2022_new,
                 guarantee_2022_new,
                 base_2023_new,
                 guarantee_2023_new,
                 base_2024_new,
                 guarantee_2024_new,
                 base_2025_new,
                 guarantee_2025_new),
               contract_value_year = contract_value / input$extension_length_1) %>% 
        group_by(Player) %>% 
        summarise(
          base_2021 = sum(base_2021_new, na.rm = T),
          guarantee_2021 = sum(guarantee_2021_new, na.rm = T),
          base_2022 = sum(base_2022_new, na.rm = T),
          guarantee_2022 = sum(guarantee_2022_new, na.rm = T),
          base_2023 = sum(base_2023_new, na.rm = T),
          guarantee_2023 = sum(guarantee_2023_new, na.rm = T),
          base_2024 = sum(base_2024_new, na.rm = T),
          guarantee_2024 = sum(guarantee_2024_new, na.rm = T),
          base_2025 = sum(base_2025_new, na.rm = T),
          guarantee_2025 = sum(guarantee_2025_new, na.rm = T),
          contract_value,
          contract_value_year
        ),
      ### output player 2 with extension length
      players_fpts %>%
        filter(Player %in% c(input$extension_player_2)&
                 Spot!="Off-Roster") %>%
        mutate(guarantee = case_when(input$extension_length_2 == 1 ~ extension_value*0.3,
                                     input$extension_length_2 == 2 ~ extension_value*0.4,
                                     input$extension_length_2 == 3 ~ extension_value*0.5,
                                     input$extension_length_2 == 4 ~ extension_value*0.6,
                                     input$extension_length_2 == 5 ~ extension_value*0.7,
                                     T ~ 0) * input$extension_length_2,
               base_2021_new = extension_value,
               guarantee_2021_new = guarantee*(input$extension_year1_2/100),
               base_2022_new = case_when(input$extension_length_2==1 ~ 0,
                                         T ~ extension_value),
               guarantee_2022_new = case_when(input$extension_length_2==1 ~ 0,
                                              T ~ guarantee*(input$extension_year2_2/100)),
               base_2023_new = case_when(input$extension_length_2 %in% c(1,2) ~ 0,
                                         T ~ extension_value),
               guarantee_2023_new = case_when(input$extension_length_2 %in% c(1,2) ~ 0,
                                              T ~ guarantee*(input$extension_year3_2/100)),
               base_2024_new = case_when(input$extension_length_2 %in% c(1,2,3) ~ 0,
                                         T ~ extension_value),
               guarantee_2024_new = case_when(input$extension_length_2 %in% c(1,2,3) ~ 0,
                                              T ~ guarantee*(input$extension_year4_2/100)),
               base_2025_new = case_when(input$extension_length_2 %in% c(1,2,3,4) ~ 0,
                                         T ~ extension_value),
               guarantee_2025_new = case_when(input$extension_length_2 %in% c(1,2,3,4) ~ 0,
                                              T ~ guarantee*(input$extension_year5_2/100)),
               contract_value = sum(
                 base_2021_new,
                 guarantee_2021_new,
                 base_2022_new,
                 guarantee_2022_new,
                 base_2023_new,
                 guarantee_2023_new,
                 base_2024_new,
                 guarantee_2024_new,
                 base_2025_new,
                 guarantee_2025_new),
               contract_value_year = contract_value / input$extension_length_2) %>% 
        group_by(Player) %>% 
        summarise(
          base_2021 = sum(base_2021_new, na.rm = T),
          guarantee_2021 = sum(guarantee_2021_new, na.rm = T),
          base_2022 = sum(base_2022_new, na.rm = T),
          guarantee_2022 = sum(guarantee_2022_new, na.rm = T),
          base_2023 = sum(base_2023_new, na.rm = T),
          guarantee_2023 = sum(guarantee_2023_new, na.rm = T),
          base_2024 = sum(base_2024_new, na.rm = T),
          guarantee_2024 = sum(guarantee_2024_new, na.rm = T),
          base_2025 = sum(base_2025_new, na.rm = T),
          guarantee_2025 = sum(guarantee_2025_new, na.rm = T),
          contract_value,
          contract_value_year
        ),
      ### output player 3 with extension length
      players_fpts %>%
        filter(Player %in% c(input$extension_player_3)&
                 Spot!="Off-Roster") %>%
        mutate(guarantee = case_when(input$extension_length_3 == 1 ~ extension_value*0.3,
                                     input$extension_length_3 == 2 ~ extension_value*0.4,
                                     input$extension_length_3 == 3 ~ extension_value*0.5,
                                     input$extension_length_3 == 4 ~ extension_value*0.6,
                                     input$extension_length_3 == 5 ~ extension_value*0.7,
                                     T ~ 0) * input$extension_length_3,
               base_2021_new = extension_value,
               guarantee_2021_new = guarantee*(input$extension_year1_3/100),
               base_2022_new = case_when(input$extension_length_3==1 ~ 0,
                                         T ~ extension_value),
               guarantee_2022_new = case_when(input$extension_length_3==1 ~ 0,
                                              T ~ guarantee*(input$extension_year2_3/100)),
               base_2023_new = case_when(input$extension_length_3 %in% c(1,2) ~ 0,
                                         T ~ extension_value),
               guarantee_2023_new = case_when(input$extension_length_3 %in% c(1,2) ~ 0,
                                              T ~ guarantee*(input$extension_year3_3/100)),
               base_2024_new = case_when(input$extension_length_3 %in% c(1,2,3) ~ 0,
                                         T ~ extension_value),
               guarantee_2024_new = case_when(input$extension_length_3 %in% c(1,2,3) ~ 0,
                                              T ~ guarantee*(input$extension_year4_3/100)),
               base_2025_new = case_when(input$extension_length_3 %in% c(1,2,3,4) ~ 0,
                                         T ~ extension_value),
               guarantee_2025_new = case_when(input$extension_length_3 %in% c(1,2,3,4) ~ 0,
                                              T ~ guarantee*(input$extension_year5_3/100)),
               contract_value = sum(
                 base_2021_new,
                 guarantee_2021_new,
                 base_2022_new,
                 guarantee_2022_new,
                 base_2023_new,
                 guarantee_2023_new,
                 base_2024_new,
                 guarantee_2024_new,
                 base_2025_new,
                 guarantee_2025_new),
               contract_value_year = contract_value / input$extension_length_3) %>% 
        group_by(Player) %>% 
        summarise(
          base_2021 = sum(base_2021_new, na.rm = T),
          guarantee_2021 = sum(guarantee_2021_new, na.rm = T),
          base_2022 = sum(base_2022_new, na.rm = T),
          guarantee_2022 = sum(guarantee_2022_new, na.rm = T),
          base_2023 = sum(base_2023_new, na.rm = T),
          guarantee_2023 = sum(guarantee_2023_new, na.rm = T),
          base_2024 = sum(base_2024_new, na.rm = T),
          guarantee_2024 = sum(guarantee_2024_new, na.rm = T),
          base_2025 = sum(base_2025_new, na.rm = T),
          guarantee_2025 = sum(guarantee_2025_new, na.rm = T),
          contract_value,
          contract_value_year
        ),
    ) %>% 
      mutate(guarantee_sum = rowSums(data.frame(
        guarantee_2021,
        guarantee_2022,
        guarantee_2023,
        guarantee_2024,
        guarantee_2025),
        na.rm = T
      ),
      guarantee_na = rowSums(is.na(data.frame(
        guarantee_2021,
        guarantee_2022,
        guarantee_2023,
        guarantee_2024,
        guarantee_2025))),
      ) %>%
      group_by(Player) %>% 
      mutate(guarantee_max = max(guarantee_sum),
             guarantee_na_max = max(guarantee_na)
      ) %>% 
      ungroup() %>% 
      mutate(guarantee_remaining= case_when((guarantee_max-guarantee_sum)<=0 ~ 0,
                                            (guarantee_na_max-guarantee_na)==0 ~ 0,
                                            T ~ guarantee_max-guarantee_sum)) %>% 
      select(-guarantee_sum,-guarantee_max,-guarantee_na,-guarantee_na_max) %>% 
      arrange(Player),
      colnames = c(
        "ID",
        "Player",
        "Base_21",
        "Garantie_21",
        "Base_22",
        "Garantie_22",
        "Base_23",
        "Garantie_23",
        "Base_24",
        "Garantie_24",
        "Base_25",
        "Garantie_25",
        "Vertragswert",
        "Vertragswert pro Jahr",
        "Zusatzliche Garantien"
      ),
      options = list(pageLength = 20),
      filter = "top",
      style = "bootstrap"
    ) %>% 
      formatRound(columns = c(2:14), digits = 2)
  })   

}

# Run the application 
shinyApp(ui = ui, server = server)

