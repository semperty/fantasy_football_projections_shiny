# load libraries
library(shiny) 
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(bslib)
library(DT)
library(dipsaus)
library(ffanalytics)

# create list for sources of points and adp
proj_source <- c("CBS", "ESPN", "FantasyPros", "FantasySharks",
                 "FFToday", "NumberFire",
                 "NFL", "RTSports", "WalterFootball")

adp_source <- c("RTS", "CBS", "Yahoo", "NFL", "FFC", "MFL")



# make shiny ui
ui <- fluidPage(
  # give the app a title
  titlePanel("Fantasy Football Projections"),
  
  # connect twitter and coffee
  splitLayout(cellWidths = c(140, 170),
              cellArgs = list(style = "padding: 10px"),
              actionButton(inputId='twitter_id', label="@semperty",
                           icon = icon("twitter"),
                           onclick ="window.open('https://twitter.com/semperty')",
                           style="color: white; background-color: #26a7de; border-color: #2e6da4"),
              actionButton(inputId='coffee_id', label="Buy Me a Coffee",
                           icon = icon("mug-hot"),
                           onclick ="window.open('https://www.buymeacoffee.com/semperty')",
                           style="background-color: #ecb02e; border-color: #2e6da4")),
  
  fluidRow(
    # create a sidebar for inputs
    sidebarPanel(width = 3,
                 # choose sources for projected points
                 dropdownButton(
                   inputId = "proj_source_dropdown",
                   label = "Projection Sources",
                   status = "primary",
                   circle = FALSE,
                   width = "100%",
                   checkboxGroupInput("proj_source", 
                                      "Which sources would you like to include in your projections", 
                                      proj_source,
                                      selected = "ESPN")),
                 
                 # add spacing 
                 tags$div(style = "height: 10px;"),
                 
                 # choose sources for adp
                 dropdownButton(
                   inputId = "adp_source_dropdown",
                   label = "ADP Sources",
                   status = "primary",
                   circle = FALSE,
                   width = "100%",
                   checkboxGroupInput("adp_source", 
                                      "Which sources would you like to include in your ADP", 
                                      adp_source,
                                      selected = "CBS")),
                
                 # add spacing 
                 tags$div(style = "height: 10px;"),
                 
                 # choose scoring system
                 dropdownButton(
                   inputId = "scoring_system_dropdown",
                   label = "Scoring System",
                   status = "primary",
                   circle = FALSE,
                   width = "100%",
                   radioButtons(
                     inputId = "scoring_system",
                     label = "Which scoring system do you use?",
                     choices = c("PPR", "1/2 PPR", "Std"),
                     selected = "PPR",
                     width = "100%"
                   )),
                 
                 # add spacing 
                 tags$div(style = "height: 10px;"),
                 
                 # choose the VBD baseline for QBs
                 dropdownButton(
                   inputId = "qb_dropdown",
                   label = "QB VBD Baseline",
                   status = "primary",
                   circle = FALSE,
                   width = "100%",
                   numericInput(
                     inputId = "qb_baseline",
                     label = "QB VBD Baseline",
                     value = 15,
                     min = 4, 
                     max = 32,
                     width = "100%"
                   )),
                 
                 # add spacing 
                 tags$div(style = "height: 10px;"),
                 
                 # choose the VBD baseline for RBs
                 dropdownButton(
                   inputId = "rb_dropdown",
                   label = "RB VBD Baseline",
                   status = "primary",
                   circle = FALSE,
                   width = "100%",
                   numericInput(
                     inputId = "rb_baseline",
                     label = "RB VBD Baseline",
                     value = 36,
                     min = 4, 
                     max = 32,
                     width = "100%"
                   )),
                 
                 # add spacing 
                 tags$div(style = "height: 10px;"),
                 
                 # choose the VBD baseline for WRs
                 dropdownButton(
                   inputId = "wr_dropdown",
                   label = "WR VBD Baseline",
                   status = "primary",
                   circle = FALSE,
                   width = "100%",
                   numericInput(
                     inputId = "wr_baseline",
                     label = "WR VBD Baseline",
                     value = 38,
                     min = 4, 
                     max = 32,
                     width = "100%"
                   )),
                 
                 # add spacing 
                 tags$div(style = "height: 10px;"),
                 
                 # choose the VBD baseline for TEs
                 dropdownButton(
                   inputId = "te_dropdown",
                   label = "TE VBD Baseline",
                   status = "primary",
                   circle = FALSE,
                   width = "100%",
                   numericInput(
                     inputId = "te_baseline",
                     label = "TE VBD Baseline",
                     value = 8,
                     min = 4, 
                     max = 32,
                     width = "100%"
                   )),
                 
                 # add spacing 
                 tags$div(style = "height: 10px;"),
                 
                 # add an action button to update the data
                 actionButtonStyled(
                   "update",
                   "Update",
                   icon = icon("refresh"),
                   type = "primary",
                   width = "100%"
                 ),
                 
                 # add spacing 
                 tags$div(style = "height: 30px;"),
                 
                 h4("VBD Explanation"),
                 p("VBD stands for Value-Based Drafting. Value-Based Drafting is the idea that ", 
                   a(href = "https://www.footballguys.com/article/2019-value-based-drafting",
                     "the value of a player is determined not by the number of points he scores. His value is determined by how much he outscores his peers at his particular position. "),
                   div("This app is set to the standard given in the linked article from Football Guys.
                       However, you can alter the baselines to your liking to develop your own, unique VBD values.
                       The numbers in the sidebar represent the baseline for each position (e.g. QB15) against which each player at the position will be measured.
                       This can show you how valuable a player is relative to the players at his own position and allow you to compare player values across positions."))),
    
    

    
    # create the space for the table
    fluidPage(column(9, shinycssloaders::withSpinner(dataTableOutput("table")))
    ))
)

# make the back end server
server <- function(input, output, session){
  # set the delay reaction
  projections <- eventReactive(input$update, {
    
    # set the scoring settings
    score_settings <- list(
      pass = list(
        pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
        pass_int = -3, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
        pass_400_yds = 0
      ),
      rush = list(
        all_pos = TRUE,
        rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
        rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
      rec = list(
        all_pos = TRUE,
        rec = case_when(
          input$scoring_system == "PPR" ~ 1,
          input$scoring_system == "1/2 PPR" ~ 0.5,
          input$scoring_system == "Std" ~ 0), 
        rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
        rec_150_yds = 0, rec_200_yds = 0
      ),
      misc = list(
        all_pos = TRUE,
        fumbles_lost = -3, fumbles_total = 0,
        sacks = 0, two_pts = 2
      ),
      kick = list(
        xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
        fg_50 = 5.0,  fg_miss = 0.0
      ),
      ret = list(
        all_pos = TRUE,
        return_tds = 6, return_yds = 0
      ),
      idp = list(
        all_pos = TRUE,
        idp_solo = 1, idp_asst = 0.5, idp_sack = 2, idp_int = 3,  idp_fum_force = 3,
        idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
      ),
      dst = list(
        dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
        dst_blk = 1.5, dst_ret_yds = 0, dst_pts_allowed = 0
      ),
      pts_bracket = list(
        list(threshold = 0, points = 10),
        list(threshold = 6, points = 7),
        list(threshold = 20, points = 4),
        list(threshold = 34, points = 0),
        list(threshold = 99, points = -4)
      )
    )
    
    # create list of sources selected
    proj_sources <- paste(input$proj_source, collapse = ",")
    adp_sources <- paste(input$adp_source, collapse = ",")
    
    # scrape selected sources
    data <- scrape_data(src = proj_sources,
                        pos = c("QB", "RB", "WR", "TE"),
                        season = 2023,
                        week = NULL
    )
    
    # convert scraped data to projections
    projections <- projections_table(data,
                                     scoring_rules = score_settings,
                                     vor_baseline = c("QB" = input$qb_baseline, 
                                                      "RB" = input$rb_baseline,
                                                      "WR" = input$wr_baseline, 
                                                      "TE" = input$te_baseline),
                                     avg_type = "average")
    
    # add more ecr, adp, and uncertainty to projections
    projections <- projections %>% 
      add_ecr() %>% 
      add_adp(sources = adp_sources) %>%
      add_uncertainty
    
    
    # add player info
    projections <- projections %>%
      add_player_info()
    
    # re-organize the table 
    projections <- projections %>%
      select(position, team, first_name, last_name, age, rank, pos_rank, adp, overall_ecr, pos_ecr, points, points_vor, tier) %>%
      rename(ecr = overall_ecr, vbd = points_vor, pos_tier = tier) %>%
      mutate_if(is.numeric,
                round,
                digits = 1)
  })
  
  
  output$table <- DT::renderDataTable({
    DT::datatable(projections(),
                  extensions = 'Buttons', 
                  options = list(filter = "top",
                                 scrollX=TRUE, lengthMenu = c(50,100,300),
                                 paging = TRUE, searching = TRUE,
                                 fixedColumns = TRUE, autoWidth = TRUE,
                                 ordering = TRUE, dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel')))})
  
  
}

shinyApp(ui, server)

