# create list for sources of points and adp
proj_source <- c(
  "CBS", "ESPN", "FantasyPros", "FantasySharks",
  "FFToday", "NumberFire",
  "NFL", "RTSports", "WalterFootball"
)

adp_source <- c("RTS", "CBS", "Yahoo", "NFL", "FFC", "MFL")

# make shiny ui
ui <-
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    # create a sidebar for inputs
    shiny::sidebarPanel(
      width = 3,
      shiny::h2("Fantasy Football Projections",
        class = "h-title",
        align = "center"
      ),
      # add spacing
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          align = "center",
          shiny::actionButton(
            inputId = "twitter_id", label = "@semperty",
            icon = shiny::icon("twitter"),
            onclick = "window.open('https://twitter.com/semperty')",
            style = "color: white; background-color: #26a7de; border-color: #2e6da4"
          )
        ),
        shiny::column(
          width = 6,
          align = "center",
          shiny::actionButton(
            inputId = "coffee_id", label = "Buy Me a Coffee",
            icon = shiny::icon("mug-hot"),
            onclick = "window.open('https://www.buymeacoffee.com/semperty')",
            style = "background-color: #ecb02e; border-color: #2e6da4"
          )
        ),
        shiny::column(
          width = 9,
          align = "center",
          # add spacing
          shiny::hr()
        ),
        shiny::column(
          width = 6,
          align = "center",
          # choose sources for projected points
          shinyWidgets::pickerInput(
            inputId = "proj_source",
            label = "Projection Sources",
            choices = proj_source,
            selected = "ESPN",
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 6,
          align = "center",
          # choose sources for adp
          shinyWidgets::pickerInput(
            inputId = "adp_source",
            label = "ADP Source",
            choices = adp_source,
            selected = "CBS",
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 12,
          align = "center",

          # choose scoring system
          shinyWidgets::prettyRadioButtons(
            inputId = "scoring_system",
            label = "Scoring System",
            choices = c("PPR", "1/2 PPR", "Std"),
            selected = "PPR",
            inline = TRUE,
            width = "100%",
            fill = TRUE
          ),

          # choose the VBD baseline for QBs
          shiny::sliderInput(
            inputId = "qb_baseline",
            label = "QB VBD Baseline",
            value = 15,
            min = 1, max = 50
          ),

          # choose the VBD baseline for RBs
          shiny::sliderInput(
            inputId = "rb_baseline",
            label = "RB VBD Baseline",
            value = 36,
            min = 1, max = 50
          ),

          # choose the VBD baseline for WRs
          shiny::sliderInput(
            inputId = "wr_baseline",
            label = "WR VBD Baseline",
            value = 38,
            min = 1, max = 50
          ),

          # choose the VBD baseline for TEs
          shiny::sliderInput(
            inputId = "te_baseline",
            label = "TE VBD Baseline",
            value = 8,
            min = 1, max = 50
          ),

          # add spacing
          shiny::br(),

          # add an action button to update the data
          dipsaus::actionButtonStyled(
            "update",
            "Update",
            icon = shiny::icon("refresh"),
            type = "primary",
            width = "100%"
          ),
          # add spacing
          shiny::br(),
          shiny::br(),
          shinyBS::bsCollapse(
            id = "vbd",
            shinyBS::bsCollapsePanel(
              "VBD Explanation",
              shiny::div(
                "Value-Based Drafting (VBD) is the idea that ",
                shiny::a(
                  href =
                    "https://www.footballguys.com/article/2019-value-based-drafting",
                  "the value of a player is determined not by the number of points
                  scored, but by how much they outscore their peers at their position."
                ),
                shiny::br(),
                shiny::br(),
                "The defaults are set to the standard given in the Football Guys
                article. However, you can alter the baselines to your liking to
                develop your own, unique VBD values. The numbers in the sidebar
                represent the baseline for each position (e.g. QB15) against which
                each player at the position will be measured. This can show you how
                valuable a player is relative to the players at his own position and
                allow you to compare player values across positions."
              )
            )
          )
        )
      )
    ),
    # create the space for the table
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Projections Data",
          shiny::fluidRow(
            shiny::column(
              width = 12,
              offset = 0.5,
              align = "center",
              shiny::br(),
              DT::dataTableOutput("table") |>
                shinycssloaders::withSpinner()
            )
          )
        ),
        shiny::tabPanel(
          "About"
        )
      )
    )
  )

# make the back end server
server <- function(input, output, session) {
  shinyjs::onclick(
    "toggle_vbd",
    shinyjs::toggle(id = "vbd", anim = TRUE)
  )

  # set the delay reaction
  projections <- shiny::eventReactive(input$update, {
    ff_raw <- readr::read_rds(here::here("data", "ff_raw.Rds"))

    # set the scoring settings
    score_settings <- list(
      pass = list(
        pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, 
        pass_tds = 4, pass_int = -3, pass_40_yds = 0, pass_300_yds = 0, 
        pass_350_yds = 0, pass_400_yds = 0
      ),
      rush = list(
        all_pos = TRUE,
        rush_yds = 0.1, rush_att = 0, rush_40_yds = 0, rush_tds = 6,
        rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0
      ),
      rec = list(
        all_pos = TRUE,
        rec = dplyr::case_when(
          input$scoring_system == "PPR" ~ 1,
          input$scoring_system == "1/2 PPR" ~ 0.5,
          input$scoring_system == "Std" ~ 0
        ),
        rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
        rec_150_yds = 0, rec_200_yds = 0
      ),
      misc = list(
        all_pos = TRUE,
        fumbles_lost = -3, fumbles_total = 0,
        sacks = 0, two_pts = 2
      ),
      kick = list(
        xp = 1.0, fg_0019 = 3.0, fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
        fg_50 = 5.0, fg_miss = 0.0
      ),
      ret = list(
        all_pos = TRUE,
        return_tds = 6, return_yds = 0
      ),
      idp = list(
        all_pos = TRUE,
        idp_solo = 1, idp_asst = 0.5, idp_sack = 2, idp_int = 3, 
        idp_fum_force = 3, idp_fum_rec = 2, idp_pd = 1, idp_td = 6, 
        idp_safety = 2
      ),
      dst = list(
        dst_fum_rec = 2, dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
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

    # create list of adp sources selected
    adp_sources <- paste(input$adp_source, collapse = ",")

    data <- purrr::map(
      ff_raw, ~ .x |> dplyr::filter(data_src %in% input$proj_source)
      )
    attr(data, "season") <- 2023
    attr(data, "week") <- 0

    # convert scraped data to projections
    projections <-
      ffanalytics::projections_table(
        data,
        scoring_rules = score_settings,
        vor_baseline = c(
          "QB" = input$qb_baseline,
          "RB" = input$rb_baseline,
          "WR" = input$wr_baseline,
          "TE" = input$te_baseline
        ),
        avg_type = "average"
      )

    # add more ecr, adp, and uncertainty to projections
    projections <- projections |>
      ffanalytics::add_ecr() |>
      ffanalytics::add_adp(sources = adp_sources) |>
      ffanalytics::add_uncertainty()


    # add player info
    projections <- projections |>
      ffanalytics::add_player_info()

    # re-organize the table
    projections <- projections |>
      dplyr::rename(ecr = overall_ecr, vbd = points_vor, pos_tier = tier) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.numeric), round, 3),
        name = stringr::str_c(first_name, last_name, sep = " ")
      ) |>
      dplyr::select(
        position, team, name, age, rank, pos_rank,
        adp, ecr, pos_ecr, points, vbd, pos_tier
      )
  })


  output$table <- DT::renderDataTable({
    DT::datatable(
      projections() |>
        janitor::clean_names(
          case = "title",
          abbreviations = c("ECR", "ADP", "VBD")
        ),
      extensions = "Buttons",
      options = list(
        filter = "top",
        pageLength = 25,
        scrollX = FALSE, lengthMenu = c(25, 50, 100, 500),
        paging = TRUE, searching = TRUE,
        fixedColumns = TRUE, autoWidth = TRUE,
        ordering = TRUE, dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
  })
}

shiny::shinyApp(ui, server)
