dashboardPage(
    dashboardHeader(
        title = "Fangraphs Visualization",
        titleWidth = 350
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Query Data", tabName = "query", icon = icon("database")),
            menuItem("Result Data", tabName = "data", icon = icon("table")),
            menuItem("Data Visualization", tabName = "graphics", icon = icon("area-chart"))
        )
    ),
    dashboardBody(theme = shinytheme('flatly'),
                  tabItems(
                      tabItem('query',
                              fluidPage(useShinyjs(),
                                        #theme = shinytheme('darkly'),
                                        titlePanel("FanGraphs Custom Leaderboards"),
                                        fluidRow(
                                            #player, team, league 
                                            column(2,radioButtons("stat_lvl",'Stat Level:',choices = stat_lvl, selected = 'Player Stats')),
                                            #batting, pitching, fielding
                                            column(2,radioButtons("stat_type",'Stat Type:',choices = stat_type, selected = 'Batting')),
                                            #league
                                            column(2,selectInput('lg', 'League:', choices = leagues, selected = 'All Leagues')),
                                            #team
                                            column(2,selectInput('team', 'Team:', choices = teams$Team, selected = 'All Teams')),
                                            #position
                                            column(2, uiOutput('pos'))
                                        ),
                                        fluidRow(
                                            #single or multiple seasons
                                            column(2,radioButtons("n_season",'',choices = c('Single Season','Multiple Seasons'), selected = 'Single Season')),
                                            #start season
                                            column(2,selectInput('season1', 'Start Season:',choices = seasons,selected = year(today()))),
                                            #end season
                                            column(2, uiOutput('season2')),
                                            #stat split
                                            column(2, uiOutput('split')),
                                            #pa/inn/ip requirement
                                            column(2, uiOutput('qual'))
                                        ),
                                        fluidRow(
                                            #Only active roster
                                            column(2,checkboxInput('active', 'Active Roster',value = F)),
                                            #Split stats by team (if traded)
                                            column(2, checkboxInput('split_team', 'Split Teams',value = F)),
                                            #Split stats by season
                                            column(2, checkboxInput('splt_season', 'Split Seasons',value = F)),
                                            #Only rookies
                                            column(2, checkboxInput('rookies', 'Rookies',value = F)),
                                            #Age
                                            column(4, sliderInput('age', 'Age Range:',min = 14, max = 58, value = c(14,58)))
                                        ),
                                        fluidRow(
                                            uiOutput('fields')
                                        ),
                                        #batting, pitching, fielding
                                        actionButton("submit", "Submit", class = "btn-primary")
                              )
                      ),
                      tabItem('data',
                              fluidPage(
                                  dataTableOutput('table')
                              )
                      ),
                      tabItem('graphics',
                              fluidPage(
                                  sidebarPanel(
                                      h4('Select variables to plot:'),
                                      selectInput('type','Type of Graph:', choices = c('Scatter','Histogram','Line','Tile')),
                                      uiOutput('x_var'),
                                      conditionalPanel("input.type != 'Histogram'",
                                                       uiOutput('y_var'),
                                                       uiOutput('color_var')),
                                      conditionalPanel("(input.stat_lvl=='Team Stats' & input.n_season = 'Multiple Seasons' & input.splt_season) | input.stat_lvl=='Player Stats'",
                                          uiOutput('group'),
                                          selectInput('funcx','Apply to function to X:', choices = c('Mean','Sum','Min','Max','Count'))),
                                      conditionalPanel("input.type != 'Histogram'",
                                                       selectInput('funcy','Apply to function to Y:', choices = c('Mean','Sum','Min','Max','Count')),
                                                       selectInput('funcc','Apply to function to Color:', choices = c('Mean','Sum','Min','Max','Count'))),
                                      div(actionButton("graph_submit", "Submit", class = "btn-primary"), align = 'center')
                                  ),
                                  mainPanel(
                                      plotlyOutput('graph')
                                  )
                              )
                      )
                  )
    )
    
)