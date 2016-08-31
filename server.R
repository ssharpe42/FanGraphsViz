
shinyServer( function(input, output, session) {
    #reactive data
    values = reactiveValues(data = NULL, columns = NULL, numeric_col = NULL)
    
    #positions
    output$pos = renderUI({
        selectInput('pos','Position:', choices = switch(input$stat_type,
                                                        'Batting' = position_bat,
                                                        'Pitching' = position_pit,
                                                        'Fielding' = position_fld),
                    selected = 'All')
    })
    #season range
    output$season2 = renderUI({
        if(input$n_season=='Single Season') choices = input$season1
        else choices = seasons[seasons>=input$season1]
        selectInput('season2', 'End Season:',choices = choices,selected = input$season1)
    })
    #splits
    output$split = renderUI({
        selectInput('split','Split:', choices = switch(input$stat_type,
                                                       'Batting' = bat_splits$Split,
                                                       'Pitching' = pit_splits$Split,
                                                       'Fielding' = pit_splits$Split),
                    selected = 'Full Season')
    })
    #min pa, ip, inn
    output$qual = renderUI({
        label = switch(input$stat_type,
                       'Batting' = 'Min PA:',
                       'Pitching' = 'Min IP:',
                       'Fielding' = 'Min Inn:')
        selectInput('qual',label, choices = qualified_list, selected = 'Qualified')
    })
    output$fields = renderUI({
        if(is.null(input$split)) choices = default = 'AB'
        else if(input$split=='Full Season'){
            choices =  switch(input$stat_type,
                              'Batting' = bat_stats$Stat,
                              'Pitching' = pit_stats$Stat,
                              'Fielding' = fld_stats$Stat)
            default =  switch(input$stat_type,
                              'Batting' = filter(bat_stats,Default==1)$Stat,
                              'Pitching' = filter(pit_stats, Default==1)$Stat,
                              'Fielding' = filter(fld_stats, Default==1)$Stat)
        }else{
            choices =  switch(input$stat_type,
                              'Batting' = bat_stats_sm$Stat,
                              'Pitching' = pit_stats_sm$Stat,
                              'Fielding' = fld_stats$Stat)
            default =  switch(input$stat_type,
                              'Batting' = filter(bat_stats_sm,Default==1)$Stat,
                              'Pitching' = filter(pit_stats_sm, Default==1)$Stat,
                              'Fielding' = filter(fld_stats, Default==1)$Stat)
        }
        
        selectizeInput('fields', 'Select Stats:', choices = choices, selected = default, multiple = T)
    })
    # splt team only with player stats
    observe({ 
        if(input$stat_lvl!='Player Stats'){
            updateCheckboxInput(session, 'split_team',value = F)
            disable('split_team')
        }else{
            enable('split_team')
        }
        
    })
    #Disable some options if not full season
    observe({ 
        if(!is.null(input$split)){
            if(input$split!='Full Season'){
                updateSliderInput(session, 'age',value = c(14,58))
                updateCheckboxInput(session, 'splt_season',value=F)
                updateCheckboxInput(session, 'rookies', value = F)
                disable('age'); disable('splt_season');disable('rookies');
            }else{
                enable('age'); enable('splt_season');enable('rookies');
            }
        } 
    })
    observeEvent(input$submit,{
        withProgress(message='Scraping FanGraphs data...', value= 0,{
            if(input$split=='Full Season'){
                stat_df =  switch(input$stat_type,
                                  'Batting' = bat_stats,
                                  'Pitching' = pit_stats,
                                  'Fielding' = fld_stats)
            }else{
                stat_df =  switch(input$stat_type,
                                  'Batting' = bat_stats_sm,
                                  'Pitching' = pit_stats_sm,
                                  'Fielding' = fld_stats)
            }
            
            url  = get_url(input$pos, input$stat_type, input$lg, input$qual, input$fields, input$season1, input$split, 
                           input$season2, input$splt_season, input$rookies, input$team, input$split_team,
                           (input$stat_lvl=='League Stats'), (input$stat_lvl=='Team Stats'), input$active, input$age, 
                           stat_df = stat_df, 
                           split_df =  switch(input$stat_type,
                                              'Batting' = bat_splits,
                                              'Pitching' = pit_splits,
                                              'Fielding' = fld_splits),
                           team_df = teams)
            
            values$data = scrape_data(url)
            values$cols = names(values$data)
            values$numeric = which(sapply(values$data, class)=='numeric')
            values$scaled = scale(values$data[,values$numeric])
            test<<-values$data
            cols <<-values$cols
        })
    })
    
    output$table = renderDataTable({
        datatable(values$data)
    })
    
    ###########  Vizualizations ###############
    graph_var = reactiveValues()
    
    output$x_var = renderUI({
        if(is.null(values$data)) choices = 'No Data'
        else choices = values$cols
        selectInput('x_var','X-Variable', choices = choices,selected = choices[values$numeric][1])
    })
    output$y_var = renderUI({
        if(is.null(values$data)) choices = 'No Data'
        else choices = values$cols
        selectInput('y_var','Y-Variable', choices = choices, selected = choices[values$numeric][1])
    })
    output$group = renderUI({
        if(is.null(values$data)) choices = 'No Data'
        else if(input$stat_lvl=='Player Stats'){
            if(input$splt_season & input$n_season=='Multiple Seasons') choices = c('None', 'Team', 'Season')
            else choices = c('None', 'Team')
        }else if(input$stat_lvl=='Team Stats'){
            if(input$splt_season & input$n_season=='Multiple Seasons') choices = c('None', 'Team', 'Season')
            else choices = c('None')
        }else{
            choices=c('None')
        }
        selectInput('group','Group Variable', choices = choices,selected = 'None')
    })
    output$color_var = renderUI({
        if(is.null(values$data)) choices = 'No Data'
        else choices = c('None',values$cols)
        selectInput('color_var','Color Variable', choices = choices, selected = 'None')
    })
    
    
    set_graph_var = function(x, y, color, group, funcx, funcy, funcc){
       graph_var$x = x
       graph_var$y = y
       if(color=='None') graph_var$color =NULL
       else graph_var$color = color
       graph_var$group = group
       if(group!='None'){
           graph_var$funcx = switch(funcx, 'Mean'='mean','Sum'='sum','Min'='min','Max'='max')
           graph_var$funcy = switch(funcy, 'Mean'='mean','Sum'='sum','Min'='min','Max'='max')
           if(color!='None') graph_var$funcc = switch(funcc, 'Mean'='mean','Sum'='sum','Min'='min','Max'='max')
       }
    }
    plot_data <- reactive({
        
        if(graph_var$group!='None'){
            if(!is.null(graph_var$color)){
                data = group_by_(values$data, graph_var$group) %>%
                    summarise_(V1=paste0(graph_var$funcx,'(',graph_var$x,')'),
                               V2=paste0(graph_var$funcy,'(',graph_var$y,')'),
                               V3=paste0(graph_var$funcc,'(',graph_var$color,')')
                               ) %>% ungroup
                names(data) = c(graph_var$group, graph_var$x, graph_var$y, graph_var$color)
            }else{
                data = group_by_(values$data, graph_var$group) %>%
                    summarise_(V1=paste0(graph_var$funcx,'(',graph_var$x,')'),
                               V2=paste0(graph_var$funcy,'(',graph_var$y,')')
                               ) %>% ungroup
                names(data) = c(graph_var$group, graph_var$x, graph_var$y)
            }
            return(data)
        }else{
            return(values$data)
        }
        
    })
    
    observeEvent(input$graph_submit, {set_graph_var(input$x_var, input$y_var, input$color_var, input$group, input$funcx, input$funcy, input$funcc) })
    
    
    output$graph = renderPlotly({
        if(is.null(values$data) || is.null(graph_var$x)) return()
        else{
            
            p <-ggplot(plot_data())  
            print(graph_var$color)
            if(input$type=='Scatter')  p <- p + geom_point(aes_string(x = graph_var$x, y = graph_var$y,colour=graph_var$color))
            if(input$type=='Line')  p <- p + geom_line(aes_string(x = graph_var$x, y = graph_var$y,colour=graph_var$color))
            if(input$type=='Tile')  p <- p + geom_tile(aes_string(x = graph_var$x, y = graph_var$y,fill=graph_var$color))
            if(input$type=='Histogram')  p <- p + geom_histogram(aes_string(graph_var$x))
            if(graph_var$group !='None' ) p <- p + geom_text(aes_string(x = graph_var$x, y = graph_var$y,label=graph_var$group, colour =graph_var$color ))
                
            ggplotly(p+ theme_fivethirtyeight()) %>% layout(autosize=T)
        }
    })
    
}
)