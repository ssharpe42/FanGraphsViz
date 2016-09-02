
shinyServer( function(input, output, session) {
    #reactive data
    values = reactiveValues(data = NULL, columns = NULL, numeric_col = NULL)
    
    #visualizations conditional on data existing
    output$menu <- renderMenu({
        if(!is.null(values$data)){
            sidebarMenu(id='tabs',menuItem("Query Data", tabName = "query", icon = icon("database"),selected = T),
                              menuItem("Result Data", tabName = "data", icon = icon("table")),
                              menuItem("Data Visualization", icon = icon("area-chart"),
                                       menuSubItem('Explore',tabName = "graphics", icon = icon('bar-chart')),
                                       menuSubItem('PCA & Clustering',tabName = 'pca_tab',icon = icon('line-chart'))))
        }else{
            sidebarMenu(id='tabs',menuItem("Query Data", tabName = "query", icon = icon("database"),selected = T),
                              menuItem("Result Data", tabName = "data", icon = icon("table")))
        }
    })
    isolate({updateTabItems(session, "tabs", "query")})
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
            if(input$split!='Full Season' | input$stat_lvl =='League Stats'){
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
            #values$scaled = scale(values$data[,values$numeric])
            #values$pca = prcomp(values$data[,values$numeric])
            #test<<-values$data
            #cols <<-values$cols
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
            if(input$splt_season & input$n_season=='Multiple Seasons') choices = c('None', 'Name', 'Team', 'Season','Season,Team')
            else choices = c('None', 'Team')
        }else if(input$stat_lvl=='Team Stats'){
            if(input$splt_season & input$n_season=='Multiple Seasons') choices = c('None', 'Team','Season')
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
    output$funcx = renderUI({
        if(grepl(input$x_var,input$group)) choices = 'None'
        else choices = c('Mean','Sum','Min','Max','var','sd')
        selectInput('funcx','Apply to function to X:', choices = choices,selected = choices[1])
    }) 
    output$funcy = renderUI({
        if(grepl(input$y_var,input$group))choices = 'None'
        else choices = c('Mean','Sum','Min','Max','var','sd')
        selectInput('funcy','Apply to function to Y:', choices =choices, selected = choices[1])
    }) 
    output$funcc = renderUI({
        if(grepl(input$y_var,input$group)) choices = 'None'
        else choices = c('Mean','Sum','Min','Max','var','sd')
        selectInput('funcc','Apply to function to Color:', choices =choices,selected = choices[1])
    })
    output$labels = renderUI({
        if(!input$label_tf) return()
        else if(input$group=='None') choices = values$cols
        else if(input$color_var!='None') choices = c(str_split(input$group,',')[[1]],input$x_var, input$y_var, input$color_var)
        else choices = c(str_split(input$group,',')[[1]],input$x_var, input$y_var)
        selectInput('labels','Choose Labels', choices =choices)
    }) 
    output$smooth_val = renderUI({
        if(!input$smooth) return()
        else return(selectInput('smooth_val','Choose Degree:', choices =1:4))
    }) 

    set_graph_var = function(x, y, color, group, funcx, funcy, funcc, label_tf, labels, type, smooth,smooth_val){
       graph_var$x = x
       graph_var$y = y
       if(color=='None') graph_var$color =NULL
       else graph_var$color = color
       graph_var$group = group
       graph_var$label_tf=label_tf
       graph_var$labels = labels
       graph_var$type = type
       graph_var$smooth= smooth
       graph_var$smooth_val = as.numeric(smooth_val)
       if(group!='None'){
   
           graph_var$funcx = switch(funcx, 'None'='first','Mean'='mean','Sum'='sum','Min'='min','Max'='max','var'='var','sd'='sd')
           graph_var$funcy = switch(funcy, 'None'='first','Mean'='mean','Sum'='sum','Min'='min','Max'='max','var'='var','sd'='sd')
           if(color!='None') graph_var$funcc = switch(funcc, 'None'='first','Mean'='mean','Sum'='sum','Min'='min','Max'='max','var'='var','sd'='sd')
       }
    }
    
    observeEvent(input$graph_submit, { set_graph_var(input$x_var, input$y_var, input$color_var, input$group, 
                                                     input$funcx, input$funcy, input$funcc, input$label_tf, 
                                                     input$labels, input$type,input$smooth, input$smooth_val) })

    plot_data <- reactive({

        if(graph_var$group!='None'){
            if(graph_var$group == 'Season,Team') data = group_by(values$data, Season, Team) 
            else data = group_by_(values$data,graph_var$group) 
            if(!is.null(graph_var$color)){
                data = data %>%
                    summarise_(V1=paste0(graph_var$funcx,'(',graph_var$x,')'),
                               V2=paste0(graph_var$funcy,'(',graph_var$y,')'),
                               V3=paste0(graph_var$funcc,'(',graph_var$color,')')
                               ) %>% ungroup
                names(data) = c(str_split(graph_var$group,',')[[1]], graph_var$x, graph_var$y, graph_var$color)
            }else{
                data = data %>%
                      summarise_(V1=paste0(graph_var$funcx,'(',graph_var$x,')'),
                               V2=paste0(graph_var$funcy,'(',graph_var$y,')')
                               ) %>% ungroup
                names(data) = c(str_split(graph_var$group,',')[[1]], graph_var$x, graph_var$y)
            }
            return(data)
        }else{
            return(values$data)
        }
        
    })
    
    output$graph = renderPlotly({
        if(is.null(values$data) || is.null(graph_var$x)) return()
        else{
            p <-ggplot(plot_data())  
            
            if(graph_var$type=='Scatter'){
                if(graph_var$label_tf){
                    p <- p + geom_text(aes_string(x = graph_var$x, y = graph_var$y, colour=graph_var$color, label=graph_var$labels))
                }else{
                    p <- p + geom_point(aes_string(x = graph_var$x, y = graph_var$y, colour=graph_var$color)) 
                }
                if(graph_var$smooth){

                    p<-p+ stat_smooth(aes_string(x = graph_var$x, y = graph_var$y),method="lm", formula = y ~ poly(x, graph_var$smooth_val), se=T)
                }
            }
            if(graph_var$type=='Line'){
                if(graph_var$group=='Season,Team' & graph_var$x=='Season'){
                    p <- p + geom_line(aes_string(x = graph_var$x, y = graph_var$y, colour=graph_var$color, group = 'Team')) 
                }else{
                p <- p + geom_line(aes_string(x = graph_var$x, y = graph_var$y, colour=graph_var$color)) 
                }
            }
            if(graph_var$type=='Tile'){
                p <- p + geom_tile(aes_string(x = graph_var$x, y = graph_var$y,fill=graph_var$color)) 
            }
            if(graph_var$type=='Histogram'){
                p <- p + geom_histogram(aes_string(x = graph_var$x), bins = input$bins) 
            }
  
            ggplotly(p+ theme_fivethirtyeight() ) %>% layout(autosize=T)
        }
    })
    
    output$pca_var = renderUI({
        selectizeInput('pca_var','Select variables for PCA',
                       choices = values$cols[values$numeric], multiple =T, selected =  values$cols[values$numeric])
    })
    output$pca_label = renderUI({
        checkboxInput('pca_label','PCA Labels', value = F)
    })
    
    output$pca_graph = renderPlotly({
        if(length(input$pca_var)<2) return()
        data = values$data
        if(input$stat_lvl=='Player Stats') row.names(data) = data$Name
        else if(input$stat_lvl=='Team Stats') row.names(data) = data$Team
        else row.names(data) = year(data$Season)
        
        if(input$pca_label) {
            p <- autoplot(prcomp(values$data[,input$pca_var]), data = data, label.size =2,
                          loadings = TRUE, loadings.colour = 'red', label =T, shape=F,
                          loadings.label = TRUE, loadings.label.size = 3)
        }else{
            p <- autoplot(prcomp(values$data[,input$pca_var]), data = data, label.size =2,
                          loadings = TRUE, loadings.colour = 'red', label =F, shape=T,
                          loadings.label = TRUE, loadings.label.size = 3)
        }
        ggplotly(p+theme_fivethirtyeight()+
                     ggtitle('PCA'))
    })
    
    output$loadings_graph = renderPlotly({
        if(length(input$pca_var)<2) return()
        df = prcomp(values$data[,input$pca_var])$rotation %>% as.data.frame()
        df = mutate(df, Variable = row.names(df))%>%
            gather(Component, Value,-Variable) %>%
            mutate(PCnum = as.numeric(gsub('[[:alpha:]]','',Component))) %>%
            arrange(PCnum)
        p<- ggplot(df,aes(x = PCnum, y = Variable,fill=Value)) +
            geom_tile()+
            theme_fivethirtyeight()+
            ggtitle('PCA Component Vector Values')+
            scale_x_continuous('Components')+
            scale_y_discrete('Variables')+
            scale_fill_gradientn(colors  = brewer.pal(10,'RdBu'))
        ggplotly(p)
    })
    
  }
)