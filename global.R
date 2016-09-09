#setwd("~/GitHub/FanGraphsViz")
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(lubridate)
library(RColorBrewer)
library(DT)
library(shiny)
library(ggthemes)
library(ggfortify)
library(scales)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(ggrepel)
options(stringsAsFactors = F)

bat_stats = read.csv('data/bat_stats.csv') %>% arrange(ID)
bat_stats_sm = read.csv('data/bat_stats_small.csv')%>% arrange(ID)
pit_stats = read.csv('data/pit_stats.csv')%>% arrange(ID)
pit_stats_sm = read.csv('data/pit_stats_small.csv')%>% arrange(ID)
fld_stats = read.csv('data/fld_stats.csv') %>% arrange(ID)
bat_splits = read.csv('data/bat_splits.csv')%>% arrange(ID)
pit_splits = read.csv('data/pit_splits.csv') %>% arrange(ID)
fld_splits = data.frame(Split = 'Full Season',ID=0)
teams = read.csv('data/teams.csv')%>% arrange(Team)

qualified_list =c('Qualified',0, 10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,
                  180,190,200,210,220,230,240,250,300,350,400,450,500,550,600,650,700,750,
                  800,850,900,950,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,
                  7000,7500,8000,8500,9000,9500,10000)

splits_for_small_stats = c('vs L', 'vs R', 'Home', 'Away', 'Grounders', 'Flies', 'Liners', 'Bunts', 'Pull', 'Center', 'Opposite', 'Low Leverage',
                           'Medium Leverage', 'High Leverage', 'Bases Empty', 'Men on Base', 'Men in Scoring', 'C', '1B', '2B', 'SS', '3B', 'RF',
                           'CF', 'LF', 'OF', 'DH', 'P', 'PH', 'PR', 'Batting 1st', 'Batting 2nd', 'Batting 3rd', 'Batting 4th', 'Batting 5th',
                           'Batting 6th', 'Batting 7th', 'Batting 8th', 'Batting 9th', 'vs L as L', 'vs R as L', 'vs L as R', 'vs R as R')

position_bat= c('All','P','C','1B','2B','SS','3B','RF','CF','LF','OF','NP')
position_pit= c('All'='pit','Starters'='sta','Relievers'='rel')
position_fld = c('All','P','C','1B','2B','SS','3B','RF','CF','LF','OF')

ages = 14:58
seasons = 1871:year(today())
stat_lvl = c('Player Stats','Team Stats','League Stats')
stat_type = c('Batting','Pitching','Fielding')
leagues = c('All Leagues','AL','NL')


####### SCRAPE FUNCTIONS  ##########
pos = 'all'
stat = 'bat'
lg = 'all'
qual = 'y'
split = 'Full Season'
fields = c('AB','H')
season = 2016
season1 = 2016
splt_season = F
rookies = F
team = 'All Teams'
team_stats = F
split_team = T
lg_stats = F
active = F
age = c(21,58)
split_df = bat_splits



get_url = function(pos, stat, lg, qual, fields, season1, split, 
                   season2, splt_season, rookies, team, split_team,
                   lg_stats, team_stats, active, age, stat_df, split_df, team_df){
    
    print(pos);print(stat);
    
    if(stat!='Pitching'){
        pos = tolower(pos)
        stat = ifelse(stat=='Batting','bat','fld')
    }else{
        stat = pos
        pos = 'all'
    }
    
    lg = ifelse(lg=='All Leagues','all', tolower(lg))
    
    team = filter(team_df, Team==team)$ID[1]

    fields = paste(filter(stat_df, Stat %in% fields)$ID, collapse=',')
    
    active = ifelse(active,1,0)
    
    split = filter(split_df, Split ==split)$ID
    
    
    if(lg_stats) 
        split_teams_lg = ',ss'
    else if(team_stats)
        split_teams_lg = ',ts'
    else if(split_team)
        split_teams_lg = ',to'
    else
        split_teams_lg = ''
    
    if(splt_season & rookies)
        split_season_rookie=3
    else if(rookies)
        split_season_rookie=2
    else if(splt_season)
        split_season_rookie=1
    else
        split_season_rookie=0
    
    if(qual=='Qualified') qual = 'y'
    
    if(all(age == c(14, 58)))
        age = 0
    else
        age = paste(age, collapse=',')
    
    url = paste0("http://www.fangraphs.com/leaders.aspx?pos=", pos,
                 "&stats=",stat,
                 "&lg=",lg,
                 "&qual=", qual,
                 "&type=c,", fields,
                 "&season=",season2,
                 "&month=",split,
                 "&season1=", season1, 
                 "&ind=", split_season_rookie, 
                 "&team=", team, split_teams_lg,
                 "&rost=", active,
                 "&age=",age, 
                 "&filter=&players=0&page=1_10000")
    print(url)
    return(url)
}

scrape_data = function(url){
    #scrape table
    df = read_html(url) %>% 
         html_node('.rgMasterTable') %>% 
         html_table()
    #column names
    cols = as.character(df[2,-1])
    #get rid of non used rows
    df = df[-1:-3,-1]
    #name columns
    names(df) = cols
    row.names(df)=1:nrow(df)
    complete_row = df[complete.cases(df),][1,]
    #percent columns
    perc_cols = which(grepl('%',complete_row))
    #make percent into decimal
    df[, perc_cols] = lapply(df[,perc_cols], function(x) as.numeric(gsub(' %','',x))/100)
    #numeric columns as numeric
    df[, grepl('[[:digit:]]',complete_row)] = lapply(df[,grepl('[[:digit:]]',complete_row)], as.numeric)
    #take out punct from names
    names(df) = gsub('\\%','perc',names(df))
    names(df) = gsub('\\+','plus',names(df))
    names(df) = gsub('\\-','minus',names(df))
    #Set seasons as factor
    if('Season' %in% names(df)) df$Season = as.Date(paste0(df$Season,'-01-01'))
    
    return(df)
    
}