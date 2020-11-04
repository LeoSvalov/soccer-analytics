# Title     : Intro guide to R language
# Objective : soccer analytics learning
# Created by: levsvalov
# Created on: 30/06/2020

library(rjson)
#library(jsonlite)
library(data.table)
library(magrittr)
library(dplyr)
## obtain competition ##
competitions <- fromJSON(file = "/Users/levsvalov/code_workspace/analytics/statsbomb-data/data/competitions.json")

# list to df
competitions.df <- data.frame(do.call(rbind,competitions),stringsAsFactors = FALSE)

# get matches
match.files <- list.files(path = "/Users/levsvalov/code_workspace/analytics/statsbomb-data/data/matches",
                          full.names = TRUE, recursive = TRUE)
matches.list <-  list()
for (i in 1:length(match.files)){
    match.temp <- fromJSON(file = match.files[i])
    matches <- lapply(match.temp, function (x) data.frame(t(unlist(x)),stringsAsFactors = FALSE))
    matches.df <- rbindlist(matches, fill = TRUE)
    matches.list[[i]] <- matches.df
}
all.matches.df <- data.frame(rbindlist(matches.list, fill = TRUE))

# clean table
columns.to.keep <- names(which(unlist(lapply(all.matches.df,function (x) length(which(is.na(x)))))==0))

all.matches.clean <- all.matches.df[,columns.to.keep] #selects the columns by column name

#convert from str to num
all.matches.clean$match_week <- as.numeric(all.matches.clean$match_week)
all.matches.clean$home_score <- as.numeric(all.matches.clean$home_score)
all.matches.clean$away_score <- as.numeric(all.matches.clean$away_score)

# get events

event.files <- list.files(path = "/Users/levsvalov/code_workspace/analytics/statsbomb-data/data/events",
                          full.names = TRUE, recursive = TRUE)

event.list <-  list()
for (i in 1:length(event.files)){
    event.temp <- fromJSON(file = event.files[i])
    teamids <- c() #Get the unique teamids participating in a match

    #obtain the index where we find the event that talks about Starting XI
    starting.x11.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Starting XI")
    starting.x11.list <- list()
    for(s in 1:2){
        starting.x11.team1 <- data.frame(matrix(t(unlist(event.temp[[s]]$tactics$lineup)),ncol=5,byrow = TRUE),stringsAsFactors = FALSE)
        colnames(starting.x11.team1) <- names(unlist(event.temp[[s]]$tactics$lineup))[1:5]
        starting.x11.team1$formation <- event.temp[[s]]$tactics$formation
        starting.x11.team1$team_id <- event.temp[[s]]$team$id

        teamids <- c(teamids,event.temp[[s]]$team$id)

        starting.x11.team1$team_name <- event.temp[[s]]$team$name
        starting.x11.list[[s]] <- starting.x11.team1
    }

    pass.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Pass")
    #obtain the passes just for team1 (the first element in teamids)

    pass.team1 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[1])]

    pass.team1.df <- data.frame(matrix(NA,nrow=1,ncol=11))
    colnames(pass.team1.df) <- c("Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part")

    for(p in 1:length(pass.team1)){
        pass.temp <- event.temp[[pass.team1[p]]]
        possession <- pass.temp$possession
        passer <- pass.temp$player$id
        pass.location <- pass.temp$location
        pass.type <- pass.temp$pass$height$name
        receiver <- pass.temp$pass$recipient$id
        receive.location <- pass.temp$pass$end_location
        pass.length <- pass.temp$pass$length
        pass.angle <- pass.temp$pass$angle
        body.part <- pass.temp$pass$body_part$name

        row.toadd <- c(possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
        pass.team1.df <- rbind(pass.team1.df,row.toadd)
    }
    pass.team1.df <- pass.team1.df[-1,]
    suppressWarnings(pass.team1.df[,c(1:4,6:10)] <- lapply(pass.team1.df[,c(1:4,6:10)],as.numeric))

    pass.team1.df <- pass.team1.df %>% group_by(Possession) %>% mutate(seq = row_number())
    pass.team1.df$team_id <- teamids[1]

    pass.team2 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[2])]
    pass.team2.df <- data.frame(matrix(NA,nrow=1,ncol=11))
    colnames(pass.team2.df) <- c("Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part")

    for(p in 1:length(pass.team2)){
        pass.temp <- event.temp[[pass.team2[p]]]
        possession <- pass.temp$possession
        passer <- pass.temp$player$id
        pass.location <- pass.temp$location
        pass.type <- pass.temp$pass$height$name
        receiver <- pass.temp$pass$recipient$id
        receive.location <- pass.temp$pass$end_location
        pass.length <- pass.temp$pass$length
        pass.angle <- pass.temp$pass$angle
        body.part <- pass.temp$pass$body_part$name

        row.toadd <- c(possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
        pass.team2.df <- rbind(pass.team2.df,row.toadd)
    }
    pass.team2.df <- pass.team2.df[-1,]
    suppressWarnings(pass.team2.df[,c(1:4,6:10)] <- lapply(pass.team2.df[,c(1:4,6:10)],as.numeric))
    pass.team2.df <- pass.team2.df %>% group_by(Possession) %>% mutate(seq = row_number())
    pass.team2.df$team_id <- teamids[2]


    pass.list <- list(pass.team1.df,pass.team2.df)

    match.id <- strsplit(basename(event.files[i]),"[.]")[[1]][1]

    event.list[[match.id]] <- list(starting.x11.list, pass.list)
}



####Analysis 1: Let's Look at the Squad Rotation per Match####
#We will use the FA Women's Super League which is competition_id == 37 and season_id == 4

#This gets me the number of matches per competition and season to check which season we have the most data for
#matches.count <- all.matches.clean %>% group_by(competition.competition_id,season.season_id) %>% summarise(count = n())

matches.wsl.1819 <- all.matches.clean[which(all.matches.clean$competition.competition_id==37 & all.matches.clean$season.season_id==4),]
matches.wsl.1819 <- matches.wsl.1819[order(matches.wsl.1819$match_week),]

wsl.teams <- unique(matches.wsl.1819$home_team.home_team_name) #get the unique list of teams so we can loop through each team

squad.rotation.list <- list() #this list is for keeping track of the number of squad rotations per matchweek
team.starting.x11 <- list() #this list is for keeping track of the starting 11 for each match week
for(w in 1:length(wsl.teams)){
    squad.rotation.list[[wsl.teams[w]]] <- list()
    team.starting.x11[[wsl.teams[w]]] <- list()
    team.matches <- matches.wsl.1819[which(matches.wsl.1819$home_team.home_team_name==wsl.teams[w] | 
                                               matches.wsl.1819$away_team.away_team_name==wsl.teams[w]),]
    team.matches$GD <- team.matches$home_score-team.matches$away_score
    
    team.events.index <- which(names(event.list) %in% team.matches$match_id)
    team.events <- event.list[team.events.index]
    team.id <- unique(matches.wsl.1819[which(matches.wsl.1819$home_team.home_team_name==wsl.teams[w]),]$home_team.home_team_id)
    team.matches$Team.GD <- ifelse(team.matches$home_team.home_team_id==team.id,team.matches$GD,team.matches$GD*-1)
    team.matches$Result <- ifelse(team.matches$Team.GD>0,"W",
                                  ifelse(team.matches$Team.GD==0,"D","L"))
    
    
    for(i in 1:length(team.events)){ #for each game of that particular team, get the starting 11 for them
        starting.x11 <- team.events[[i]][[1]]
        starting.x11.index <- which(lapply(starting.x11, function(x) unique(x$team_id))==team.id)
        
        team.11 <- starting.x11[[starting.x11.index]]
        team.starting.x11[[wsl.teams[w]]][[i]] <- team.11$player.name
    }
    
    num.matches <- length(team.events)
    #for all the matches after the first match, calculate the difference in players from matchweek X and matchweek X+1
    squad.rotation <- c(0,sapply(seq(1:(num.matches-1)),function(x) length(setdiff(team.starting.x11[[w]][[x]],team.starting.x11[[w]][[x+1]]))))
    team.matches$Rotated <- squad.rotation 
    squad.rotation.list[[w]] <- team.matches[,c("match_week","Result","Rotated")]
}

result.colors <- c("W"="forestgreen","L"="red","D" = "yellow") #define a set of colors to use in our plot

#ggplot is where you bind the data. the aes stands for aesthetic and defines what data is bound to what part of the graph
ggplot(data=squad.rotation.list[[1]], aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat="identity",width=0.5)+
    scale_fill_manual(values=result.colors)

all.squad.rotations <- plyr::ldply(squad.rotation.list,.id="Team") #binds all the rows of the list elements together and adds the list element name as an additional column

ggplot(data=all.squad.rotations, aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat="identity",width=0.5)+
    scale_fill_manual(values=result.colors) + facet_grid(rows=vars(Team)) #adds a plot for each team

