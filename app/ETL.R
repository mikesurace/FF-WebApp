# ETL
#Read in data
reg_data <- read.csv(file = "data/Reg_Data.csv" )
playoff_data <- read.csv(file = "data/Playoff_Data.csv")
proj <- read.csv(file = "data/2017proj.csv")
draft <- read.csv(file = "data/Draft_Data.csv")

#Clean regular season data
Reg_Data_01 <- select(reg_data,Year, Season, Week,Team, Real.Opponent.Name, Outcome, Points.For, Points.Against, Margin.of.Victory)
Reg_Data_01 <- plyr::rename(Reg_Data_01,c('Real.Opponent.Name' = 'Opponent_Name'))
Reg_Data_01$Team <- as.character(Reg_Data_01$Team) 
Reg_Data_01$Opponent_Name <- as.factor(Reg_Data_01$Opponent_Name)

#Create indicator if matchup was won
Reg_Data_01$Win_Pct <- ifelse(Reg_Data_01$Outcome == 'Win',1,0)

#Create table that contains game logs
gamelog <- Reg_Data_01 %>%
  group_by(Season, Week) %>%
  slice(1:6) %>%
  ungroup

gamelog <- select(gamelog, -Season, -Win_Pct)

############################################################
##############  Overall Statistics #########################
############################################################ 
#Overall Points For
Historical_Pts_For <- aggregate(Points.For ~ Team, data = Reg_Data_01, mean, na.rm = TRUE)
Historical_Pts_For <- plyr::rename(Historical_Pts_For,c('Points.For' = 'Overall_Pts_For'))

#Overall Points Against
Historical_Pts_Against <- aggregate(Points.Against ~ Team, data = Reg_Data_01, mean, na.rm = TRUE)
Historical_Pts_Against <- plyr::rename(Historical_Pts_Against,c('Points.Against' = 'Overall_Pts_Against'))

#Overall Winning Percentage
Historical_Pts_WinPct <- aggregate(Win_Pct ~ Team, data = Reg_Data_01, mean,na.rm = TRUE)

############################################################
############## Season Statistics ###########################
############################################################
#Best single season - Points for
season_ptsfor <-aggregate(Reg_Data_01$Points.For, by = list(Team = Reg_Data_01$Team, Season = Reg_Data_01$Season),FUN = "mean")
season_ptsfor_best <- season_ptsfor %>% 
  group_by(Team) %>%
  top_n(n = 1, wt = x)
season_ptsfor_best<- plyr::rename(select(season_ptsfor_best,Team,x),c('x' = 'Best_Season'))

#Worse single season - Points for
season_ptsfor_worse <- season_ptsfor %>%
  group_by(Team) %>%
  arrange(x) %>%
  slice(1) %>%
  ungroup
season_ptsfor_worse<- plyr::rename(select(season_ptsfor_worse,Team,x),c('x'='Worse_Season'))


#Best Winning percentage in single season
season_winpct <-aggregate(Reg_Data_01$Win_Pct, by = list(Team = Reg_Data_01$Team, Season = Reg_Data_01$Season),FUN = "mean")
season_winpct_best <- season_winpct %>% 
  group_by(Team) %>%
  top_n(n = 1, wt = x)  

season_winpct_best<- plyr::rename(select(season_winpct_best,Team,x),c('x'='Best_WinPct'))
season_winpct_best <- unique(season_winpct_best)

#Worse winning percentage in single season
season_winpct_worse <- season_winpct %>%
  group_by(Team) %>%
  arrange(x) %>%
  slice(1) %>%
  ungroup
season_winpct_worse<- plyr::rename(select(season_winpct_worse,Team,x),c('x'='Worse_WinPct'))
season_winpct_worse <- distinct(season_winpct_worse)

############################################################
############## Single Game Statistics ######################
############################################################
#Best game
game_ptsfor_best <- Reg_Data_01 %>% 
  group_by(Team) %>%
  top_n(n = 1, wt = Points.For)
game_ptsfor_best<- plyr::rename(select(game_ptsfor_best,Team,Points.For),c('Points.For' = 'Best_Week'))

#Worse game
game_ptsfor_worse <- Reg_Data_01 %>%
  group_by(Team) %>%
  arrange(Points.For) %>%
  slice(1) %>%
  ungroup
game_ptsfor_worse<- plyr::rename(select(game_ptsfor_worse,Team,Points.For),c('Points.For' = 'Worse_Week'))

############################################################
############## Merge Regular Season Statistics #############
############################################################

reg_statistics <- join_all(list(Historical_Pts_For,Historical_Pts_Against,Historical_Pts_WinPct,season_ptsfor_best,
                                season_ptsfor_worse,season_winpct_best,season_winpct_worse, game_ptsfor_best,game_ptsfor_worse), 
                           by = 'Team', type = 'left')

#Add in win and losses, based on overall win percentage
reg_statistics <- mutate(reg_statistics,Wins = 65*Win_Pct, Losses = 65*(1-Win_Pct))
reg_statistics <- reg_statistics[,c(1,11,12,4,2,3,5:10)]

############################################################
################## Playoff  Statistics #####################
############################################################

#Create indicators for playoffs wins, byes, championships, and appearances
playoff_data$Playoff_Wins <- ifelse(playoff_data$Outcome != 'Win',0,ifelse(playoff_data$Opponent == "Bye",0,1))
playoff_data$Playoff_Losses <- ifelse(playoff_data$Outcome == "Loss",1,0)
playoff_data$Byes <- ifelse(playoff_data$Opponent == "Bye",1,0)
playoff_data$Championships <- ifelse(playoff_data$Round != "Final",0,ifelse(playoff_data$Outcome == "Win",1,0))
playoff_data$Playoff_Apperances <- ifelse(playoff_data$Round == "Quarter Final",1,0)

#Sum playoff wins, byes, championships, and appearances
playoff_data_01 <- aggregate(cbind(Playoff_Wins,Playoff_Losses,Byes,Championships,Playoff_Apperances,Points.For, Points.Against)~Team, data = playoff_data,sum,na.rm = TRUE)
playoff_data_02 <- mutate(playoff_data_01,Games_Played = Playoff_Wins + Playoff_Losses, Playoffs_Pts_For = Points.For/Games_Played,
                          Playoffs_Pts_Against= Points.Against/Games_Played)

Playoff_Data_03 <- playoff_data_02[c(1,6,2,3,4,5,10,11)]

############################################################
############## Merge Regular and Playoff Statistics ########
############################################################
full_data = join_all(list(reg_statistics,Playoff_Data_03), by = 'Team', type = 'full')
full_data[is.na(full_data)] <- 0

############################################################
################## Caluclate Rankings ######################
############################################################

rankings <- mutate(full_data, pts_for.rank = ave(Overall_Pts_For,FUN = function(x) rank(x, ties.method = "first")),
                   winpct.rank = ave(Win_Pct,FUN = function(x) rank(x, ties.method = "first")),
                   season.rank = ave(Best_Season,FUN = function(x) rank(x, ties.method = "first")),
                   Inebo_Pts = pts_for.rank + winpct.rank + season.rank + Playoff_Apperances + Playoff_Wins + Byes + (12*Championships)
)
reg_final <- arrange(rankings[c(1,23,2,3,17,5:7,9:12)], desc(Inebo_Pts))
playoffs_final <- arrange(rankings[c(1,13:19)],desc(Playoff_Apperances))
rankings_final <- rankings[c(1,23,2,3,5,7,13,14,16,17)] %>% 
                    unite(Record, Wins:Losses, sep = "-") %>%
                    arrange(desc(Inebo_Pts)) %>%
                    mutate(PlayoffWinsByes = Playoff_Wins + Byes) %>%
                    select(-Playoff_Wins,-Byes)

###########################################################
###############Calculate Data for Player Bios###############
###########################################################
AllTeams <- reg_final %>%
            mutate(Wins_all = mean(Wins),
                             Points_all = mean(Overall_Pts_For),
                              Diff_Wins = Wins - Wins_all,
                              Diff_PtsFor = Overall_Pts_For - Points_all,
                              Diff_PtsAgainst = Overall_Pts_Against - Points_all) %>%
            select(Team,Diff_Wins,Diff_PtsFor,Diff_PtsAgainst) %>%
                gather(stats,val,Diff_Wins:Diff_PtsAgainst) %>% 
                spread(Team,val)

###########################################################
###############Calculate Data for Draft Analysis###########
###########################################################
#Scrape NFL Player Data
players2012 <- season_player_game(2012)
players2013 <- season_player_game(2013)
players2014 <- season_player_game(2014)
players2015 <- season_player_game(2015)
players2016 <- season_player_game(2016)

all_seasons <- bind_rows(players2015,players2016)

#Calculate fantasy points
gamestats <- all_seasons %>%
    mutate(pass_pts = passyds/25 + pass.tds*4 - pass.ints*2 + pass.twoptm*2,
           rush_pts = rushyds/10 + rushtds*6 + rush.twoptm*2 - fumbslost*2,
           rec_pts = recept*.5 + recyds/10 + rec.tds*6 + rec.twoptm*2,
           spec_pts = kickret.tds*6 + puntret.tds*6,
           total_pts = pass_pts + rush_pts + rec_pts + spec_pts) %>%
    select(Season,Team,name, pass_pts,rush_pts,rec_pts,spec_pts, total_pts)
  
agg_stats <- gamestats %>% 
    group_by(Season, name) %>% 
    summarise(season_pts = sum(total_pts)) %>%
    filter(season_pts > 0)

#Clean draft data and merge with fantasy points
draft_cln <- draft %>% 
              mutate(name = paste(substr(Player,1,1),stringr::word(Player,2),sep = '.')) %>%
              select(-X,-X.1,-X.2,-Player)

full_draft <- left_join(draft_cln,agg_stats, by = c("Year" = "Season","name" = "name"))
############################################################
#####################Save Data#############################
##########################################################
#define lists for user input selection in ui
team_list <- reg_data$Team %>% unique
year_list <- c(2012,2013,2014,2015,2016)

save(full_data, game_ptsfor_best, game_ptsfor_worse, gamelog, Historical_Pts_Against, 
     Historical_Pts_For, Historical_Pts_WinPct, playoff_data, playoff_data_01, playoff_data_02,
     Playoff_Data_03, playoffs_final, projections, rankings, rankings_final, reg_data, Reg_Data_01, reg_statistics,
     season_ptsfor, season_ptsfor_best, season_ptsfor_worse, season_winpct, season_winpct_best,
     season_winpct_worse, AllTeams, reg_final, team_list, year_list, file = 'data/all.RData')