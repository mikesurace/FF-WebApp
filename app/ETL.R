#################################################################################
############################# Read in all Data ##################################
#################################################################################
#Read csv files
source(file = 'actuals.R')
source(file = 'readProj.R')

reg_data <- read.csv(file = "data/Reg_Data.csv" )
playoff_data <- read.csv(file = "data/Playoff_Data.csv")
draft <- read.csv(file = "data/Draft_Data.csv")
actuals <- read_hist()
proj <- read_proj()

#################################################################################
################################### Clean Data ##################################
#################################################################################
###REGULAR SEASON DATA: 5 SEASONS
Reg_Data_01 <- reg_data %>%
              select(-Score,-Opponent,-Opponent.Name,-X) %>%
              plyr::rename(c('Real.Opponent.Name' = 'Opponent_Name'))
Reg_Data_01$Team <- as.character(Reg_Data_01$Team) 
Reg_Data_01$Opponent_Name <- as.factor(Reg_Data_01$Opponent_Name)
Reg_Data_01$Win_Pct <- ifelse(Reg_Data_01$Outcome == 'Win',1,0)

#PLAYOFF DATA: 5 SEASONS
playoff_data$Playoff_Wins <- ifelse(playoff_data$Outcome != 'Win',0,ifelse(playoff_data$Opponent == "Bye",0,1))
playoff_data$Playoff_Losses <- ifelse(playoff_data$Outcome == "Loss",1,0)
playoff_data$Byes <- ifelse(playoff_data$Opponent == "Bye",1,0)
playoff_data$Championships <- ifelse(playoff_data$Round != "Final",0,ifelse(playoff_data$Outcome == "Win",1,0))
playoff_data$Playoff_Apperances <- ifelse(playoff_data$Round == "Quarter Final",1,0)

####DRAFT RESULTS DATA: 5 SEASONS
draft$WR <- ifelse(draft$POS == "WR",1,0)
draft$RB <- ifelse(draft$POS == "RB",1,0)
draft$QB <- ifelse(draft$POS == "QB",1,0)
draft$TE <- ifelse(draft$POS == "TE",1,0)
draft$DEF <- ifelse(draft$POS == "DEF",1,0)
draft$K <- ifelse(draft$POS == "K",1,0)

draft$Name <- gsub("\\s*\\([^\\)]+\\)","",as.character(draft$Player))
draft$Name <- gsub('Jr',"",gsub('Jr.',"",draft$Name))
draft$Name <- gsub('Sr.',"",draft$Name)

draft <- select(draft,Year,Round,Pick,Name,POS,Team,WR,RB,QB,TE,DEF,K)

#Calculate average defense projections for 2014 to 2016: Impute average score for 2012 and 2013 with no data
Ave_Def <- proj %>%
  filter(Year > 2013, POS == "DEF") %>%
  group_by(Name) %>%
  summarize(Ave_Def = mean(Proj_Pts))

#Calculate average defense projections for 2014 to 2016: Impute average score for 2012 and 2013 with no data
Ave_K <- proj %>%
  filter(Year > 2013, POS == "K") %>%
  group_by(Name) %>%
  summarize(Ave_K = mean(Proj_Pts))

###MERGE DRAFT RESULTS, PROJECTED FF PTS, AND ACTUAL FF PTS 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
draft$Name <- trim(draft$Name)
actuals$Name <- trim(actuals$Name)

#Merge draft results with Actuals Points Scored
Draft_Actuals <- left_join(draft,actuals, by = c('Year' = 'Year','Name' = 'Name'))
Draft_Actuals[is.na(Draft_Actuals)] <- 0 #missing actuals for players drafted and did not play

#Merge in Projections
Draft_Act_Proj_00 <- left_join(Draft_Actuals,proj, by = c('Year' = 'Year','Name' = 'Name','POS' = 'POS'))
Draft_Act_Proj_01 <- left_join(Draft_Act_Proj_00,Ave_Def, by = c('Name' = 'Name'))
Draft_Act_Proj_02 <- left_join(Draft_Act_Proj_01,Ave_K, by = c('Name' = 'Name'))
#Assign Average Defense projections for Kickers and Def prior to 2014.
Draft_Act_Proj_02$Projections <- ifelse(Draft_Act_Proj_02$Year < 2014 & Draft_Act_Proj$POS == "DEF",Draft_Act_Proj_02$Ave_Def,
                                 ifelse(Draft_Act_Proj_02$Year < 2014 & Draft_Act_Proj$POS == "K",Draft_Act_Proj_02$Ave_K,
                                        Draft_Act_Proj_02$Proj_Pts))

Draft_Act_Proj <- Draft_Act_Proj_02 %>%
                      select(-Proj_Pts,-Ave_Def,-Ave_K)
Draft_Act_Proj[is.na(Draft_Act_Proj)] <- Draft_Act_Proj$Fantasy_Pts

#################################################################################
#####################  Regular Season Statistics ################################
################################################################################# 
#Historical overall statistics. Based on data through 2012. 
Overall_Stats <- Reg_Data_01 %>%
            group_by(Team) %>%
            summarize(Overall_Pts_For = mean(Points.For), 
                      Overall_Pts_Against = mean(Points.Against),
                      Win_Pct = mean(Win_Pct))

#Season statistics
Season_Stats <- Reg_Data_01 %>%
                  group_by(Team,Season) %>%
                  summarize(Pts_For = mean(Points.For),
                            Pts_Against = mean(Points.Against),
                            Win_Pct = mean(Win_Pct))

BestWorst_Seasons <- Season_Stats %>%
                group_by(Team) %>%
                summarize(Best_Season = max(Pts_For),
                          Best_WinPct = max(Win_Pct),
                          Worse_Season = min(Pts_For),
                          Worse_WinPct = min(Win_Pct))

#Individual Game Statistics
Game_Stats <- Reg_Data_01 %>%
                group_by(Team) %>%
                summarize(Best_Week = max(Points.For),
                          Worse_Week = min(Points.For))

#Merge Statistics, Calc Wins and Losses, Format
reg_statistics <- join_all(list(Overall_Stats,BestWorst_Seasons,Game_Stats), by = 'Team', type = 'left') %>%
                      mutate(Wins = 65*Win_Pct, Losses = 65*(1-Win_Pct))

reg_statistics <- reg_statistics[,c(1,11,12,4,2,3,5:10)]

############################################################
################## Playoff  Statistics #####################
############################################################

#Sum playoff wins, byes, championships, and appearances
Playoff_Stats <- playoff_data %>%
          group_by(Team) %>%
          summarize(Playoff_Apperances = sum(Playoff_Apperances),
                    Playoff_Wins = sum(Playoff_Wins),
                    Playoff_Losses = sum(Playoff_Losses),
                    Byes = sum(Byes),
                    Championships = sum(Championships),
                    Playoffs_Pts_For = mean(Points.For),
                    Playoff_Pts_Against = mean(Points.Against))

#Merge Regular Season and Playoff Statistics
full_data <- join_all(list(reg_statistics,Playoff_Stats), by = 'Team', type = 'full')
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

############################################################
#####################Save Data#############################
##########################################################
#define lists for user input selection in ui
team_list <- reg_data$Team %>% unique
year_list <- c(2012,2013,2014,2015,2016)

save(full_data, playoffs_final, rankings_final, Reg_Data_01, reg_statistics,reg_final, 
        team_list, year_list, Draft_Act_Proj, file = 'data/all.RData')