#################################################################################
############################# Read in all Data ##################################
#################################################################################
#Read csv files
source(file = 'actuals.R')

reg_data <- read.csv(file = "data/Reg_Data.csv" )
playoff_data <- read.csv(file = "data/Playoff_Data.csv")
proj <- read.csv(file = "data/2017proj.csv")
draft <- read.csv(file = "data/Draft_Data.csv")
actuals <- read_hist()

#################################################################################
################################### Clean Data ##################################
#################################################################################
#Clean regular season data
Reg_Data_01 <- reg_data %>%
              select(-Score,-Opponent,-Opponent.Name,-X) %>%
              plyr::rename(c('Real.Opponent.Name' = 'Opponent_Name'))
Reg_Data_01$Team <- as.character(Reg_Data_01$Team) 
Reg_Data_01$Opponent_Name <- as.factor(Reg_Data_01$Opponent_Name)
Reg_Data_01$Win_Pct <- ifelse(Reg_Data_01$Outcome == 'Win',1,0)

#Clean Playoff Data
playoff_data$Playoff_Wins <- ifelse(playoff_data$Outcome != 'Win',0,ifelse(playoff_data$Opponent == "Bye",0,1))
playoff_data$Playoff_Losses <- ifelse(playoff_data$Outcome == "Loss",1,0)
playoff_data$Byes <- ifelse(playoff_data$Opponent == "Bye",1,0)
playoff_data$Championships <- ifelse(playoff_data$Round != "Final",0,ifelse(playoff_data$Outcome == "Win",1,0))
playoff_data$Playoff_Apperances <- ifelse(playoff_data$Round == "Quarter Final",1,0)

#Clean Draft Data
draft$WR <- ifelse(draft$POS == "WR",1,0)
draft$RB <- ifelse(draft$POS == "RB",1,0)
draft$QB <- ifelse(draft$POS == "QB",1,0)
draft$TE <- ifelse(draft$POS == "TE",1,0)
draft$DEF <- ifelse(draft$POS == "DEF",1,0)
draft$K <- ifelse(draft$POS == "K",1,0)
draft$Name <- gsub("\\s*\\([^\\)]+\\)","",as.character(draft$Player))
draft$Name <- gsub('Jr',"",gsub('Jr.',"",draft$Name))
draft$Name <- gsub('Sr.',"",draft$Name)

draft <- select(draft,Year,Round,Pick,Name,Team,WR,RB,QB,TE,DEF,K)

final_draft <- left_join(draft,actuals, by = c('Year' = 'Year','Name' = 'Name'))
final_draft$Fantasy_Pts[is.na(final_draft$Fantasy_Pts)] <- 0

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

save(full_data, playoffs_final, rankings_final, Reg_Data_01, reg_statistics,reg_final,draft, 
        team_list, year_list, final_draft, file = 'data/all.RData')