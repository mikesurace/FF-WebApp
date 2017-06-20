read_hist <- function(){
  
#Read in csv files
rush_stats <- read.csv(file = "data/Rushing_Stats.csv")
catch_stats <- read.csv(file = "data/Recieving_Stats.csv")
pass_stats <- read.csv(file = "data/Passing_Stats.csv")
pr_stats <- read.csv(file = "data/PR_Stats.csv")
kr_stats <- read.csv(file = "data/KR_Stats.csv")
kick_stats <- read.csv(file = "data/Kicking_Stats.csv")
to_stats <- read.csv(file = "data/turnover_stats.csv")
deftd_stats <- read.csv(file = "data/Def_TD_Stats.csv")
ptsallowed_stats <- read.csv(file = "data/PtsAllowed_Stats.csv")
sack_stats <- read.csv(file = "data/Sack_Stats.csv")

#RUSHING FANTASY POINTS
rush <- rush_stats %>%
            mutate(Rush_Yds = as.numeric(as.character(gsub(",","",Yds))),
                   Points = Rush_Yds*.1 + TD*6) %>%
            select(Year,POS,Player,Points)
            
#RECEIVING FANTASY POINTS
catch <- catch_stats %>%
            mutate(Rec_Yds = as.numeric(as.character(gsub(",","",Yds))),
                   Points = Rec*.5 + Rec_Yds*.1 + TD*6) %>%
            select(Year,POS,Player,Points) 

#PASSING FANTASY POINTS
pass <- pass_stats %>%
            mutate(Pass_Yds = as.numeric(as.character(gsub(",","",Yds))),
                   Points = Pass_Yds*.04 + TD*4 + Int*-1) %>%
            select(Year,POS,Player,Points)

#KICKOFF RETURN FANTASY POINTS
##Points to Player
kr <- kr_stats %>%
            mutate(Points = TD*6) %>%
            select(Year,POS,Player,Points)

##Points to Defense
def_kr <- kr_stats %>%
  mutate(Points = TD*6) %>%
  select(Year,POS,Team,Points) %>%
  plyr::rename(c('Team' = 'Player'))
def_kr$POS <- "DEF"
def_kr$Player <- toupper(def_kr$Player)
def_kr$Player[def_kr$Player == "STL"] <- "LAR"

#PUNT RETURN FANTASY POINTS
##Points to Player
pr <- pr_stats %>%
  mutate(Points = TD*6) %>%
  select(Year,POS,Player,Points)
##Points to Defense
def_pr <- pr_stats %>%
          mutate(Points = TD*6) %>%
          select(Year,POS,Team,Points) %>%
          plyr::rename(c('Team' = 'Player'))
def_pr$POS <- "DEF"
def_pr$Player <- toupper(def_pr$Player)
def_pr$Player[def_pr$Player == "STL"] <- "LAR"

#KICKING FANTASY POINTS
kick <- kick_stats %>%
  mutate(Points = PAT + FG_0_19*3 + FG_20_29*3 + FG_30_39*3,FG_40_49*4,FG_50*5) %>%
  select(Year,POS,Player,Points)


#DEFENSIVE INTERCEPTIONS & FUMBLES FANTASY POINTS
to <- to_stats %>%
  mutate(Points = Int*2 + Fum*2) %>%
  select(Year,POS,Team,Points) %>%
  plyr::rename(c('Team' = 'Player'))

#DEFENSIVE TOUCHDOWNS & SAFETY FANTASY POINTS
deftd <- deftd_stats %>%
  mutate(Points = IR*6 + FR*6 + BK*6 + BP*6 + FGR*6 + Saf*2) %>%
  select(Year,POS,Team,Points) %>%
  plyr::rename(c('Team' = 'Player'))

#DEFENSIVE SACKS FANTASY POINTS
sack <- sack_stats %>%
  mutate(Points = Sack*1) %>%
  select(Year,POS,Team,Points) %>%
  plyr::rename(c('Team' = 'Player'))

#DEFENSIVE POINTS ALLOWED FANTASY POINTS
ptsallowed <- ptsallowed_stats %>%
  mutate(Points = (Pts_0*10 + Pts_1_6*7 + Pts_7_13*4 + Pts_14_20*2 + Pts_21_27*1 + Pts_28_34*-1 + Pts_34*-4)*16) %>%
  select(Year,POS,Team,Points) %>%
  plyr::rename(c('Team' = 'Player'))


Hist_Stats <- bind_rows(rush,catch,pass,kr,pr,to,kick,deftd,def_kr,def_pr,sack,ptsallowed) %>%
                group_by(Year,Player) %>%
                summarize(Fantasy_Pts = sum(Points))

Hist_Stats$dummy <- gsub("\\s*\\([^\\)]+\\)","",as.character(Hist_Stats$Player))
Hist_Stats$dummy <- gsub('Jr',"",gsub('Jr.',"",Hist_Stats$dummy))
Hist_Stats$Name <- gsub('Sr.',"",Hist_Stats$dummy)

final <- select(Hist_Stats,Year,Name,Fantasy_Pts)

return(final)
}