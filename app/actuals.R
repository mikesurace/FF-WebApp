read_hist <- function(){
  
#Read in csv files
rush_stats <- read.csv(file = "data/Rushing_Stats.csv")
catch_stats <- read.csv(file = "data/Recieving_Stats.csv")
pass_stats <- read.csv(file = "data/Passing_Stats.csv")
pr_stats <- read.csv(file = "data/PR_Stats.csv")
kr_stats <- read.csv(file = "data/KR_Stats.csv")
kick_stats <- read.csv(file = "data/Kicking_Stats.csv")

#Calculate Rush Fantasy Points
rush_stats <- rush_stats %>%
            mutate(Rush_Yds = as.numeric(as.character(gsub(",","",Yds))),
                   Points = Rush_Yds*.1 + TD*6) %>%
            select(Year,POS,Player,Points)
            
#Calculate Receiving Fantasy Points
catch_stats <- catch_stats %>%
            mutate(Rec_Yds = as.numeric(as.character(gsub(",","",Yds))),
                   Points = Rec*.5 + Rec_Yds*.1 + TD*6) %>%
            select(Year,POS,Player,Points) 

#Calculate Pass Fantasy Points
pass_stats <- pass_stats %>%
            mutate(Pass_Yds = as.numeric(as.character(gsub(",","",Yds))),
                   Points = Pass_Yds*.04 + TD*4 + Int*-1) %>%
            select(Year,POS,Player,Points)

#Calculate KR Fantasy Points
kr_stats <- kr_stats %>%
            mutate(Points = TD*6) %>%
            select(Year,POS,Player,Points)

#Calculate PR Fantasy Points
pr_stats <- pr_stats %>%
  mutate(Points = TD*6) %>%
  select(Year,POS,Player,Points)

#Calculate Kicking Fantasy Points
kick_stats <- kick_stats %>%
  mutate(Points = PAT + FG_0_19*3 + FG_20_29*3 + FG_30_39*3,FG_40_49*4,FG_50*5) %>%
  select(Year,POS,Player,Points)


Hist_Stats <- bind_rows(rush_stats,catch_stats,pass_stats,kr_stats,pr_stats,kick_stats) %>%
                group_by(Year,Player) %>%
                summarize(Fantasy_Pts = sum(Points))

Hist_Stats$dummy <- gsub("\\s*\\([^\\)]+\\)","",as.character(Hist_Stats$Player))
Hist_Stats$dummy <- gsub('Jr',"",gsub('Jr.',"",Hist_Stats$dummy))
Hist_Stats$Name <- gsub('Sr.',"",Hist_Stats$dummy)

final <- select(Hist_Stats,Year,Name,Fantasy_Pts)

return(final)
}