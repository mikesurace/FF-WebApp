#Script to clean projections data
read_proj <- function(){
  
projections <- read.csv(file = "data/Projections.csv")

#Change name of Leveon Bell, RG3, Stevie Johnson
projections$player <- as.character(projections$player)
projections$player[projections$player == "LeVeon Bell"] <- "Le'Veon Bell"
projections$player[projections$player == "Robert Griffin"] <- "Robert Griffin III"
projections$player[projections$player == "Steve Johnson"] <- "Stevie Johnson"


#Assign Name of Player as the Team name for Defenses
projections$team <- as.character(projections$team)
projections$Name <- ifelse(projections$playerposition == "DST",projections$team,projections$player)

#Change name of DST to DEF
projections$playerposition <- as.character(projections$playerposition)
projections$playerposition[projections$playerposition == "DST"] <- "DEF"

#Change Defense Name of Rams and Jags
projections$Name[projections$Name == "LA"] <- "LAR"
projections$Name[projections$Name == "STL"] <- "LAR"
projections$Name[projections$Name == "JAC"] <- "JAX"

#Remove any leading and trailing blanks
projections$Name <- trim(projections$Name)


final <- projections %>%
          plyr::rename(c('points' = 'Proj_Pts','playerposition' = 'POS')) %>%
          select(Year,Name,POS,Proj_Pts)

return(final)

}



