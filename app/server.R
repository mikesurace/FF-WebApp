#server script

shinyServer(function(input, output, session) {

######################################################################################################################
################################### Create Data Views for Output #####################################################
######################################################################################################################  
  
  #######################################################################
  ########################### Trend Analysis ############################
  #######################################################################
  
  datatrend <- reactive({
        Reg_Data_01 %>%
            filter(Team %in% input$team, Year %between% input$year) %>%
            group_by(Opponent_Name) %>%
            dplyr::summarise(Games_Playes = n(),
                            Wins = sum(Win_Pct),
                            Loses = n()-sum(Win_Pct),
                            Win_Pct = mean(Win_Pct))
        })
  
    dataplusminus <- reactive({
              all <- Reg_Data_01 %>% 
                filter(Team %in% input$team, Year %between% input$year) %>%
                dplyr::summarise(Tot_Pts_Scored = mean(Points.For),
                                 Tot_Pts_Against = mean(Points.Against))
              
              opp <- Reg_Data_01 %>%
                  filter(Team %in% input$team, Year %between% input$year) %>%
                  group_by(Opponent_Name) %>%
                  dplyr::summarise(Points_Scored = mean(Points.For),
                                   Points_Against = mean(Points.Against))
                
              opp$Tot_Pts_Scored <- all$Tot_Pts_Scored
              opp$Tot_Pts_Against <- all$Tot_Pts_Against
              opp$Diff_Pts_For <- opp$Points_Scored - opp$Tot_Pts_Scored
              opp$Diff_Pts_Against <- opp$Points_Against - opp$Tot_Pts_Against
              
          return(opp)
    })
    
    output$trend <- DT::renderDataTable({formatPercentage(datatable(datatrend(), options = list(pageLength = 25, dom = 't'), 
                                                                    rownames = FALSE, 
                                                                    colnames = c('Opponent', 
                                                                                 'Games Played',
                                                                                 'Wins', 
                                                                                 'Loses',
                                                                                 'Winning %')),'Win_Pct')})
    


    ###############################################################################
    ############################### GameLogs ######################################
    ###############################################################################
    gamelogs <- reactive({
      data <- Reg_Data_01 %>%
              filter(Team == input$GM, Year %between% input$period) %>%
              select(-Season,-Win_Pct,-Team)
      return(data)})

    output$gamelogs <- DT::renderDataTable({DT::datatable(gamelogs(), options = list(pageLength = 100), rownames = FALSE)})
    
    
    ###############################################################################
    ############################### Rankings ######################################
    ###############################################################################
    output$TeamRankings <- DT::renderDataTable(formatRound(DT::datatable(rankings_final, 
                                                              options = list(pageLength = 25, dom = 't'), 
                                                              rownames = FALSE,
                                                              colnames = c('Team','Inebo Points','Record',
                                                                     'Average Points Scored','Best Season: Points',
                                                                      'Playoff Births','Championships','Playoff Wins/Byes')),
                                                        c('Overall_Pts_For','Best_Season'),2))
                                                                          
    ###############################################################################
    ############################### Playoffs ######################################
    ###############################################################################
    output$PlayoffData <- DT::renderDataTable(DT::datatable(playoffs_final, options = list(pageLength = 25, dom = 't'), 
                                                  rownames = FALSE,
                                                  colnames = c('Team','Appearances','Wins','Loses','Byes',
                                                               'Championships','Ave Points Scored','Ave Points Against')) %>%
                                                formatRound(c('Playoffs_Pts_For','Playoff_Pts_Against'),2))
  
  
    ###############################################################################
    ############################### Draft Analysis ################################
    ###############################################################################
    
    ############# START TAB - DRAFT TENDENCIES ####################
    draftTendTeam <- reactive({
          data <- Draft_Act_Proj %>%
                    filter(Round < 11, Team == input$dtTeam) %>%
                    group_by(Team,Round) %>%
                    summarize(WideReceiver = mean(WR),
                              QuarterBack = mean(QB),
                              TightEnd = mean(TE),
                              RunningBack = mean(RB)) %>% 
                    ungroup() %>%
                    select(Round,QuarterBack,RunningBack,WideReceiver,TightEnd) 
          return(data)
          })
    
    
    output$Draft <- DT::renderDataTable({datatable(draftTendTeam(), options = list(pageLength = 25, dom = 't'), 
                                                                    rownames = FALSE, 
                                                                    colnames = c('Round','QuarterBack','Running Back',
                                                                                 'Wide Receiver','Tight End')) %>%
                                            formatPercentage(c('QuarterBack','RunningBack','WideReceiver','TightEnd'))})

    #Data set used to produce chart for Draft trend analysis
    dtCompare <- reactive({
        data_team <- Draft_Act_Proj %>%
                  filter(Round < 7) %>%
                  group_by(Team, Round) %>%
                  summarize(WR = mean(WR),
                            QB = mean(QB),
                            TE = mean(TE),
                            RB = mean(RB)) %>% 
                  ungroup() %>%
                  select(Team,Round,QB,RB,WR,TE) %>%
                  gather("Position","Draft_Perc",3:6)

        data_all <- Draft_Act_Proj %>%
          filter(Round < 7) %>%
          group_by(Round) %>%
          summarize(WR = mean(WR),
                    QB = mean(QB),
                    TE = mean(TE),
                    RB = mean(RB)) %>% 
          ungroup() %>%
          select(Round,QB,RB,WR,TE) %>%
          gather("Position","Draft_Perc_All",2:5)
        
        data_merge <- left_join(data_team,data_all,by = c('Round' = 'Round','Position' = 'Position')) %>%
                        filter(Team == input$dtTeam, Position == input$dtPOS)
        
        return(data_merge)})
    
    ############# END TAB - DRAFT TENDENCIES ####################
    ############## START TAB - DRAFT RESULTS ##############
    # Data set used to produce the draft grid that hold historical results.
    grid <- reactive({
      data <- Draft_Act_Proj %>% 
        filter(Year == input$yr, Round < 11) %>%
        select(Round,Team,Name) %>%
        spread(Round,Name)
      return(data)})
    
    output$draftgrid <- DT::renderDataTable({datatable(grid(),options = list(pageLength = 25,dom = 't'),selection = 'single',rownames = FALSE,
                                                       colnames = c('Team','Round 1','Round 2','Round 3','Round 4','Round 5','Round 6',
                                                                    'Round 7','Round 8','Round 9','Round 10')) %>%
        formatStyle('Team',fontWeight = 'Bold')})
    
    ############# END TAB - DRAFT RESULTS ####################
    ############# START TAB - PLAYER RETURNS ####################
      #Dataset used to produce the data table in the Player returns tab.    
      drTeam <- reactive({
          data <- Draft_Act_Proj %>%
            filter(Year == input$SeasonYear, Team == input$SeasonTeam) %>%
            mutate(Performance = (Fantasy_Pts - Projections),Percent = Performance/Projections) %>%
            select(Round, Name, POS,Projections,Fantasy_Pts,Performance,Percent)
          
          return(data)
    
        })
    
        output$draftresults <- DT::renderDataTable({datatable(drTeam(), options = list(pageLength = 25, dom = 't'),rownames = FALSE, 
                                                              colnames = c('Round','Player','Position','Projected Points','Actual Points','Return','% Return')) %>%
                                                        formatPercentage(c('Percent')) %>%
                                                        formatRound(c('Projections','Performance','Fantasy_Pts'))})
    

    #Data set for leaderboard of the overal draft section.
DraftLead <- function(){
                     
            BestDraft_00 <- Draft_Act_Proj %>%
              filter(Round < 11, POS %in% c('RB','WR','QB','TE')) %>%
              mutate(Performance = Fantasy_Pts - Projections,Wgt = Performance*Projections) %>%
              group_by(Team, Year) %>%
              summarize(Total = sum(Projections),Wgt_Return = sum(Wgt))
            
            BestDraft_01 <- BestDraft_00 %>%
              mutate(Total_Return = Wgt_Return/Total) %>%
              dplyr::arrange(desc(Total_Return)) %>%
              select(-Total, -Wgt_Return) %>%
              head(10)
            
            data <- Draft_Act_Proj %>%
                  filter(POS %in% c('RB','WR','QB','TE')) %>%
                  mutate(Performance = (Fantasy_Pts - Projections),Percent = Performance/Projections)
            data$Percent[is.infinite(data$Percent)] <- -1
            
            data <- data %>%
              select(Team, Year,Name, Percent) %>%
              group_by(Team,Year) %>%
              top_n(5) %>%
              mutate(Rank = rank(-Percent, ties.method = "first")) %>%
              select(-Percent) %>%
              spread(Rank,Name)

            final <- left_join(BestDraft_01,data, by = c('Team' = 'Team','Year' = 'Year'))

    return(final)
}
    
BestDraft <- DraftLead()

    output$TopDraft <- DT::renderDataTable({datatable(BestDraft, 
                                                          options = list(pageLength = 25, dom = 't',
                                                          initComplete = JS("function(settings, json) {",
                                                          "$(this.api().table().header()).css({'background-color': '#FFF', 'color': '#000'});","}")),
                                                          rownames = FALSE,
                                                          colnames = c('Team','Season','Overall Return','Best Pick','Second Pick','Third Pick','Fourth Pick','Fifth Pick')) %>%
                                                          formatRound(c('Total_Return')) %>%
                                                          formatStyle(names(BestDraft),background = 'white')})
    
          
    #Data table shows GM Draft returns by Round
    PtsPerRound <- reactive({
      
          data <- Draft_Act_Proj %>%
                    mutate(Return = Fantasy_Pts - Projections) %>%
                    select(Team, Year, Round, POS, Return) %>%
                    filter(Year %between% input$RoundYear) %>%
                    group_by(Team,Round) %>%
                    summarize(Pts = mean(Return))
          
          data1 <- Draft_Act_Proj %>%
                    mutate(Return = Fantasy_Pts - Projections) %>%
                    select(Year, Round, POS, Return) %>%
                    filter(Year %between% input$RoundYear) %>%
                    group_by(Round) %>%
                    summarize(Rd_Pts = mean(Return))
          
          data_all <- left_join(data,data1,by = c('Round' = 'Round')) %>%
                      mutate(Difference = Pts - Rd_Pts) %>%
                      filter(Round < 11, Team == input$RoundTeam)
          
          return(data_all)})
    
    output$RdPts <- DT::renderDataTable({datatable(PtsPerRound(), options = list(pageLength = 25, dom = 't'), 
                                                   rownames = FALSE, 
                                                   colnames = c('Team','Round','Return (Pts)',
                                                                'League Average Return (Pts)','Difference')) %>%
                                                   formatRound(c('Pts','Rd_Pts','Difference'))})
    ##Dataset for leaderboard by position
      BestRd1 <- Draft_Act_Proj %>%
        select(Team, Year, Round, POS, Fantasy_Pts) %>%
        group_by(Team,Round) %>%
        summarize(Pts = mean(Fantasy_Pts))
      
      BestRd2 <- Draft_Act_Proj %>%
        select(Year, Round, POS, Fantasy_Pts) %>%
        group_by(Round) %>%
        summarize(Rd_Pts = mean(Fantasy_Pts))
      
      BestRound <- left_join(BestRd1,BestRd2,by = c('Round' = 'Round')) %>%
        mutate(Difference = Pts - Rd_Pts) %>%
        filter(Round < 11) %>%
        dplyr::arrange(desc(Round),desc(Difference)) %>%
        select(-Rd_Pts,-Pts)
      
      BestRound <- ddply(BestRound, "Round", function(x) head(x[order(x$Difference, decreasing = TRUE) , ], 1))
    
    output$BestRd <- DT::renderDataTable({datatable(BestRound, 
                                                    options = list(pageLength = 25, dom = 't',
                                                    initComplete = JS("function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': '#FFF', 'color': '#000'});","}")), 
                                                    rownames = FALSE, 
                                                    colnames = c('Team','Round','+/- League')) %>%
                                            formatRound(c('Difference'))})
    
    
    
    ## Data table shows GM Draft returns by Position
    PtsPerPos <- reactive({
      
      data <- Draft_Act_Proj %>%
        select(Team, Year, Round, POS, Fantasy_Pts) %>%
        filter(Year %between% input$PositionYear) %>%
        group_by(Team,POS) %>%
        summarize(Pts = mean(Fantasy_Pts))
      
      data1 <- Draft_Act_Proj %>%
        select(Year, Round, POS, Fantasy_Pts) %>%
        filter(Year %between% input$PositionYear) %>%
        group_by(POS) %>%
        summarize(Rd_Pts = mean(Fantasy_Pts))
      
      data_all <- left_join(data,data1,by = c('POS' = 'POS')) %>%
        mutate(Difference = Pts - Rd_Pts) %>%
        filter(Team == input$PositionTeam)
      
      return(data_all)
      
    })
    
    output$PosPts <- DT::renderDataTable({datatable(PtsPerPos(), options = list(pageLength = 25, dom = 't'), 
                                                   rownames = FALSE, 
                                                   colnames = c('Team','Position','Return (Pts)',
                                                                'League Average Return (Pts)','Difference')) %>%
                                                    formatRound(c('Pts','Rd_Pts','Difference'))})

      #Leaderboard for position
      BestPos1 <- Draft_Act_Proj %>%
        select(Team, Year, Round, POS, Fantasy_Pts) %>%
        group_by(Team,POS) %>%
        summarize(Pts = mean(Fantasy_Pts))
      
      BestPos2 <- Draft_Act_Proj %>%
        select(Year, Round, POS, Fantasy_Pts) %>%
        group_by(POS) %>%
        summarize(Rd_Pts = mean(Fantasy_Pts))
      
      BestPos <- left_join(BestPos1,BestPos2,by = c('POS' = 'POS')) %>%
        mutate(Difference = Pts - Rd_Pts) %>%
        dplyr::arrange(POS,desc(Difference)) %>%
        select(-Rd_Pts,-Pts)
      
      BestPos <- ddply(BestPos, "POS", function(x) head(x[order(x$Difference, decreasing = TRUE) , ], 1))
      

    output$BestPosition <- DT::renderDataTable({datatable(BestPos, 
                                                    options = list(pageLength = 25, dom = 't',
                                                    initComplete = JS("function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': '#FFF', 'color': '#000'});","}")), 
                                                    rownames = FALSE, 
                                                    colnames = c('Team','Pos','+/- League')) %>%
                                                  formatRound(c('Difference'))})
    
    
   
    
######################################################################################################################
################################### Create Plots for Output ##########################################################
######################################################################################################################

      
    ###############################################################################
    ############################### Draft Analysis ################################
    ###############################################################################
            output$TrendRound <- renderPlotly({
              plot_ly(dtCompare(),
                      type = "bar",
                      x = ~Round,
                      y = ~Draft_Perc, name = "GM Average") %>%
                add_trace(y = ~Draft_Perc_All, name = "League Average") %>%
                layout(title = "GM Tendencies vs League",
                       barmode = 'group',
                       yaxis = list(title = '% Drafted'))})
    
    #######################################################################
    ########################### Trend Analysis ############################
    #######################################################################
          output$barchart <- renderPlotly({
            plot_ly(dataplusminus(), 
                    type = "bar",
                    x = ~Opponent_Name, 
                    y = ~Diff_Pts_For, name = "Points For") %>%
              add_trace(y = ~Diff_Pts_Against, name = "Points Against") %>%
              layout(title = "Performance Splits",
                     barmode = 'group',
                     autosize = F, width = 600, height = 500, margin = m,
                     yaxis = list(title = '+/- Period Average'),
                     xaxis = list(title = ''),
                     legend = list(x = .99, y = .99))})
                  
    
    
  
})
