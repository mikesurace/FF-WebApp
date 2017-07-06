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
    draftTendTeam <- reactive({
          data <- Draft_Act_Proj %>%
                    filter(Round %between% input$dtRound, Team == input$dtTeam, Year %between% input$dtTime) %>%
                    group_by(Team,Round) %>%
                    summarize(WR_Pct = mean(WR),
                              QB_Pct = mean(QB),
                              TE_Pct = mean(TE),
                              RB_Pct = mean(RB),
                              DEF_Pct = mean(DEF),
                              K_Pct = mean(K)) %>%
                    select(Team, Round,QB_Pct,RB_Pct,WR_Pct,TE_Pct,DEF_Pct,K_Pct)
          return(data)
          })
    
    
    output$Draft <- DT::renderDataTable({datatable(draftTendTeam(), options = list(pageLength = 25, dom = 't'), 
                                                                    rownames = FALSE, 
                                                                    colnames = c('Team', 'QB','RB','WR','TE','DEF','K')) %>%
                                            formatPercentage(c('WR_Pct','RB_Pct','QB_Pct','TE_Pct','DEF_Pct','K_Pct'))})
    
    draftTendRound <- reactive({
      data <- Draft_Act_Proj %>%
        filter(Round %between% c(1,6)) %>%
        group_by(Round) %>%
        summarize(WR_Pct = mean(WR),
                  QB_Pct = mean(QB),
                  TE_Pct = mean(TE),
                  RB_Pct = mean(RB),
                  DEF_Pct = mean(DEF),
                  K_Pct = mean(K))
      return(data)
    })
    
    drTeam <- reactive({
      data <- Draft_Act_Proj %>%
        filter(Year == input$draftyear, Team == input$draftGM) %>%
        mutate(Performance = (Fantasy_Pts - Projections),Percent = Performance/Projections) %>%
        select(Round, Name, POS,Projections,Fantasy_Pts,Performance,Percent)
      
      return(data)

    })
    
    output$draftresults <- DT::renderDataTable({datatable(drTeam(), options = list(pageLength = 25, dom = 't'), 
                                                          rownames = FALSE, 
                                                          colnames = c('Round','Player','Position',
                                                                       'Projected Points','Actual Points',
                                                                       'Return','% Return')) %>%
        formatPercentage(c('Percent')) %>%
        formatRound(c('Projections','Performance','Fantasy_Pts'))})
    
    
        grid <- reactive({
          data <- Draft_Act_Proj %>% 
                    filter(Year == input$yr, Round < 11) %>%
                    select(Round,Team,Name) %>%
                    spread(Round,Name)
          return(data)})
    
    output$draftgrid <- DT::renderDataTable({datatable(grid(), 
                                                       options = list(pageLength = 25, 
                                                                      dom = 't'), 
                                                       rownames = FALSE,
                                                       colnames = c('Team','Round 1','Round 2',
                                                                    'Round 3','Round 4',
                                                                    'Round 5','Round 6',
                                                                    'Round 7','Round 8',
                                                                    'Round 9','Round 10')) %>%
                                              formatStyle('Team',
                                                          fontWeight = 'Bold')})
    
    
    # data <- Draft_Act_Proj %>%
    #           #filter(Year == 2016) %>%
    #           select(-Round,-Pick,-Year) %>%
    #           group_by(Team,POS) %>%
    #           summarize(Ave = mean(Fantasy_Pts)) %>%
    #           ungroup() %>%
    #           dplyr::arrange(POS, desc(Ave)) %>%
    #           spread(POS,Ave)
              
    
    
    # data <- Draft_Act_Proj %>%
    #           filter(Round < 11, POS %in% c('RB','WR','QB','TE')) %>%
    #           mutate(Performance = Fantasy_Pts - Projections,Wgt = Performance*Projections) %>%
    #           group_by(Team, Year) %>%
    #           summarize(Total = sum(Projections),
    #                     Wgt_Return = sum(Wgt))
    # data1 <- data %>%
    #           mutate(WtvAvg = Wgt_Return/Total)
                              
    
######################################################################################################################
################################### Create Plots for Output ##########################################################
######################################################################################################################

      
    ###############################################################################
    ############################### Draft Analysis ################################
    ###############################################################################
            output$TrendRound <- renderPlotly({
              plot_ly(draftTendRound(), 
                      type = "bar",
                      x = ~Round, 
                      y = ~WR_Pct, name = "WR") %>%
                add_trace(y = ~RB_Pct, name = "RB") %>%
                add_trace(y = ~QB_Pct, name = "QB") %>%
                add_trace(y = ~TE_Pct, name = "TE") %>%
                layout(title = "Overall Draft Tendency",
                       barmode = 'group',
                       autosize = F, width = 600, height = 500, margin = m,
                       yaxis = list(title = '% Drafted'),
                       xaxis = list(title = ''),
                       legend = list(x = .99, y = .99))})
    
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
