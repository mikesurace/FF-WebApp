#server script

shinyServer(function(input, output, session) {

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
    
    gamelogs <- reactive({
      data <- Reg_Data_01 %>%
              filter(Team == input$GM, Year %between% input$period) %>%
              select(-Season,-Win_Pct,-Team)
      return(data)})
   

  output$PlayerProjections <- DT::renderDataTable(formatRound(DT::datatable(rankings_final, options = list(pageLength = 25, dom = 't'), 
                                                        rownames = FALSE,
                                                        colnames = c('Team','Inebo Points','Record',
                                                                     'Average Points Scored','Best Season: Points','Playoff Births',
                                                                      'Playoff Wins/Byes','Championships')),
                                                        c('Overall_Pts_For','Best_Season'),2))
                                                                          
  
  output$PlayoffData <- DT::renderDataTable(formatRound(DT::datatable(playoffs_final, options = list(pageLength = 25, dom = 't'), 
                                                  rownames = FALSE,
                                                  colnames = c('Team','Appearances','Wins','Loses','Byes',
                                                               'Championships','Ave Points Scored','Ave Points Against')),
                                                  c('Playoffs_Pts_For','Playoffs_Pts_Against'),2))
  
  output$gamelogs <- DT::renderDataTable({DT::datatable(gamelogs(), options = list(pageLength = 100), rownames = FALSE)})
  
  output$trend <- DT::renderDataTable({formatPercentage(datatable(datatrend(), options = list(pageLength = 25, dom = 't'), 
                                                 rownames = FALSE, 
                                                 colnames = c('Opponent', 
                                                              'Games Played',
                                                              'Wins', 
                                                              'Loses', 
                                                              'Winning %')),'Win_Pct')})
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