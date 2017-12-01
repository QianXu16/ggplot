library(readr)
library(shiny)
library(shinythemes)
library(readxl)
library(plotly)
library(dplyr)
library(tidyr)
library(plyr)

#Prepare the traditional data
df <- data.frame(read_excel("New Microsoft Excel Worksheet.xlsx"))
adlist <- data.frame(read_csv("adlist.csv"))

colnames(df) <- c("Player", "Team", "Age", "Game Played", "Wins", "Losses", "Minutes Played", "Points", 
                  "Field Goals Made", "Field Goals Attempted", "Field Goal Percentage", 
                  "3 Point Field Goals Made", "3 Point Field Goals Attempted",
                  "3 Point Field Goal Percentage", "Free Throws Made", "Free Throws Attempted", 
                  "Free Throw Percentage", "Offensive Rebounds", "Defensive Rebounds", 
                  "Rebounds", "Assists", "Turnovers", "Steal", "Blocks","Personal Fouls", 
                  "Fantasy Points", "Double Doubles", "Triple Doubles", "Plus-Minus")

#font
f <- list(family = "Courier New, monospace", size = 18, color = "#050505")
xs <- list(title = "Number of Games Played", titlefont = f)
Rank <- c(1:10)
#prepare data for overall ranking
dfT <- df[, c(1, 2, 8, 9, 12, 21, 26)]
dfK <- dfT
#quantile values are in dfK
for(i in 3:7){
  dfK[,i] <- ecdf(dfT[,i])(dfT[,i])
}

ui <- navbarPage("Best NBA Player of 2017",
                 tabPanel("Introduction and Background", uiOutput("page0")),
                 tabPanel("Single Measurement", uiOutput("page1")),
                 tabPanel("Combinations of Measurement ", uiOutput("page2")),
                 tabPanel("Advanced Statistics and Final Thoughts", uiOutput("page3")))

server <- function(input, output){
  ##interface of introductory tab
  output$page0 <- renderUI({
    ui = fluidPage(theme = shinytheme("cosmo"),
      fluidRow(
        column(width = 9, offset=3, h1("Who is the Best NBA Player of Season 2017-2018?"))),
      fluidRow(
        column(width = 5, offset = 1, h2("Introduction and Background")),
        column(width = 5, offset = 1, h2("Structure of this App"))
        ),
      fluidRow(
        column(width = 5, offset = 1, h3("Who is the best best NBA player in the season 2017-2018? This question might
                                         be easy to answer, since every NBA fan and website could give us an answer without much
                                         hesitation; it is, at the same time a difficult one, as their answers may differ
                                         a lot. I will explore this topic using statistics."),
               h3("In general, there are 2 types of game statistics we can utilise to assign the rankings. The first
                  type is 'traditional statistics'. These data is straightforward and correlated with each other. 
                  For example: points per game and field goals made per game. They both could be a great criterion, though 
                  they contain repeated information. "),
               h3("The second type are advanced measurements created by NBA statisticians. They are combinations of several 'traditional statistics' and are assigned 
                  new meanings. For example, the variable 'Player Impact Estimate' is combination of several traditional statistics, 
                  and could be used to measure overall contribution in games certain players play in.")),
        column(width = 5, offset = 1, h3("This web application is interactive. You could switch between pages by clicking
                                          tabs above."),
               h3("Second and third tab are about 'traditional statistics'. In the second tab, you could select the criterion 
                  interesting to you and explore the distribution of this variable. Ranking of players regarding this criterion is presented as well."),
               h3("In the third tab, you could personalize your own criterion, by adjusting the weights of different traditional measurements; the app will 
                  present updated rankings using your criterion."),
               h3("In the last tab, you can explore advanced statistics about NBA players. Conclusion and final thoughs are presented in the last tab."),
               h3("All the data used in this project is from NBA official statistics website: https://stats.nba.com"),
               h3("You could find meanings of advanced statistics in here: http://stats.nba.com/help/glossary"))
      )
    )
  })
  
  ##interface of first tab
  output$page1 <- renderUI({
    ui = fluidPage(theme = shinytheme("cosmo"),
      titlePanel("Ranking by Single Measurement"),
      sidebarLayout(
        sidebarPanel(width = 2,
                     selectInput("y", "Choose a Traditional Measurement (per Game):",
                                 list("Points", "Field Goals Made", "3 Point Field Goals Made", 
                                      "Free Throws Made", "Rebounds", "Assists", "Turnovers", 
                                      "Steal", "Blocks", "Fantasy Points"))),
        mainPanel(fluidRow(
                    column(width = 7, plotlyOutput("scatter")),
                    column(width = 5, h4("You can select the measurements interesting to you on the left part of this page, charts on this page 
                                         will adjust themselves accordingly. You could also move the cursor to see the interactive hover text."),
                           h4("Up left plot shows the relationship between selected measurements and number of games played. Each point represents
                              one certain player. The best player that we are looking for, should have high number of games played, as well as high
                              values in selected measurements."),
                           h4("Down left bar chart, together with down right table, shows top 10 players with respect to the selected single measurement."),
                           h4("I think the  best player should rank top 10 in at least one of these lists. And several players rank top 10 more 
                              than several times: for example James Hardon, Stephen Curry and DeMarcus Cousins; which could potentially make 
                              them candidates for the Best Player."))),
                  fluidRow(
                    column(width = 7, plotlyOutput("bar")),
                    column(width = 5, tableOutput("table"))
                  ))
      )
    )
  })
  
  ##interface of second tab
  output$page2 <- renderUI({
    ui = fluidPage(theme = shinytheme("cosmo"),
      titlePanel("Personalized Ranking Criterion"),
      sidebarLayout(
        
        sidebarPanel(width = 2,
                     sliderInput("point", "Points Making Ability",
                                 min = 0, max = 100, value = 30),
                     sliderInput("fieldGoal", "Field Goals Making Ability",
                                 min = 0, max = 100, value = 20),
                     sliderInput("threePoint", "3 Point Goals Ability",
                                 min = 0, max = 100, value = 20),
                     sliderInput("assist", "Assists Ability",
                                 min = 0, max = 100, value = 15),
                     sliderInput("fantasy", "Fantasy Points",
                                 min = 0, max = 100, value = 15)),
        mainPanel(fluidRow(
                    column(width = 7, plotlyOutput("pie")),
                    column(width = 5, h4("Like different University ranking institutions would have varied criteria, here in this page, we can try 
                                         different criteria by adjusting weights of the measurements listed on the left, and get a top 10 list, 
                                         as well as the Best NBA Player using personalized weights."),
                           h4("The more important you think certain quanlity is, the further to the right you could slide."),
                           h4("The Donut plot on the top left shows weights of your criterion"),
                           h4("List on the down right, is the updated Top 10 NBA player in 2017 based on your personal criterion."),
                           h4("As we could see, several players constantly make it to the Top 10 list; and James Hardon, LeBron James and Stephen Curry are 
                              incredibly stable. While if we put less weights on 3 points ability and assistant ability, Giannis Antetokounmpo and 
                              Kristaps Porzingis become top players."))),
                  fluidRow(
                    column(width = 5, offset = 1, 
                           br(),
                           br(),
                           h4("Explaination of the calculation of scores:"),
                           h4("Firstly, I calculated percentile of each player's value in each measurement; for example, 0.85 means this player is in the 
                              top 15% of all players."),
                           h4("Secondly I use 100 times this percentile to indicate the rank of this player of this measurement. The higher
                              this value is, the better this player is doing, regarding this measurement."),
                           h4("Then I calculate weighted average of values calculated above, using weights defined by the App user.")),
                    column(width = 5, offset = 1,tableOutput("tableT"))))
      )
    )
  })
  
  ##interface of the third tab
  output$page3 <- renderUI({
    ui = fluidPage(theme = shinytheme("cosmo"),
    titlePanel(h3("Advanced Statistics and Final Thoughts")),
      sidebarLayout(
        sidebarPanel(width = 2,
                     selectInput("advan", "Choose an Advanced Measurement:",
                                 list("Player Impact Estimate", "Net Rating", " Assist Ratio", "Turnover Ratio", 
                                      "Effective Field Goal", "True Shooting", "Usage", "Pace"
                                      ))),
        mainPanel(
          fluidRow(
            column(width = 6, plotlyOutput("barFreq")),
            column(width = 6, plotlyOutput("barAdv"))
          ),
          fluidRow(
            column(width = 6, 
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   h4("The bar chart above shows the players who ranks Top 10 at least once, and the frequency. 
                      Some players' names are not shown due to lack of space; though their corresponding bars
                      are shown above."),
                   h4("Each bar represents one player. For example, James Hardon has a frequency value of 7, 
                      which means he ranks 'Top 10' seven times out of the 10 Single Measurement Ranking in the
                      'Single Measurement' tab."),
                   h4("I suppose the best NBA player should be in the chart above, so I treat players shown above
                      as candidates for the Best Player, and explore further by analysing advanced statistics."),
                   h4("As shown in the up left chart, some advanced statistics are correlated with the frequency
                      bar plot, such as 'Player Impact Estimate' and 'Usage': those who show up more often on the lists 
                      tend to score higher in terms of these 2 measurements.")),
            column(width = 6, 
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   h4("While these candidates score similarly in terms of some other criteria, such as 'Pace' and
                      'Assist Ratio'."),
                   h4("Personally, I will say I think James Hardon is having a good season, and could potentially 
                       be the Best NBA Player of season 2017 - 2018."),
                   h4("First reason is that, James is in 7 out of 10 Top 10 Lists, meaning that he is doing a great job
                      in terms of many criteria. Secondly, James Hardon scores No.1, with my personalized ranking 
                      weights. Also, as shown above, he is among the top players in terms of many other NBA
                      advanced statistics."),
                   h4("We might analyse their historical data as well. Also, Twitter could be a great data source, if
                       we would like to analyse how popular these players are, and how fans are talking about them."))
          )
        )
      )
    )
  })
  
  ##Values of the third tab
ctr_adv <- c("Net Rating", " Assist Ratio", "Turnover Ratio", 
             "Effective Field Goal", "True Shooting", "Usage", "Pace", 
             "Player Impact Estimate")
colnames(adlist)[c(11, 14, 18, 19, 20, 21, 22, 23)] <- ctr_adv
responseAdv <- reactive({adlist %>% pull(which(colnames(adlist) == input$advan))})
  
  ##Output of the third tab
output$barFreq <- renderPlotly({
  plot_ly(
    data = adlist,
    y = ~reorder(PLAYER, Freq),
    x = ~ Freq,
    type = "bar")%>% 
    layout(yaxis = list(title = "Player", tickangle = 0, titlefont = f),
           xaxis = list(title = "Frequency on Top 10 Lists", tickangle = 0, titlefont = f),
           margin = list(l = 150), height = 500)
})

output$barAdv <- renderPlotly({
  plot_ly(
    data = adlist,
    y = ~reorder(PLAYER, Freq),
    x = ~ responseAdv(),
    type = "bar")%>% 
    layout(yaxis = list(title = "Player", tickangle = 0, titlefont = f),
           xaxis = list(title = input$advan, tickangle = 0, titlefont = f),
           margin = list(l = 150), height = 500)
  
})
  
  
  ##values of first tab
  response <- reactive({df %>% pull(which(colnames(df) == input$y))})
  
  ys <- reactive({list(title = paste(input$y, "per Game"), titlefont = f)})
  
  Player <- reactive({df[order(response(), decreasing = T), ][1:10, 1]})
  Team <- reactive({df[order(response(), decreasing = T), ][1:10, 2]})
  Scr <- reactive({df[order(response(), decreasing = T), ][1:10, which(colnames(df) == input$y)]})
  tb <- reactive({data.frame(Rank, Player = Player(), Team = Team())})
  
  ##output of first tab
  output$scatter <- renderPlotly({
    plot_ly(
      data = df, 
      x = ~ `Game Played`,
      y = ~ response(),
      text = ~paste(Player),
      color = ~`Game Played`, size = ~`Game Played`) %>% 
      layout(xaxis = xs, yaxis = ys())})
  
  output$table <- renderTable(data.frame(tb()))
  
  output$bar <- renderPlotly({
    plot_ly(
      y = Scr(),
      x = reorder(Player(), -Scr()) ,
      type = "bar")%>% 
      layout(xaxis = list(title = "Player", tickangle = -35, titlefont = f),
             yaxis = ys(),
             margin = list(b = 120))})
  
  ##values of second tab
  catgr <- c("Points Making Ability", "Field Goals Making Ability", "3 Point Goals Ability", 
             "Assists Ability", "Fantasy Points")
  val <- reactive({c(input$point, input$fieldGoal, input$threePoint, input$assist, input$fantasy)})
  dfPie <- reactive({data.frame(catgr, val = val())})
  wt <- reactive({val()/sum(val())})#weights
  #final scores (adjusted with tailered weights) are stored in scrT
  scrT <- reactive({100*as.matrix(dfK[,3:7]) %*% matrix(wt(), ncol = 1)})
  # show top 10 player; rank, name, score, team
  PlayerT <- reactive({dfK[order(scrT(), decreasing = T), ][1:10, 1]})
  TeamT <- reactive({dfK[order(scrT(), decreasing = T), ][1:10, 2]})
  scrtop <- reactive({sort( round(scrT(), digits = 3) , decreasing = T)[1:10]})
  tbT <- reactive({data.frame(Rank, Player = PlayerT(), Team = TeamT(), Score = scrtop())})
  
  ## output of second tab
  output$pie <- renderPlotly({
    plot_ly(
      data = dfPie(), labels = ~catgr, values = ~val, type = "pie",
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hole = 0.3,
      marker = list(line = list(color = '#FFFFFF', width = 1)),
      showlegend = FALSE
    )%>% 
      layout(margin = list(b = 0, u = 0, l = 0, r = 0))})
  
  output$tableT <- renderTable({
    tbT()
  })
}

shinyApp(ui, server)
