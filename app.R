library(dplyr)
library(shiny)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(plotly)
library(shinydashboard)

NBA_xPlayer_P36M = read.csv(file = "NBA_xPlayer_P36M.csv", stringsAsFactors = F)

NBA_corrplot = select(NBA_xPlayer_P36M, 
                      PTS = playPTS.36, 
                      `FG%` = playFGp.36,
                      `FT%` = playFTp.36,
                      `3PM` = play3PM.36,
                      REB = playTRB.36,
                      AST = playAST.36, 
                      TO = playTO.36,
                      STL = playSTL.36,
                      BLK = playBLK.36,
                      Poss = playTotPoss.36)

NBA_scatter = select(NBA_xPlayer_P36M, Player = playDispNm, Position = playPos, Points = playPTS.36, `FG%` = playFGp.36, `FT%` = playFTp.36, 
                      `3PM` = play3PM.36, Rebounds = playTRB.36, 
                     Assists = playAST.36, Turnovers = playTO.36,
                     Steals = playSTL.36, Blocks = playBLK.36, Possessions = playTotPoss.36)

NBA_scatter$Position = factor(NBA_scatter$Position, levels = c('PG', 'SG', 'SF', 'PF', 'C'))

NBA_scatterFeatures = NBA_scatter[,3:12]
NBA_scatter[,3:12] = round(NBA_scatter[,3:12], digits = 2)

#### 1. Correlation matrix in plotly (https://plot.ly/~kontsevoi/21/correlation-matrix/#code)
#### 3. Checkboxes to add/remove position ()
#### 4. Add hover text (https://plot.ly/r/text-and-annotations/)
#### 5. Server storage
#### 6. Smooth scatter

ui = dashboardPage(skin = "black",
  
  dashboardHeader(title = "NBA 2017-2018"),
  
  dashboardSidebar(
      sidebarUserPanel(image = "https://media.licdn.com/dms/image/C4D03AQFzxiBTdCrk1A/profile-displayphoto-shrink_200_200/0?e=1530316800&v=beta&t=Br1JJ9u_GeGcXxor2h7koVJ5q05EO49xGzUSCWx4BEY", 
                        name = "Sam Marks", subtitle = "sjfmarks@gmail.com"
        ),
        
      sidebarMenu(
        menuItem(text = ''),
          selectInput(inputId = "x_coord_Input", label = "Select X Coordinate", 
                  choices = colnames(NBA_scatterFeatures))),
          
        menuItem(text = ''),
          selectInput(inputId = "y_coord_Input", label = "Select Y Coordinate", 
                      choices = colnames(NBA_scatterFeatures))
    
      ),
  
  dashboardBody(
    h3("NBA 2017-2018 | Statistical Correlations"),
    
    fluidRow(
      box(title = ' ', status = "warning", plotlyOutput("NBAscatter"), width = 7),
      box(title = "Reference: Correlation Matrix by Statistic", plotOutput("NBAcorrMap"), 
          collapsible = T, width = 5, solidHeader = T, status = "info")
    )
  )
)

# Define server logic required to draw a histogram
server = shinyServer(function(input, output, session) {
  
  source("helpers.R")
  
  output$NBAcorrMap = renderPlot({
    
        corrplot(cor(NBA_corrplot), method = "circle", tl.cex = .75,
                 type = "lower", tl.srt = 45, order = "hclust", tl.col = "black", 
                 col = rev(brewer.pal(n = 8, name = "RdYlBu")))
        })
  
  observeEvent(c(input$y_coord_Input, input$x_coord_Input), {
        plot.df = data.frame(x = NBA_scatter[, input$x_coord_Input],
                             y = NBA_scatter[, input$y_coord_Input],
                             Position = NBA_scatter$Position,
                             Player = NBA_scatter$Player
                             )
    
  colnames(plot.df) = c('x', 'y', 'Position', 'Player')
  
  legend <- list(font = list(size = 12), x = 100, y = 0.5)
  
  output$NBAscatter = renderPlotly({
      
        plot_ly(data = plot.df, x = ~x, y = ~y, type = "scatter", mode = "markers", color = ~Position, 
                hoverinfo = 'text', text = ~paste(Player, "\n", input$x_coord_Input, ": ", x, "\n", input$y_coord_Input, ": ", y), 
                colors = brewer.pal(n = 5, name = "Set1")) %>%
      layout(title = paste(input$x_coord_Input, 'Per 36 vs.', input$y_coord_Input,"Per 36"),
             yaxis = list(title = input$y_coord_Input, zeroline = FALSE),
             xaxis = list(title = input$x_coord_Input, zeroline = FALSE),
             legend = legend, title = "Filter By Position") %>%
        add_trace(y = ~median(plot.df$y), name = "Median line 1", mode = 'line', line = list(dash = "dot", color = "black", width = .5), showlegend = F, hoverinfo = 'none') %>%
        add_trace(x = ~median(plot.df$x), name = "Median line 2", mode = 'line', line = list(dash = "dot", color = "black", width = .5), showlegend = F, hoverinfo = 'none')
    })
  })
})

shinyApp(ui, server)
