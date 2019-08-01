#to do
#make scoring out of 100
#make write up -
#   explain c-score, where it comes from, how to get it to be out of 100
#   explain payout distribution
#   explain how score works and how it's all relative
#   make legends
#   any other math that's present that I'm not thinking of
#   REAL DEADLINE: Friday
#   round scores to 2 decimal places (not really necessary)
#make more in depth user acquisition model
#   number of games to cash out, minimum amount to cash out, only top % of players can cash out

library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(formattable)
source("accuracymodel.R")
source("revmodel1.R")
source("revmodelShiny.R")
source("explanations.R")

  #Dashboard header carrying the title of the dashboard
  header <- dashboardHeader(title = "Ontra Analytics") 
  #Sidebar content of the dashboard
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Individual Games", tabName = "games", icon = icon("dice-six")),
      menuItem("Revenue Model", tabName = "revenue", icon = icon("poll")),
      menuItem("Explanations", tabName = "explanations", icon = icon("book")),
      menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
               href = "https://ontra.app")
    )
  )
  
  #dashboard UI elements
  #------------------
  welcomePage <- fluidRow(
    box(
      h1('Welcome to the Ontra Analytics Platform.'),
      h3('If you have questions or comments, please submit them to'),
      h3('cyrus@ontra.app'),
      width = 12
    )
  )
  
  #Individual Games UI
  #-----------------------
  #input parameters for individual games
  #gameparams <- fluidRow(box(
  gamevalues1 <- fluidRow(
    valueBoxOutput("value1"),
    valueBoxOutput("value2"),
    valueBoxOutput("value3")
  )
  gamevalues2 <- fluidRow(
    valueBoxOutput("value4"),
    valueBoxOutput("value5"),
    valueBoxOutput("value6")
  )
  
  indivgames <- fluidRow(
    box(
      radioButtons("radio", h4("What Behavior Do Players Have When Making Their Lists?"),
                   choices = list("More Likely To Choose Correctly" = "A", 
                                  "Guessing Randomly" = "B",
                                  "Less Likely To Choose Correctly" = "C"),
                   width = 500
      ),
      numericInput("num", h4("Number of Participants"), value = 1000,
                   width = 500
      ),
      numericInput("buyin", h4("Buy-in Amount"), value = 1,
                   width = 500
      ),
      h4('Minimum Payout'),
      numericInput("min_pay", h6("Note: This must be 0 or greater and less than the buy-in"),
                   value = 0, step = .01,
                   min = 0,
                   width = 500
      ),
      radioButtons("payout", h4("Payout Distribution"), 
                   choices = list("Steep - A small group of players make a large amount" = "A",
                                  "Medium - A medium group of players make a medium amount" = "B",
                                  "Shallow - A large group of players make a small amount" = "C"),
                   selected = "B",
                   width = 500
      ),
      sliderInput("cut", h4("What Percent of Total Buy-ins Will Ontra Retain As Net Revenue?"),
                  min = 0, max = 100, value = 5,
                  width = 500
      ),
      title = "Game Parameters",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE
      ),
    
    #visualizations of game outcomes
    box(
      plotOutput("histPlot"),
      h6("The red line is the mean score and mean payout, respectively"),
      plotOutput("payout"),
      title = "Game Outcome",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE
      )
    )
  #----------------------
  
  tabs <- tabItems(
    tabItem(tabName = "dashboard",
            welcomePage
            ),
    tabItem(tabName = "games",
            gamevalues1,
            gamevalues2,
            indivgames
            # gameparams,
            # visuals
            ),
    tabItem(tabName = "revenue",
              revenuemodelUI, width = 12
            ),
    tabItem(tabName = "explanations",
            explanationsUI)
  )
  
  # combine the two fluid rows to make the body
  body <- dashboardBody(tabs)
  
  ui <- dashboardPage(title = 'Ontra Dashboard', header, sidebar, body, skin = 'blue')
  
  
  
  
  # create the server functions for the dashboard  
  server <- function(input, output) { 
    refresh <- function() {
      WORKAROUND1 <- input$radio #re-renders this plot when radio button is pressed
      WORKAROUND2 <- input$num
      WORKAROUND3 <- input$payout
      WORKAROUND4 <- input$cut
      #WORKAROUND5 <- input$tails
      WORKAROUND6 <- input$buyin
      WORKAROUND7 <- input$min_pay
    }
    
    #plots the outcome of the game with these parameters
    output$histPlot <- renderPlot({
      refresh()
      
      correct <- c(.18, .15, .12, .1, .1, .1, .1, .08, .05, .02)
      equal <- rep(.1, 10)
      incorrect <- c(.02, .05, .08, .1, .1, .1, .1, .12, .15, .18)
      probvec <- switch(input$radio,
                        A = correct, B = equal, C = incorrect)
      #global variable so that data can be used in payout
      x <<- sim.run(num.entries = input$num, sim.prob = probvec, 
                    score = cScoreFinal, 
                    general = FALSE, genExp = input$tails)
      x.df <- data.frame(x)
      ggplot(data = x.df, aes(x=x)) +
             geom_histogram(binwidth = 5) + xlab("Scores") + ylab("Frequency") + 
             xlim(50, 220) + ylim(0, input$num/4.5) + 
             #geom_vline(xintercept = mean(x.df$x), linetype = "dashed", color = "red") +
             theme(legend.position = "bottom")
    })
    
    output$payout <- renderPlot({
      refresh()
      #both global variables so that can be used in printing function
      payout.method <- switch(input$payout,
                              A = payout.equation.steepApprox,
                              B = payout.equation.medium2,
                              C = payout.equation.shallow)
      ###INCLUDE MIN PAYOUT HERE ONCE IT WORKS FOR ALL 3 DISTROS
      payouts <<- payout.method(data = x, cut = input$cut/100, buyin = input$buyin, min = input$min_pay)
      payouts <<- payouts / (sum(payouts)/(input$num * input$buyin * (1-input$cut/100)))
      profiters <<- length(which(payouts >= input$buyin))/length(payouts)
      index <- seq(length(payouts))
      payouts.df <- cbind(index,data.frame(payouts))
      ggplot(data = payouts.df, aes(x=index, y=payouts)) +
        geom_point() + xlab("Position (Higher Means Better Performance)") + ylab("Payout") + 
        scale_y_continuous(labels = comma)
        #+ geom_hline(yintercept = mean(payouts.df$payouts), linetype = "dashed", color = "red")
    })
    
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
      refresh()
      valueBox(
        paste(round(profiters * 100, digits = 2), '%'),
        paste('of players get paid more than their buy-in', sep = ''),
        icon = icon("stats",lib='glyphicon'),
        color = "blue")  
    })
    
    output$value2 <- renderValueBox({ 
      refresh()
      valueBox(
        round((input$num * input$cut/100 * input$buyin), digits = 2),
        'Ontra Revenue After Payouts',
        icon = icon('dollar-sign'),
        color = "green")  
    })
    
    output$value3 <- renderValueBox({
      refresh()
      valueBox(
        round(max(payouts), 2),
        paste('First Prize'),
        icon = icon("star",lib='glyphicon'),
        color = "yellow")   
    })
    
    output$value4 <- renderValueBox({
      refresh()
      valueBox(
        input$buyin * (1-input$cut/100),
        'Average Payout',
        color = 'blue'
      )
    })
    
    output$value5 <- renderValueBox({
      refresh()
      valueBox(
        round(sum(payouts), 2),
        'Total Paid Out',
        color = 'green'
      )
    })
    
    output$value6 <- renderValueBox({
      refresh()
      valueBox(
        round(min(payouts), 2),
        'Minimum Prize',
        color = 'yellow'
      )
    })
    
    #REVENUE MODEL
    #------------------------------------
    refresh2 <- function(){
      WORKAROUND1 = input$linear_adoption + input$virality + input$retention_new +
        input$retention_ret + input$predownloads + input$data_price1 + input$data_price2 +
        input$data_price3 + input$credit + input$cut.avg + input$monthly_players +
        input$weekly_players + input$daily_players + input$intradaily_players + 
        input$client1 + input$client2 + input$client3 + input$daily_buyin +
        input$weekly_buyin + input$monthly_buyin + input$invert_costs + input$use_arppu +
        input$arppu + input$paid_linear_adoption + input$refcredit + input$limcredit +
        input$limnum + input$additionalcosts
    }
    
    #currently runs master sim, all other data received from users.df output of this
    output$games <- renderPlot({
      refresh2()
      users.df <- findusers(adoption = (input$linear_adoption + input$paid_linear_adoption), virality = input$virality, 
                            retention.newuser = input$retention_new, retention.retuser = input$retention_ret,
                            predownloads = input$predownloads)
      netrevenue.games.val <- netrevenue.games(cut.avg = input$cut.avg, users.df = users.df,
                                               players.monthly = input$monthly_players,
                                               players.weekly = input$weekly_players,
                                               players.daily = input$daily_players,
                                               players.intradaily = input$intradaily_players,
                                               dailybuyin = input$daily_buyin,
                                               weeklybuyin = input$weekly_buyin,
                                               monthlybuyin = input$monthly_buyin)
      if(input$use_arppu){
        netrevenue.games.val <- netrevenue.games.arppu(cut.avg = input$cut.avg, arppu = input$arppu, 
                                                       users.df = users.df)
      }
      netrevenue.data.val <- netrevenue.data(phase.prices = c(0,
                                                              input$data_price1, 
                                                              input$data_price2, 
                                                              input$data_price3),
                                             phase.clients = c(
                                                               input$client1,
                                                               input$client2,
                                                               input$client3)
                                             )
      costs.freecredit.val <- costs.freecredit(credit =input$credit, users.df = users.df,
                                               referralcredit = input$refcredit, virality = input$virality) -
                              costs.acquisition(freecredit = input$limcredit, freecredit.players = input$limnum,
                                                additional = input$additionalcosts)
      
      final.df <- cbind(users.df, netrevenue.games.val, netrevenue.data.val, costs.freecredit.val)
      
      netrevenue.gross <- final.df$games.rev.net + final.df$netrevenue.data + final.df$costs.freecredit
      final.df <<- cbind(months, final.df, netrevenue.gross)
      final.df
      
      #user growth graph
      ggplot(data = final.df, aes(x=months, y=totalusers)) +
        geom_line(size = 1.25) + xlab("Months") + ylab("Number of Users") +
        geom_line(data=final.df, aes(x=months, y=newusers), size = 1.25, color = 'blue') + 
        geom_line(data=final.df, aes(x=months, y=retusers), size = 1.25, color = 'green') +
        ggtitle('User Growth') + theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
        scale_y_continuous(labels = comma)
    })
    
    output$revenue <- renderPlot({
      refresh2()
      invert <- 1
      if(input$invert_costs == TRUE){invert <- -1}
      ggplot() + 
        geom_line(data=final.df, aes(x=months, y=netrevenue.gross), size = 2, color = 'black') + 
        geom_line(data=final.df, aes(x=months, y=netrevenue.data.val), size = 1.2, color = 'blue') + 
        geom_line(data=final.df, aes(x=months, y=games.rev.net), size = 1.2, color = 'green') +
        geom_line(data=final.df, aes(x=months, y=invert * -costs.freecredit.val), size = 1.2, color = 'red') + 
        xlab("Months") + ylab("Monthly Revenue or Cost") + ggtitle('Net Revenue Growth from Games and Data Sales') + 
        theme(plot.title = element_text(hjust = 0.5, face = 'bold')) + scale_y_continuous(labels = comma) +
        scale_fill_manual(name = "Test")
    })
    
  }
  
  
shinyApp(ui, server)