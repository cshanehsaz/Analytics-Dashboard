#revenue model UI
revenuemodelUI <- fluidPage(
  fluidRow(
    column(6,
      plotOutput("games", width = "100%")
    ),
    column(6,
      plotOutput("revenue", width = "100%")
    )
  ),
  fluidRow(column(12, align = 'center' ,
                  p('______________')
                  )),
  fluidRow(
      #FIRST ROW OF INPUTS
      column(4,
      box(
        numericInput("linear_adoption", h4("How many users will Ontra gain each month organically?"),
                     value = 5000, step = 100, min = 0
        ),
        numericInput("predownloads", h4("How many predownloads were there?"),
                     value = 10000, step = 100, min = 0
        ),
        title = "User Acquisition Rates",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        width=12
      )
      ),
      column(4,
             box(
               sliderInput("retention_new", h4("What percentage of new users will play next month?"),
                           value = .4, step = .01, min = 0, max = 1
               ),
               sliderInput("retention_ret", h4("What percentage of retained users will play next month?"),
                           value = .8, step = .01, min = 0, max = 1
               ),
               h4('What is the virality coefficient?'),
               numericInput("virality", h6("Number of new users each retained user recruits each month"),
                            value = .5, step = .01, min = 0
               ),
               title = "User Retention",
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE, collapsed = TRUE,
               width=12
             )
      ),
      column(4,
             box(
               numericInput("limnum", h4("How many new players will Ontra give credit to after launch?"),
                            value = 100000, step = 10000, min = 0
               ),
               numericInput("limcredit", h4("How much free credit will Ontra give to these players?
                                            (Amortized over the first 6 months)"),
                            value = 3, step = 1, min = 0
               ),
               numericInput("credit", h4("How much free credit will Ontra give to ALL new players?"),
                            value = 0, step = 1, min = 0
               ),
               numericInput("refcredit", h4("How much free credit will Ontra give for referrals?"),
                            value = 5, step = 1, min = 0
               ),
               numericInput("additionalcosts", h4("How much will Ontra spend on miscellaneous acquisition costs?"),
                            value = 250000, step = 10000, min = 0
               ),
               numericInput("paid_linear_adoption", h4("How many users will Ontra gain each month from paid sources (Advertising, Marketing, Promotions, etc)?"),
                            value = 5000, step = 100, min = 0
               ),
               title = "Cost of Acquisition",
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE, collapsed = TRUE,
               width=12
             )
      )
  ),
  
  ###SECOND ROW OF INPUTS
  fluidRow(
    column(4,
           box(
             sliderInput("cut.avg", h4("What average percentage of buy-ins will Ontra keep as net revenue?"),
                         value = .1, step = .01, min = 0, max = 1
             ),
             checkboxInput("use_arppu", h4("Use ARPPU instead of Games and Buyins"),
                          value = TRUE
                          ),
             HTML("<hr> </hr>"),
             numericInput("arppu", h4("ARPPU"), value = 10, min = 0),
             HTML("<hr />"),
             h4("Games and Buyins"),
             numericInput("monthly_players", h4("What percentage of players play 1-2 times per month?"),
                          value = .75, step = .01, min = 0, max = 1),
             numericInput("weekly_players", h4("1-2 times a week?"),
                          value = .15, step = .01, min = 0, max = 1),
             numericInput("daily_players", h4("About once a day?"),
                          value = .075, step = .01, min = 0, max = 1),
             numericInput("intradaily_players", h4("More than once per day?"),
                          value = .025, step = .01, min = 0, max = 1),
             numericInput("daily_buyin", h4("What is the buyin for daily, monthly, and weekly games?"),
                          value = 1, step = .5, min = 0),
             numericInput("weekly_buyin", p(" "),
                          value = 2, step = .5, min = 0),
             numericInput("monthly_buyin", p(" "),
                          value = 5, step = .5, min = 0),
             title = "Game Revenue",
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE, collapsed = TRUE,
             width=12
           )
           ),
    column(4,
           box(
             numericInput("data_price1", h4("How much will Ontra sell the data for at the end of year 1, year 2, and year 3?"),
                          value = 25000, step = 1000, min = 0
             ),
             numericInput("data_price2", p(" "),
                          value = 35000, step = 1000, min = 0
             ),
             numericInput("data_price3", p(" "),
                          value = 45000, step = 1000, min = 0
             ),
             numericInput("client1", h4("How many clients will Ontra sell to in Year 1, Year 2, and Year 3?"),
                          value = 1, step = 1, min = 0),
             numericInput("client2", p(" "),
                          value = 10, step = 1, min = 0),
             numericInput("client3", p(" "),
                          value = 25, step = 1, min = 0),
             title = "Data Sales Revenue",
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE, collapsed = TRUE,
             width=12
           )   
    ),
    column(4,
       box(
         checkboxInput("invert_costs", h4("  Make Costs Appear As Negative"), 
                       value = TRUE),
         title = "Graph Options",
         status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE, collapsed = TRUE,
         width=12
       )
    )    
  ),
  fluidRow(
    h6('ERROR - Legends Seem To Be Temporarily Unavailable:'),
    h6('User Growth: Black - Total Users,   Green - Retained Users,   Blue - New Users'),
    h6('Revenue Growth: Black - Net Revenue from Games and Data Sales, Blue - Data Sales Revenue,
       Green - Net Game Revenue (After Payouts),    Red - Acquisition Costs')
  )
)