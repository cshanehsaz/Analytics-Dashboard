#Revenue Model
#Variables:
 # 1. Attrition
 # 2. Adoption
 # 3. num of people, amount they play (weekly, semi weekly, daily)
 # 4. Number of games
 # 5. Average Ontra Cutss
 # 
 # Outputs:
 # 1. Gross Revenue, Expenses, Net Revenue
 # 2. CAC per user
 # 3. Variable/Fixed/Total Costs, Total Rev, Break even points
 # 
 # 6 Phases: 1-6, 6-12, 12-18, 18-24, 24-30, 30-36

#daily games: 1   weekly: 2    monthly: 5

#Target CAC: 5-10 dollars
#CURRENT NEXT STEPS: make the table in shiny display all of these costs and things. 
                    #combine them so we get total gross rev, costs, net and some pretty charts

#what is the attrition rate for new users? = What percentage of new users this month will continue to play next month?
#what percentage of retained users will continue to play next month?
#source: localytics
attrition.newuser <- .6
attrition.retuser <- .2
adoption <- 1
#how many predownloaded?
users.predownloaded <- 100
months <- 36
users.df <- 0


#add virality to this to make it more linear and less expo. Make growth linearly constant then add virality
findusers <- function(adoption = 2500, retention.newuser = .4, retention.retuser = .75, 
                            virality = .5, predownloads = 1000, months = 36){
  totalusers <- c(predownloads, rep(0, months-1))
  retusers <- rep(0, months)
  newusers <- c(predownloads, rep(0, months-1))
  churned <- rep(0, months)
  users.df <- data.frame(totalusers, retusers, newusers, churned, row.names = seq(36))
  
  for(i in 2:36){
    users.df$newusers[i] <- round(adoption + users.df$retusers[i-1] * virality)
    users.df$churned[i] <- round(users.df$newusers[i-1] * (1-retention.newuser) + 
                                    users.df$retusers[i-1] * (1-retention.retuser))
    users.df$retusers[i] <- round(users.df$newusers[i-1] * retention.newuser + 
                                   users.df$retusers[i-1] * retention.retuser)
    users.df$totalusers[i] <- users.df$retusers[i] + users.df$newusers [i]
  }
  return(users.df)
}


#make it so we can change the number of people playing games based on the number of phases
netrevenue.games <- function(cut.avg = .1,
                             players.monthly = .75, players.weekly = .15,
                             players.daily = .075, players.intradaily = .025,
                             dailygames = 3, dailybuyin = 1,
                             weeklygames = 2, weeklybuyin = 2,
                             monthlygames = 1, monthlybuyin = 5,
                             phases = 6, users.df = findusers()){
  buyin.avg <- (30*dailybuyin*dailygames + 4*weeklybuyin*weeklygames + monthlybuyin*monthlygames)/
                  (30*dailygames+4*weeklygames+1)
  plays.avg <- (1.5*players.monthly + 4*players.weekly + 30 * players.daily + 45 * players.intradaily)

  games.rev.gross <- rep(0, length(users.df$totalusers))
  games.costs <- rep(0, length(users.df$totalusers))
  games.rev.net <- rep(0, length(users.df$totalusers))
  for(i in 1:length(users.df$totalusers)){
    games.rev.gross[i] <- users.df$retusers[i] * buyin.avg * plays.avg
    games.costs[i] <- -games.rev.gross[i] * (1-cut.avg)
  }
  games.rev.net <- games.rev.gross + games.costs
  netrevenue.games <- cbind(games.rev.gross, games.costs, games.rev.net)
  return(netrevenue.games)
}

netrevenue.games.arppu <- function(cut.avg = .1,
                             arppu = 5.72,
                             phases = 6, users.df = findusers()){
  games.rev.gross <- rep(0, length(users.df$totalusers))
  games.costs <- rep(0, length(users.df$totalusers))
  games.rev.net <- rep(0, length(users.df$totalusers))
  for(i in 1:length(users.df$totalusers)){
    games.rev.gross[i] <- users.df$retusers[i] * arppu
    games.costs[i] <- -games.rev.gross[i] * (1-cut.avg)
    games.rev.net[i] <- games.rev.gross[i] * (cut.avg)
  }
  netrevenue.games <- cbind(games.rev.gross, games.costs, games.rev.net)
  return(netrevenue.games)
}





netrevenue.data <- function(months = 36, phases = 3, phase.prices = c(0, 15000, 25000, 35000),
                            phase.clients = c(1, 2, 4)){
  data.rev.gross <- rep(0, 36)
  interval <- months/phases
  for(i in 1:months){
    #FIX THIS, WEIRD DROP OFF HAPPENING AT START OF YEAR 3
    if(i <= 12){
      data.rev.gross[i] <- phase.clients[1]/interval * i * ((phase.prices[2] - phase.prices[1]) / interval) * i / interval
    }
    if(i>12 && i<=24){
      data.rev.gross[i] <- ((phase.clients[2]-phase.clients[1])/interval * (i-interval) + phase.clients[1]) *  
                            (((phase.prices[3] - phase.prices[2]) / interval) * (i - interval) + phase.prices[2]) / interval
    }
    if(i>24 && i<=36){
      data.rev.gross[i] <- ((phase.clients[3]-phase.clients[2])/interval * (i-2*interval) + phase.clients[2]) * 
                            (((phase.prices[4] - phase.prices[3]) / interval) * (i - 2*interval) + phase.prices[3]) / interval
    }
  }
  return(data.rev.gross)
}

netrevenue.processingfees <- function(fee = .03){
  #unsure of what this is exactly. Venmo style fees?
}

###MUST BE IMPLEMENTED WITH INPUTS
costs.acquisition <- function(freecredit = 3, freecredit.players = 100000, monthstoamortize = 6,
                              additional = 250000){
  costs.acquisition.val <- rep(0,36)
  freecredit.cost <- freecredit * freecredit.players
  freecredit.cost.amortized <- freecredit.cost / monthstoamortize
  costs.acquisition.val[0:monthstoamortize] <- freecredit.cost.amortized
  additional.amortized <- additional / 36
  costs.acquisition.val <- costs.acquisition.val + additional.amortized
  return(costs.acquisition.val)
}

costs.acquisition.referral <- function(referralcredit = 5, virality = .5, retusers, month = 1){
  costs.acquisition.referral.val <- retusers * referralcredit * virality * .2
  return(costs.acquisition.referral.val)
}

costs.freecredit <- function(credit = 5, users.df = findusers(),
                             referralcredit = 5, virality = .5) {
  costs.freecredit <- rep(0, 36)
  for(i in 2:length(users.df$totalusers)){
    costs.freecredit[i] <- credit * users.df$newusers[i] + 
                            costs.acquisition.referral(referralcredit = referralcredit, virality = virality,
                                                       retusers = users.df$retusers[i], month = i)
  }
  return(-costs.freecredit)
}



costs.taxes <- function(grossrev, taxrate){
  costs.taxes <- rep(0, length(grossrev))
  for(i in 1:length(grossrev)){
    costs.taxes[i] <- grossrev[i]*taxrate
  }
  return(costs.taxes)
}

#function list
#findusers()
#netrevenue.games()
#netrevenue.data()
#netrevenue.processingfees()???
#costs.freecredit()
#costs.taxes()
months <- seq(36)
users.df <- findusers()
netrevenue.games.val <- netrevenue.games(users.df = users.df)
netrevenue.data.val <- netrevenue.data()
costs.freecredit.val <- costs.freecredit(credit = 1,users.df = users.df)
final.df <- cbind(users.df, netrevenue.games.val, netrevenue.data.val, costs.freecredit.val)
netrevenue.gross <- final.df$games.rev.net + final.df$netrevenue.data.val + final.df$costs.freecredit.val
final.df <- cbind(months, final.df, netrevenue.gross)
final.df
