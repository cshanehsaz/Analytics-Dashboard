#explanations.R

explanationsUI <- fluidPage(
  fluidRow(
    h1("Ontra's Scoring Method"),
    column(8, offset = 2,
      withMathJax(
        h2("1."),
        HTML("<center>"),
          h4("Subtract the delta between a prediction and the actual from the maximum possible delta. 
              For example, a user put the top performing item in second place."),
          h2("$$(10 - 1) - abs(2 - 1) = 9 - 1 = 8$$"),
        HTML("</center>"),
          h2("2."), 
        HTML("<center>"),
          h4("Weight the point values along a symmetrical parabolic distribution i.e. 
              predicting the end positions are more important than predicting the middle positions."), 
          
          h2("$$1   -    2   -    3   -    4   -   5   -   6   -   7   -   8   -   9   -   10$$"),
          h4("Point Values:"), 
          h2("$$9-8-7-6-5-5-6-7-8-9$$", "$$ \\downarrow$$",
             "$$29.5-24.6-20.0-15.7-11.9-11.9-15.7-20.0-24.6-29.5$$"
              ),
          h4("
              The new score, using the same example from above, will be:"),
          h2("$$9^{1.54} - 1^{1.54} = 29.5 - 1 = 28.5$$"
            ),
        HTML("</center>"),
          h2("3."),
        HTML("<center>"),
          h4("The system uses exponential decay to penalize marginal inaccuracy 
              i.e. the penalty decreases as the inaccuracy increases."), 
          h2("$$ {deltamax}^{1.54} \\over {\\sqrt[deltamax]{deltamax^{1.54}}}^{delta}$$"),
        HTML("</center>"),
          h2("4."),
        HTML("<center>"),
          h4("Repeat this process for each position and sum the points to get a result."),
        HTML("</center>"),
          h2("5."),
        HTML("<center>"),
          h4("The final score, out of 100, is calculated by translating and transforming the points."),
          h2("$${(points - minimum) \\over (maximum - minimum)} * 100$$"),
        HTML("</center>")
      )
    )
  )
)




# h2("Game Scoring"),
# h3("Ontra employs a scoring metric based on the relative positions an individual's order to the true order.
#     At it's most basic level before undergoing transformations, score is determined as follows:
#     There are two lists of stocks A through J. One list was chosen by a player, the other is true outcome of
#     stock performance over a given time period."
#    ),
# ###insert image of two lists
# h3("First, look only at position 1 in each list and some stock A. Assume stock A was the best performer and
#     belongs in position 1. In any player's list, the maximum possible delta 
#     (the difference between where they placed A and where it was in reality) would be 9, in the case
#     that the player put stock A at position 10. 
#     Now look at some stock B that belongs in position 5. The maximum possible delta in this case would only be 5,
#     assuming the player put it in position 10.
#     Since position 1 has a higher maximum delta than position 5, there is a higher chance of being farther away
#     from the correct answer. This means position 1 and 10 are significantly harder to guess correctly than 
#     positions 5 and 6. For this reason, we want to weight the value of each position."
#     ),
# h1("$$3^2$$"),
# h3("    
#     To accomplish this weighting, score is determined by taking the maximum possible delta of each position in 
#     the list and then
#     subtracting the player's delta to get a final point score. For example, say that there was a list of 5 
#     stocks. The true order of the list is from position 1 to position 5: A,B,C,D,E. The player instead chooses
#     to place them as E,D,C,B,A.
#     Position 1, which should be A, has a maximum delta of 4. The player's delta for this, since they placed stock
#     A in Position 5, is 5-1=4. Therefore, they would earn 4-4 (maximum delta - current delta) = 0 points for that
#     position. They would earn 3-2=1 for Position 2, 2-0=2 for Position 3, 3-2=1 for Position 4,
#     and 4-4=0 for Position 5. This gives a grand total of 4 points out of the possible 16. This is represented
#     with the equation."
#    ),
# ###insert equation
# h3("Ontra takes this scoring system and applies transformations in order to target specific behaviors,
#    outcomes, and data. First, both the maximum delta and current delta are raised to a power. This serves to
#    make the top and bottom values weighted more heavily than the middle values in order to encourage players 
#    to be the most accurate with these positions. For example, if we use an exponent 2,
#    rather than having 10 values be worth"), 
# h1("9-8-7-6-5-5-6-7-8-9"),
# h3("they would instead be worth"),
# h1("81-64-49-36-25-25-36-49-64-81"),
# h3("This has effectively made position 1 go from being worth about twice as many points as position 5 
#    to about 3 times more. It also serves to provide more possible outcomes of final scores, providing
#    an increased insight into accuracy."),
# h3("However, this method creates an issue in that decay takes on the shape of a negative exponential. This
#     means that the marginal loss of points increases the higher the delta. For example, assume Stock A belongs
#     position 1. If someone were to place it in position 1 then move it to position 2, they would only lose
#     one point $$(2-1)^2 - (1-1)^2 = 1$$
#     However, if they initially place it in position 9 then move it to position 10, they would lose 17 points
#     $$((10-1)^2 - (9-1)^2 = 17$$
#     This creates a scoring metric in which many scores are inflated since a player must have stocks positioned
#     very far from their correct position in order to receive a large point penalty, but this is impossible to
#     to do with a fixed list of only 10 positions as some will need to be closer to their correct position."),
# h3("For this reason, Ontra has slightly modified the scoring equation in order to produce the desired behaviors.
#     The following operations are iterated over each position.
#     First, find the maximum delta for this position $$max(i - 1, 10 - i)$$
#     Second, put the maximum delta up to the exponent. If it is 2, then $$(max(i - 1, 10 - i))^2$$
#     Third, find the nth root, where n is maximum delta $$((sqrt[max delta]{max(i - 1, 10 - 1))^2})$$
#     Fourth, find the delta between the real list and the player's list. Say their position 1 stock was in 
#     position 4 so their delta is 3.
#     Fifth: Final score is max delta to the power of the exponent, divided by the nth root of maximum delta to
#     the power of the current delta. $$(max(i - 1, 10 - i))^2 \\above 1pt ((\\sqrt[max delta]{max(i - 1, 10 - 1))^2})^{delta}$$
#   "),
# h3("The purpose of this is to make it so that if someone's delta is 0, they get full points, if they have
#     the max delta they should get close to 0 points, and the points should drop off steeply at first and 
#     then less so as delta increases (exponential decay).
#     In doing this transformation, the minimum amount of points awarded is 1 as a result of the structure.
#     "),
# h3("This fully defines the scoring metric used for the game.")