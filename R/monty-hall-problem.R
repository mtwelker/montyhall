#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Randomly select one of three doors.
#'
#' @description
#'   `select_door()` selects one of three doors.
#'
#' @details
#'   This is the contestant making their first choice from
#'   among doors 1, 2, and 3.
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns an integer between 1 and 3.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a door with a goat behind it.
#'
#' @description
#'   `open_goat_door()` opens an unopened door with a goat 
#'   behind it.
#'
#' @details
#'   The function selects a goat door from among the unopened doors.
#'
#' @param game A vector containing one "car" and two "goat"s.
#' @param a.pick The initial door chosen; integer between 1 and 3.
#' 
#' @return The function returns an integer between 1 and 3, representing
#'   the door the host opens (which has a goat behind it).
#'
#' @examples
#'   x=c("goat","goat","car")
#'   open_goat_door( game=x, a.pick=1)
#'
#'   y=c("car","goat","goat")
#'   open_goat_door( y, 3)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Will the contestant choose a different door?
#'
#' @description
#'   `change_door()` allows the contestant to stay with their 
#'   initial pick or select the other unopened door.
#'
#' @details
#'   The function returns the initial pick as the final pick if
#'   the contestant chooses to STAY.  If the contestant chooses
#'   not to STAY, then the function returns the other unopened door.
#'
#' @param stay A logical variable (T, F) with a default STAY=TRUE.
#' @param opened.door The opened door; integer between 1 and 3.
#' @param a.pick The initial door chosen; integer between 1 and 3.
#' 
#' @return The function returns an integer between 1 and 3, representing
#'   the contestant's final choice of door to open.
#'
#' @examples
#'   change_door( stay=T, 1, 2 )
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Did the contestant win?
#'
#' @description
#'   `determine_winner()` determines whether the contestant won.
#'
#' @details
#'   The function returns "WIN" if the contestant's final pick
#'   reveals a car and "LOSE" if the contestant's final pick reveals
#'   a goat.
#'
#' @param final.pick The final door chosen; integer between 1 and 3.
#' @param game A vector containing one "car" and two "goat"s.
#'
#' @return The function returns either "WIN" or "LOSE".
#'
#' @examples
#'   x=c("goat","goat","car")
#'   determine_winner( final.pick=2, game=x )
#'
#'   y=c("car","goat","goat")
#'   open_goat_door( 3, y )
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play one round of the Monty Hall game.
#'
#' @description
#'   `play_game()` calls a series of functions to play one 
#'   complete round of the Monty Hall game.
#'
#' @details
#'   The function goes through the steps of the Monty Hall game:
#'   The contestant chooses a door, the host opens a different door
#'   (which has a goat behind it), the contestant is then given the
#'   option to stay with their initial choice or switch to the other
#'   unopened door.  Their final choice is then opened, revealing
#'   a car if they won and a goat if they lost.
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a one-vector data frame that
#'   includes whether the contestant stayed with their initial
#'   door choice or switched and whether they won or lost.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title
#'   Play the Monty Hall game multiple times and save results.
#'
#' @description
#'   `play_n_games()` plays n complete rounds of the Monty Hall 
#'   game and saves all of the results as a data frame.
#'
#' @details
#'   The function goes through the steps of the Monty Hall game n
#'   times.  For each game, the function saves what the result would
#'   be if the contestant stayed with their intitial pick and what the
#'   result would be if they switched.  Those results are then reported
#'   as a table which shows the win percentage for each strategy.
#'
#' @param n The number of times to play the game; integer default n=100.
#' 
#' @return The function returns a data frame containing the "stay"
#'   result and the "switch" result for each round of the game.
#'
#' @examples
#'   play_n_games( n=500 )
#'   play_n_games( 100 )
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
