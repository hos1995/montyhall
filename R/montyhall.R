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
#'  Select a random door for the contestant.
#'
#' @description
#'  `select_door()` generates 3 doors numbered 1-3 and randomly assigns
#'  one to the contestant.
#'
#' @details
#'  This function specifically grabs 3 doors and numbers them from 1-3
#'  and then returns a random number. This random number is the number
#'  that corresponds to the door. In this case, the order of the doors
#'  is corresponding to the order of the `create_game()` function that
#'  determines whether or not the randomly selected door has a goat or
#'  a car behind it.
#'
#' @param
#'  This function passes no parameters in it.
#'
#' @return
#'  This function returns a single value from the `doors` vector (a
#'  number between 1 and 3)
#' @examples
#'  `select_door()`
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Open a door that contains a goat.
#'
#' @description
#'  `open_goat_door()` takes the door number that is not the one
#'  selected from the `select_door()` function and a door that
#'  has a goat as determined by the `create_game()` function.
#'
#' @details
#' `open_goat_door()` is a function that returns the number of a
#'  door that contains a goat behind it. This is part of the game
#'  where the host opens a door that has a goat behind it. The
#'  door that he opens is not the selected door. So the contestant
#'  has a choice of staying with the selected door or switching to
#'  the only other closed door.
#'
#' @param
#'  This function only passes the `create_game()` vector (some
#'  combination of 1 car and 2 goats) as `game` and the `a.pick`
#'  return from the `select_doors()` function. A good way to do
#'  this would be to pass the current game and the selected
#'  door into objects:
#'  this.game <- `create_game()`
#'  my.initial.pick <- `select_door()`
#'
#' @return
#'  This function returns a the door number that isn't the one
#'  picked in `a.pick` and does not correspond to a car.
#'
#' @examples
#'  `open_goat_door(this.game, my.inital.pick)`
#'
#'  `open_goat_door(c("goat", "goat", "car"), 2)`
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
#'  Decide if you want to change the selected door or not.
#'
#' @description
#'  `change_door()` tells the game whether or not a person switches
#'  the door or stays on the originally selected door.
#'
#' @details
#'  This function takes all the previous information from the game:
#'  the order of the car and goats, the originally selected door,
#'  and the open door that shows one of the unchosen doors that has
#'  a goat behind it. This function then goes further to allow the
#'  contestant to decide if they want to stay with the selected
#'  door or switch to the only other unopened door.
#'
#' @param
#'  This function passes the `opened.door` (the opened goat door),
#'  the `a.pick` (the originally selected door), and the new
#'  parameter: `stay = T` (stay with `a.pick`) or `stay = F` (switch
#'  to the other unopened door). Creating an object for the opened
#'  door would be useful and easy to pass for the game's sake:
#'  opened.doors <- `open_goat_door( this.game, my.initial,pick)`
#'
#' @return
#'  This function returns the final door (a number between 1 and 3)
#'  that the contestant picks. This is dependent on the stay or
#'  switch that is decided in the parameter.
#'
#' @examples
#'  `change_door( stay = T, opened.doors, my.initial.pick )`
#'
#'  `change_door( stay = F, opened.doors, my.initial.pick )`
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
#'  The game decides if the final door has a car (Win) or a goat (Lose).
#'
#' @description
#'  `determine_winner()` takes the final selected door and passes it
#'  through to determine if the door has a goat or a car behind it.
#'
#' @details
#'  This function pretty simply looks at the final door from the
#'  `final.pick` and matches it to the `game` that was created in the
#'  beginning. If the `final.pick` contains a car, it is considered
#'  a win while inversely, if `final.pick` contains a goat, it is
#'  considered a loss.
#'
#' @param
#'  The parameters for this function include the `final.pick` from the
#'  previous step and the `game` that was created using the `create_game()`
#'  function. Creating objects for both scenarios whether
#'  or not the contestant stayed with the door or switched makes it
#'  more useful:
#'  my.final.pick.stay <- `change_door( stay = T, opened.doors, my.initial.pick )`
#'  my.final.pick.switch <- `change_door( stay = F, opened.doors, my.initial.pick )`
#'
#' @return
#'  This function returns "WIN" or "LOSE" depending on the final door
#'  that is selected.
#' @examples
#'  `determine_winner( my.final.pick.stay, this.game )`
#'
#'  `determine_winner( my.final.pick.switch, this.game )`
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
#'  Goes through all of the game functions to run a full game simulation to
#'  return the game results if you either stay with your door or switch.
#'
#' @description
#'  `play_game()` is the function that will run through all of the previous
#'  functions together and return a dataframe of the game result if I stay
#'  and if I switch doors.
#'
#' @details
#'  This function uses all of the other functions together and runs through
#'  every one so one can easily run the game over and over to see which
#'  method is best to win the game.
#'
#' @param
#'  Because this function uses all the previous functions, it is
#'  unnecessary for a parameter to be used. Simply run the function
#'  `play_game()` and it will return which strategy was successful
#'  in winning the game.
#'
#' @return
#'  `play_game()` returns a dataframe of the strategy by the outcome.
#'  For example, a dataframe that will be returned would have the stay and
#'  the switch strategies and next to it whether or not that strategy won
#'  or lost.
#'
#' @examples
#'  `play_game()`
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
#'  Allows one to simulate the game however many times they want.
#'
#' @description
#'  `play_n_games` is the function that will allow the contestant to
#'  simulate the game an `n` amount of times.
#'
#' @details
#'  This function is useful to run if one wants to play the game over
#'  and over to try and find out which strategy was dominant over a
#'  set number of times.
#'
#' @param
#'  The only paramter that is passed into this function is an integer
#'  that will tell the function how many times to run the `play_game()`
#'  function.
#'
#' @return
#'  This function returns a dataframe of however many games that is
#'  set in the parameter. Telling the contestant for each case whether
#'  or not each strategy was a Win or Lose.
#'
#' @examples
#'  `play_n_games( n = 100 )`
#'  `play_n_games( 100 )`
#'  `play_n_games( 10000 )`
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
