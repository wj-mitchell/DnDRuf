#' A complex roll resolution
#'
#'  @description The complex roll resolution function evaluates each elements of a complex
#'  roll command and compute their total result
#'
#' @param parsed_cmd a list of dice and operators as returned by parse_roll_cmd.
#'
#' @return roll result.
#'
evaluate_roll_cmd <- function(parsed_cmd){
  final_result <- NA
  
  for (i in 1:length(parsed_cmd$dice)) {
    result <- roll_one(parsed_cmd$dice[i])
    if (is.na(final_result)) {
      final_result = result
    } else {
      final_result = eval(parse(text = paste(final_result, parsed_cmd$operators[i-1], result)))
    }
  }
  
  return(final_result)
}

#' A roll command parser
#'
#' @description The roll command parser transform a complex roll command into a set of
#' individual rolls and operations.
#'
#' @param roll_cmd a string containing the roll command.
#'
#' @return a list containing dice to roll and mathematical operators.
parse_roll_cmd <- function(roll_cmd) {
  pattern = "[\\+-\\/\\*]"
  
  dices <- trimws(stringr::str_split(roll_cmd, pattern)[[1]])
  
  operators <- stringr::str_extract_all(roll_cmd, pattern)[[1]]
  
  parsed_cmd <- list(dices=dices,
                     operators=operators)
  return(parsed_cmd)
}

#' Roll One
#'
#' @description Roll one die from string command.
#'
#' @param roll a string corresponding to a roll command.
#'
#' @return result of the roll
roll_one <- function(roll){
  for (r in roll_types) { # we try rolls patterns one by one
    detected = stringr::str_detect(roll,r$pattern)
    if (detected) { # when a pattern matches
      match = stringr::str_match(roll, r$pattern)
      result = r$compute(match) # it is possible to make the roll and apply its rules
      return(result)
    }
  }
  warning("This roll command is not recognized")
}


no_dice = list(pattern = "^\\d+$",
               compute = function(match) {
                 result = match[1]
                 return(result)
               })

simple = list(pattern = "^(\\d+)[dD](\\d+)$",
              compute = function(match) {
                n = match[2]
                sides = match[3]
                rolls = sample(1:sides, n, replace = TRUE)
                message('rolls: ', paste(rolls, collapse = ', '))
                result = sum(rolls)
              })

keep_h = list(pattern = "^(\\d+)[dD](\\d+)[Hh](\\d+)$",
              compute = function(match) {
                n = match[2]
                sides = match[3]
                kept = match[4]
                rolls = sample(1:sides, n, replace = TRUE)
                message('rolls: ', paste(rolls, collapse = ', '))
                kept_dice = sort(rolls, decreasing = T)[1:as.numeric(kept)]
                message('keeping ',kept, " highest(s): ", paste(kept_dice, collapse = ', '))
                result =  sum(kept_dice)
              })

keep_l = list(pattern = "^(\\d+)[dD](\\d+)[Ll](\\d+)$",
              compute = function(match) {
                n = match[2]
                sides = match[3]
                kept = match[4]
                rolls = sample(1:sides, n, replace = TRUE)
                message('rolls: ', paste(rolls, collapse = ', '))
                kept_dice = sort(rolls)[1:as.numeric(kept)]
                message('keeping ',kept, " lowest(s): ", paste(kept_dice, collapse = ', '))
                result =  sum(kept_dice)
              })

exploding = list(pattern ="^(\\d+)[dD](\\d+)\\!$",
                 compute = function(match) {
                   n = match[2]
                   sides = match[3]
                   rolls = sample(1:sides, n, replace = TRUE)
                   explode = rolls[rolls == sides]
                   message('rolls: ', paste(rolls, collapse = ', '))
                   message("exploding ", length(explode),' dice...')
                   while (length(explode) != 0) {
                     new_rolls = sample(1:sides, length(explode), replace = TRUE)
                     message('new rolls : ', paste(new_rolls, collapse = ', '))
                     rolls = c(rolls, new_rolls)
                     explode = new_rolls[new_rolls==sides]
                     if (length(explode) != 0) { message("exploding ", length(explode),' dice...') }
                   }
                   result = sum(rolls)
                 })

reroll = list(pattern = "^(\\d+)[dD](\\d+)[rR](\\d+)$",
              compute = function(match) {
                n = match[2]
                sides = match[3]
                to_reroll = match[4]
                rolls = sample(1:sides, n, replace = TRUE)
                message('rolls: ', paste(rolls, collapse = ', '))
                reroll = rolls[rolls == to_reroll]
                message("rerolling ",length(reroll),' dice')
                while (length(reroll) != 0) {
                  new_rolls = sample(1:sides, length(reroll), replace = TRUE)
                  message('new rolls : ', paste(new_rolls, collapse = ', '))
                  rolls[rolls == to_reroll] = new_rolls
                  reroll = rolls[rolls == to_reroll]
                  if (length(reroll) != 0) { message("rerolling ",length(reroll),' dice')}
                }
                result = sum(rolls)
              })

success = list(pattern = "^(\\d+)[dD](\\d+) ?([<>]?=?) ?(\\d+)$",
               compute = function(match) {
                 n = match[2]
                 sides = match[3]
                 comparator = match[4]
                 if (comparator == "=") {comparator="=="}
                 threshold = match[5]
                 rolls = sample(1:sides, n, replace = TRUE)
                 message('rolls: ', paste(rolls, collapse = ', '))
                 success = eval(parse(text = paste("rolls[rolls",comparator,"threshold]")))
                 result = length(success)
                 message('number of success: ',
                         result ,
                         ' (', paste(sort(success,decreasing = TRUE), collapse = ', '),')')
                 return(result)
               })

roll_types = list(
  no_dice,
  simple,
  keep_h,
  keep_l,
  exploding,
  reroll,
  success,
  reroll
)

#' Roll Dice
#'
#' @description Main function to use the library. This function wraps all the computing functions of the library. let the user choose between print or hide all rolls details in the console.
#'
#' @param cmd character string describing the dice roll to compute.
#' @param roll_history boolean, if TRUE, dice rolls details are visible in the console.
#'
#' @return result of the dice roll computation
#'
#' @export
#'
#' @examples
#'
#' roll_dice("1d10 + 20")
#' roll_dice("1d4 * 2")
#' roll_dice("2d20h1")
#'
roll_dice <- function(cmd, roll_history=FALSE) {
  if (roll_history) {
    message(paste0('Evaluating "', cmd,'" \n',
                   '=========='))
  }
  
  parsed_cmd <- parse_roll_cmd(cmd)
  if (roll_history) {
    result <- evaluate_roll_cmd(parsed_cmd)
  } else {
    result <- suppressMessages(evaluate_roll_cmd(parsed_cmd))
  }
  
  if (roll_history) {
    message(paste('==========\n',
                  "Result is", result))
  }
  
  return(result)
  
}