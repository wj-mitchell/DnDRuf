# DEV NOTES:
# Things I can't yet account for:
# Darkvision
# Resistances
# Class Abilities
# Proficiencies
# Saving Throw Bonuses
# 


# Values present represent defaults which can be modified as needed. Only value required is Groups.
FightersSim <- function(nFighters = 5,  #Value determining the number of fighters to be generated. The default value is 5.
                        Uniform = TRUE, #A logical value which specifies whether all members of a given group should have uniform stats (i.e., AC, HP, etc.). If false, stats will be randomly generated within reasonable parameters 
                        Sources = "All"
                        ATK = NA, #Value determining the attack bonus of each character.
                        DMG = NA, #Value determining the damage die of each character. Should be specified as a string and any bonus noted added with spaces on both sides of the + sign (i.e., "1d8 + 2")
                        HPmax = NA, #Value determining the HPmax of each character. 
                        AC = NA, #Value determining the AC bonus of each characters. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE)..
                        STR = NA, #Value specifying the strength scores (Range: 1-20) of each character. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE)..
                        DEX = NA, #Value specifying the dexterity scores (Range: 1-20) of each character. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE)..
                        CON = NA, #Value specifying the constitution scores (Range: 1-20) of each character. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE)..
                        INT = NA, #Value specifying the intelligence scores (Range: 1-20) of each character. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE)..
                        WIS = NA, #Value specifying the wisdom scores (Range: 1-20) of each character. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE)..
                        CHR = NA) #Value specifying the charisma scores (Range: 1-20) of each character. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE)..
{ # Creating a dataframe to house the data on all of the individual relevant to the battle
  rows <- 1:sum(nFighters)
  cols <- c("Person", "Affiliation", "Race", "Class","Subclass", "Lvl", "Background", "Speed", "ATK", "DMG", "HPmax", "AC", "STR", "DEX", "CON", "INT", "WIS", "CHR")
  df <- data.frame(matrix(NA, 
                          nrow = length(rows), 
                          ncol = length(cols), 
                          dimnames = list(rows, cols)))
  
  # Populating that dataframe with data
  # No names to start, just numbers; sequential list equivalent to the sum of nFighters
  df$Person <- rows
  if (Uniform == TRUE){
    #If races have been specified...
    if (!is.na(Race)){
      df$Race <- rep(Race, nFighters)
    }
    #If races must be simulated...    
    if (is.na(Race)){
      df$Race <- rep(sample(0:9,
                           size = 1,
                           replace = TRUE,
                           prob = seq(10,1,-1)), 
                    nFighters)
    }
    #If classes have been specified...
    #If classes must be simulated...     
    #If subclasses have been specified...
    #If subclasses must be simulated...     
    #If levels have been specified...
    #If levels must be simulated...     
    #If background has been specified...
    #If background must be simulated...     
    #If speed has been specified....
    #If speed must be simulated...     
    #If attack bonus values are provided...
    if (!is.na(ATK)){
      df$ATK <- rep(ATK, nFighters)
    }
    #If attack bonus values must be simulated...
    if (is.na(ATK)){
      df$ATK <- rep(sample(0:9,
                             size = 1,
                             replace = TRUE,
                             prob = seq(10,1,-1)), 
                    nFighters)
    }
    #If damage specifications are are provided...
    if (!is.na(DMG)){
      df$DMG <- rep(DMG, nFighters)
    }
    #If damage values must be simulated...
    if (is.na(DMG)){
      DMGdice <- c("1d4", "1d4 + 1", "1d4 + 2", "1d4 + 3", "1d4 + 4",
                   "2d4", "2d4 + 1", "2d4 + 2", "2d4 + 3", "2d4 + 4",
                   "1d6", "1d6 + 1", "1d6 + 2", "1d6 + 3", "1d6 + 4",
                   "2d6", "2d6 + 1", "2d6 + 2", "2d6 + 3", "2d6 + 4",
                   "1d8", "1d8 + 1", "1d8 + 2", "1d8 + 3", "1d8 + 4",
                   "2d8", "2d8 + 1", "2d8 + 2", "2d8 + 3", "2d8 + 4",
                   "1d10", "1d10 + 1", "1d10 + 2", "1d10 + 3", "1d10 + 4",
                   "2d10", "2d10 + 1", "2d10 + 2", "2d10 + 3", "2d10 + 4")
      DMGprob <- c(40, 32, 24, 16, 8, 
                   36, 28, 20, 12, 4,
                   39, 31, 23, 15, 7,
                   35, 27, 19, 11, 3,
                   38, 30, 22, 14, 6,
                   34, 26, 18, 10, 2,
                   37, 29, 21, 13, 5,
                   33, 25, 17, 9, 1)
      df$DMG <- rep(sample(DMGdice,
                           size = 1,
                           replace = TRUE,
                           prob = DMGprob), 
                    nFighters)
    }
    #If HPmax values are provided ....
    if (!is.na(HPmax)){
      df$HPmax <- rep(HPmax, nFighters)
    }
    #If HPmax values must be simulated ...
    if (is.na(HPmax)){
      df$HPmax <- rep(sample(seq(20,70,1),
                               size = 1,
                               replace = TRUE,
                               prob = seq(70,20,-1)), 
                      nFighters)
    }
    #If AC is specified ...
    if (!is.na(AC)){
      df$AC <- rep(AC, nFighters)
    }
    #If AC needs to be simulated ...
    if (is.na(AC)){
      df$AC <- rep(sample(seq(10,21,1),
                            size = 1,
                            replace = TRUE,
                            prob = seq(21,10,-1)), 
                     nFighters)
    }
    #If strength specifications are are provided...
    if (!is.na(STR)){
      df$STR <- rep(STR, nFighters)
    }
    #If strength values must be simulated...
    if (is.na(STR)){
      df$STR <- rep(roll_dice("4d6h3"), 
                    nFighters)
    } 
    #If dexterity specifications are are provided...
    if (!is.na(DEX)){
      df$DEX <- rep(DEX, nFighters)
    }
    #If dexterity values must be simulated...
    if (is.na(DEX)){
      df$DEX <- rep(roll_dice("4d6h3"), 
                    nFighters)
    }
    #If constitution specifications are are provided...
    if (!is.na(CON)){
      df$CON <- rep(CON, nFighters)
    }
    #If constitution values must be simulated...
    if (is.na(CON)){
      df$CON <- rep(roll_dice("4d6h3"), 
                    nFighters)
    } 
    #If intelligence specifications are are provided...
    if (!is.na(INT)){
      df$INT <- rep(INT, nFighters)
    }
    #If intelligence values must be simulated...
    if (is.na(INT)){
      df$INT <- rep(roll_dice("4d6h3"), 
                    nFighters)
    } 
    #If wisdom specifications are are provided...
    if (!is.na(WIS)){
      df$WIS <- rep(WIS, nFighters)
    }
    #If wisdom values must be simulated...
    if (is.na(WIS)){
      df$WIS <- rep(roll_dice("4d6h3"), 
                    nFighters)
    } 
    #If charisma specifications are are provided...
    if (!is.na(CHR)){
      df$CHR <- rep(CHR, nFighters)
    }
    #If charisma values must be simulated...
    if (is.na(CHR)){
      df$CHR <- rep(roll_dice("4d6h3"), 
                    nFighters)
    }     
    
  }
  if (Uniform == FALSE){
    #If attack bonus values must be simulated...
    if (is.na(ATK)){
      df$ATK <- sample(0:9,
                         size = nFighters,
                         replace = TRUE,
                         prob = seq(10,1,-1))
    }
    #If damage values must be simulated...
    if (is.na(DMG)){
      DMGdice <- c("1d4", "1d4 + 1", "1d4 + 2", "1d4 + 3", "1d4 + 4",
                   "2d4", "2d4 + 1", "2d4 + 2", "2d4 + 3", "2d4 + 4",
                   "1d6", "1d6 + 1", "1d6 + 2", "1d6 + 3", "1d6 + 4",
                   "2d6", "2d6 + 1", "2d6 + 2", "2d6 + 3", "2d6 + 4",
                   "1d8", "1d8 + 1", "1d8 + 2", "1d8 + 3", "1d8 + 4",
                   "2d8", "2d8 + 1", "2d8 + 2", "2d8 + 3", "2d8 + 4",
                   "1d10", "1d10 + 1", "1d10 + 2", "1d10 + 3", "1d10 + 4",
                   "2d10", "2d10 + 1", "2d10 + 2", "2d10 + 3", "2d10 + 4")
      DMGprob <- c(40, 32, 24, 16, 8, 
                   36, 28, 20, 12, 4,
                   39, 31, 23, 15, 7,
                   35, 27, 19, 11, 3,
                   38, 30, 22, 14, 6,
                   34, 26, 18, 10, 2,
                   37, 29, 21, 13, 5,
                   33, 25, 17, 9, 1)
      df$DMG <- sample(DMGdice,
                         size = nFighters,
                         replace = TRUE,
                         prob = DMGprob)
    }
    #If HPmax values must be simulated ...
    if (is.na(HPmax)){
      df$HPmax <- sample(seq(20,70,1),
                               size = nFighters,
                               replace = TRUE,
                               prob = seq(70,20,-1))
    }
    #If AC needs to be simulated ...
    if (is.na(AC)){
      df$AC <- sample(seq(10,21,1),
                        size = nFighters,
                        replace = TRUE,
                        prob = seq(21,10,-1))
    }
    #If strength values must be simulated...
    if (is.na(STR)){
      for (i in 1:nFighters){
        df$STR[i] <- roll_dice("4d6h3")        
      }
    } 
    #If dexterity values must be simulated...
    if (is.na(DEX)){
      for (i in 1:nFighters){
        df$DEX[i] <- roll_dice("4d6h3")        
      }
    }
    #If constitution values must be simulated...
    if (is.na(CON)){
      for (i in 1:nFighters){
        df$CON[i] <- roll_dice("4d6h3")        
      }
    }
    #If intelligence values must be simulated...
    if (is.na(INT)){
      for (i in 1:nFighters){
        df$INT[i] <- roll_dice("4d6h3")        
      }
    }
    #If wisdom values must be simulated...
    if (is.na(WIS)){
      for (i in 1:nFighters){
        df$WIS[i] <- roll_dice("4d6h3")        
      }
    }
    #If charisma values must be simulated...
    if (is.na(CHR)){
      for (i in 1:nFighters){
        df$CHR[i] <- roll_dice("4d6h3")        
      }
    }      
  }
  # Returning the final summary dataframe. 
  return(df)
}

# Example
df <- FightersSim(Groups = c("Good", "Bad"),  #Vector consisting of the names of the conflicting groups
                  nFighters = c(7,18),  #Vector consisting of the number of people in each group, respectively
                  Uniform = FALSE)