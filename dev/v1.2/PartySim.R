## PartySim.R | v1.1 - 2022.01.01 

# Values present represent defaults which can be modified as needed. Only value required is Groups.
PartySim <- function(nGroups = 1, # Value determining the number of groups that should be generated.
                     GroupName = LETTERS[1:nGroups], #Vector consisting of the names of the conflicting groups
                     nPeople = rep(6,nGroups),  #Vector consisting of the number of people in each group, respectively
                     Uniform = TRUE, #A logical value which specifies whether all members of a given group should have uniform stats (i.e., AC, HP, etc.). If false, stats will be randomly generated within reasonable parameters
                     Class = NA, # Vector of values of class character determining the classes of characters in each group.
                     LVL = NA, # Vector of numeric values between 1 and 20 that determine the level of the characters generated.
                     LVL_Low = rep(3 ,nGroups), # Vector of numeric values determining the lower range of possible simulated levels
                     LVL_Up = rep(20 ,nGroups), # Vector of numeric values determining the upper range of possible simulated levels
                     ATK = NA, #Vector consisting of the attack bonus for each group, respectively
                     DMG = NA, #Vector consisting of the damage die for each group, respectively. Should be specified as a string and any bonus noted added with spaces on both sides of the + sign (i.e., "1d8 + 2")
                     HPmax = NA, #Vector consisting of the HPmax for each group, respectively. It is assumed each group starts the battle at full health.
                     HPmax_Low = rep(20 ,nGroups), #Vector consisting of the lower range values for HPmax for each group.
                     HPmax_Up = rep(345,nGroups), #Vector consisting of the upper range values for HPmax for each group.
                     AC = NA, #Vector consisting of the AC bonus of each group, respectively.
                     AC_Low = rep(11 ,nGroups), #Vector consisting of the AC bonus of each group, respectively.
                     AC_Up = rep(26 ,nGroups), #Vector consisting of the AC bonus of each group, respectively.
                     STR = NA, #Vector specifying the strength scores (Range: 1-20) of characters in each group, respectively. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE).
                     DEX = NA, #Vector specifying the dexterity scores (Range: 1-20) of characters in each group, respectively. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE).
                     CON = NA, #Vector specifying the constitution scores (Range: 1-20) of characters in each group, respectively. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE).
                     INT = NA, #Vector specifying the intelligence scores (Range: 1-20) of characters in each group, respectively. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE).
                     WIS = NA, #Vector specifying the wisdom scores (Range: 1-20) of characters in each group, respectively. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE).
                     CHR = NA, #Vector specifying the charisma scores (Range: 1-20) of characters in each group, respectively. If left blank, this value with be randomly generated uniformly across all fighters (Uniform = TRUE) or on an individual basis (Uniform = FALSE).
                     nHealStd = NA, # Vector specifying the number of standard healing potions the characters in each group should have, respectively, in their inventory.
                     nHealGtr = NA, # Vector specifying the number of greater healing potions the characters in each group should have, respectively, in their inventory.
                     nHealSuper = NA, # Vector specifying the number of superior healing potions the characters in each group should have, respectively, in their inventory.
                     nHealSuprm = NA, # Vector specifying the number of supreme healing potions the characters in each group should have, respectively, in their inventorr.
                     Seed = NA, #Seed value, which if entered, allows the user to reliably generate the same simulated characters repeatedly.
                     Gender = c(.800,.196,.004)) #Vector denoting the proportion of males, females, and nonbinary fighters, repectively

{ options(warn=-1)
  
  #Dependent Functions

  # For general use ...
  source('SupportFunctions/rollr.R')
  source('CivSim.R')
  source('SupportFunctions/chunkyloop.R')
  source('SupportFunctions/again.R')
  
  # Misspecifications List----
  
  # Checking for consistency in the length of vectors
  vectors <- c('GroupName', 'nPeople', 'Class', 'LVL', 'LVL_Low', 'LVL_Up', 'ATK', 
               'DMG', 'HPmax', 'HPmax_Low', 'HPmax_Up', 'AC', 'AC_Low', 'AC_Up', 
               'STR', 'DEX', 'CON', 'INT', 'WIS', 'CHR', 'nHealStd', 'nHealGtr', 
               'nHealSuper', 'nHealSuprm')
  for (i in vectors){
    if (!is.na(get(i))){
      if (length(get(i)) != nGroups)
        stop(paste0(i, " [", get(i), "] must be either be of value 'NA' or a vector whose length is equal to 
                    the value of nGroups [", nGroups,  "]"),
             call. = F)
      if (i != 'GroupName' | i != 'Class' | i != 'DMG'){
        if (get(i) < 0){
          stop(paste0("The values contained in vector ", i, " must be 0 or greater."),
               call. = F)
        }
      }
    }
  }
  
  # I create a list of variables that I want to compare ...
  ranges <- list(lows = c('LVL_Low', 'HPmax_Low', 'AC_Low'),
                 ups = c('LVL_Up', 'HPmax_Up', 'AC_Up'))
  
  # Then I run a forloop to check through each of them
  for (i in 1:length(ranges$lows)){
    for (j in 1:nGroups){
      if (!is.na(get(ranges$lows[i])[j]) & !is.na(get(ranges$ups[i])[j])){
        if (get(ranges$lows[i])[j] >= get(ranges$ups[i])[j]){
          stop(paste0(ranges$lows[i], " must be a value greater than or equal to 0 and less than its corresponding value in ", ranges$ups[i]),
               call. = F)
          }
        }
      }
    }

  # No Errors Detected ----
  
  # Setting a seed to reliable generate the same values in the future...
  if (!is.na(Seed)){
    set.seed(Seed)
  }
  
  # Creating a dataframe to house the data on all of the individual relevant to the battle
  rows <- 1:sum(nPeople)
  cols <- c("Name", "Group")
  df <- data.frame(matrix(NA, 
                          nrow = length(rows), 
                          ncol = length(cols), 
                          dimnames = list(rows, cols)))
  
  df_civ <- CivSim(nPeople = length(rows),
                   pMale = Gender[1],
                   pFemale = Gender[2],
                   pNonbinary = Gender[3])
  
  # Populating that dataframe with data
  df$Name <- paste(df_civ$First, df_civ$Surname, sep = " ")
  rm(df_civ)
  
  # This function should be essential for us to generate fighters regardless of how many groups or how many members in each group by letting us write data in chunks
  df$Group <- chunkyloop(chunk = nGroups,
                          quantity = nPeople,
                          input = GroupName)

  # If you want all members of the same group to have the same values . . .
  if (Uniform == TRUE){
    
    # If values are provided...
    # I create an list of columns to output to and variables from which to pull inputs from ...
    vars <- c('ATK', 'DMG', 'HPmax', 'AC', 'STR', 'DEX', 'CON', 'INT',
               'WIS', 'CHR', 'nHealStd', 'nHealGtr', 'nHealSuper', 'nHealSuprm')

    # Then I run the chunkyloop function to keep this all compacted
    for (i in 1:length(vars)){
      if (!is.na(vars[i])){
        df[, vars[i]] <- chunkyloop(chunk = nGroups,
                                    quantity = nPeople,
                                    input = get(vars[i]))
      }
    }
    
    #If attack bonus values must be simulated...
    if (is.na(ATK)){
      df$ATK <- chunkyloop(chunk = nGroups,
                           quantity = nPeople,
                           input = sample(x = 0:15,
                                          size = nGroups, 
                                          replace = T,
                                          prob = exp(0.125 * 15:0)))
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
      df$DMG <- chunkyloop(chunk = nGroups,
                            quantity = nPeople,
                            input = sample(x = DMGdice,
                                           size = nGroups, 
                                           replace = T,
                                           prob = DMGprob))
    }

    #If HPmax values must be simulated ...
    LoopTracker <- 0
    output <- NA
      for (i in 1:nGroups){
        if (i == 1){
          output[1:nPeople[i]] <- rep(sample(x = HPmax_Low[i]:HPmax_Up[i],
                                             size = 1,
                                             replace = T,
                                             prob = exp(HPmax_Low[i]:HPmax_Up[i] * 0.0125)),
                                      nPeople[i])
          LoopTracker <- LoopTracker + nPeople[i]
        }
        if (i > 1){
          output[(LoopTracker + 1):(LoopTracker + nPeople[i])] <- rep(sample(x = HPmax_Low[i]:HPmax_Up[i],
                                                                             size = 1,
                                                                             replace = T,
                                                                             prob = exp(HPmax_Low[i]:HPmax_Up[i] * 0.0125)), 
                                                                      nPeople[i])
          LoopTracker <- LoopTracker + nPeople[i]
        }
      }
    rm(LoopTracker, i)
    df$HPmax <- output

    #If AC needs to be simulated ...
    LoopTracker <- 0
    output <- NA
    for (i in 1:nGroups){
      if (i == 1){
        output[1:nPeople[i]] <- rep(sample(x = AC_Low[i]:AC_Up[i],
                                           size = 1,
                                           replace = T,
                                           prob = exp(AC_Low[i]:AC_Up[i] * 0.175)),
                                    nPeople[i])
        LoopTracker <- LoopTracker + nPeople[i]
      }
      if (i > 1){
        output[(LoopTracker + 1):(LoopTracker + nPeople[i])] <- rep(sample(x = AC_Low[i]:AC_Up[i],
                                                                           size = 1,
                                                                           replace = T,
                                                                           prob = exp(AC_Low[i]:AC_Up[i] * 0.175)), 
                                                                    nPeople[i])
        LoopTracker <- LoopTracker + nPeople[i]
      }
    }
    rm(LoopTracker, i)
    df$AC <- output

    #If strength values must be simulated...
    if (is.na(STR)){
      df$STR <- chunkyloop(chunk = nGroups,
                           quantity = nPeople,
                           input = again(func = roll_dice("4d6h3"),
                                         nrep = nGroups))
    } 
    
    #If dexterity values must be simulated...
    if (is.na(DEX)){
      df$DEX <- chunkyloop(chunk = nGroups,
                           quantity = nPeople,
                           input = again(func = roll_dice("4d6h3"),                                               
                                         nrep = nGroups))
    }

    #If constitution values must be simulated...
    if (is.na(CON)){
      df$CON <- chunkyloop(chunk = nGroups,
                            quantity = nPeople,
                            input = again(func = roll_dice("4d6h3"),                                               
                                          nrep = nGroups))
    } 

    #If intelligence values must be simulated...
    if (is.na(INT)){
      df$INT <- chunkyloop(chunk = nGroups,
                            quantity = nPeople,
                            input = again(func = roll_dice("4d6h3"),                                               
                                          nrep = nGroups))
    } 

    #If wisdom values must be simulated...
    if (is.na(WIS)){
      df$WIS <- chunkyloop(chunk = nGroups,
                            quantity = nPeople,
                            input = again(func = roll_dice("4d6h3"),                                               
                                          nrep = nGroups))
    } 

    #If charisma values must be simulated...
    if (is.na(CHR)){
      df$CHR <- chunkyloop(chunk = nGroups,
                            quantity = nPeople,
                            input = again(func = roll_dice("4d6h3"),                                               
                                          nrep = nGroups))
    }   

    #If healing potions must be simulated...
    if (is.na(nHealStd)){
      df$nHealStd <- chunkyloop(chunk = nGroups,
                           quantity = nPeople,
                           input = sample(x = 0:20,
                                          size = nGroups, 
                                          replace = T,
                                          prob = exp((1 * 0.4) * (20:0))))
    }

    #If greater healing potions must be simulated...
    if (is.na(nHealGtr)){
      df$nHealGtr <- chunkyloop(chunk = nGroups,
                                 quantity = nPeople,
                                 input = sample(x = 0:20,
                                                size = nGroups, 
                                                replace = T,
                                                prob = exp((2 * 0.4) * (20:0))))
    }

    #If superior healing potions must be simulated...
    if (is.na(nHealSuper)){
      df$nHealSuper <- chunkyloop(chunk = nGroups,
                                 quantity = nPeople,
                                 input = sample(x = 0:20,
                                                size = nGroups, 
                                                replace = T,
                                                prob = exp((3 * 0.4) * (20:0))))
    }

    #If supreme healing potions must be simulated...
    if (is.na(nHealSuprm)){
      df$nHealSuprm <- chunkyloop(chunk = nGroups,
                                 quantity = nPeople,
                                 input = sample(x = 0:20,
                                                size = nGroups, 
                                                replace = T,
                                                prob = exp((4 * 0.4) * (20:0))))
    }
  }
  
  # If you want all members of the same group to have random values . . . 
  if (Uniform == FALSE){
    #If attack bonus values must be simulated...
    if (is.na(ATK)){
      df$ATK <- sample(x = 0:15,
                       size = sum(nPeople),
                       replace = T,
                       prob = exp(0.125 * 15:0))
    }
    #If damage values must be simulated...
    if (is.na(DMG)){
      df$DMG <- sample(x = DMGdice,
                       size = sum(nPeople),
                       replace = TRUE,
                       prob = DMGprob)
    }
    #If HPmax values must be simulated ...
    if (is.na(HPmax)){
      df$HPmax <- sample(x = HPmax_Low[1]:HPmax_Up[1],
                         size = sum(nPeople),
                         prob = exp(HPmax_Low[1]:HPmax_Up[1] * 0.0125))
    }
    #If AC needs to be simulated ...
    if (is.na(AC)){
      df$AC <- sample(x = AC_Low[1]:AC_Up1[1],
                      size = sum(nPeople),
                      prob = exp(AC_Low[1]:AC_Up[1] * 0.175))
    }
    #If strength values must be simulated...
    if (is.na(STR)){
      for (i in 1:sum(nPeople)){
        df$STR[i] <- roll_dice("4d6h3")        
      }
    } 
    #If dexterity values must be simulated...
    if (is.na(DEX)){
      for (i in 1:sum(nPeople)){
        df$DEX[i] <- roll_dice("4d6h3")        
      }
    }
    #If constitution values must be simulated...
    if (is.na(CON)){
      for (i in 1:sum(nPeople)){
        df$CON[i] <- roll_dice("4d6h3")        
      }
    }
    #If intelligence values must be simulated...
    if (is.na(INT)){
      for (i in 1:sum(nPeople)){
        df$INT[i] <- roll_dice("4d6h3")        
      }
    }
    #If wisdom values must be simulated...
    if (is.na(WIS)){
      for (i in 1:sum(nPeople)){
        df$WIS[i] <- roll_dice("4d6h3")        
      }
    }
    #If charisma values must be simulated...
    if (is.na(CHR)){
      for (i in 1:sum(nPeople)){
        df$CHR[i] <- roll_dice("4d6h3")        
      }
    }    
    #If healing potions must be simulated...
    if (is.na(nHealStd)){
      df$nHealStd <- sample(x = 0:20, 
                            size = sum(nPeople), 
                            prob = exp((1 * 0.4) * (20:0)))
    }
    #If greater healing potions must be simulated...
    if (is.na(nHealGtr)){
      df$nHealGtr <- sample(x = 0:20, 
                            size = sum(nPeople), 
                            prob = exp((2 * 0.4) * (20:0)))
    }
    #If superior healing potions must be simulated...
    if (is.na(nHealSuper)){
      df$nHealSuper <- sample(x = 0:20, 
                            size = sum(nPeople), 
                            prob = exp((3 * 0.4) * (20:0)))
    }
    #If greater healing potions must be simulated...
    if (is.na(nHealSuprm)){
      df$nHealSuprm <- sample(x = 0:20, 
                            size = sum(nPeople), 
                            prob = exp((4 * 0.4) * (20:0)))
    }
  }
  
  # Calculating a Strength Modifier
  if (df$STR < 10){
    df$STR_mod <- ceiling((df$STR - 10)/2)
  }
  if (df$STR >= 10){
    df$STR_mod <- floor((df$STR - 10)/2)
  }
    # Calculating a Dexterity Modifier
  if (df$DEX < 10){
    df$DEX_mod <- ceiling((df$DEX - 10)/2)
  }
  if (df$DEX >= 10){
    df$DEX_mod <- floor((df$DEX - 10)/2)
  }

  # Calculating a Constitution Modifier
  if (df$CON < 10){
    df$CON_mod <- ceiling((df$CON - 10)/2)
  }
  if (df$CON >= 10){
    df$CON_mod <- floor((df$CON - 10)/2)
  }

  # Calculating a Intelligence Modifier
  if (df$INT < 10){
    df$INT_mod <- ceiling((df$INT - 10)/2)
  }
  if (df$INT >= 10){
    df$INT_mod <- floor((df$INT - 10)/2)
  }

  # Calculating a Wisdom Modifier
  if (df$WIS < 10){
    df$WIS_mod <- ceiling((df$WIS - 10)/2)
  }
  if (df$WIS >= 10){
    df$WIS_mod <- floor((df$WIS - 10)/2)
  }

  # Calculating a Charisma Modifier
  if (df$CHR < 10){
    df$CHR_mod <- ceiling((df$CHR - 10)/2)
  }
  if (df$CHR >= 10){
    df$CHR_mod <- floor((df$CHR - 10)/2)
  }
  
  # Returning the final summary dataframe. 
  return(df)
}