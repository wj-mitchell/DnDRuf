# You may or may not need these packages in order to run this function
# install.packages("remotes")
# remotes::install_github("felixmil/rollr")

# Values present represent defaults which can be modified as needed. Only value required is Groups.
CombatSim <- function(df, #Dataframe generated by FighterSim or mirroring its column structure.
                      Ending = c("Death", "Timed"), #Logical statement that will dictate how many rounds the simulation should run. "Death" will run until all members of a group have been terminated. "Timed" will run for a number of rounds you specify in the "nRounds" command.
                      nRounds = NA, #The number of rounds that the simulation should run if the ending is "Timed".
                      PlayByPlay = FALSE) #Logical Command that will include output of what happens on each turn.  
{
  # Dependent packages    
  pacman::p_load(rollr)
  
  # Simulating the battle round by round with a for loop
  Groups <- unique(df$Group)
  nPeople <- c(length(unique(df$Person[df$Group == Groups[1]])),
               length(unique(df$Person[df$Group == Groups[2]])))
  rows <- df$Person
  ATK <- NA
  DMG <- NA
  Summary <- NA
  if (Ending == "Death"){
    Round <- 0
    # During each round of battle (however many it may take)...
    while (sum(df$LifeStatus == "Inactive" & df$Group == Groups[1]) != nPeople[1] &
           sum(df$LifeStatus == "Inactive" & df$Group == Groups[2]) != nPeople[2]){
      Round <- Round + 1
      # ... For each initiative position ....
      for (j in sort(unique(df$Initiative))){
        # ... For each battle participant ....
        for (k in rows){
          # ... skip over participants that don't have priority initiative.
          if (df$Initiative[k] == j){
            # ... and check whether they are currently alive. 
            if (df$LifeStatus[k] == "Active"){
              # if there opposing party is dead, cease this turn.
              if ((sum(df$LifeStatus == "Inactive" & df$Group == Groups[1]) == nPeople[1] |
                   sum(df$LifeStatus == "Inactive" & df$Group == Groups[2]) == nPeople[2])){
                break}
              # if there opposing party is alive, continue this turn.
              if ((sum(df$LifeStatus == "Inactive" & df$Group == Groups[1]) != nPeople[1] |
                   sum(df$LifeStatus == "Inactive" & df$Group == Groups[2]) != nPeople[2])){
                # If someone has not engaged, randomly choose someone to attack.            
                if (is.na(df$EngagedWith[k])){
                  df$EngagedWith[k] <- sample(df$Person[(df$Group != df$Group[k]) & df$LifeStatus == "Active"], 1)
                  Action <- paste0(df$Person[k], " has engaged with ", df$EngagedWith[k], ".")
                  Summary <- c(Summary, Action)
                }        
                # If the person they were previously engaged with died, randomly choose someone else to attack.            
                if (!is.na(df$EngagedWith[k]) & df$LifeStatus[df$EngagedWith[k]] == "Inactive"){
                  df$EngagedWith[k] <- sample(df$Person[(df$Group != df$Group[k]) & df$LifeStatus == "Active"], 1)
                  Action <- paste0(df$Person[k], " has engaged with ", df$EngagedWith[k], ".")
                  Summary <- c(Summary, Action)
                }
                # If someone is engaged with someone, let them try to attack..            
                if (!is.na(df$EngagedWith[k]) & df$LifeStatus[df$EngagedWith[k]] == "Active"){
                  ATK <- roll_dice("1d20")
                  # ... if they roll a Natural 1, they'll inflict damage on themselves
                  if (ATK == 1){
                    DMG <- roll_dice(df$DMG[k])
                    df$HP[df$Person[k]] <- df$HP[df$Person[k]] - DMG
                    Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but hurt themselves in the process and took ", DMG, " points of damage.")
                    Summary <- c(Summary, Action)
                    # ... and if their HP drops below 0 as a result, they will die.                
                    if (df$HP[df$Person[k]] <= 0){
                      df$HP[df$Person[k]] <- 0
                      df$LifeStatus[df$Person[k]] <- "Inactive"
                      df$RoundKilled[df$Person[k]] <- Round
                      Action <- paste0(df$Person[k], " has accidentally committed suicide!")
                      Summary <- c(Summary, Action)
                    }
                  }
                  # ... if they roll a Natural 20, they'll inflict critical damage
                  if (ATK == 20){
                    DMG <- (roll_dice(df$DMG[k]) * 2)
                    df$HP[df$EngagedWith[k]] <- df$HP[df$EngagedWith[k]] - DMG
                    Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k]," and done critical damage, at ", DMG, " points total.")
                    Summary <- c(Summary, Action)
                    # ... and if their target's HP drops below 0 as a result, they will die.  
                    if (df$HP[df$EngagedWith[k]] <= 0){
                      df$HP[df$EngagedWith[k]] <- 0
                      df$LifeStatus[df$EngagedWith[k]] <- "Inactive"
                      df$RoundKilled[df$EngagedWith[k]] <- Round
                      Action <- paste0(df$EngagedWith[k], " has been annhiliated!")
                      Summary <- c(Summary, Action)
                    }
                  }
                  # ... and if their attack roll is somewhere in the middle...
                  if (ATK > 1 & ATK < 20){
                    # ... add their attack modifier
                    ATK <- ATK + df$ATK[k]
                    # ... if the attack roll is greater than their target's AC, roll damage.
                    if (ATK > df$AC[df$EngagedWith[k]]){
                      DMG <- roll_dice(df$DMG[k])
                      df$HP[df$EngagedWith[k]] <- df$HP[df$EngagedWith[k]] - DMG
                      Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k]," and done ", DMG, " points of damage.")
                      Summary <- c(Summary, Action)
                      # ... and if their target's HP drops below 0 as a result, they will die. 
                      if (df$HP[df$EngagedWith[k]] <= 0){
                        df$HP[df$EngagedWith[k]] <- 0
                        df$LifeStatus[df$EngagedWith[k]] <- "Inactive"
                        df$RoundKilled[df$EngagedWith[k]] <- Round
                        Action <- paste0(df$EngagedWith[k], " has been subdued!")
                        Summary <- c(Summary, Action)
                      }
                    }
                    # ... if the attack roll is less than or equal to their target's AC, the attack misses.
                    if (ATK < df$AC[df$EngagedWith[k]]){
                      Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but missed.")
                      Summary <- c(Summary, Action)
                    }
                    if (ATK == df$AC[df$EngagedWith[k]]){
                      Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but ", df$EngagedWith[k], " just barely escaped without damage.")
                      Summary <- c(Summary, Action)
                    }              
                  }
                }
              }
            }
          }
        }
      }
    }
    # Removing the NA value we needed to start the Summary variable
    Summary <- Summary[-1]
    # Displaying the Play By Play if it was requested
    if (PlayByPlay == TRUE){
      print(Summary)
    }
    # Displaying the final summary of the battle.
    for (i in 1:length(Groups)){
      if (sum(df$LifeStatus == "Inactive" & df$Group == Groups[i]) == nPeople[i]){ 
        (Action <-print(paste("Battle ended! In a fight to the death, the", Groups[i], "have fallen. After", Round, "rounds,", 
                              sum(df$LifeStatus == "Active" & df$Group == Groups[length(Groups)+1-i]), Groups[length(Groups)+1-i], "remain!", sep= " ")))
        Summary <- c(Summary, Action)
      }
    }
  }
  if (Ending == "Timed"){
    # During each round of battle (however many it may take)...
    for (i in 1:nRounds){
      # ... For each initiative position ....
      for (j in sort(unique(df$Initiative))){
        # ... For each battle participant ....
        for (k in rows){
          # ... skip over participants that don't have priority initiative.
          if (df$Initiative[k] == j){
            # ... and check whether they are currently alive. 
            if (df$LifeStatus[k] == "Active"){
              # if there opposing party is dead, cease this turn.
              if ((sum(df$LifeStatus == "Inactive" & df$Group == Groups[1]) == nPeople[1] |
                   sum(df$LifeStatus == "Inactive" & df$Group == Groups[2]) == nPeople[2])){
                break}
              # if there opposing party is alive, continue this turn.
              if ((sum(df$LifeStatus == "Inactive" & df$Group == Groups[1]) != nPeople[1] |
                   sum(df$LifeStatus == "Inactive" & df$Group == Groups[2]) != nPeople[2])){
                # If someone has not engaged, randomly choose someone to attack.            
                if (is.na(df$EngagedWith[k])){
                  df$EngagedWith[k] <- sample(df$Person[(df$Group != df$Group[k]) & df$LifeStatus == "Active"], 1)
                  Action <- paste0(df$Person[k], " has engaged with ", df$EngagedWith[k], ".")
                  Summary <- c(Summary, Action)
                }        
                # If the person they were previously engaged with died, randomly choose someone else to attack.            
                if (!is.na(df$EngagedWith[k]) & df$LifeStatus[df$EngagedWith[k]] == "Inactive"){
                  df$EngagedWith[k] <- sample(df$Person[(df$Group != df$Group[k]) & df$LifeStatus == "Active"], 1)
                  Action <- paste0(df$Person[k], " has engaged with ", df$EngagedWith[k], ".")
                  Summary <- c(Summary, Action)
                }
                # If someone is engaged with someone, let them try to attack..            
                if (!is.na(df$EngagedWith[k]) & df$LifeStatus[df$EngagedWith[k]] == "Active"){
                  ATK <- roll_dice("1d20")
                  # ... if they roll a Natural 1, they'll inflict damage on themselves
                  if (ATK == 1){
                    DMG <- roll_dice(df$DMG[k])
                    df$HP[df$Person[k]] <- df$HP[df$Person[k]] - DMG
                    Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but hurt themselves in the process and took ", DMG, " points of damage.")
                    Summary <- c(Summary, Action)
                    # ... and if their HP drops below 0 as a result, they will die.                
                    if (df$HP[df$Person[k]] <= 0){
                      df$HP[df$Person[k]] <- 0
                      df$LifeStatus[df$Person[k]] <- "Inactive"
                      df$RoundKilled[df$Person[k]] <-i
                      Action <- paste0(df$Person[k], " has accidentally committed suicide!")
                      Summary <- c(Summary, Action)
                    }
                  }
                  # ... if they roll a Natural 20, they'll inflict critical damage
                  if (ATK == 20){
                    DMG <- (roll_dice(df$DMG[k]) * 2)
                    df$HP[df$EngagedWith[k]] <- df$HP[df$EngagedWith[k]] - DMG
                    Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k]," and done critical damage, at ", DMG, " points total.")
                    Summary <- c(Summary, Action)
                    # ... and if their target's HP drops below 0 as a result, they will die.  
                    if (df$HP[df$EngagedWith[k]] <= 0){
                      df$HP[df$EngagedWith[k]] <- 0
                      df$LifeStatus[df$EngagedWith[k]] <- "Inactive"
                      df$RoundKilled[df$EngagedWith[k]] <-i
                      Action <- paste0(df$EngagedWith[k], " has been annhiliated!")
                      Summary <- c(Summary, Action)
                    }
                  }
                  # ... and if their attack roll is somewhere in the middle...
                  if (ATK > 1 & ATK < 20){
                    # ... add their attack modifier
                    ATK <- ATK + df$ATK[k]
                    # ... if the attack roll is greater than their target's AC, roll damage.
                    if (ATK > df$AC[df$EngagedWith[k]]){
                      DMG <- roll_dice(df$DMG[k])
                      df$HP[df$EngagedWith[k]] <- df$HP[df$EngagedWith[k]] - DMG
                      Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k]," and done ", DMG, " points of damage.")
                      Summary <- c(Summary, Action)
                      # ... and if their target's HP drops below 0 as a result, they will die. 
                      if (df$HP[df$EngagedWith[k]] <= 0){
                        df$HP[df$EngagedWith[k]] <- 0
                        df$LifeStatus[df$EngagedWith[k]] <- "Inactive"
                        df$RoundKilled[df$EngagedWith[k]] <-i
                        Action <- paste0(df$EngagedWith[k], " has been subdued!")
                        Summary <- c(Summary, Action)
                      }
                    }
                    # ... if the attack roll is less than or equal to their target's AC, the attack misses.
                    if (ATK < df$AC[df$EngagedWith[k]]){
                      Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but missed.")
                      Summary <- c(Summary, Action)
                    }
                    if (ATK == df$AC[df$EngagedWith[k]]){
                      Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but ", df$EngagedWith[k], " just barely escaped without damage.")
                      Summary <- c(Summary, Action)
                    }              
                  }
                }
              }
            }
          }
        }
      }
    }
    # Removing the NA value we needed to start the Summary variable
    Summary <- Summary[-1]
    # Displaying the Play By Play if it was requested
    if (PlayByPlay == TRUE){
      print(Summary)
    }
    # Displaying the final summary of the battle.
      (Action <-print(paste(nRounds, "round(s) have passed! The", Groups[length(Groups)], "have", 
                            sum(df$LifeStatus == "Active" & df$Group == Groups[length(Groups)]), "fighters left alive, while", 
                            sum(df$LifeStatus == "Active" & df$Group == Groups[length(Groups)-1]), Groups[length(Groups)-1], "remain!", sep= " ")))
      Summary <- c(Summary, Action)
  }
  # Returning the final summary dataframe. 
  return(df)
}

# Example
df.Round1 <- CombatSim(df = df,
                       Ending = ("Timed"),
                       nRounds = 1,
                       PlayByPlay = TRUE)
df.Round6 <- CombatSim(df = df.Round1,
                       Ending = ("Timed"),
                       nRounds = 5,
                       PlayByPlay = TRUE)
df.Death <- CombatSim(df = df.Round1,
                       Ending = ("Death"),
                       PlayByPlay = FALSE)
