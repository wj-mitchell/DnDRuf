## InitiatSim.R v1.1 - 2022.01.05

InitiatSim <- function(df, # A dataframe containing the list of participants
                       DEX_mod = df$DEX_mod) # A list of the dexterity scores respective to each participant; usually takes the form of a dataframe column
{ # Dependent Functions
  # source('C:/Users/Administrator/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/DnDRuf/dev/v1.1/SupportFunctions/rollr.R')
  # source('C:/Users/tui81100/Dropbox/My PC (UncleSplashysSaddnessEmporium)/Desktop/Scripts/DnDRuf/dev/v1.1/SupportFunctions/rollr.R')
  source('SupportFunctions/rollr.R')
  
  # Rolling dice for initiative
  for (i in 1:length(rownames(df))){
    df$InitativeRoll[i] <- roll_dice("1d20") + DEX_mod[i]
  }
  
  # Creating an Initiative variable
  df$InitiativeOrder <- 0

  # Assigning initiative based upon rolls and deciding ties based opon Dex scores
  for (j in sort(unique(df$InitativeRoll), decreasing =T)){ 
    for (k in sort(unique(DEX_mod), decreasing =T)){
      for (l in 1:length(rownames(df))){
        if (df$InitativeRoll[l] == j){
          if (DEX_mod[l] == k){
            df$InitiativeOrder[l] <- max(df$InitiativeOrder) + 1
          }
        }
      }
    }
  }
  
  return(df)
}