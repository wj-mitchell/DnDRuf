## InitiatSim.R v1.1 - 2022.01.30

InitiatSim <- function(df, # A dataframe or an array containing the list of participants
                       DEX = NA, # OPTIONAL: An array of Dexterity scores respective to each character; can be a dataframe column; if neither DEX nor DEX_mods are provided, DEX_mod will be simulated.  
                       DEX_mod = NA, # OPTIONAL: An array of the dexterity scores respective to each participant; can be a dataframe column; if neither DEX nor DEX_mods are provided, DEX_mod will be simulated. 
                       Seed = sample(0:99999, size = 1)) #Seed value, which if entered, allows the user to reliably generate the same simulated initative repeatedly.
  
{ # Dependent Functions
  # source('C:/Users/Administrator/Desktop/Scripts/DnDRuf/dev/v1.1/SupportFunctions/rollr.R', local = TRUE)
  source('SupportFunctions/rollr.R', local = TRUE)
  
  # Errors ----
  
  ## Source is not formatted correctly ----
  if (!is.data.frame(df)){
    df <- as.data.frame(df)
    if (!is.data.frame(df)){
      stop(paste0("The object [df] you've specified is not formatted as a dataframe and could not be coerced to a dataframe format. Please reformat the object and run InitiatSim again."),
           call. = F)
    }
  }
  
  ## Length of DEX is not compatible with df ----
  if (!is.na(DEX)){
    if (length(DEX) != length(rownames(df))){
      stop(paste0("There is a discrepency between the length of your DEX variable and the number of characters you indicated in your dataframe. You likely either have too many or too few DEX scores. Please reconcile this error."),
           call. = F)
    }
  }
  
  ## Length of DEX_mod is not compatible with df ----
  if (!is.na(DEX_mod)){
    if (length(DEX_mod) != length(rownames(df))){
      stop(paste0("There is a discrepency between the length of your DEX mod variable and the number of characters you indicated in your dataframe. You likely either have too many or too few DEX mod scores. Please reconcile this error."),
           call. = F)
    }
  }

  ## DEX is not numeric ----
  if (!is.na(DEX)){
    if (any(!is.numeric(DEX))){
      stop(paste0("Your DEX variable does not seem to contain numeric values. Please correct this issue and rerun InitiatSim again."),
           call. = F)
    }
  }
    
  ## DEX_mod is not numeric ----
  if (!is.na(DEX_mod)){
    if (any(!is.numeric(DEX_mod))){
      stop(paste0("Your DEX mod variable does not seem to contain numeric values. Please correct this issue and rerun InitiatSim again."),
            call. = F)
    }
  }

  ## Seed Input is Either Non-Numeric or Negative
  if (!is.na(Seed)){
    if (!is.numeric(Seed) | Seed < 0){
      stop(paste0("Seed must take a numeric value of 0 or greater. You have entered: ", 
                  Seed),
           call. = F)
    }
  }
  
  # Function Start -----
  
  # Seeding Random Generators -----
  if (!is.na(Seed)){
    # Setting the seed the allows us to regenerate the same simulations again and again
    set.seed(Seed)
    # Printing tha value so that the user has it, should they want it in the future.
    print(paste0("InitiatSim Seed: ", Seed))
  }
  
  if (is.na(DEX)){
    for (i in 1:length(rownames(df))){
      df$DEX[i] <- roll_dice("4d6h3")        
    }
  }

  if (is.na(DEX_mod)){
    # Calculating a Dexterity Modifier
    if (df$DEX < 10){
      df$DEX_mod <- ceiling((df$DEX - 10)/2)
    }
    if (df$DEX >= 10){
      df$DEX_mod <- ceiling((df$DEX - 10)/2)
    }
    DEX_mod <- df$DEX_mod
  }  

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