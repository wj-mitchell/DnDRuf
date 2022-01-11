## chunkyloop.R | v1.0 - 2022.01.01

# A function that allows us to take a small number of values from an array of any 
# size and paste them into a dataframe in chunks, which may or may not be equal in size 
chunkyloop <-function(chunk, # Value noting how many chunks to iterate through. The value of chunk should be equal to the length of the arrays Quantity and Input
                      quantity, # Arrays noting the size of, or how many rows are contained within, each chunk
                      input) # The values that each chunk should take
{LoopTracker <- 0
output <- NA
for (i in 1:chunk){
  if (i == 1){
    output[1:quantity[i]] <- rep(input[i], quantity[i])
    LoopTracker <- LoopTracker + quantity[i]
  }
  if (i > 1){
    output[(LoopTracker + 1):(LoopTracker + quantity[i])] <- rep(input[i], quantity[i])
    LoopTracker <- LoopTracker + quantity[i]
  }
}
rm(LoopTracker, i)
df
return(output)
}