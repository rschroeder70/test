# Identify which parents rolls are part of the selection for each 
# N = min:max rolls to pick from.  N_min = the number of grade/CalDW 
# combinations


# Create an index to simplify the looping over each grade/CalDW combination.
# Use a windowing function.
numOptions <- mutate(numOptions, prod_index = 
                       dense_rank(paste(Grade, CalDW))) %>%
  arrange(Grade, CalDW, Sol.Num.Parents) %>% ungroup()

# Build a data frame of the parent grade & CalDWs
prodList <- numOptions %>% group_by(Grade, CalDW) %>% 
  summarize(numSols = n(), minNumParents = min(Sol.Num.Parents), 
            maxNumParents = max(Sol.Num.Parents))
numProd <- nrow(prodList)

print(paste("Selecting the optimal set of parent rolls from", numProd,
            "Grade/CalDWs and", nrow(numOptions), "Options"),
      quote = FALSE)

nRollMinParents <- sum(prodList$minNumParents) # Minimum number of parent rolls
nRollMaxParents <- sum(prodList$maxNumParents) # Maximum number of parent rolls
# Number of different parent roll combinations to cost out
nRollSolCount <- nRollMaxParents - nRollMinParents + 1

# Configure an LP model to determine how to combine the roll sets so as to
# minimize the total cost of each number of parent rolls.
# x_pn = 1 if we select the nth Num.Parents solution set from the pth product family.
# p is from 1 to number of producs and n varies by p and is indexed from 1 to 
# the number of rolls to stock of each p.  
# x is a vector of length nRollMaxParents 

# Create the objective coefficients
obj <- numOptions$Sol.Total.Cost
# Define the constraint matrix.  
mat <- matrix(data = 0, nrow = 1 + nrow(prodList), ncol = nrow(numOptions))
# First row is the number of parent rolls in each options
mat[1,] <- numOptions$Sol.Num.Parents
# Next rows are used to make sure we get 1 solution for each product
for (n in 1:numProd) {
  # Start filling the row at the index of the first parent
  pos <- match(n, numOptions$prod_index)
  len <- prodList$numSols[n]
  end <- pos + len - 1
  mat[n + 1, ] <- replace(mat[n + 1, ], pos:end, 1)
}

# Defint the rhs.  We will update the first entry in the loop
rhs <- c(0, rep_len(1, numProd)) 

dir <- c("==", rep_len("==", numProd))

# define objective type (TRUE=maximum, FALSE=minimum)
types <- c(rep_len("B", nrow(numOptions)))

max <- FALSE

# Create empty var matrix and cost vector to hold solution
solution <- matrix(0, nrow = nRollSolCount, ncol = nrow(numOptions))
solCost <- rep(0, nRollSolCount)

# n <- 55 # For Testing
# Loop over the desired number of rolls in the solution
for (n in nRollMinParents:nRollMaxParents) {
  # Adjust the first constraint
  rhs[1] <- n
  
  # solve optimization problem 
  sol <- Rsymphony_solve_LP(obj, mat, dir, rhs, types= types, max=max)
  
  if(sol$status != 0) {next}  # No Solution Found, move to next n
  solution[n - nRollMinParents + 1, ] <- sol$solution
  solCost[n - nRollMinParents + 1] <- sol$objval
}

# Build the results dataframe.  This will contain the roll selection
# numbers for the lowest cost roll selection for each number of parent
# rolls
# Each parent grade/CalDW will show up once in each roll selection
# option
solutionNbr <- rep(1:(nRollSolCount), each = nrow(prodList))
solutionGrade <- rep(prodList$Grade, nRollSolCount)
solutionCalDW <- rep(prodList$CalDW, nRollSolCount)
solutionCost <- rep(solCost, each = nrow(prodList))
# This next vector will get the number of rolls of each parent that belongs
# to the solution of total rolls
parentNbrs <- rep(0, nrow(solution) * nrow(prodList))
for (n in 1:nRollSolCount) {
  # Good luck untangling this next hair ball! :)
  # As we loop through the solutions, update the parentNbrs vector.  
  # We select the 
  # elements of the Num.Parents vector by converting the selected solution 
  # row (one for each number of parent rolls) to a logical vector, and 
  # subsetting the elements corresponding to the 'TRUE' values (the 1's).
  # We are essentially filtering the list based on a logical vecor 
  parentNbrs <- replace(parentNbrs, 
                        ((n - 1) * numProd + 1):(((n - 1) * numProd) + numProd),
                        numOptions$Sol.Num.Parents[as.logical(solution[n,])])
}

solresults <- data.frame(solutionNbr, solutionGrade, solutionCalDW, parentNbrs,
                       stringsAsFactors = FALSE)

# Combine with numOptions to get the costs for each parent option
solresults <- left_join(solresults, numOptions, 
                      by = c("solutionGrade" = "Grade",
                             "solutionCalDW" = "CalDW",
                             "parentNbrs" = "Sol.Num.Parents"))

solresults <- solresults %>% group_by(solutionNbr) %>%
  mutate(SolParentCount = as.character(sum(parentNbrs)))

solresults_sum <- solresults %>% group_by(solutionNbr) %>%
  summarize(NbrParents = sum(parentNbrs), 
            Total.Cost = sum(Sol.Total.Cost), 
            Trim.Cost = sum(Sol.Trim.Cost),
            Handling.Cost = sum(Sol.Handling.Cost),
            Storage.Cost = sum(Sol.Exp.OH.Storage.Cost),
            Inv.Carrying.Cost = sum(Sol.Exp.OH.Inv.Cost),
            Exp.OH.Tons = sum(Sol.Exp.OH.Tons),
            SUM.Trim.Tons = sum(Sol.Trim.Tons),
            SS.Cost = sum(Sol.SS.Cost),
            SS.Tons = sum(Sol.SS.Tons))
# Cleanup
rm(solutionNbr, solutionGrade, solutionCalDW, solutionCost)

# This ends the changes.  The final results go in the AllRollSelOptions table
