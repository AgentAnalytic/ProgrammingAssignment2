

## This function "makeCacheMatrix((x = matrix(), SET = "FALSE")" takes in two arguments
## where 'x' is the inputed matrix and 'SET' is used for initializing the list that will store the inputed matrix(s)
## IMPORTANT : when executing this function 'makeCacheMatrix' for the FIRST time ONLY ,
## the 'SET' argument must be passed as "TRUE"  and after executing it once do not enter the 'SET' argument(let it be default)
## for eg.
## x <- matrix(1:4,2,2)
## makeCacheMatrix(x,"TRUE") // only enter "TRUE" for the first time while executing the function
## x <- matrix(1:4,2,2)
## makeCacheMatrix(x)         // do not enter "TRUE" OR "FALSE" after the first execution
## z <- matrix(rnorm(4),2,2)
## makeCacheMatrix(z)     

makeCacheMatrix <- function(x = matrix(), SET = "FALSE") {
  if (SET) {
    cachedMatrix <<- list(1)
    cachedInverse <<- list(1)
    
    inverse <- cacheSolve(x, cachedMatrix, cachedInverse)
    inverse
  }
  else{
    inverse <- cacheSolve(x, cachedMatrix, cachedInverse)
    inverse
  }
  
  
  
  
  
  
}


## This function "cacheSolve(x, cachedMatrix, cachedInverse)", takes in three agruments
## where : 'x' is inputed matrix,
## 'cachedMatrix' is a list of cached matrix that were inputed previously,
## 'cachedInverse' is the list of the previously cached matrix inputs.
## This function does a linear search on the cachedMatrix with the inputed matrix 'x'
## if the search is positive then the cached inverse matrix is returned else
## the inputed matrix 'x' is inserted at the end of the list 'cachedMatrix' and its inverse
## at the end of the list 'cachedInverse', finally the computed inverse is returned



cacheSolve <- function(x, cachedMatrix, cachedInverse) {
  len <<- length(cachedMatrix)
  
  
  
  for (i in 1:len) {
    if (identical(cachedMatrix[[i]], x))
    {
      message("getting cached data")
      
      return(cachedInverse[[i]])
      
      
      
    }
    
  }
  
  cachedMatrix[[(len + 1)]] <<- x
  cachedInverse[[(len + 1)]] <<- solve(x)
  return(cachedInverse[[len + 1]])
  
  
}
