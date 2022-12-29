##makeCacheMatrix is a function that creates a matrix(list) which contains 
##      a function that will set and get the elements of the matrix and 
##      will set and get the elements of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL     ##initialize Vector as NULL
        
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        
        get <- function()x
        
        setInverse <- function(inverse) j <<- inverse
        
        getInverse <- function() j 
        
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}

##the cacheSolve function checks to see if the inverse of the matrix has 
##      already been calculated and uses that value if it is found. Otherwise, 
##      it will calculate the inverse matrix and add it to the cache

cacheSolve <- function(x, ...) {
        j <- x$getInverse()
        
        if(!is.null(j)){
                message("Getting Cached Data")
                return(j)
        }
       
        matrix <- x$get()
       
        j <- solve(matrix,...)
       
        x$setInverse(j)
        j
}