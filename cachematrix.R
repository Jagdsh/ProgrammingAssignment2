## makeCacheMatrix is a function which gets, sets the matrix value and its inverse
## cacheSolve is a function where it determines the data is available in cache or not
## 

## originalMatrix is passed to the function which is strored 
## inverseMatrix is stored as NULL later the Inverse is set in the setInverse function

makeCacheMatrix <- function(originalMatrix = matrix()) {
	 inverseMatrix <- NULL
	set <- function(setMatrix){
			originalMatrix <<- setMatrix
			inverseMatrix <<- NULL
			}
	get <- function() originalMatrix
	setInverse <- function(originalMatrix) inverseMatrix <<- solve(originalMatrix)
	getInverse <- function() inverseMatrix
	list(set = set, get = get,
	      setInverse = setInverse,
	      getInverse = getInverse)
}


## This funtion determines if the matrix passed is already present in the Cached
## data and returns if present or creates a inverse variable

cacheSolve <- function(instance, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverseMatrix <- instance$getInverse()	
	if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
	matrix <- instance$get()
	inverseMatrix <- instance$setInverse(matrix)
	inverseMatrix
}
