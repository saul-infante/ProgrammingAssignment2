



## The purpose of this functions is to be able to cache the inverse of a matrix for the purpose of avoiding re-calculating ##it in in the event that it has already been calculated and its available in Memory

## This function creates a special "Matrix" in reality is a list of functions that allow to:
##1 set the value of a matrix
##2 get the value of the matrix
##3 set the value of the inverse matrix
##4 retrive the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  	m <- NULL						##erases any cached matrix
        set <- function(y) { 					##define function setting the value of a matrix
                x <<- y						##on a persistent mode 
                m <<- NULL
        }
        get <- function() x					#define function to get the value of the matrix
        setinvmatrix <- function(invmatrix) m <<- invmatrix 	#define function to set the value of the inverse matrix
        getinvmatrix <- function() m				#define funtion to retrive the value of the inverse matrix
             
        list(set = set, get = get,				#create a list with all the functions
 		setinvmatrix = setinvmatrix,				
             getinvmatrix = getinvmatrix )


}


## This function will return the inverse of a Matrix. If the inverse has been calculated before it will not perfom any 
## caculations and it will return the inverse cached in memory. If the inverse has NOT been calculated it will calculate it ## and return that value

cacheSolve <- function(x, ...) {
        							## Return a matrix that is the inverse of 'x'

	   m <- x$getinvmatrix()				## Return cached version 
        if(!is.null(m)) {					## If inverse has already been calculated and cached return it
                message("getting cached data")
                return(m)
        }
        data <- x$get()						##If inverse has not been calculated retrieve matrix
        m <- solve(data, ...)					##and calculate inverse
        x$setinvmatrix(m)					##Persist calculation of inverse in cache
        m							##Return calculated inverse
}