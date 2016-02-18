## In this file it is solved the Programming Assignament 2: Lexical Scoping 
## The assignament problem is to obtain the inverse of a matrix and to cache it
## to avoid the time of re-calculation
## 
## Example of use function
## > m<-matrix(rnorm(100),10,10)
## > mat <-makeCacheMatrix(m)
## > inv1 <- cacheSolve(mat)
## > inv2 <- cacheSolve(mat)
## 
## For inv1 the inverse calculation is done and the matrix returned
## For inv2 the cached matrix is returned without calculation


## makeCacheMatrix create an objet (CacheMatrix) able to store a the inverse of the matrix supplied in x
## Input x is a matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) inv<<-inverseMatrix
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve return the inverse of the matrix.
## Input x must be a CacheMatrix. x is not checked (type, square, determinant greater than 1.0,...)
## If x is not a CacheMatrix an error arise
## The inverse is calculated if it is not saved frome previous analyses.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv <-solve(data, ...)
        x$setInverse(inv)
        inv        
}
