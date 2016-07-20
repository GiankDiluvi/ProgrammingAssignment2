## makeCacheMatrix takes an invertible square matrix and caches it, returning a special
## matrix object (which is actually a list of four function). This way, if the
## inverse of the matrix is calculated, the user can save that inverse for further
## reference, without having to calculate it again (which can be costly.)
##
##----------------------------------------------------------------------------------
##
## solveCache takes a special invertible square matrix (such as the one makeCacheMatrix
## returns) and returns its inverse, by first checking if it exists and calculating it
## if it doesn't.
##
#########################################################################################
##
## Code written by Gian Carlo Diluvi for the Coursera R Programming course, given
## by the John Hopkins University. July 2016.
##
#########################################################################################

## makeCacheMatrix takes an invertible square matrix and caches it, that is,
## it returns a special matrix object, that is, a list of four functions:
## set, get, setinverse and getinverse.
## 1. set lets the user set a  new value for the original matrix
## 2. get returns the matrix
## 3. setinverse let's the user specify the matrix's inverse
## 4. getinverse returns the matrix's inverse (or a NULL matrix if it
##    hasn't been specified.)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## We first set inv to be NULL
    
    ## Now we begin with the first element that's going to be returned: set
    set <- function(y){
        if(x!=y){
            x <<- y              ## x is set to be equal to y
            inv <<- NULL         ## inv is set to NULL, because the inverse has now changed
        }
        ## If x=y, we don't do anything. The inverse still remains the same.
        ## This is helpful because if a new assignment is made, but the matrix
        ## doesn't really change, then it's more efficient to not set the inverse
        ## to NULL, but rather to keep it
    }
    
    ## Now we define the second element to be returned: get
    get <- function(){
        x                    ## This function simply returns x
    }
    
    ## Now the third element: setinverse
    setinverse <- function(inverse){
        inv <<- inverse      ## inv is set to equal the user-specified inverse
    }
    
    ## Finally the fourth element: getinverse
    getinverse <- function(){
        inv                  ## This function simply returns the inverse, inv
    }
    
    ## All four values (or functions) are now returned in a list:
    return(list(set = set, get = get,
                setinverse = setinverse, 
                getinverse = getinverse))
}

##-------------------------------------------------------------------------------------

## cacheSolve takes a "special" invertible square matrix and returns its inverse.
## It does this by first checking if that inverse matrix has already been calculated,
## in which case that is the value it returns.
## If the inverse has not been calculated, then cacheSolve calculates it (using R's solve()),
## sets that value as the inverse (in case cacheSolve is later called again for the
## same matrix) and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()     ## We get x's inverse (which can be NULL)
    if(!is.null(inv)){
        return(inv)           ## If the inverse has been calculated, it is returned
    }else{
        matrix <- x$get()     ## If it has not been calculated, we calculate it now
        inv <- solve(matrix, ...) 
        x$setinverse(inv)     ## The inverse is now set, for further occasions
        return(inv)           ## And it's also returned
    }
    
}
