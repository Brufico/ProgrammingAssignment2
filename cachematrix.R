## ***************************************************************************
## Programming Assignment 2: makeCacheMatrix and cacheSolve
## ***************************************************************************
## 
## = Overall goal: 
## compute the inverse of a matrix only if it has not been already
## computed, otherwise simply retrieve and return the inverse
## 
## = Method:
## similar to the example given, with one exception: In the example  the 
## mean m was set to NULL in order to indicate that it was not already cached. 
## Here, a boolean indicator InvCached is used instead (InvCached is FALSE if the
## inverse matrix is not available)
##
## = Description:
## a) Function makeCacheMatrix creates the object to pass to cachesolve, as a 
## list of closures sharing the same parent environment. 
## That environment contains three variables:
##      Mat = the matrix for which an inverse is to be found
##      MatInv = the cached inverse matrix
##      InvCached =  a boolean : InvCached is TRUE if the cached 
##              inverse is valid, else InvCached is FALSE
## makeCacheMatrix returns a list of 5 functions to access these variables.
## 
## b) Function cacheSolve takes the object created with makeCacheMatrix and returns
## the desired inverse matrix. The inverse is only computed (and cached) if it is not
## already cached.
## 
## 
## ==========================================================================
## <code begins>


## ------------------------------------------------------------
## a) Function makeCacheMatrix creates the object to be used by cachesolve as a 
## list of closures sharing the same parent environment.

makeCacheMatrix <- function(x = matrix()) {
        ## function makeCacheMatrix initializes a local environment and
        ## returns a list of functions to access and modify it:
        ## 
        ## Args: x = an initial value for the matrix
        ## Returns: a list of  functions, which read and manipulate the 
        ## local environment :
        ##      set : sets the matrix value
        ##      get : gets the matrix value
        ##      iscached : is the cached inverse valid? TRUE if yes, else FALSE
        ##      setinverse : sets the cached inverse value (does not compute it)
        ##      getinverse : gets the cached inverse

        ## Begin: initializing the local environment variables: 
        ##      Mat = the cached matrix, initially equal to x (NULL if not supplied)
        ##      MatInv = the cached inverse matrix
        ##      InvCached =  a boolean : InvCached is TRUE if the cached 
        ##              inverse is valid, else InvCached is FALSE
        Mat<- x
        MatInv <- matrix(numeric(0))
        InvCached <- FALSE
        
        
        #definition of local function 'set'
        set <- function(Matval) {
                # function 'set' sets the value of the cached matrix
                # Args : Matval = the value to assign to Mat
                # Returns : TRUE
                Mat <<- Matval
                InvCached <<- FALSE
                # return value
                TRUE
        }
        
        #definition of local function 'get'
        get <- function() {
        # function 'get' returns the value of the cached matrix
                Mat
        }
        
        #definition of local function 'iscached'
        iscached <- function(){
        # function 'iscached' returns the value of the local boolean InvCached
                InvCached
        }
        
        #definition of local function 'setinverse'
        setinverse <- function(InvM) {
        # function 'setinverse' sets the value of the cached inverse matrix
        #  It also sets the value of InvCached to TRUE
        #  Args: InvM = the value to assign to the inverse
        #  Returns: the value of the argument
                InvCached <<- TRUE
                MatInv <<- InvM
        }
        
        #definition of local function 'getinverse'
        getinverse <- function() {
                # function 'getinverse' returns the cached value of the inverse
                MatInv
        }
        
        # return the list of functions
        list(set = set, 
             get = get, 
             iscached = iscached,
             setinverse = setinverse,
             getinverse = getinverse)
}

## -------------------------------------------------------------------
## b) Function 'cacheSolve' takes an object created with makeCacheMatrix  
## and returns the inverse of the matrix stored in that object, computing 
## and caching the inverse, only if it is not already cached

cacheSolve <- function(x, ...) {
        ## function 'cacheSolve' returns a matrix that is the inverse of 'x'
        ## Args : x = an object created with 'makeCacheMatrix'
        ## 
        ## function cachesolve tests if the cached inverse is valid, 
        ##      if ok, returns the cached value 
        ##      else computes the inverse, caches the new inverse and returns it
        ##      
        ## Returns: a matrix that is the inverse of 'x'
        
        ## Begin: test if the inverse matrix cached in x is valid
        ##              if yes, return the cached value (and say it)
        ##              else compute the inverse and cache it (and say it)

        if(x$iscached()) {
                message("getting cached data")
                x$getinverse()
        } else {
                message("computing and caching the new inverse matrix")
                x$setinverse(solve(x$get()))
        }
                
}

