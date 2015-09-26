## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## function makeCacheMatrix initializes a local environment and
        ## returns a list of functions to access it:
        ## 
        ## Args: x = an initial value for the matrix
        ## Returns: a list of the following functions, which manipulate the 
        ## local environment :
        ##      set : sets the matrix data
        ##      get : gets the matrix data
        ##      iscached : is the cached inverse valid? TRUE if yes, else FALSE
        ##      setinverse : sets the cached inverse to a certain value
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
                MatInv <<- FALSE
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
        #  Args: InvM = the value to set
        #  Returns: TRUE
                MatInv <- InvM
                InvCached <- TRUE
        }
        
        #definition of local function 'getinverse'
        getinverse <- function() {
                # function 'getinverse' returns the cached value of the 
                # inverse matrix
                MatInv
        }
        
        # return the list of functions
        list(set = set, 
             get = get, 
             iscached = iscached,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

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
        ##              else compute the inverse and cache it

        if(x$iscached()) {
                message("getting cached data")
                x$getinverse
        } else {
                m <-x$get()
                print(m)
                x$setinverse(solve(x$get()))
        }
                
}

