
## ************************************************************
## Test functions for the programming assignment 2
## ************************************************************


# some data
# ==========
mat0 <- matrix()

mat2 <- matrix(1:4)
dim(mat2) <- c(2,2)
imat2 <- solve(mat2)

mat3 <- c(1:4,1:5)
dim(mat3) <- c(3,3)
imat3 <- solve(mat3)

dummy <- "dummy-string"

# begin test makeCacheMatrix()
#=============================
# obj2 <- makeCacheMatrix(mat2)
# obj3 <- makeCacheMatrix(mat3)

#function testmake
testmake <- function(mx) {
        #args: mx = a square matrix
        # begin
        # # create the object
        mobj <- makeCacheMatrix(mx)
        # save value of matrix inside object
        mat <- mobj$get()
        
        # play with object
        #-----------------
        
        # get, set
        message("get")
        print(mobj$get())
        
        message("set null")
        print(mobj$set(NULL))
        message("get (null)")
        print(mobj$get())
        
        message("set (restore)")
        print(mobj$set(mat))
        message("get (restored)")
        print(mobj$get())
        
        #iscached, setinverse, getinverse
        message("iscached (original state)")
        print(mobj$iscached())
        message("setinverse dummy-value")
        print(mobj$setinverse("dummy"))
        message("iscached (after setinverse)")
        print(mobj$iscached())
        message("getinverse (dummy-value)")
        print(mobj$getinverse())
        message("set matrix again (dummy)")
        print(mobj$set(dummy))
        message("check iscached")
        print(mobj$iscached())

}


#function testmake
testcache <- function(mx) {
        #args: mx = a square matrix
        # begin
        # # create the object
        mobj <- makeCacheMatrix(mx)
        message("solve 1")
        print(cacheSolve(mobj))
        message("solve 2")
        print(cacheSolve(mobj))
}
        