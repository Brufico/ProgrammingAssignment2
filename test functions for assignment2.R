## ************************************************************
## Test functions for the programming assignment 2
## ************************************************************

## ------------------------------------------------------------
## to test global functionnality, enter : testmain()
## (it is done automatically when sourcing this file)
## 
## other test functions are included below
##      testmake(<matrix>)
##      testmake(<matrix>)
## ------------------------------------------------------------



# some data ============================================================

mat0 <- matrix()

mat2 <- matrix(1:4)
dim(mat2) <- c(2,2)
imat2 <- solve(mat2)

mat3 <- c(1:4,1:5)
dim(mat3) <- c(3,3)
imat3 <- solve(mat3)

#generate a big invertible random matrix
rmat <- function(size = 200) {
        d <- 0
        while (abs(d) < 0.000001) {
                bigmat <- matrix(rnorm(size^2, 100, 30), size, size)
                d <- det(bigmat)
        }
        bigmat      
}


dummy <- "dummy-string"



# test functions ============================================================

## testmain :
testmain <- function(big = 500){
        message("                            ")
        message("****************************")
        message("inverse of a 2x2 matrix")
        message("****************************")
        testcache(mat2)
        
        message("                            ")
        message("                            ")
        message("****************************")
        message("inverse of a 200x200 matrix")
        message("****************************")
        testcache(rmat(200), printsolve = FALSE)
        
        bigmsg <- paste(c(big, big), collapse = " x ")
        message("                            ")
        message("                            ")
        message("****************************")
        message(paste("inverse of a", bigmsg, "matrix", collapse = " "))
        message("****************************")
        testcache(rmat(big), printsolve = FALSE)
        
}




# function testmake
testmake <- function(mx) {
        #args: mx = a square matrix
        
        # begin
        ## create the object
        mobj <- makeCacheMatrix(mx)
        # save value of matrix inside object
        mat <- mobj$get()
        
        # play with object and test things
        #---------------------------------
        
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


# function testcache
testcache <- function(mx, printsolve = TRUE) {
        #args: mx = a square matrix
        #       printsolve = print the result ?
        # begin
        # # create the object
        mobj <- makeCacheMatrix(mx)
        message("solve (1st attempt)")
        if (printsolve) {
                print(cacheSolve(mobj))
        } else {
                cacheSolve(mobj)
                print("========>>>>>>> DONE: long result not printed")
        }
        message(" ")
        message("solve (2nd attempt)")
        if (printsolve) {
                print(cacheSolve(mobj))
        } else {
                cacheSolve(mobj)
                print("========>>>>>>> DONE: long result not printed")
        }
}



# demonstration ================================================
testmain()
