## Programming Assignment number 2 by Sanjay Pol, R Programming Coursera Course
## Date : August 23, 2016
## This program will compute an inverse of an incoming matrix.  If the 
## inverse has already been computed before then it is simply extracted 
## from cache otherwise the solve function is invoked to compute the inverse
## 

## makeCacheMatrix will return a list of four functions and will also
## set the global variables x and m based on lexical scoping rules of R
## The four functions are : setmatrix, getmatrix, setinverse, getinverse
## These four functions and the two variabls (x which is the matrix to be 
## inverted and m which is the inverse of the matrix) will reside in memory
## and will be available to subsequent R code during execution as needed.

makeCacheMatrix <- function(x = matrix()) {
        
                
                # Extract dimensions of incoming matrix x
                #
                xrows <-dim(x)[1]
                xcols <- dim(x)[2]
                
                # initialize global matrix m to all NA's
                # initialize a new local matrix y to all NA's
                #
                
                m <- matrix(data = NA, nrow = xrows , ncol = xcols)
                
                #y <- matrix(data = NA, nrow = xrows , ncol = xcols)
                
                # define setmatrix function, it will set the global variable x
                # to the incoming argument of the setmatrix function and it will
                # set the inverse of the matrix to NULL
                # 
                setmatrix <- function(y){
                        x <<- y
                        
                        # initialize global matrix m to NA
                        #
                        m <- matrix(data = NA, nrow = yrows , ncol = ycols)
                }
                
                # define getmatrix function, it returns the global matrix x
                # 
                getmatrix <- function() 
                {
                        x
                }
                
                # define setinverse function, it sets the global variable m to
                # the inverse of the matrix x, using the argument passed to it
                # 
                setinverse <- function (solve) {
                        m <<- solve
                        #print (m)
                }
                
                # define getinverse function, it returns the global matrix m
                getinverse <- function () {
                        m
                }
                
                # set up list of four functions that will be returned back 
                # from this function makeCacheMatrix
                # 
                list (setmatrix = setmatrix, getmatrix = getmatrix, 
                      setinverse=setinverse, getinverse=getinverse)
        
}


## Return a matrix that is the inverse of 'x'
## Get the current value of matrix m, if it is NA then compute the inverse
## Otherwise extract the copy from cache and return it to save on computation

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        m_all_na <- all(is.na(m))
        
        rows_m = dim(m)[1]
        cols_m = dim(m)[2]
        
        if (rows_m != cols_m)
        { 
                message ("Not a square matrix, Inverse cannot be computed")
                return
        }
        # if m already exists in cache then it contains valid elements and it
        # is not an NA matrix
        if (!m_all_na)  { 
                message("Found inverse already in cache, returning it")
                return(m)
        }
        
        message("Computing matrix inverse for First time with solve")
        # first get the matrix in the variable data
        temp_matrix <- x$getmatrix()
        
        m <- solve(temp_matrix,...)
        
        # now put the result in cache for future use
        x$setinverse(m)
        
        # return m the inverted matrix
        message ("Inverse returned")
        m
}
