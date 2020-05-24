## code by :: thilakraj devadiga
## Put comments here that give an overall description of what your
## functions do

#the main moto of the program is to understand how lexical scoping is works in R programming envirnoment
#the function creates special caching matrix which can be retrieved by other functions 
#and returns a list of list 
#the second function trys to fetch the cached data using resultant of first function
#it checks whether data/values exists ,if yes return the output,else generate the output & cache it,
#for further verification if the second method/function is re-executed it will again check and return output

#This function creates a special "matrix" object that can cache its inverse.
#when the first function is execute with argument has 
#an  square matrix since we are assuming that
#all matrix inputed will be a invertible matrix that is 
#det(matrix)>0 according to defination of invertible matrix
#once the first function is executed  it return 
#a list of all value from getmat(),setmat(),
#getinvmat(),setinvmat() with matrix variable 'x' has naming schema
#that is we can retrieve values using x$getmat or X$getinvmat ,etc.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL#set value of inverse = null
    setmat <- function(y) {#setting matrix
        x <<- y
        i <<- NULL
    }
    getmat <- function() x#this function returns orginal matrix
    setinvmat <- function(solve) i <<- solve#sets inverse of matrix value
    getinvmat <- function() i#returns inverse of matrix value
    #special list with names for fetching values once execute using names        
    list(setmat = setmat, 
         getmat = getmat, setinvmat = setinvmat, getinvmat = getinvmat)
}

# the second function takes one argument which is resultant of first function
#the function block has a trycatch() for error handling which behaves similar to 
#other languages try catch block ,in R the tryCatch() can return resultants
#in tryCatch() first set of code try to fetch/get inverse of matrix value from first function
#in if-else it check whether the fetched values in null or not ,if notnull the else statement 
#will execute or if statment will execute 
#if any error or warning occurs tryCatch() will handle it.

cacheSolve <- function(x, ...) {
    out<-tryCatch(
        {
            iv <- x$getinvmat()#fetching inverse of matrix value
            if(!is.null(iv)) {#checking for value if iv != null
                print("getting cached data")
                return(iv)
            }
            else{#if iv == null
                print("setting data into cache")
                data <- x$getmat()#fetch the orginal matrix from special vector
                iv <- solve(data, ...)#using solve() function get inverse of matrix
                x$setinvmat(iv)#set the inverse value using setinvmat() function into cache
                return(iv)#return the result
            }
        },
        error=function(condition){#if any error encountered while execution
            return(paste("some error occured while performing cache : \n",condition))
        },
        warning=function(condition){#if any warning encountered while execution
            return(paste("warning while performing cache : \n",condition))
        }
    )
    return(out)#result from the complete tryCatch() block will return output
}

#sample execution steps for testing
A <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)#matrix
results<-makeCacheMatrix(A)#executing first function
cacheSolve(results)#second function to check values
cacheSolve(results)

B <- matrix(c(1,2,3,4),2,2)#matrix 2
results1<-makeCacheMatrix(B)
cacheSolve(results1)
cacheSolve(results1)
