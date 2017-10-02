## makeCacheMatrix function creates a list that contains functions which are used
## to set the value of the matrix, get the value of the matrix. It also contains
## functions which are used to set the inverse of the matrix and get that inverse
## value afterwards

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }                    
        get<-function()x         ##get function for the value of x
        setinv<-function(inverse) inv<<-inverse ##this function sets the inv value
        getinv<-function()inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The following function calculates the inverse of the matrix from the function
## created above. Although, it checks first if the inverse of the given matrix has
## already been calculated. If it yes, then it skips the computation and gets the
## inverse value from the cache. Otherwise, it calculates inverse of the matrix and
## sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")##get value from already calculated 
                                                ##inverse
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}

