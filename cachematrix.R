## Put comments here that give an overall description of what your
## The first functions creates a matrix object that can cache its inverse and 
## second function detemines if a given matrix inverse has been produced or not. If not produced, it will produce and returns it

## creates matrix object cacheing its inverse

makeCacheMatrix <- function(x = matrix()) 
{
                inversed_matrix <- NULL
                set <- function(y) 
                        {
                                x <<- y
                                inversed_matrix <<- NULL
                        }
                
                get <- function() x
                setinverse<- function(inverse) inversed_matrix <<-inverse
                getinverse <- function() inversed_matrix
                list(
                        set = set, get = get,
                        setinverse = setinverse,
                        getinverse = getinverse
                )
}


## computes the inverse of matrix produced by makeCacheMatrix function

cacheSolve <- function(x, ...) 
{
        inversed_matrix <- x$getinverse()
        if (!is.null(inversed_matrix)) 
                {
                        return(inversed_matrix)
                } 
        else 
                {
                        inversed_matrix <- solve(x$get())
                        x$setinverse(inversed_matrix)
                        return(inversed_matrix)
                }
}
