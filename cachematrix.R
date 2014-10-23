## The first function, makeCacheMatrix cache a matrix, and return
## a list containing a function to

makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL
  set<-function(y) ##  set the value of the cache of the matrix
  {
    x<<-y
    m<<-NULL
  }
  get<-function() x ##  get the value of the cache
  setrev_matrix<-function(solve) m<<-solve ## 3. set the value of the inverse matrix
  getrev_matrix<-function() m ## get the value of the inverse matrix
  list(set=set,get=get, setrev_matrix=setrev_matrix,getrev_matrix=getrev_matrix)
}


##   Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) 
{
  m<-x$getrev_matrix()
  if(!is.null(m)) 
 # if the reverse matrix has already been calculated, 
 #it gets the mean from the cache and skips the computation
  {
    message("getting cached reverse matrix")
    return (m)
  }
  else 
 #otherwise,
  {
    m<-solve(x$get(),...) #it calculates the inverse matrix through the function solve 
    x$setrev_matrix(m) # it sets the value of the inverse matrix in the cache via the setrev_matrix function.
    m
  }
}
