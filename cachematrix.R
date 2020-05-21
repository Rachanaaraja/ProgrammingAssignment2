##makeCacheMatrix fuction creates a special matrix object that can cache its inverse
##cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix


## This function has a list of functions to set the value of matrix,get the matrix value,set the inverse value of the matrix and to get the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) #input matrix as argument
{
  inver<-NULL   #initial inverse value to null
  set<-function(y)   #set function to set input matrix value
  {
    x<<-y
    #print(x)
  }
  
  get<-function() x  #get function to get matrix value 
  setinverse<-function(inverse) inver <<-inverse   #setinverse function to set inverse matrix value
  getinverse<-function() inver                     #getinverse function to get inverse matrix value
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)  #list of functions
  
}


## Computes the inverse of the matrix

cacheSolve <- function(x, ...) 
{
  matdata<-x$get()                       #matdata stores the matrix data from makeCachematrix function
  inver<-solve(matdata,...)              #inver stores the inverse matrix computed by solve function
  x$setinverse(inver)                   
  inver<-x$getinverse()
  if(!is.null(inver))
  {
    print("Inverse of the given matrix")
    return(inver)                          #returns the inverse matrix value
  }
}

mat<-matrix(c(1,2,3,0,1,4,5,6,0),3,3)      #matrix input
print("The input matrix")
print(mat)
cacheout<-makeCacheMatrix(mat)             #creates special matrix object that cache its inverse
inversionans<-cacheSolve(cacheout)         #computes the inverse of input matrix and returns the value to inversionans
print(inversionans)                        #inverse matrix as output returned from cacheSOlve function