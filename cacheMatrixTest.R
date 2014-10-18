## cacheMatrixTest() uses a simple 4x4 square matrix to test that:
##   makeCacheMatrix() returns the proper special list
##   cacheSolve() calculates and returns an identity matrix
##   resulting from m ultiplying the matrix times its inverse.
## If everything works, the returned matrix should have 
##   ones on the main diagonal and near-zeros elsewhere 

cacheMatrixTest <- function() {
    
    id=NULL
    
    mval=c(2,14,4,3,5,9,9,7,3,6,3,8,5,7,2,6)
    m=matrix(mval,nrow=4,ncol=4)
    x=makeCacheMatrix(m)
    inv=cacheSolve(x)
    id=(m %*% inv)
    
    return(id)
    
}