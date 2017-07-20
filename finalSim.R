Field <- R6Class(
  classname = "Field",
  public = list(
    relMat = NULL,
    initialize = function(init=10){
      self$relMat = diag(init)
    },   
    reproduce = function(mat, x,y){
      mat= cbind(mat,0) 
      mat=rbind(mat,0)
      mat[nrow(mat),]= (mat[x,]+mat[y,])/2 
      mat[nrow(mat),ncol(mat)]=1
      return(mat)
    }
  )
)

relMat=reproduce(relMat,1,3)
relMat=reproduce(relMat,2,4)