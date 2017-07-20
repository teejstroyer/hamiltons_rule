library(R6)

Field <- R6Class(
  classname = "Field",
  public = list(
    relMat = NULL,
    initialize = function(init=5){
      self$relMat = diag(init)
    },   
    reproduce = function(x,y){
      
      #Add another Column and Row
      self$relMat = cbind(self$relMat,0.0) 
      self$relMat = rbind(self$relMat,0.0)
      
      n = ncol(self$relMat)
      
      #Create Child Row
      self$relMat[n,] = ( self$relMat[x, ] + self$relMat[y, ] )/2 
      self$relMat[n,n]=1.0
      
      #Add relationship to the rest of the tree
      self$relMat[ ,n]= self$relMat[n, ]
    }
  )
)

FieldTest <- Field$new()
print(FieldTest$relMat)
