library(R6)

Field <- R6Class(
  classname = "Field",
  public = list(
    relMat = NULL,
    df = NULL,#TODO add positionx and positiony , and lastbirth
    maturity = NULL,
    litter = NULL,
    k = NULL,
    initialize = function(naltM=2,naltF=2,altM=2,altF=2,mature=1,litter=3,k = 100){
      self$maturity = mature
      self$k = k
      self$litter = litter
      
      n = naltM+naltF+altM+altF
      
      self$relMat = diag(n)
      
      sexV=c(rep('M',times=altM),rep('F',times=altF),rep('M',times=naltM),rep('F',times=naltF))
      ageV=rep(0,times=n)
      gene_momV=c(rep(1,times=altM),rep(1,times=altF),rep(0,times=naltM),rep(0,times=naltF))
      gene_dadV=c(rep(1,times=altM),rep(1,times=altF),rep(0,times=naltM),rep(0,times=naltF))
      visiblityV=rep(T,times=n)
      aliveV=rep(T,times=n)
      
      self$df=data.frame(id = 1:n, sex=sexV, age=ageV, gene_mom=gene_momV, 
              gene_dad=gene_dadV,visiblity=visiblityV, alive=aliveV )
      
    },
    pickMates = function(){
      #k = carrying capacity
      vMales=self$df[self$df$sex == 'M' & self$df$age >= self$maturity & self$df$alive == T,]
      vFeMales=self$df[self$df$sex == 'F' & self$df$age >= self$maturity & self$df$alive == T,]
      
      popsize=length(self$df$alive[self$df$alive == T])
      nf = ceiling((self$k-popsize)/self$litter)
      
      af=min(nf,length(vFeMales$id))
      
      chosenF=sample(vFeMales$id,size=af, replace=F)
      chosenM=sample(vMales$id,size=af, replace=T)
      pairs=list(females=chosenF,males=chosenM)
      return(pairs)
    },
    reproduce = function(x,y,size=self$litter){
     for(i in 1:size){ 
       #Add another Column and Row
       self$relMat = cbind(self$relMat,0.0) 
       self$relMat = rbind(self$relMat,0.0)
      
       n = ncol(self$relMat)
      
       #Create Child Row
       self$relMat[n,] = ( self$relMat[x, ] + self$relMat[y, ] )/2 
       self$relMat[n,n]=1.0
      
       #Add relationship to the rest of the tree
       self$relMat[ ,n]= self$relMat[n, ]
       
       #Add new critter to data frame
       self$storeDF(x,y,n)
     }
    },
    storeDF = function(x,y,n){
      geneM = sample(c(self$df$gene_mom[x],self$df$gene_dad[x]),1)
      geneF = sample(c(self$df$gene_mom[y],self$df$gene_dad[y]),1)
      
      temp=list(n,sample(c('M','F'),1) , 0, geneM, geneF, T, T)
      
      self$df=rbind(self$df,temp)
    }
  )
)
fieldSim <- function(){
  FieldTest <- Field$new(mature=0, k=50)
  pairs=FieldTest$pickMates()
  FieldTest$df$age =FieldTest$df$age+1 
  Map(FieldTest$reproduce,x=pairs$females,y=pairs$male)
  View(FieldTest$df)
  print(FieldTest$relMat)
  return(FieldTest)
}
