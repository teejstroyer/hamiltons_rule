library(R6)
library(ggplot2)
library(data.table)
library(reshape2)

Field <- R6Class(
  classname = "Field",
  public = list(
    relMat = NULL,
    df = NULL,#TODO add positionx and positiony , and lastbirth
    dfSmall= NULL,
    maturity = NULL,
    litter = NULL,
    k = NULL,
    altPop = NULL,
    naltPop = NULL,
    recPop = NULL,
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
      
      self$df=data.table(id = 1:n, sex=sexV, age=ageV, gene_mom=gene_momV, 
              gene_dad=gene_dadV,visiblity=visiblityV, alive=aliveV )
      
    },
    shrinkData = function(){
      self$dfSmall = self$df[alive == T] 
    },
    pickMates = function(){
      #k = carrying capacity
      popsize=self$population()
      cat ("popsize = ",popsize, " k = ",self$k,"\n")
      if(popsize >= self$k){
        #population is at carrying compacity
        cat("capacity met\n") 
        return(NULL)
      }
      
      vMales=self$dfSmall[self$dfSmall$sex == 'M' & self$dfSmall$age >= self$maturity,]
      vFeMales=self$dfSmall[self$dfSmall$sex == 'F' & self$dfSmall$age >= self$maturity,]
      
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
    },
    getPops = function(){
      #get populations at every time step 
      self$altPop=c( self$altPop,nrow(self$dfSmall[self$dfSmall$gene_mom ==1 & self$dfSmall$gene_dad==1,]))
      self$naltPop=c( self$naltPop,nrow(self$dfSmall[self$dfSmall$gene_mom ==0 & self$dfSmall$gene_dad==0,]))
      self$recPop=c( self$recPop,nrow(self$dfSmall[self$dfSmall$gene_mom ==0 & self$dfSmall$gene_dad==1,])+
                       nrow(self$dfSmall[self$dfSmall$gene_mom ==1 & self$dfSmall$gene_dad==0,]))
    },
    population = function(){
      return(nrow(self$dfSmall))
    },
    stepUp = function(){
      self$df$age[self$df$alive == T] = self$df$age[self$df$alive == T] + 1  
    },
    graphPops = function(){
      pdf = data.frame(NAlt_Pop = self$naltPop, Alt_Pop=self$altPop, Rec_Pop=self$recPop,steps = seq(1:length(self$altPop)) ) 
      
      pdf_long <- melt(pdf, id="steps")  # convert to long format
      
      ggplot(data=pdf_long, aes(x=steps, y=value, colour=variable)) + geom_line() 
               
    },
    culling = function(age=3){
      self$df$alive[self$df$age >= age]=F 
    }
  )
)

fieldSim <- function(reps=100, k=NULL){
  FieldTest <- Field$new(altM=10,altF=10,
                         naltM=10,naltF=10,
                         mature=0, k=k,
                         litter=3)
  FieldTest$stepUp()
  for(i in 1:reps){
    FieldTest$stepUp()
    FieldTest$getPops()
    FieldTest$shrinkData()
    #summary function
        
    #populations
    
    #kill elderly
    FieldTest$culling()
    #Reproduction Phase
    pairs=FieldTest$pickMates()
    if(!is.null(pairs)){
      Map(FieldTest$reproduce,x=pairs$females,y=pairs$male)
    }
  }
  
  #View(FieldTest$df)
  #print(FieldTest$relMat)
  FieldTest$graphPops()
}
