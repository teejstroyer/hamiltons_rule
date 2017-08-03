library(R6)
library(ggplot2)
library(data.table)
library(reshape2)
source("./eagleAttack.R")

Field <- R6Class(
  classname = "Field",
  portable = F,
  public = list(
    relMat = NULL,
    df = NULL,#TODO add positionx and positiony , and lastbirth
    maturity = NULL,
    litter = NULL,
    k = NULL,
    pop = NULL,
    altPop = NULL,
    naltPop = NULL,
    recPop = NULL,
    curId = NULL,
    initialize = function(naltM=2,naltF=2,altM=2,altF=2,mature=1,litter=3,k = 100){
      maturity <<- mature
      k <<- k
      litter <<- litter
      
      n = naltM+naltF+altM+altF
      
      relMat <<- diag(n)
      
      idV =c(1:n, rep(NA,litter*k))
      sexV=c(rep('M',times=altM),rep('F',times=altF),rep('M',times=naltM),rep('F',times=naltF),rep(NA,litter*k))
      ageV=c(rep(0,times=n),rep(NA,litter*k))
      gene_momV=c(rep(1,times=altM),rep(1,times=altF),rep(0,times=naltM),rep(0,times=naltF),rep(NA, times=litter*k))
      gene_dadV=c(rep(1,times=altM),rep(1,times=altF),rep(0,times=naltM),rep(0,times=naltF),rep(NA,times=litter*k))
      visiblityV=c(rep(T,times=n),rep(NA,litter*k))
      aliveV=c(rep(T,times=n),rep(NA,litter*k))
      
      df <<- data.table(id = idV, sex=sexV, age=ageV, gene_mom=gene_momV, 
              gene_dad=gene_dadV,visiblity=visiblityV, alive=aliveV )
      
      curId <<- n+1
      pop <<- n
    },
    shrinkData = function(){
      dead = which(self$df$alive==F & !is.na(self$df$alive)) 
      self$df[dead,] = NA
    },
    pickMates = function(){
      #k = carrying capacity
      popsize=population()
      print(popsize) 
      if(popsize >= self$k){
        #population is at carrying compacity
        return(NULL)
      }
      
      vMales=self$df[self$df$sex == 'M' & self$df$age >= self$maturity & !is.na(self$df$age),]
      vFeMales=self$df[self$df$sex == 'F' & self$df$age >= self$maturity & !is.na(self$df$age),]
      
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
       relMat <<- cbind(self$relMat,0.0) 
       relMat <<- rbind(self$relMat,0.0)
      
       n = ncol(self$relMat)
      
       #Create Child Row
       relMat[n,] <<- ( self$relMat[x, ] + self$relMat[y, ] )/2 
       relMat[n,n] <<- 1.0
      
       #Add relationship to the rest of the tree
       relMat[ ,n] <<- self$relMat[n, ]
       
       #Add new critter to data frame
       self$storeDF(x,y,tdf=self$df)
     }
    },
    storeDF = function(x,y,tdf){
      geneM = sample(c(tdf$gene_mom[x],tdf$gene_dad[x]),1)
      geneF = sample(c(tdf$gene_mom[y],tdf$gene_dad[y]),1)
      
      temp=list(self$curId,sample(c('M','F'),1) , 0, geneM, geneF, T, T)
      
      j<-which(is.na(tdf$id))[1]
      tdf[j,]=temp
      df <<- tdf
      curId <<- self$curId+1
      
    },
    getPops = function(df){
      #get populations at every time step 
      altPop  <<- c( self$altPop,nrow(df[df$gene_mom ==1 & df$gene_dad==1,]))
      naltPop <<- c( self$naltPop,nrow(df[df$gene_mom ==0 & df$gene_dad==0,]))
      recPop  <<- c( self$recPop,nrow(df[df$gene_mom ==0 & df$gene_dad==1,])+
                       nrow(df[df$gene_mom ==1 & df$gene_dad==0,]))
    },
    population = function(){
      pop <<- nrow(self$df[self$df$alive ==T & !is.na(self$df$alive)])
      return(pop)
    },
    stepUp = function(){
      df$age[self$df$alive == T & !is.na(self$df$alive)] <<- self$df$age[self$df$alive == T & !is.na(self$df$alive)] + 1  
    },
    graphPops = function(){
      pdf = data.frame(NAlt_Pop = self$naltPop, Alt_Pop=self$altPop, Rec_Pop=self$recPop,steps = seq(1:length(self$altPop)) ) 
      
      pdf_long <- melt(pdf, id="steps")  # convert to long format
      
      ggplot(data=pdf_long, aes(x=steps, y=value, colour=variable)) + geom_line() 
               
    },
    culling = function(age=3){
      df$alive[self$df$age >= age & !is.na(self$df$age)] <<- F 
    }
  )
)

source("./eagleAttack.R")