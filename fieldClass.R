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
    xDim = NULL,
    yDim = NULL,
    initialize = function(naltM=2,naltF=2,altM=2,altF=2,mature=1,litter=3,k = 100,reps,xDim=200,yDim=200){
      maturity <<- mature
      k <<- k
      litter <<- litter
      xDim <<- xDim
      yDim <<- yDim
      
      n = naltM+naltF+altM+altF
      
      #relMat <<- diag(((k/2)*reps)+n)
      
      idV =c(1:n, rep(NA,litter*k))
      sexV=c(rep('M',times=altM),rep('F',times=altF),rep('M',times=naltM),rep('F',times=naltF),rep(NA,litter*k))
      ageV=c(rep(0,times=n),rep(NA,litter*k))
      gene_momV=c(rep(1,times=altM),rep(1,times=altF),rep(0,times=naltM),rep(0,times=naltF),rep(NA, times=litter*k))
      gene_dadV=c(rep(1,times=altM),rep(1,times=altF),rep(0,times=naltM),rep(0,times=naltF),rep(NA,times=litter*k))
      visiblityV=c(rep(T,times=n),rep(NA,litter*k))
      aliveV=c(rep(T,times=n),rep(NA,litter*k))
      
      posXV= c(sample(1:(xDim/2),replace=T,size=altM+altF), sample(seq(xDim/2,xDim),replace=T,size=naltM+naltF),rep(NA, times=litter*k))
      posYV= c(sample(1:yDim,replace=T,size=n),rep(NA, times=litter*k))
      
      df <<- data.table(id = idV, sex=sexV, age=ageV, gene_mom=gene_momV, 
              gene_dad=gene_dadV,visiblity=visiblityV, alive=aliveV ,posX=posXV,posY=posYV)
      
      curId <<- n+1
      pop <<- n
    },
    shrinkData = function(){
      dead = which(self$df$alive==F & !is.na(self$df$alive)) 
      self$df[dead,]=NA
    },
    pickMates = function(){
      #k = carrying capacity
      popsize=population()
      if(popsize >= self$k){
        #population is at carrying compacity
        return(NULL)
      }
      
      vMales=self$df[!is.na(self$df$age)& self$df$sex == 'M' & self$df$age >= self$maturity & self$df$alive==T,]
      vFeMales=self$df[!is.na(self$df$age) & self$df$sex == 'F' & self$df$age >= self$maturity & self$df$alive==T,]
      
      nf = ceiling((self$k-popsize)/self$litter)
      
      af=min(nf,length(vFeMales$id))
      
      chosenF=sample(vFeMales$id,size=af, replace=F)
      
      chosenM=sample(vMales$id,size=af, replace=T)
      pairs=list(females=chosenF,males=chosenM)
      
      if(!is.null(chosenF)){
        Map(self$reproduce, chosenF, chosenM,self$litter)
      }
      
    },
    reproduce = function(x,y,size=self$litter){
     for(i in 1:size){ 
       n = self$curId
       
       #Create Child Row
       #relMat[n,1:(n-1)] <<- ( self$relMat[x,1:(n-1) ] + self$relMat[y,1:(n-1)])/2 
      
       #Add relationship to the rest of the tree
      # relMat[ ,n] <<- self$relMat[n, ]
       
       #Add new critter to data frame
       self$storeDF(x,y,tdf=self$df)
     }
    },
    storeDF = function(x,y,tdf){
      #sample from mothers genes to pass down childrens genes
      geneM = sample(c(tdf$gene_mom[!is.na(tdf$id) & tdf$id==y],tdf$gene_dad[!is.na(tdf$id) & tdf$id==y]),1)
      
      #sample from fathers genes to pass down childrens genes
      geneF = sample(c(tdf$gene_mom[!is.na(tdf$id) & tdf$id==x],tdf$gene_dad[!is.na(tdf$id) & tdf$id==x]),1)
      
      
      pY <- tdf$posY[x]
      pX <- tdf$posX[x]
      
      cat(pX, " , ", pY, "\n")
      
      temp=list(self$curId,sample(c('M','F'),1),0,geneM,geneF,T,T,pX,pY)
      
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
      #print(pdf_long)
      
      ggplot(data=pdf_long, mapping=aes(x=steps, y=value, colour=variable)) + geom_line() 
               
    },
    culling = function(age=3){
      df$alive[self$df$age >= age & !is.na(self$df$age)] <<- F 
    },
    move = function(d){
      ln=length(self$df$posX[!is.na(self$df$posX)])
      
      df$posX[!is.na(self$df$posX)] <<- self$df$posX[!is.na(self$df$posX)]+sample(-d:d, ln,replace=T)
      df$posY[!is.na(self$df$posY)] <<- self$df$posX[!is.na(self$df$posY)]+sample(-d:d, ln,replace=T)
      
      #condition where critter is out of bounds
      df$posX[!is.na(self$df$posX) & self$df$posX >= self$xDim] <<- self$xDim-10
      df$posX[!is.na(self$df$posX) & self$df$posX <= 0] <<- 10
      
      df$posY[!is.na(self$df$posY) & self$df$posY >= self$yDim] <<- self$yDim-10
      df$posY[!is.na(self$df$posY) & self$df$posY <= 0] <<- 10
    }
  )
)

source("./eagleAttack.R")