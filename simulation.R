source("fieldClass.R")
# Simulation
fieldSim <- function(reps=100, k=NULL,aM, aF, nM,nF, m, l){
  
  
  FieldTest <- Field$new(altM=aM,altF=aF,
                         naltM=nM,naltF=nF,
                         mature=m, k=k,
                         litter=l, reps=reps,
                         xDim=200, yDim=200)
  
  for(i in 1:reps){
    FieldTest$shrinkData()
    FieldTest$stepUp()
    FieldTest$move(5)
    
    #populations
    #cat("Population: ", FieldTest$population(),"\n")
    FieldTest$getPops(FieldTest$df)
    
    #Eagle Attack
    #FieldTest$eagleAttack()
    
    #kill elderly
    FieldTest$culling()
    
    #Reproduction Phase
    pairs=FieldTest$pickMates()
    
    #print(object.size(FieldTest$relMat))
  }
  
  View(FieldTest$df)
  #print(FieldTest$relMat)
  FieldTest$graphPops()
  #print(FieldTest$altPop)
  #print(FieldTest$naltPop)
  #print(FieldTest$recPop)
  #return(FieldTest$curId)
}

fieldSim(5,25,2,2,2,2,1,3)

# r = rep(c(50,75,100),3)
# k = rep(c(50,100,200),each=3)

#pop=Map(fieldSim,reps=r, k=k,altM=2,altF=2,naltM=2,naltF=2,m=1,l=3)
#pop=unlist(pop)

#pop=c(855 ,1284 ,1704 ,1680 ,2538 ,3387 ,3261 ,4947 ,6648)
#df = data.frame(reps=r, k=k, pops=pop)

#summary(lm(pops~reps+k,data=df))

#library(scatterplot3d)
#with(df, {
#  scatterplot3d(reps,   # x axis
#                k,     # y axis
#              pops,    # z axis
#            main="3-D Scatterplot Example 1")
#})
