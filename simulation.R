source("fieldClass.R")
# Simulation
fieldSim <- function(reps=100, k=NULL){
  FieldTest <- Field$new(altM=10,altF=10,
                         naltM=10,naltF=10,
                         mature=0, k=k,
                         litter=3)
  for(i in 1:reps){
    FieldTest$stepUp()
    FieldTest$getPops(FieldTest$dfSmall)
    FieldTest$dfSmall=FieldTest$shrinkData(FieldTest$df)
    #summary function
    
    #populations
    
    #Eagle Attack
    FieldTest$eagleAttack()
    
    #kill elderly
    FieldTest$culling()
    FieldTest$dfSmall=FieldTest$shrinkData(FieldTest$df)
    
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
