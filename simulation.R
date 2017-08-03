source("fieldClass.R")
# Simulation
fieldSim <- function(reps=100, k=NULL,altM, altF, naltM,naltF, m, l){
  
  
  FieldTest <- Field$new(altM=altM,altF=altF,
                         naltM=naltM,naltF=naltF,
                         mature=m, k=k,
                         litter=l)
  
  for(i in 1:reps){
    FieldTest$stepUp()
    #FieldTest$getPops(FieldTest$df)
    #FieldTest$df=FieldTest$shrinkData(FieldTest$df)
    #summary function
    
    #populations
    
    #Eagle Attack
    #FieldTest$eagleAttack()
    
    #kill elderly
    FieldTest$culling()
    FieldTest$shrinkData()
    
    #Reproduction Phase
    pairs=FieldTest$pickMates()
    if(!is.null(pairs)){
      Map(FieldTest$reproduce,x=pairs$females,y=pairs$male)
    }
  }
  
  View(FieldTest$df)
  #print(FieldTest$relMat)
  #FieldTest$graphPops()
}
