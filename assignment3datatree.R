library(data.tree)
library( readr )
library(rpart)
mushrooms <- read.csv("mushrooms.csv")
#print(mushropoms)
#data(mushrooms)

IsPure<-function(data){
  
  length(unique(data[,1])) == 1
} 
#print(IsPure(mushrooms))

Entropy<-function(vl){
  en <- vl/sum(vl) * log2(vl/sum(vl))
  en[vl == 0] <-0 #asign Nan
  -sum(en)
}
print(Entropy(table(mushrooms[,ncol(mushrooms)])))

InformationGain<-function(tble){
  
  tble<- as.data.frame.matrix(tble)
  enBefore<- Entropy(colSums(tble))
  s <- rowSums(tble)
  enAfter <- sum(s/sum(s)*apply(tble, MARGIN = 1, FUN = Entropy))#apply = for loop
  informationGain <- enBefore - enAfter
  return(informationGain)
}
#print(InformationGain(table(mushrooms[,c('odor','class')])))
TrainID3 <- function(node, data) {
  node$obsCount <- nrow(data)
  if (IsPure(data)) {
    child <- node$AddChild(unique(data[,1]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    ig <- sapply(colnames(data)[-1], 
                 function(x) InformationGain(
                   table(data[,x], data[,1])
                 )
    )
    feature <- names(ig)[ig == max(ig)][1]
    node$feature <- feature
 
    childObs <- split(data[,!(names(data) %in% feature)], 
                      data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      
      child <- node$AddChild(names(childObs)[i])   
      TrainID3(child, childObs[[i]])
    }
  }
}


Predict <- function(tree,feature){
  if(tree$children[[1]]$isLeaf)
    return(tree$children[[1]]$name)
  child <- tree$children[[feature[[tree$feature]]]]
  return(Predict(child,feature))
}
tree <- Node$new("")
TrainID3(tree,mushrooms)
print(tree,"feature","obsCount")



print(Predict(tree, c(cap.shape='x', cap.surface='s', cap.color='n', bruises='t', odor='p',  gill.attachment='NA',
                gill.spacing='c', gill.size='n', gill.color='k', stalk.shape='e', stalk.root='NA', stalk.surface.above.ring='NA', 
                stalk.surface.below.ring='s', talk.color.above.ring='w',  stalk.color.below.ring='w', veil.type='p', veil.color='w', ring.number='o',
                ring.type='p', spore.print.color='k', population='s', habitat='u')))
