dataset = read.csv('data.csv')
##dataset = dataset[,2:25]
dataset$Level = factor(dataset$Level,
                       levels = c('Low','Medium','High'),
                       labels = c(1,2,3))
library(caTools)
set.seed(123)
split = sample.split(dataset$Level, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
test_set=test_set[-(1:3)]
training_set=training_set[-(1:3)]


##training_set[, 1:23] = scale(training_set[,1:23])
##test_set[, 1:23] = scale(test_set[,1:23]) 


##str(dataset)
library(nnet)
mmodel = multinom(Level~.,data = training_set)
summary(mmodel)

##ppredict = predict(mmodel,type = "prob",newdata = test_set[-22])
#cm = table(test_set[, 22],ppredict)
for(i in 1:21)
   {
        cc = readline(prompt = colnames(test_set[i]) )
        cc = as.integer(cc)
        test_set[201,i] = cc
}
ppredict = predict(mmodel,type = "prob",newdata = test_set[201,-22])
if(ppredict[1]>ppredict[2])
{if(ppredict[1]>ppredict[3])
{
  print("low")
}
}
if(ppredict[2]>ppredict[1])
{if(ppredict[2]>ppredict[3])
{
  print("medium")
  print("be aware of symptoms")
}
}
if(ppredict[3]>ppredict[1])
{if(ppredict[3]>ppredict[2])
{
  print("high")
  print("consult doctor immediately")
}
}
plot(ppredict) 

