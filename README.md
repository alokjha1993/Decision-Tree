# Decision-Tree
Codes
```{r}
title: "Decision Trees"
author: "alok"
date: "2 May 2018"
install.packages("tree")
library(tree)
```


```{r}
adv<-read.csv("E:/Advertising.csv")
View(adv)
model_tree<-tree(sales~TV,data=adv)
model_tree

{{plot(model_tree)
  text(model_tree)}}
```

```{r}
cuts<-c()
mses<-c()
curr_mse<-c()
TV_uniqs<-sort(unique(adv$TV))
for(i in seq(1,length(TV_uniqs)-1)){
  curr_cut<-(TV_uniqs[i]+TV_uniqs[i+1])/2
  cuts<-c(cuts,curr_cut)
  samples_left<-adv%>%filter(TV<curr_cut)
  samples_right<-adv%>%filter(TV>curr_cut)
  avg_left<-mean(samples_left$sales)
  avg_right<-mean(samples_right$sales)
  adv$predicted_sales<-if_else(adv$TV<curr_cut,avg_left,avg_right)
  curr_mse<-sum((adv$sales-adv$predicted_sales)^2)/nrow(adv)
  mses<-c(mses,curr_mse)
}

models_perf<-data.frame(TV_cut=cuts,MSE=mses)
models_perf%>%arrange(MSE)%>%head(1)
```


### Which predictor is the best.(TV,Radio,Newspaper)
### And what is the best cut for the predictor.

## Decision trees with more than one input predictor.

```{r}
View(adv)
model=tree(sales~.,data = adv)
{{plot(model)
  text(model)}}
```
```{r}
TV_uniqs<-sort(unique(adv$TV))
length(TV_uniqs)
TV_uniqs[1:10]
```
```{r}
cuts_tv<-(TV_uniqs[1:length(TV_uniqs)-1]+
            TV_uniqs[2:length(TV_uniqs)])/2
length(cuts_tv)
```

```{r}
radio_uniqs<-sort(unique(adv$radio))
cuts_radio<-(radio_uniqs[1:length(radio_uniqs)-1]+
  radio_uniqs[2:length(radio_uniqs)])/2
length(cuts_radio)

```
```{r}
nwspaper_uniqs<-sort(unique(adv$newspaper))
cuts_nwsppr<-(nwspaper_uniqs[1:length(nwspaper_uniqs)-1]+
  nwspaper_uniqs[2:length(nwspaper_uniqs)])/2
length(cuts_nwsppr)
```

##Method
```{r}
cuts<-c(cuts_tv,cuts_radio,cuts_nwsppr)
predictors<-c(rep('TV',length(cuts_tv)),rep('radio',length(cuts_radio)),rep('newspaper',length(cuts_nwsppr)))
result<-data.frame(cut=cuts,predictor=predictors)
result
```

```{r}
library(dplyr)
cuts_mse<-c()
temp<-adv
for(i in seq(1,nrow(result))){
  cut=cuts[i]
  curr_col=predictors[i]
  samples_left=temp%>%filter_(curr_col<cut)
  samples_right=temp%>%filter_(curr_col>cut)
  pred_left=mean(samples_left$sales)
  pred_right=mean(samples_right$sales)
  temp$pred=ifelse(temp[,curr_col]<cut,pred_left,pred_right)
  curr_mse=sum((temp$sales-temp$pred)^2)/nrow(temp)
  cuts_mse=c(cuts_mse,curr_mse)
}
```
```{r}
a<-var(temp$sales)
a
```

```{r}
setwd("E:/Machine Learning")
hr<- read.csv("E:/Machine Learning/HR Analytics.csv", 
              header=TRUE)
View(hr)
hr_train<-hr[1:(0.7*nrow(hr)),]
hr_test<-hr[(0.7*nrow(hr)+1):nrow(hr),]
hr_test
hr_train
model=tree(Attrition~.,data = hr_train)
model
{{plot(model)
  text(model)}}
```

```{r}
m1<-tree(Attrition~OverTime+Gender,data=hr_train)
{{plot(m1)
  text(m1)}}

install.packages("rattle")
library(rattle) 
                                 
```



