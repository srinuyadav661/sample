---
title: "sammy project"
author: "katam samuel hinn lionel"
output: 
 html_document:
   code_folding: hide
---
## Data source

This is the source of my data:

https://www.kaggle.com/bravehart101/sample-supermarket-dataset

# Super store

The sample superstore dataset consists data ranging from the type of products sold, where it is sold and how they are shipped.

```{r Setup,include=FALSE}
library(tidyverse)
library(ggplot2)
library(readr)
library(plotly)
library(GGally)
library(psych)
library(knitr)
library(kableExtra)
library(treemapify)
library(highcharter)
library(scatterplot3d)
library(plotrix)
library(ggcorrplot)
library(scales)
library(tm)
library(wordcloud)
knitr::opts_chunk$set(
  fig.align = 'center',
  out.width = '90%',
  warning = FALSE,
  message = FALSE,
  tidy = FALSE
)
```

```{r Import}
super_store<-read_csv('superstore.csv')
```

```{r Type of variables,echo=FALSE}
super_store <- super_store %>%
  mutate(shipment=factor(shipment),
         segment=factor(segment),
         country=factor(country),
         city=factor(city),
         state=factor(state),
         region=factor(region),
         category=factor(category),
         sub_category=factor(sub_category))
head(super_store)%>%kbl()%>%kable_material(c("striped","hover"))%>%scroll_box(width = "100%", height = "400px")
```

This is a dataset containing a sample of 9994 transactions in superstore located in united states. The columns I used for my analysis are:

•	Shipment mode: We have 4 shipment modes – standard class, same day, first class, second class

•	Segment: We have three segments consumer, corporate and home office 

•	City: The city in which we deliver the products

•	Region: The region in which we deliver the products

•	Category: We have 3 categories furniture, office supplies and technology

•	Sub-category: Here we have 17 categorical variables nested inside the sub-category

•	Sales: Total amount for goods(Price) purchased for each transaction

•	Quantity: Total amount of goods sold(No.of goods) for each transaction 

•	Discount: discount applied for each transaction

•	Profit: profit the store gets for each transaction

## Visualizing data using segment 
### data preparation

```{r Seg barplot preparation}
visualization_segment<-ggplot(super_store)+
  geom_bar(aes(x=segment,
               fill=segment))+
  ggtitle("Visualization of data using segment")
```

### visualization

```{r Seg barplot visualization}
ggplotly(visualization_segment)
```

This plot shows number of sales(Transactions) for each segment. As we can see here the sales(transactions) made by consumer segment is more than the other two segments  

## Visualizing data using shipment and region
### data preparation 

```{r Ship/reg barplot preparation}
visualization_shipreg<-ggplot(super_store)+
  geom_bar(aes(x=region,
               fill=shipment))+
  ggtitle("visualization of data using shipment and region")
```

### visualization

```{r Ship/reg barplot visualization}
ggplotly(visualization_shipreg)
```

In this plot we can see that delivery of goods to region west is higher and the highest shipment mode used is standard class

## Visualizing data with density plot
### data preparation

```{r Density plot preparation}
vis_den<-ggplot(super_store,aes(x=quantity))+geom_density(fill='blue')
#+scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))
```

### visualization

```{r Density plot visualization}
vis_den
```

Here we have density curve for the quantity.As we can see the plot it peaks between o to 5. so, we can understand that maximum number of transactions were done between the number of goods between 0 to 5.

## Visualizing data with Jittering
### data preparation

```{r Jittering preparation}
vis_jit<-ggplot(super_store,aes(y=segment,x=profit))+geom_jitter(size=1.5,alpha=0.3)+scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))+theme_bw()
```

### visualization

```{r}
vis_jit
```

In this plot we can see a Jittering plot between segments and profit. I have used Jittering plot because for this plot almost all the values lie in the same interval and if I use point plot, may be it will hide data(Over plotting). As we can see the concentration of points in segment consumer is high and maximum part lies between 10^0 and 10^2 that means consumer segment has highest profit.

## Visualizing pie chart
### data preparation

```{r pie chart preparation}
pi<-table(super_store$category)
pi<-data.frame(pi,row.names = T)
```

### visualization

```{r pie chart visualization}
pie3D(pi$Freq,labels=row.names(pi),main='pie chart based on category')
```

The above pie chart represents sales in each category. We can see that office supplies category is higher.

## Visualizing data using sub category and sales
### data preparation

```{r lollipop preparation}
vis_lol=super_store%>%group_by(sub_category)%>%summarise(sales.m=mean(sales))%>%mutate(sub_category=fct_reorder(sub_category,sales.m))
visualization_subsa<-ggplot(vis_lol,aes(x=sub_category,y=sales.m))+geom_segment(aes(x=sub_category,xend=sub_category,y=0,yend=sales.m),color='skyblue')+geom_point(color="blue",size=5,alpha=0.4)+coord_flip()+scale_y_continuous(name='sales',labels=comma)
```

### visualization

```{r lollipop visualization}
ggplotly(visualization_subsa)
```

Here we can see lollipop plot with mean values of sales in each sub-category. As we can see from above copiers sub-category is making highest sales

## Visualizing violin plot 
### visualization

```{r violin plot preparation}
ggplot(super_store,aes(x=fct_reorder(category,sales),y=sales))+geom_violin(fill='light blue')+geom_boxplot(width=0.1,outlier.alpha = 0)+ggtitle("sales among category")+xlab("")+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))
```

Violin plot is a great concept that visualizes multiple information like density of the data as well as five number summary. As we can see from the above plot furniture category is having higher median and 3rd quartile is almost higher hence we can say that the customers of super store averagly spend more in furniture category.

## Analyzing the numeric variables in dataset

```{r numerical data}
sup_num<-psych::describe(super_store[,c("sales","quantity","discount","profit")])
sup_num%>%kbl()%>%kable_material(c("striped","hover"))%>%scroll_box(width = "100%", height = "300px")
```

From the above table we can find some numerical information of each numerical variable. we have 11 columns of numerical information like mean,median,minimum,maximum,etc...As we can see the table, minimum of sales is 0.444,maximum of slaes is 22,638.4,median of sales is 54.49,mean of sales is 229.8,minimum of discount is 0,maximum of discount is 0.80,median of discount is 0.20,mean of discount is 0.15,minimum of quantity is 1,maximum of quantity is 14,median of quantity is 3.0,mean of quantity is 3.7,minimum of profit is -6599.9,maximum of profit is 8399.9,median of profit is 8.6,mean of profit is 28.65.

## visualizing 2d bin
### data preparation

```{r 2d bin preparation}
geom_2d_bin<-ggplot(super_store,aes(x=sales,y=profit,fill=as.factor(category)))+geom_bin2d(color='black')
```

### visualization

```{r 2d bin visualization}
geom_2d_bin
```

we have 2d-bin for the variables sales and profit. As we can see the above plot we can understand that the average spend of customers is mostly on technology but in the previous pie chart, the number of transactions of office supplies category is higher this can give us a clear picture of transactions and sales(amount spend). When it comes to profit of categories we can say that there is no profit for the category furniture, office supplies has both profit and loss, when customers spend low amount in office supplies category, it results in loss and when customers spend high amount in office supplies category, store gains profit, Technology too has both profit and loss but it doesn't depend on the spend of customers.

## visualizing ggpairs()
### data preparation

```{r ggpairs()}
vis_ggpairs<-ggpairs(super_store[,c("sales","quantity","discount","profit")],
        title = 'correlogram with ggpairs()')
```

### visualization

```{r ggpairs() visualization}
vis_ggpairs
```

From the above plot we can see that diagonal plots are densities of respective variables mentioned above in the plot,lower triangular plots are point plots and upper triangular are correlations.

## visualizing corrplot
### data preparation

```{r corrplot preparation}
cor_dt<-cor(super_store[10:13])
vis_corr<-ggcorrplot(cor_dt,hc.order=T,method='circle')
```

### visualization

```{r corrplot visualization}
vis_corr
```

In this plot we can find correlation between any two numerical variable in the dataset. Correlation means the relation between two variables. It mainly depends on the direction and strength of the variables.In the above plot
smaller circles represents weakness between the two variables and bigger circles represents strength between two variables.colour represents direction between two variables.Red colour represents positive correlation and blue colour represents negative correlation.   

## Visualizing Tree map
### data preparation

```{r Tree map preparation}
tree_dt<-super_store%>%count(sub_category)
vis_tree<-ggplot(tree_dt,aes(fill=sub_category,area=n,label=sub_category))+
  geom_treemap()+geom_treemap_text(color='white',place = 'centre')
```

### visualization

```{r Tree map visualization}
vis_tree
```

Here we have treemap of our sub-categories. Having hierarchical structure seems good. Each rectangle’s area is proportional to the dimension of the data. So, the bigger rectangles represent the more sales for each sub-category. We can see that paper and binders have more sale in the super store.

## Visualizing Mosaic plot
### visualization

```{r mosaic plot}
mosaic_dt<-table(super_store$segment,super_store$shipment)
mosaicplot(mosaic_dt)
```

Mosaic plot is also called as cross tabs or two way table.By looking at this mosaic plot we understand that consumer segment and standard shipment mode are higher.

## Word cloud 
### data preparation

```{r word cloud preparation}
corpus<-Corpus(VectorSource(super_store$state))
corpus<-tm_map(corpus,content_transformer(tolower))
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,removeWords,stopwords("english"))
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,stripWhitespace)
tdm<-TermDocumentMatrix(corpus)
m<-as.matrix(tdm)
v<-sort(rowSums(m),decreasing = T)
d<-data.frame(word=names(v),freq=v)
```

### visualization

```{r}
wordcloud(d$word, d$freq,random.order=F,rot.per=0.3,scale=c(4,.5),max.words=200,colors = brewer.pal(8,"Dark2"))
```

The above word cloud is based on cities that store will deliver goods. Font size of the words in word cloud will describe the dimension of the variable.As we can see California is having higher sales.

## visualizing data according to segment, shipment, category and region
### visualization

```{r alluvial plot}
vis_allu<-super_store%>%select(segment,shipment,category,region)
hchart(data_to_sankey(vis_allu),"sankey",name="mixed outcomes")
```

Flows in above plot represents group of categorical variables so we can easily trace which categorical variable is going to which categorical variable.This is the summary of the data what we did all in the visualization.
