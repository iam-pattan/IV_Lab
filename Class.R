library(ggplot2)

dataset <- read.csv('C:\\Users\\Afrid\\Documents\\R\\titanic.csv')

data <- na.omit(dataset)

### males in the ship
males <- data$Sex == 'male'

males

males_data <- data[males,]

### females in the ship
females <- dataset$Sex == 'female'

females_data <- data[females,]


ggplot(data, aes(x=Sex, fill=Sex))+geom_bar()+ggtitle("Male and Female in Ship")

ggplot(data, aes(x=Pclass, fill=as.factor(Survived)))+geom_bar()+ggtitle("Class Distribution in Ship")

ggplot(data, aes(x=Sex, fill=as.factor(Survived)))+geom_bar()+ggtitle("Male and Female in Ship who survived")

ggplot(data, aes(x= SibSp, fill=as.factor(Survived)))+geom_bar()+labs(y="Passenger Count" , title="Titanic Surviral by Numbers of Siblings or Spouse")

# histogram
ggplot(data, aes(x=Age, fill=Age))+geom_histogram(color="blue",binwidth=2)+ggtitle('Age distribution')
# Scatter
ggplot(data, aes(x=PassengerId, y=Fare))+geom_point(size=1, shape=22)+ggtitle('relationship between Passenger and Fare')
# hist_multi
ggplot(data, aes(x = Age, fill = Survived)) + geom_histogram(color="blue") + theme_bw() + facet_wrap(Sex ~ Pclass)+ labs(x = "survival", y = "Age", title = "Survival by Age, Sex and Class")
# pie_Chart
ggplot(data, aes(x="", fill=Embarked))+geom_bar(width = 0.5)+coord_polar("y")+ggtitle('Board into Ship')



##############LAB-2(KMeans)

dataset1 <- read.csv('C:\\Users\\Afrid\\Documents\\R\\computers.csv')
data <- na.omit(dataset1)
# View(data)
str(data)
summary(data)
head(data)

data.new <- data[,-c(1,3,7,8,9)]
head(data.new)
data.new <- data[-c(1,3,7,8,9),]
head(data.new)
data.class <- data[,c(3)]
head(data.class)

normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}
data.new$price<- normalize(data.new$price)
data.new$hd<- normalize(data.new$hd)
data.new$ram<- normalize(data.new$ram)
data.new$screen<- normalize(data.new$screen)
data.new$ads<- normalize(data.new$ads)
data.new$trend<- normalize(data.new$trend)
data.new1 <- data.new[,c(1,2,3,4,5,6,10)]
data.new1
head(data.new)

#data.new1 <- na.omit(data.new)
result <- kmeans(data.new1,4)
result$size
result$centers
result$cluster
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(data.new[c(2, 3)], col=result$cluster)
plot(data.new[c(2, 3)], col=data.class)
plot(data.new[c(1, 4)], col=result$cluster)
plot(data.new[c(1, 4)], col=data.class)

#plot(data.new[,], col=result$cluster)

result$cluster <- as.factor(result$cluster)

library(ggplot2)
ggplot(data.new1, aes(hd, ram), color = result$cluster) + geom_point()

ggplot(data.new1, aes(price, screen, color = result$cluster)) + geom_point()

table(result$cluster, data.class)

# install.packages("animation")	
library(animation)
kmeans.ani(data.new, 4)


kmeans.ani(data.new[c(2,3)], 4)


# install.packages("factoextra")
library(factoextra)
fviz_cluster(result, data = data.new1)
#result
k2 <- kmeans(data.new1, centers = 2, nstart = 25)
k3 <- kmeans(data.new1, centers = 3, nstart = 25)
k4 <- kmeans(data.new1, centers = 4, nstart = 25)
k5 <- kmeans(data.new1, centers = 5, nstart = 25)
k6 <- kmeans(data.new1, centers = 6, nstart = 25)
k7 <- kmeans(data.new1, centers = 7, nstart = 25)
p1 <- fviz_cluster(k2, geom = "point", data = data.new1) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = data.new1) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = data.new1) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = data.new1) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point", data = data.new1) + ggtitle("k = 6")
p6 <- fviz_cluster(k7, geom = "point", data = data.new1) + ggtitle("k = 7")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2) 



############LAB-3(Association Rules)

library(arulesViz)
library(arules)
data <- read.csv('C:\\Users\\Afrid\\Documents\\R\\10_medication_descriptions.csv')
rules <- apriori(data, parameter=list(support=0.008, confidence=0.5))
rules
plot(rules, jitter = 1)

try_sel <- plot(rules, interactive=TRUE)
## Two-key plot is a scatterplot with shading = "order"
plot(rules, shading="order", control=list(main = "Two-key plot", col=rainbow(5)), jitter=0)

subrules <- subset(rules, lift>2.5)
subrules                                          
plot(subrules, method="matrix", measure="lift")                                          


plot(subrules, method="matrix3D", measure="lift")

plot(rules, method="grouped")


# graphs only work well with very few rules
subrules2 <- sample(rules, 10)
plot(subrules2, method="graph")

plot(rules, method = "grouped", control = list(k = 100))


plot(subrules2, method="graph", control=list(layout=igraph::in_circle()))


plot(subrules2, method="graph", control=list(
  layout=igraph::with_graphopt(spring.const=5, mass=200)))


plot(subrules2, method="graph", control=list(type="itemsets"))

plot(subrules2, method = "paracoord")



plot(subrules2, method = "paracoord", control = list(reorder = TRUE))

oneRule <- sample(rules, 1)
inspect(oneRule)

plot(oneRule, method ="doubledecker", data = data)
itemsets <- eclat(data, parameter = list(support = 0.02, minlen=2))
plot(itemsets)

plot(itemsets, method="graph")

itemsets <- eclat(data, parameter = list(support = 0.03, minlen=2))
plot(itemsets, method="graph")

itemsets <- eclat(data, parameter = list(support = 0.04, minlen=2))
plot(itemsets, method="graph")

plot(itemsets, method="paracoord", control=list(alpha=.5, reorder=TRUE))

quality(itemsets) <- interestMeasure(itemsets, trans=data)
head(quality(itemsets))


plot(itemsets, measure=c("support", "allConfidence"), shading="lift",control = list(col=rainbow(7)))

rules<-apriori(data=data, parameter=list(supp=0.001,conf = 0.15,minlen=2),
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")

inspect(rules[1:5])

library(arulesViz)
plot(rules,method="graph",engine = 'default',shading=NA)

rules<-apriori(data=data, parameter=list(supp=0.005,conf = 0.08),
               appearance = list(default="lhs",rhs="butter"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

library(arulesViz)
plot(rules,method="graph",engine = 'default',shading=NA)
data_ <- data.frame(data)
itemFrequencyPlot(data_,topN=20,type="absolute")


############ Lab-4(Lattice plot)

library(lattice)
hsb2 <- read.csv("C:\\Users\\Afrid\\Documents\\R\\hsb2.csv")
attach(hsb2)
#defining ses.f to be a factor variable
hsb2$ses.f = factor(hsb2$ses, labels=c("low", "middle", "high")) 

#histograms
histogram(~write, hsb2)
#conditional plot
histogram(~write | ses.f, hsb2)

#conditional plot
densityplot(~socst | ses.f, hsb2)
densityplot(~socst, hsb2)

#Quantile-quantile plots
qqmath(~write, hsb2)
#conditional plot
qqmath(~write | ses.f, hsb2)

#Box and whiskers plots
bwplot(~math, hsb2)
#conditional plot
bwplot(ses.f~math, hsb2)

#Scatter plots
xyplot(write~math, hsb2)
#conditional plot
xyplot(write~math | ses.f, hsb2)


#Scatter plot matrices
subset <- hsb2[ , 8:12] 
splom(~subset[ , 1:4])
#conditional plot
splom(~subset[, 1:3] | subset[, 5])

# 3d scatterplot by factor level
cloud(write~math*science, hsb2)

cloud(write~math*science, hsb2,
      group = race,
      auto.key = TRUE)

# 3d scatterplot by factor level
cloud(write~math | ses.f, data=hsb2)
contourplot(write~math | ses.f, data=hsb2)

#Regression analysis
reg <- lm(write~math+socst+ses.f, hsb2)
par(mfrow=c(3,2))
plot(reg, which=1:2)
plot(reg, which=3:4)
plot(reg, which=5:6)



##############Lab-5
setwd('C:\\Users\\Afrid\\Documents\\R')

library(igraph)

g<-graph.empty(n=10, directed=TRUE)
g<-graph.full(n=10, directed = FALSE, loops = FALSE)

g<-graph.star(n=10, mode="out")
g<-graph.star(n=10, mode="in")

g<-graph.ring(n=10)

edges <- c(1,2,3,2,2,4)
g<-graph(edges, n=max(edges), directed=TRUE)

edges <- c(1,2,3,2,2,4)
g<-graph(edges, n=max(edges), directed=TRUE)
vcount(g)

ecount(g)
neighbors(g, V(g)[1], mode = 1)
incident(g,V(g)[2], mode=c("all", "out", "in", "total"))
is.directed(g)
are.connected(g, V(g)[1], V(g)[3])
get.edgelist(g)

V(g) # vertex sequence
E(g, P=NULL, path=NULL, directed=TRUE) # edge sequences

# add.edges(graph, edges, ..., attr=list())
# add.vertices(graph, vertices, ..., attr=list())

pdf("Graph.pdf")
plot(g)
dev.off()

setwd('C:\\Users\\Afrid\\Documents\\R')
#import
g <- read.graph("graph.txt", format="edgelist")
g <- read.graph("graph.dl", format="pajek")
# advice_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Advice')
g <- graph.data.frame(advice_data_frame)

######################

dat=read.csv(file.choose(),header=TRUE,sep=',', row.names=1,check.names=FALSE)
m=as.matrix(dat)
net=graph.adjacency(m,mode="directed",weighted=TRUE,diag=FALSE)
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold,
            edge.arrow.size=0.5)

#export
write.graph(g, file='my_graph.dl', format="pajek")
write.graph(g, file='my_graph.txt', format="edgelist")


e <- read.csv(file ="C:\\Users\\Afrid\\Documents\\R\\Lab5-Dataset_Edges.csv")
a <- read.csv(file ="C:\\Users\\Afrid\\Documents\\R\\Lab5-DataSet_Adjacency.csv")
m1=as.matrix(e)
m2=as.matrix(a)

net=graph.adjacency(m1,mode="directed",weighted=TRUE,diag=FALSE)
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold,
            edge.arrow.size=0.5)

net=graph.adjacency(m2,mode="directed",weighted=TRUE,diag=FALSE)
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold,
            edge.arrow.size=0.5)
m1
m2



##########Lab-6(Trees)
mydata<-read.csv('C:\\Users\\Afrid\\Documents\\R\\iris.csv')
names(mydata)

# Check attributes of data
str(mydata)

#Check number of rows and columns
dim(mydata)

#Make dependent variable as a factor (categorical)
mydata$petal.length = as.factor(mydata$petal.length)

#Splitting dataset
dt = sort(sample(nrow(mydata), nrow(mydata)*.7))
train<-mydata[dt,]
val<-mydata[-dt,]
nrow(train)
nrow(val)

# Decision Tree Implementation
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

#trees formation
mtree <- rpart(variety~petal.width, data = train, method="class", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))
mtree

#Plot tree
plot(mtree)
text(mtree)

par(xpd = NA, mar = rep(0.7, 4))
plot(mtree, compress = TRUE)
text(mtree, cex = 0.7, use.n = TRUE, fancy = FALSE, all = TRUE)

#view1
prp(mtree, faclen = 0,box.palette = "Reds", cex = 0.8, extra = 1)

#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}
prp(mtree, faclen = 0, cex = 0.8, node.fun=tot_count)

printcp(mtree)

bestcp <- mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]
# Prune the tree using the best cp.
pruned <- prune(mtree, cp = bestcp)
#Plot pruned tree
prp(pruned, box.palette = "Blues",faclen = 0, cex = 0.8, extra = 1)

#confusion matrix for decision tree
conf.matrix <- table(train$variety, predict(pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

val
val1[,2]
#Scoring
library(ROCR)
val1 = predict(pruned, val, type = "prob")
#Storing Model Performance Scores
pred_val <-prediction(val1[,2],val$variety)
# Calculating Area under Curve
perf_val <- performance(pred_val,"auc")
perf_val

# Plotting Lift curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, "tpr", "fpr")
#Plot the ROC curve
plot(perf_val, col = "green", lwd = 1.5)

#Calculating KS statistics
ks1.tree <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
ks1.tree

########################LAB-8,9
# SCRIPT_INT('
# set.seed(42);
# result <- kmeans(data.frame(.arg1,.arg2,.arg3,.arg4), 3);
# result$cluster;',
#            
#            SUM([Children]), 
#            SUM([Income]),
#            SUM([Cars]),
#            SUM([Age]))
# 
# 
# SCRIPT_INT("
# set.seed(42);
# result <- lm(.arg1~.arg2)
# result",
#            SUM([Temp]),
#            SUM([Number of Records])
# )

##################lab-9
library(Rserve)
Rserve()








