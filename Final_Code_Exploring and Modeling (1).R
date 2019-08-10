library(tidyverse)
library(ggthemes)
library(tidytext)
library(lexicon)
library(cluster)
library(mclust)
library(RColorBrewer)
library(factoextra)
library(caret)
library(car)
library(ranger)
library(gbm)


data = read.csv("clean_movie_data_final.csv")

data$budget = as.numeric(data$budget)
data$genres = as.character(data$genres)
data$cast = as.character(data$cast)
data$keywords = as.character(data$keywords)
data$original_title = as.character(data$original_title)
data$overview = as.character(data$overview)
data$production_companies = as.character(data$production_companies)
data$release_date = as.Date(data$release_date)
data$spoken_languages = as.character(data$spoken_languages)
data$tagline = as.character(data$tagline)
data$title = as.character(data$title)
data$vote_count = as.numeric(data$vote_count)

#Adding new columns for year and seasonality
data$year = as.numeric(format(data$release_date, format = "%Y"))
data$month = as.numeric(format(data$release_date, format = "%m"))
spring = c(3,4,5)
summer = c(6,7,8)
fall = c(9,10,11)
winter = c(12,1,2)
data$release_season = ifelse(data$month %in% spring, 'spring',
                             ifelse(data$month %in% summer, 'summer',
                                    ifelse(data$month %in% fall, 'fall',
                                           ifelse(data$month %in% winter, 'winter',NA))))


data$release_season = as.factor(data$release_season)
data$year = as.factor(data$year)

data = data %>% 
  select(-month)

#Exploring vote_average column
ggplot(data, aes(x = "", y = vote_average)) + 
  geom_boxplot() 

#Removing all rows with missing values in vote_average - ambiguous data
data = data %>% 
  filter(vote_average != 0)

data = data %>% 
  filter(vote_average <8)

data = data %>% 
  filter(vote_average >2)

# Splitting the genres column into primary genre and subgenres
data = data %>% 
  separate(genres, into = c('primary_genre','subgenres'),sep = ',',extra = "merge")

data$primary_genre = as.factor(data$primary_genre)

# Assigning 'none' to movies with no subgenres
sum(is.na(data$subgenres))
data[is.na(data$subgenres),]$subgenres = 'none'

###### SENTIMENT ANALYSIS
##
# Getting Bing Sentiments 
as.data.frame(get_sentiments('bing'))[,]

# Bing sentiments for keywords
bing_data <- data%>%
  select(id, keywords)%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)

# Bing Sentiment Frequencies
data%>%
  select(id, keywords)%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()

# Bing Sentiment Frequency Plot for keywords 
data%>%
  select(id, keywords)%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment))+
  geom_col()+ 
  guides(fill = F)+
  coord_flip()

# Bing Sentiments Proportions (%) for keywords
data%>%
  select(id, keywords)%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)*100)

# Bing Negative Sentiment Proportions for each movie's keywords
keywords_bing_sentiments <- data %>% 
  group_by(id) %>% 
  unnest_tokens(output = word, input = keywords) %>% 
  inner_join(get_sentiments('bing'))%>%
  group_by(id)%>%
  summarize(negativity = sum(sentiment =='negative')/n())

# Bing Sentiment Proportions for Genre - genre not very insightful so we decided to focus on keywords instead
genre_bing_sentiments <- data %>% 
  select(id, keywords, primary_genre) %>% 
  group_by(id) %>% 
  unnest_tokens(output = word, input = keywords) %>% 
  ungroup() %>%
  inner_join(get_sentiments('bing')) %>% 
  group_by(primary_genre, sentiment) %>% 
  summarize(n = n()) %>% 
  mutate(proportion = n/sum(n))

# Categorising bing sentiment proportions for keywords as positive, negative or neutral
keywords_bing_sentiments$negativity[keywords_bing_sentiments$negativity > 0.5] <- 'negative'
keywords_bing_sentiments$negativity[keywords_bing_sentiments$negativity < 0.5] <- 'positive'
keywords_bing_sentiments$negativity[keywords_bing_sentiments$negativity == 0.5] <- 'neutral'

# Merging bing sentiments for each movie with original data - bing had too many missing values so we decided to test nrc
data_bing_sentiments = merge(data, keywords_bing_sentiments,
                             by = 'id')
View(data_bing_sentiments)

# Getting nrc sentiments
nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',header = F,col.names = c('word','sentiment','num'),sep = '\t'); nrc = nrc[nrc$num!=0,]; nrc$num = NULL
levels(nrc$sentiment)

#Converting nrc sentiments to 2 levels - positive & negative
nrc$sentiment[nrc$sentiment == 'anger'] = c('negative')
nrc$sentiment[nrc$sentiment == 'anticipation'] = c('negative')
nrc$sentiment[nrc$sentiment == 'disgust'] = c('negative')
nrc$sentiment[nrc$sentiment == 'fear'] = c('negative')
nrc$sentiment[nrc$sentiment == 'joy'] = c('positive')
nrc$sentiment[nrc$sentiment == 'sadness'] = c('negative')
nrc$sentiment[nrc$sentiment == 'surprise'] = c('positive')
nrc$sentiment[nrc$sentiment == 'trust'] = c('positive')
nrc$sentiment <- factor(nrc$sentiment)

# nrc Sentiments Frequencies for keywords
data%>%
  select(id, keywords)%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()

# nrc Sentiments Frequency Plot for keywords
data%>%
  select(id, keywords)%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment))+
  geom_col()+ 
  guides(fill = F)+
  coord_flip()

# nrc Sentiments Proportions for keywords (%) 
data%>%
  select(id, keywords)%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)*100)

# nrc Negative Sentiment Proportions for each movie's keywords
keywords_nrc_sentiments <- data %>% 
  group_by(id) %>% 
  unnest_tokens(output = word, input = keywords) %>% 
  inner_join(nrc)%>%
  group_by(id)%>%
  summarize(negativity = sum(sentiment =='negative')/n())

# Categorising nrc sentiment proportions for keywords as positive, negative or neutral
keywords_nrc_sentiments$negativity[keywords_nrc_sentiments$negativity > 0.5] <- 'negative'
keywords_nrc_sentiments$negativity[keywords_nrc_sentiments$negativity < 0.5] <- 'positive'
keywords_nrc_sentiments$negativity[keywords_nrc_sentiments$negativity == 0.5] <- 'neutral'

# Merging original data with nrc sentiments for keywords
data = merge(data, keywords_nrc_sentiments,
             by = 'id')

# Renaming the "negativity" column in the dataframe 
names(data)[names(data) == 'negativity'] <- 'sentiment'

data$sentiment = as.factor(data$sentiment)

# The final dataframe has 763 rows less, as not all words were found in the nrc dictionary
nrow(data)
View(data)

########
## CLUSTERING
########
#vote_average is the base factor in clustering. We will subset these variables. 

data_cluster = data[,c("vote_average")]
#we are using the scale() which will by default subtract mean and divide by standard deviation. 
data_cluster = scale(data_cluster)

#Hierarchical Cluster Analysis
d = dist(x = data_cluster,method = 'euclidean')  
#Similarity is generally measured using a distance-based measure (as opposed to a correlational measure). Here, we will make use of Euclidean distance to assess similarity. 
clusters = hclust(d = d,method='ward.D2')
#plot compare 2 vs 3 vs 4 
plot(clusters)
rect.hclust(tree=clusters,k = 2,border='tomato')
plot(clusters)
rect.hclust(tree=clusters,k = 3,border='tomato')
#compare 4 clusters
plot(clusters)
rect.hclust(tree=clusters,k = 4,border='tomato')
#compare 5 clusters
plot(clusters)
rect.hclust(tree=clusters,k = 5,border='tomato')
#Number of clusters may be determined by examining the height between branch splits. A five-cluster solution seems to be best. Most of the respondents seem to be clustering into three groups. Based on these considerations, we cut the data into five clusters.
h_segments_2 = cutree(tree = clusters,k=2)
table(h_segments_2)
h_segments_3 = cutree(tree = clusters,k=3)
table(h_segments_3)
h_segments_4 = cutree(tree = clusters,k=4)
table(h_segments_4)
h_segments_5 = cutree(tree = clusters,k=5)
table(h_segments_5)

#Result we choose 5 clusters
h_segments=h_segments_5

##Now, lets use K- mean cluster see if we get same result
#K-means Clustering
set.seed(617)
km_2 = kmeans(x = data_cluster,centers = 2)
# of observation
table(km_2$cluster)
km_3 = kmeans(x = data_cluster,centers = 3)
# of observation
table(km_3$cluster)
# now compare to 4 centers
km_4 = kmeans(x = data_cluster,centers = 4)
# now compare to 5 centers
km_5 = kmeans(x = data_cluster,centers = 5)
# of observation
table(km_5$cluster)
#k-means examines sum of squared distances in evaluating cluster solutions. The total sum of squares is the sum of total within-cluster sum of squares and the between cluster sum of squares
paste(km_2$totss,'=',km_2$betweenss,'+',km_2$tot.withinss,sep = ' ')
paste(km_3$totss,'=',km_3$betweenss,'+',km_3$tot.withinss,sep = ' ')
paste(km_4$totss,'=',km_4$betweenss,'+',km_4$tot.withinss,sep = ' ')
paste(km_5$totss,'=',km_5$betweenss,'+',km_5$tot.withinss,sep = ' ')

#Total within sum of squares Plot
#Compute total within sum of squares for a number of values of k. Plot a line graph of k (on x-axis) against total within sum of squares (on y-axis). Ideal number of clusters is inferred from a sudden change in the line graph or what is commonly known as the “elbow”.
#To construct the plot, we will get the total within sum of squares for a set of 10 cluster solutions.

within_ss = sapply(1:10,FUN = function(x) kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)$tot.withinss)
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#3 clusters was recommended for k-means clustering
k_segments=km_3$cluster

## Model based cluster
#Key idea of model-based clustering is that observations come from groups with different statistical distributions 
clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)
#2_cluster
clusters_mclust_2 = Mclust(data_cluster,G=2)
summary(clusters_mclust_2)

#3_cluster
clusters_mclust_3 = Mclust(data_cluster,G=3)
summary(clusters_mclust_3)
#4_cluster
clusters_mclust_4 = Mclust(data_cluster,G=4)
summary(clusters_mclust_4)

#5_cluster
clusters_mclust_5 = Mclust(data_cluster,G=5)
summary(clusters_mclust_5)

#BIC 2
clusters_mclust_2$bic
#BIC 3
clusters_mclust_3$bic
#vs BIC 4
clusters_mclust_4$bic
#plot
mclust_bic = -sapply(1:10,FUN = function(x) Mclust(data_cluster,G=x)$bic)
mclust_bic
ggplot(data=data.frame(cluster = 1:10,bic = mclust_bic),aes(x=cluster,y=bic))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
#we choose 3 cluster, as the graph suggest 2 & 3 clusters has lowest BIC, and 3 clusters agrees with the other 2 methods.
m_segments = clusters_mclust_3$classification
table(m_segments)

##Now compare the 3 models
table(h_segments)
table(k_segments)
table(m_segments)

###now we combine the segments with original data#####
data = cbind(data,h_segments, k_segments,m_segments)

##examine the profiles with k segments and h segments
#Use k-means segments to examine means of each needs-based variable
data %>%
  select(vote_average,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

#Use h segments to examine means of each variable
data %>%
  select(vote_average,h_segments)%>%
  group_by(h_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

#examine by primary_genre the %
prop.table(table(data$h_segments,data[,c("primary_genre")]),1)
# result meanly that action, comedy & drama has high percentage.
# movies in the cluster 2 has lower percentage on action & comedy --> where action comedy movies has lower average voting
#movies in the drama genre has higher average voting → which fall under cluster 2


#graphic plot for cluster interpretation

tab = prop.table(table(data$h_segments,data[,c("primary_genre")]),1)
tab2 = data.frame(round(tab,2))
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=2)+
  xlab(label = '')+
  ylab(label = '')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))

#transforming clusters to factor variables
data$h_segments = as.factor(data$h_segments)
data$k_segments = as.factor(data$k_segments)
data$m_segments = as.factor(data$m_segments)

####### MODELING
#####################
#selecting columns for modelling 
cols_to_delete = c("id",
                   "subgenres",
                   "keywords",
                   "original_title",
                   "overview",
                   "status",
                   "tagline",
                   "title",
                   "cast",
                   "production_companies",
                   "release_date",
                   "spoken_languages",
                   "with_overview",
                   "k_segments",
                   "m_segments") #taking out overview because all have it aside from 1

clean_data = data %>%
  select(-cols_to_delete)

#########
## FEATURE SELECTION
#########
set.seed(1031)
split = createDataPartition(y = clean_data$vote_average,
                            p = 0.70,
                            list = F,
                            groups = 194)

train = clean_data[split, ]
test = clean_data[-split, ]

mean(train$vote_average)
mean(test$vote_average)

## Forward Stepwise
start_mod = lm(vote_average ~ 1, data=train)
empty_mod = lm(vote_average ~ 1, data=train)
full_mod = lm(vote_average ~ ., data=train)

forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,
                                  lower=empty_mod),
                       direction='forward')

summary(forwardStepwise)


set.seed(100)
tree_ranger = ranger(vote_average ~ h_segments + primary_genre + popularity + runtime + 
                       budget + with_tagline, data = train, num.trees = 300, write.forest = TRUE, max.depth = 20, min.node.size = 10, importance = 'impurity')

pred_tree_ranger = predict(tree_ranger,train)
pred_vote_average = data.frame(vote_average=pred_tree_ranger$predictions,row.names=NULL)
rmse_ranger = sqrt(mean((pred_vote_average$vote_average-train$vote_average)^2)); rmse_ranger

pred_tree_ranger = predict(tree_ranger,test)
pred_vote_average = data.frame(vote_average=pred_tree_ranger$predictions,row.names=NULL)
rmse_ranger = sqrt(mean((pred_vote_average$vote_average-test$vote_average)^2)); rmse_ranger

importance(tree_ranger) #variable importance

## Backward Stepwise
start_mod = lm(vote_average ~ 1, data=train)
empty_mod = lm(vote_average ~ 1, data=train)
full_mod = lm(vote_average ~ ., data=train)

backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,
                                   lower=empty_mod),
                        direction='backward')

summary(backwardStepwise)

## Hybrid Stepwise
start_mod = lm(vote_average ~ 1, data=train)
empty_mod = lm(vote_average ~ 1, data=train)
full_mod = lm(vote_average ~ ., data=train)

hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,
                                 lower=empty_mod),
                      direction='both')

summary(hybridStepwise)

## RANGER
set.seed(100)
tree_ranger = ranger(vote_average ~., data = train, num.trees = 300, write.forest = TRUE, max.depth = 20, min.node.size = 10, importance = 'impurity')

pred_tree_ranger = predict(tree_ranger,train)
pred_vote_average = data.frame(vote_average=pred_tree_ranger$predictions,row.names=NULL)
rmse_ranger = sqrt(mean((pred_vote_average$vote_average-train$vote_average)^2)); rmse_ranger

pred_tree_ranger = predict(tree_ranger,test)
pred_vote_average = data.frame(vote_average=pred_tree_ranger$predictions,row.names=NULL)
rmse_ranger = sqrt(mean((pred_vote_average$vote_average-test$vote_average)^2)); rmse_ranger


## RANGER CV
fit_control = trainControl(## 10-fold CV
  method = "cv",
  number = 10)
rf_grid = expand.grid(mtry = c(15,16,17,18),
                      splitrule = c("extratrees"),
                      min.node.size = 10)
rf_grid

set.seed(100)
rf_fit <- train(vote_average ~ ., 
                data = train, 
                method = "ranger",
                trControl = fit_control,
                # provide a grid of parameters
                tuneGrid = rf_grid)
rf_fit

pred_rf_ranger_train = predict(rf_fit,train)
rmse_rf_ranger_train = sqrt(mean((pred_rf_ranger_train-train$vote_average)^2)); rmse_rf_ranger_train

pred_rf_ranger = predict(rf_fit,test)
rmse_rf_ranger = sqrt(mean((pred_rf_ranger-test$vote_average)^2)); rmse_rf_ranger


## BOOSTING
set.seed(100)
boost= gbm(vote_average~h_segments + popularity + runtime + budget + vote_count + 
             num_keywords + with_tagline+ top_paid_actor+release_season,data=train,distribution="gaussian",
           n.trees = 100,interaction.depth = 5,shrinkage = 0.1)

predBoosttrain= predict(boost,n.trees = 100, train)
rmse_boost_train = sqrt(mean((predBoosttrain-train$vote_average)^2)); rmse_boost_train

predBoosttest= predict(boost,n.trees = 100, newdata = test)
rmse_boost = sqrt(mean((predBoosttest-test$vote_average)^2)); rmse_boost

#### PCA
library(caret)
trainPredictors = train[,c(0:8,10:17)]
testPredictors = test[,c(0:8,10:17)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$vote_average = train$vote_average

train_model = lm(vote_average~.,trainComponents)
summary(train_model)

testComponents = predict(x,newdata=testPredictors)
testComponents$vote_average = test$vote_average

model_pca = lm(vote_average~h_segments+primary_genre+production_countries+release_season+PC1+PC2+PC4,data=trainComponents)
pred_pca_train=predict(model_pca)
rmse_pca_train = sqrt(mean((pred_pca_train-train$vote_average)^2)); rmse_pca_train

pred_pca_test=predict(model_pca,newdata=testComponents)
rmse_pca_test = sqrt(mean((pred_pca_test-test$vote_average)^2)); rmse_pca_test

