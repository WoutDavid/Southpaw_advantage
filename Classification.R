############################################
## Classification of pitching metric data ##
############################################

#The idea of this script is to see if i can make a supervised classifier that can tell the difference between a left handed
#or right handed pitcher based on their pitch metrics.
pitchers <- read.csv("data/baseballsavant_2019.csv")
for(i in 1:ncol(pitchers)) {   
  print(sum(is.na(pitchers[,i])))
}
pitchers <- subset(pitchers, select=-X)
pitchers <- na.omit(pitchers)
table(pitchers$pitch_hand)

###########
## TREES ##
###########
attach(pitchers)
library(rpart)
library(rpart.plot)
rpart.tree<-rpart(pitch_hand~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers,method="class")
rpart.plot(rpart.tree, yesno=TRUE)
printcp(rpart.tree)
plotcp(rpart.tree)

rpart.pred <- predict(rpart.tree,type="class")
table(pitchers$pitch_hand, rpart.pred, dnn=c("From","Classified into"))
##alright alright this tree is starting to do a lil better: 38-34

##checking overfitting by applying this tree on the pitching dataset of 2020
pitchers_2020 <- read.csv("data/baseballsavant_2020.csv")
##how many Left and Right handed pitchers are there actually
pitchers_2020 <- subset(pitchers_2020, select=-X)
pitchers_2020 <- na.omit(pitchers_2020)
rpart.pred <- predict(rpart.tree, pitchers_2020,type="class")
table(pitchers_2020$pitch_hand, rpart.pred, dnn=c("From","Classified into"))

##################################
## Attempt at boosting the tree ##
##################################
library(rsample)      
library(gbm)          
library(caret)        
pitchers$pitch_hand <- as.numeric(as.factor(pitchers$pitch_hand))-1
##0 = left, 1 = right
##split data in training and test
pit.split <- initial_split(pitchers, prop = .7)
pit.train <- training(pit.split)
pit.test  <- testing(pit.split)
attach(pit.train)

##try a random hyper parameter config
gbm.fit <- gbm(
  formula = pitch_hand ~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,
  data = pit.train,
  distribution = "bernoulli",
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
print(gbm.fit)
##RMSE
sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")

##grid searching for the best parameters
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,              
  min_RMSE = 0                    
)

# randomize data before using train.fraction
random_index <- sample(1:nrow(pit.train), nrow(pit.train))
random_pit_train <- pit.train[random_index, ]

##grid search to find the best parameters
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  gbm.tune <- gbm(
    formula = pitch_hand ~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,
    distribution = "bernoulli",
    data = random_pit_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

##this shows the best 10 model configs, I choose the parameters of the top model
##This is a stochastic algorithm so if you rerun this, the parameters might differ
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

gbm.fit.final <- gbm(
  formula = pitch_hand ~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,
  distribution = "bernoulli",
  data = pit.train,
  n.trees = 6,
  interaction.depth = 5,
  shrinkage = 0.3,
  n.minobsinnode = 15,
  bag.fraction = .65, 
  train.fraction = 1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
##predict using the final model on the training data
pred <- predict.gbm(gbm.fit.final, n.trees = gbm.fit.final$n.trees, pit.test, type="response")
calibrate.plot(
  pit.test$pitch_hand,
  pred,
  distribution = "bernoulli",
  replace = TRUE,
  line.par = list(col = "black"),
  shade.col = "lightyellow",
  shade.density = NULL,
  rug.par = list(side = 1),
  xlab = "Predicted value",
  ylab = "Observed average",
  xlim = NULL,
  ylim = NULL,
  knots = NULL,
  df = 6,
)

## exploring results
##calculating RMSE
caret::RMSE(pred, pit.test$pitch_hand)
##plotting decision making of variables
plot.gbm(gbm.fit.final, i.var=c(1,4))
plot.gbm(gbm.fit.final, i.var=c(1,8))

##exploring relative influence of all variables
library(dplyr)
effects <- tibble::as_tibble(gbm::summary.gbm(gbm.fit.final, 
                                                  plotit = FALSE))
effects %>% utils::head()
library(ggthemes)
effects %>% 
  # arrange descending to get the top influencers
  dplyr::arrange(desc(rel.inf)) %>%
  # sort to top 10
  dplyr::top_n(9) %>%
  # plot these data using columns
  ggplot(aes(x = forcats::fct_reorder(.f = var, 
                                      .x = rel.inf), 
             y = rel.inf, 
             fill = rel.inf)) +
  geom_col() +
  # flip
  coord_flip() +
  # format
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  xlab('Features') +
  ylab('Relative Influence') +
  ggtitle("variables plotted by influence in classification score")

#I choose to put an arbitrary threshold on 0.7, this is a part that i do not 100% understand, so there is bias in this decisian
table(pit.test$pitch_hand==0,pred<0.7)

-##################################################################################################################
## End of boosting attempt: This turned out to be quite a topic, so i'm not 100% sure I did everything correctly ##
###################################################################################################################

##trying another tree building method with pruning
library(tree)
attach(pitchers)
pitchers$pitch_hand <- as.factor(pitchers$pitch_hand)  
tree.tree <-tree(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers,method="recursive.partition",split="deviance")
plot(tree.tree)         
text(tree.tree) 
##here i have unnecessary splits in the end, so i'm gonna have to do some pruning
tree.tree.cv <- cv.tree(tree.tree,FUN=prune.tree)
plot(tree.tree.cv$size,tree.tree.cv$k,type="l") 
plot(tree.tree.cv$size,tree.tree.cv$dev,type="l",xlab="Number of end nodes",ylab="Deviance") 
#use info from these plots to then prune the actual tree
pruned.tree<-prune.tree(tree.tree,best=3) 
plot(pruned.tree)
text(pruned.tree)
summary(pruned.tree)
tree.pred <- predict(pruned.tree,type="class")
table(pitchers$pitch_hand, tree.pred, dnn=c("From","Classified into"))
##here it seems that the process really seems to prefer classifying things into R instead of R, since it's dominating. 
##this might be a clue that pointing the tree towards classifying L correctly (like boosting) might improve performance.
##40-32



######################################
## Multiple vanilla ML Classifiers: ##
######################################

#Mclust
library(mclust)
clas <- MclustDA(pitchers[,7:15], pitchers$pitch_hand)
summary(clas)
##33-39
plot(clas)
##choose 2 for a pairs-classification plot like previously

##flexible discriminant analysis
library(mda)
# fit model
fit <- fda(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers)
summary(fit)
predictions <- predict(fit, pitchers[,7:15])
table(pitchers$pitch_hand, predictions)
##24-48

##KNN
# fit model
attach(pitchers)
library(caret)
fit <- knn3(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers, k=5)
summary(fit)
predictions <- predict(fit, pitchers[,7:15], type="class")
table(pitchers$pitch_hand, predictions)
##28-44

##Naive Bayes
library(e1071)
# fit model
fit <- naiveBayes(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers)
summary(fit)
predictions <- predict(fit, pitchers[,7:15])
table(pitchers$pitch_hand, predictions)
##31-41