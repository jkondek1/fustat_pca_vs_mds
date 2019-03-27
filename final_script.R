library(ggplot2)
library(magrittr)
library(tidyverse)
library(Amelia) #missmap
library(xtable)

#helper function for data preprocessing
add_vars <- function(dataframe_toplot,data){
  dataframe_toplot$age <-  data$age
  dataframe_toplot$race <- as.factor(data$race)
  dataframe_toplot$hand <- as.factor(data$hand)
  dataframe_toplot$engnat <- as.factor(data$engnat)
  dataframe_toplot$index <- c(1:1000)
  return(dataframe_toplot)
}
##


#Data Loading
data <- read.csv("data.csv",header = TRUE,sep = "",nrows = 1000)
data_ord <- data[,8:57]
data_poly <- data_ord

col_names <- colnames(data_ord)
data_ord[,col_names] <- lapply(data_ord[,col_names] , function(x) as.ordered(factor(x)))


#PCA
#####
library(Gifi)
pca_results <- princals(data_ord, ndim = 2)
pca_toplot <- as.data.frame(pca_results$objectscores)
pca_toplot <- add_vars(pca_toplot,data)

xtable(as.data.frame(t(pca_results$loadings[11:20,1:2])))
########
########
library(psych)
pca_cor <- polychoric(data_poly)
pca_toplot_poly <- pca(pca_cor$rho,nfactors = 2)
loadings <- pca_toplot_poly$loadings
xtable(as.data.frame(t(loadings[11:20,1:2])))
pca2 <- as.data.frame(scale(as.matrix(data_poly)) %*% loadings)
colnames(pca2) <- c("D1","D2")
pca2 <- add_vars(pca2,data)

########
pca_toplot %>% 
  filter(age < 100,hand != '0') %>%
  ggplot(aes(x=D1,y=D2)) + coord_cartesian(xlim = c(-2.1, 3), ylim = c(-3.2,3.2))   + geom_point()

pca_toplot %>% 
  filter(age < 100,hand != '0') %>%
  ggplot(aes(x=D1,y=D2,size = age,color = engnat)) + coord_cartesian(xlim = c(-2.1, 3), ylim = c(-3.2,3.2))   + geom_point()




pca_toplot %>% 
  filter(age < 100,hand != '0') %>%
  ggplot(aes(x = D1, y = D2)) + facet_wrap( ~ race, ncol = 4) + geom_point()
pca_toplot %>% ggplot(aes(x = D1,color = decision)) + geom_density()

#comparison in x-axiss (princals)
pca_toplot%>%
  filter(age < 100,hand != '0',D1 > 1.5) -> D1_positive
pca_toplot%>%
  filter(age < 100,hand != '0',D1 < 0) -> D1_negative

table(data_ord[D1_negative$index,])
table(data_ord[D1_positive$index,1])

#comparison in x-axiss (poly)
pca2 %>%
  filter(age < 100,hand != '0',D1 > 13) -> D1_positive_poly
pca2 %>%
  filter(age < 100,hand != '0',D1 < 0) -> D1_negative_poly

pca2 %>% 
  filter(age < 100,hand != '0') %>%
  ggplot(aes(x=D1,y=D2,alpha = age, shape = hand, color = race)) + geom_point()

table(data_ord[D1_negative$index,])
table(data_ord[D1_positive$index,1])








#MDS
###########
distances <- dist(data_ord) #using euclidean distance, skus minkowski = to iste kvoli ordinalnym datam
library(smacof)
mds_results <- smacof::mds(delta = distances, ndim = 2, type = "ordinal",verbose = TRUE)
final <- add_vars(final,data)
###########
final %>% 
  filter(age < 100,hand != '0') %>%
  ggplot(aes(x=D1,y=D2)) + coord_cartesian(xlim = c(-2.1, 3), ylim = c(-3.2,3.2)) + geom_point() #tuto staci pomenit parametre... data su nachystane

final %>% 
  filter(age > 30,hand != '0') %>%
  ggplot(aes(x=D1,y=D2,color = engnat)) + geom_point() #tuto staci pomenit parametre... data su nachystane

#tu je zaujimave ze mladsi su skor na okrajoch, pripadne v lavej casti
#country neukazuje nic extra
final%>%
  filter(age < 100,hand != '0',D1 > 1) -> D1_final_positive
final%>%
  filter(age < 100,hand != '0',D1 < 0) -> D1_final_negative

plot(mds_results, "Shepard")
cor(distances,mds_results$confdist,method = "pearson")

#Differences in Quadrants
comp_q <- function(input1,input2,q,x = 0,y = 0){
  subset1 <- NULL
  subset2 <- NULL
  get_subset <- function(input,q,x,y){
      if(q == 1){
      input %>%
      filter(D1 >= x, D2 >= y, age < 100,hand != '0') -> subset
      } else if (q == 2){
        input %>%
        filter(D1 >= x, D2 < y, age < 100,hand != '0') -> subset  
      } else if (q == 3){
        input %>%
        filter(D1 < x, D2 < y, age < 100,hand != '0') -> subset  
      } else if (q == 4){
        input %>%
        filter(D1 < x, D2 >= y, age < 100,hand != '0') -> subset  
      }
    return(subset)
  }
  subset1 <- get_subset(input1,q,x,y)
  subset2 <- get_subset(input2,q,x,y)
  return(length(intersect(subset1$index,subset2$index))/length(subset2$index))
}

tabulka <- cbind(comp_q(final,pca_toplot,q = 1),comp_q(final,pca_toplot,q = 2),comp_q(final,pca_toplot,q = 3),comp_q(final,pca_toplot,q = 4))
tabulka2 <- cbind(comp_q(final,pca_toplot,q = 1,x=-0.2,y=0.05),comp_q(final,pca_toplot,q = 2,x=-0.2,y=0.05),comp_q(final,pca_toplot,q = 2,x=-0.2,y=0.05),comp_q(final,pca_toplot,q = 4,x=-0.2,y=0.05))

table_comparison <- as.data.frame(tabulka)
table_comparison[2,] <-tabulka2
colnames(table_comparison) <- c("Q1","Q2","Q3","Q4")
rownames(table_comparison) <- c("origin (0,0)","origin (-0.2,0.05)")
xtable(table_comparison)
