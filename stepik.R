good_month <- AirPassengers
good_month <- append(good_month,0)
for(i in 1:length(AirPassengers)){
  good_months[i]<- ifelse(good_month[i]<good_month[i+1],good_month[i+1],0 )
} 
good_months <- good_months[good_months!=0]    
 

moving_average <- numeric(135)
for(i in 1:135){
  moving_average[i] <- cumsum(AirPassengers[i:(i+10)])[10]/10
} 
data("mtcars")
descriptions_stat <- aggregate(mtcars[, c("hp", "disp")], by = list(mtcars$am), sd)

data("airquality")
df <- airquality 
df <- df
df <- subset(df,(df$Month==7|df$Month==8|df$Month==9))
result <- aggregate(df$Ozone, list(df$Month), function(x) length(!is.na(x)))
df <- df[df$Month==7|df$Month==8|df$Month==9,]
describeBy(airquality[,1:3], airquality$Month)



data("iris")
summary(iris[,1:4])

require(ggplot2)
boxplot(Ozone~Month, airquality)


plot1 <- ggplot(mtcars, aes(x=mpg,y=disp, col=hp))+ geom_point()

#das gleiche
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))

ggplot(aes(Sepal.Length, Sepal.Width, col = Species)) + 
  geom_point(iris, size = Petal.Length)

data("HairEyeColor")
dimnames(HairEyeColor)
HairEyeColor[ , ,'Male']
HairEyeColor[ ,"Blue" ,'Male']
red_men <- HairEyeColor[ "Red","Blue" ,'Male']/sum(HairEyeColor[ ,"Blue" ,'Male'])
red_men <- prop.table(HairEyeColor[ , ,'Male'],2)['Red','Blue']
sum(HairEyeColor[ ,"Green" ,'Female'])

mydata <- as.data.frame(HairEyeColor)
library("ggplot2")

obj <- ggplot(data = mydata[mydata$Sex=='Female',], aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = PositionDodge )+ 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

fem <- HairEyeColor[ 'Brown', ,'Female']
chisq.test(fem)

data("diamonds")
str(diamonds)
chisq.test(diamonds$cut,diamonds$color)
main_stat <- chisq.test(diamonds$cut,diamonds$color)$statistic
main_stat$statistic

factor_price <- factor(ifelse(diamonds$price>=mean(diamonds$price),1,0))
factor_carat <- factor(ifelse(diamonds$carat>=mean(diamonds$carat),1,0))
main_stat <- chisq.test(factor_price,factor_carat)$statistic

data("mtcars")
fisher_test <- fisher.test(mtcars$am,mtcars$vs)$p.value

data("ToothGrowth")
t_stat <- t.test(ToothGrowth$len[ToothGrowth$supp=="OJ"&ToothGrowth$dose==0.5], ToothGrowth$len[ToothGrowth$supp=="VC"&ToothGrowth$dose==2])
t_stat <- t_stat$statistic

#lekarstva
lekarstva <- read.csv("./Downloads/lekarstva.csv")
attach(lekarstva)
t.test(Pressure_before, Pressure_after, paired = T)

bla <- read.table("./Downloads/dataset_11504_15 (4).txt")

bla$V2 <- factor(bla$V2)

bartlett.test(bla$V1, bla$V2)
wilcox.test(V1~V2, data=bla)
t.test(V1~V2, data=bla, var.equal = TRUE)


tttt <- read.table("./Downloads/dataset_11504_16 (1).txt")
t.test(tttt$V1,tttt$V2)


data("npk")
b <-aov(yield~N+P+K, data = npk)
summary(b)

data("iris")
attach(iris)
summary(aov(Sepal.Width~Species, data = iris[iris$Species==1|iris$Species==2]))


temp <- read.csv("./Downloads/Pillulkin.csv")
temp$patient <- factor(temp$patient)
attach(temp)
summary(aov(temperature~pill+Error(patient/pill)))
summary(aov(temperature~pill*doctor+Error(patient/(pill*doctor))))

data("ToothGrowth")
library(ggplot2)
library(Hmisc)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))

my_vector <- c(1, -2, 3, NA, NA,8, 190, -60, 25, 10)
NA.position <- function(x){
  which(is.na(x)) 
}
NA.position(my_vector)

NA.counter <- function(x){
  sum(is.na(x)) 
}
NA.counter(my_vector)

filtered.sum <- function(x){
  sum(x[x>0],na.rm = T)
}
filtered.sum(my_vector)

outliers.rm <- function(x){
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = T)
  iqr <- q[2]-q[1]
#  ifelse(x>q[2]+1.5*iqr|x<q[1]-1.5*iqr, "NA", x)
  return(x[x<q[2]+1.5*iqr&x>q[1]-1.5*iqr])
}

outliers.rm(my_vector)

data("mtcars")
library("psych")

corr.calc <- function(df) {
  if (lapply(df, is.numeric)[1]==FALSE | lapply(df, is.numeric)[2]==FALSE)
    return("One of the given variables is not numeric")
  if (is.numeric(df))
    return("There are less than 2 required variables")
  if (ncol(df)>2)
    return("There are more than 2 variables")
   else 
 #     pearson <- cor(df, method="pearson")[1,2]
      pt <- cor.test(df[,1],df[,2], method="pearson")
    return(c(pt$estimate,pt$p.value))   
 }
corr.calc( mtcars[] )
max(apply(df,2, function(x) max(abs(x[!x%in%diag(x)]))))

filtered.cor <- function(df){
#  df <- as.data.frame(sapply(df, as.numeric, simplify = "matrix"))
  df <- df[,sapply(df, is.numeric, simplify = "matrix")]
  df<- cor(df)
  diag(df) <- 0
  ind <- which.max(abs(df))
  return(df[ind,1])
#  return(max(abs(df)))
 }

g <- filtered.cor(test_data)
diag(g)

#### correct solution
filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])}

smart_cor <- function(df){
  if (lapply(df, is.numeric)[1]==FALSE | lapply(df, is.numeric)[2]==FALSE)
    return("One of the given variables is not numeric")
  if (is.numeric(df))
    return("There are less than 2 required variables")
  if (ncol(df)>2)
    return("There are more than 2 variables")
  if (shapiro.test(df[,1])$p.value<0.05 || shapiro.test(df[,2])$p.value<0.05)
    return(cor(df, method = "spearman")[1,2])
  else
    return(cor(df, method = "pearson")[1,2])
}

smart_cor( mtcars[c(1,2)] )

df <- read.table("./Downloads/dataset_11508_12.txt")
attach(df)
fit <- lm(V1~V2, data = df)

data("diamonds")
fit <- lm(price ~ depth, diamonds[(diamonds$cut == "Ideal" & diamonds$carat==0.46),])
fit$coefficients


regr.calc <- function(df){
  if (lapply(df, is.numeric)[1]==FALSE | lapply(df, is.numeric)[2]==FALSE)
    return("One of the given variables is not numeric")
  if (is.numeric(df))
    return("There are less than 2 required variables")
  if (ncol(df)>2)
    return("There are more than 2 variables")
  if (cor.test(df[,1],df[,2], method="pearson")$p.value>0.05)
    return("There is no sense in prediction")
  else
    df$fit <-lm(df[,1]~df[,2])$fitted.values
  return(df)
} 
regr.calc(test_data)
regr.calc(iris[,1:2])
test_data <- as.data.frame(list(V1 = c(3.66, 4.31, 5.73, 3.76, 4.6, 5.14, 5.08, 5.44, 5.45, 5.94), V2 = c(4.13, 2.19, 5.3, 3.81, 5.03, 5.44, 4.91, 5.99, 5.61, 5.09)))

data("iris")
library(ggplot2)
my_plot <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, 
                                col = Species, group = Species))  + 
            geom_point() +
           geom_smooth(method="lm")
          

fill_na <- function(df){
  if (lapply(df, is.numeric)[1]==FALSE | lapply(df, is.numeric)[2]==FALSE|lapply(df, is.numeric)[3]==FALSE)
    return("One of the given variables is not numeric")
  if (ncol(df)<3)
    return("There are less than 3 required variables")
  if (ncol(df)>3)
    return("There are more than 3 variables")
  else
    df$y_full <- df$y
    fit <- lm (y_full ~ x_1 + x_2, df[!is.na(df$y_full),])
    df$y_full[is.na(df$y_full)] <- predict(fit, df[is.na(df$y_full),])
  return(df)
} 

#Пример правильного решения студента Aleksei Dobrokhotov stepic.org/users/2840003:    
  
  fill_na <- function(my_df){    
    fit <- lm(y ~ x_1+x_2, my_df)    
    my_df$y_full = ifelse(is.na(my_df$y), predict(fit, my_df), my_df$y)    
    return(my_df)}
  
  
data("mtcars")
df <- mtcars
fit1 <- lm(wt~mpg+disp+hp, data = df)
summary(fit1)

data("attitude")
summary(lm(rating~complaints*critical, data = attitude))

data("mtcars")
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
summary(lm(mpg~wt*am, data=mtcars))

my_plot <- ggplot(mtcars, aes(x=wt, y=mpg, col=factor(am), group = factor(am))) +
  geom_smooth(method='lm')

data("attitude")
model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
scope = list(lower = model_null, upper = model_full)
ideal_model <- step(model_full)
anova(model_full, ideal_model)

data("LifeCycleSavings")
attach(LifeCycleSavings)
model <- lm(sr~pop15*pop75*dpi*ddpi)
model1 <- lm(sr~(pop15+pop75+dpi+ddpi)^2, data=LifeCycleSavings)


my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
hist(log(my_vector))
shapiro.test(1/(my_vector))
ggplot(data.frame(my_vector), aes(x = sqrt(my_vector)))+
  geom_histogram()


data("mtcars")
beta.coef <- function(x){
  x <- sapply(x,scale)
  fit <- lm(x[,1]~x[,2])$coefficients
  return(fit)
}
#можно сделать с помощью QuantPsyc::lm.beta

normality.test <- function(x){
  p_values <- vector(mode="list", length=ncol(x))
  p_values <- sapply( x, function(df)shapiro.test(df)$p.value)
  return(p_values)
}
#normality.test  <- function(x){    
#  return(sapply(x, FUN =  shapiro.test)['p.value',])}
normality.test(mtcars[,1:6])
normality.test(iris[,-5])

df <- read.csv("./Downloads/homosc.csv")
fit <- lm(DV~IV, data = df)
library(gvlma)
fit_glvma <- gvlma(DV~IV, data = df)


resid.norm <- function(fit){
  res <- shapiro.test(fit$residuals)
  if (res$p.value < 0.05){
    return(ggplot(data.frame(fit$residuals), aes(x = fit$residuals )) + geom_histogram(fill = "red"))
  } else {
    return(ggplot(data.frame(fit$residuals), aes(x = fit$residuals )) + geom_histogram(fill = "green"))
  }
}


fit <- lm(mpg ~ disp, mtcars)
my_plot <- resid.norm(fit)


fit <- lm(mpg ~ wt, mtcars)
my_plot <- resid.norm(fit)
my_plot

#Passed. 
#Пример правильного решения:
  
  resid.norm <- function(fit) {    
    resid.norm.pv <- shapiro.test(fit$residuals)$p.value    
    plt <- ggplot(data.frame(fit$model), aes(x = fit$residuals)) +    
      geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))    
    return(plt)}
data(swiss)
data(iris)
df <- swiss

high.corr <- function(df){
  df_num <- df[,sapply(df, is.numeric)]
  corr <- cor(df_num)
  diag(corr) <- 0
  maximum_value <- which.max(abs(corr))
  indices <- arrayInd(maximum_value, dim(corr))
  return(c(colnames(corr)[indices[,2]], row.names(corr)[indices[,1]]))
}

high.corr(iris[,-5])
high.corr(swiss)
high.corr(test_data)
test_data <- as.data.frame(list(V1 = c(-0.1, 1.6, -1.7, -0.1, 1.3, -1.3, 1.7, -1.2, -0.5, 0.8, -0.5, -0.7, 0.7, 0, 0.7, 0.4, -0.7, 1.8, -0.7, 0.3, 0.6, 1.3, 0.7, -1.4, -2.3), V2 = c(-1.4, 0.5, 0.2, 1, -0.4, 0.7, -0.6, -0.8, 1.5, 0.3, -1, 0, -0.1, -0.7, 0, 2.1, 0.5, -1.1, 0.5, 1, -1.4, 1.4, 0, -2.3, -0.2), V3 = c(0.1, -1.6, 1.7, 0.1, -1.3, 1.3, -1.7, 1.2, 0.5, -0.8, 0.5, 0.7, -0.7, 0, -0.7, -0.4, 0.7, -1.8, 0.7, -0.3, -0.6, -1.3, -0.7, 1.4, 2.3)))
cor(test_data)

high.corr <- function(x){    
  cr <- cor(x)    
  diag(cr) <- 0    
  return(rownames(which(abs(cr)==max(abs(cr)),arr.ind=T)))}

data("mtcars")
log_coef <- glm(am~disp+vs+mpg, data = mtcars, family = "binomial")$coefficients

data("ToothGrowth")
library("ggplot2")

obj <- ggplot(data = ToothGrowth, aes(x=supp, y = len, group = factor(dose),fill = factor(dose)))+
  facet_grid(.~supp)+
  geom_boxplot()

df <- read.csv("./Downloads/data.csv")
df$admit <- factor(df$admit)
train <- df[!is.na(df$admit),]
test<- df[is.na(df$admit),]            

fit <- glm(admit~rank*gpa, data = df, family = "binomial")
fitted_model <- predict(fit, newdata = test, type = "response")
sum(fitted_model[fitted_model>=0.4])
sum(fitted_model>=0.4)

test_data <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), V2 = c(NA, -10.2, -10.1, -9.3, -12.2), V3 = c(NA, NA, -9.3, -10.9, -9.8)))
test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))
test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))

get_negative_values <- function(test_data){
test_data <- lapply(test_data, function(x) x[x<0])
test_data <- lapply(test_data, function(x) x[!is.na(x)])
ind <- sapply(test_data,length) > 0
return(test_data[ind])
}
get_negative_values(test_data)

get_negative_values <- function(test_data){    
  negative_col <- apply(test_data, 2, function(x) any(x[!is.na(x)] < 0))    
  return(apply(test_data[negative_col], 2, function(x) x[!is.na(x) & x <0]))}

test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))
na_rm  <- function(test_data){
  return(test_data <- apply(test_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = T), x)))
}
test_data <- na_rm(test_data)

library(ggplot2)
data("diamonds")
diamonds[c("carat","depth")]
diamonds[, c(seq(1:ncol(diamonds))%%2==1)]
diamonds[c(seq(1:nrow(diamonds))%%2==1),]


data(mtcars)
my_df <- mtcars %>% 
  select(mpg, hp, am, vs) %>% 
  filter(mpg>14, hp >100)  %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename('Miles per gallon'=mpg, 'Gross horsepower'=hp)


test_data <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))
test_data <- as.data.frame(list(X1 = c(-0.2, -0.1, 1.5, -0.7, 0.9), X2 = c(NA, NA, NA, NA, NA), X3 = c(NA, NA, NA, NA, NA), X4 = c(0.5, 0.4, 1, -0.5, 0.8)))
positive_sum <-  function(test_data){
#  test_data <- test_data[,sapply(test_data, is.numeric)]
  rows <- lapply(test_data, function(c) sum(c[c>0],na.rm=T))

  return(rows)
}

dataset <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")
my_names <- function (dataset, names){
  obs <- as.data.frame(sapply(names, function(name) grepl(name, dataset$name )))
  obs$yes <- apply(obs, 1, any)
  return(dataset[obs$yes,])
}

my_names <- function (dataset, names){    
  dataset[as.numeric(lapply(names, function(x) which(grepl(x, dataset$name)))),]}


l <- table(mtcars[,c("am", "vs")])
any(l<5)
fisher.test(x)

smart_test <-  function(x){
  l <- table(x[,1],x[,2])
  if(any(l<5)) {
    res <- fisher.test(l)
    return(c(res$p.value))
  }else{
    res <- chisq.test(l)
    return(c(res$statistic, res$parameter, res$p.value))
  }
}

most_significant <-  function(x){
  x <- as.data.frame(apply(x,2,factor))
  res <- c()
  res <- c(res,apply(x,2, function(col) chisq.test(table(col))$p.value))
  return( names(res)[res==min(res)])  
}
most_significant(x)

data(iris)
df_num <- iris[,sapply(iris, is.numeric)]
c <- colMeans(df_num)
iris$important_cases <- apply(df_num,1, function(x) factor(ifelse(sum(x>c)>=3, "YES", "NO" )))

get_important_cases <- function(x){
  df_num <- x[,sapply(x, is.numeric)]
  c <- colMeans(df_num)
  x$important_cases <- apply(df_num,1, function(l) factor(ifelse(sum(l>c)>(ncol(df_num)/2), "Yes", "No" ), levels=c("Yes","No")))
  return(x)
}                     



stat_mode <- function(x){
  w<-table(x) 
  f <- w[max(w)==w]
  res <- data.frame(names(f))
  return(c(as.numeric(levels(res$names.f.))))
}

max_resid <- function(x) {
 tab <- table(x)
  f <- as.data.frame(chisq.test(tab)$stdres)
  l <- max(f$Freq)
  return(c(levels(f$Drugs)[levels(f$Drugs)%in%f$Drugs[f$Freq==l]],levels(f$Result)[levels(f$Result)%in%f$Result[f$Freq==l]]))
}



library("ggplot2")
# теперь данные diamonds доступны для работы
data(diamonds)
ggplot(diamonds, aes(x = color, fill=cut)) +
  geom_bar(position="dodge")

data(iris)
fit1 <- glm(Species~1, iris,family = 'binomial')
fit2 <- glm(Species~Sepal.Width, iris,family = 'binomial')
plot(predict(fit2,type = 'response'))
 
data("msleep")
msleep$genus <- as.factor(msleep$genus)
msleep$vore <- as.factor(msleep$vore)
msleep$order <- as.factor(msleep$order)
msleep$conservation <- as.factor(msleep$conservation)
fit3 <- glm(vore~order*genus,msleep,family=binomial)
plot(predict(fit3,type = 'response'))


data("diamonds")
depth_hist <- qplot(diamonds$depth,xlab = "depth")

price_carat_clarity_points <- qplot(carat,price,data=diamonds,colour=clarity)
x_density <- qplot(x, data = diamonds, geom = "density")
x_cut_density <- qplot(x, data = diamonds, geom = "density", colour = cut)


price_violin <- ggplot(diamonds, aes(x = color, y = price)) + 
  geom_violin()

centered <- function(dataframe, var_names) {
  if(length(var_names)>1){
    dataframe[, var_names] <- apply(dataframe[, var_names], 2, function(x) x <- x - mean(x))
  }
  else {
    dataframe[, var_names] <- dataframe[, var_names] - mean(dataframe[, var_names])
  }
  return(dataframe)
}
#


get_features <- function(dataset){
  fit <- glm(is_prohibited ~ ., dataset, family = "binomial")
  result <- anova(fit, test = "Chisq")
#  dataset_t <- dataset
#  dataset_t$is_prohibited <- NULL 
  features <- result$`Pr(>Chi)`<=0.05
  f <- names(dataset)[features]
  ifelse(all(is.na(f)), return("Prediction makes no sense"), return(f[-1]))
}

most_suspicious <- function(test_data, data_for_predict){
  fit <- glm(is_prohibited~., test_data, family = "binomial")
  pred <- predict(fit, data_for_predict)
  return(data_for_predict$passangers[which.max(pred)])
}

data("iris")
normality_test <- function(dataset){
  dataset_num <- dataset[,sapply(dataset, is.numeric)]
  return(unlist(p.v <- as.data.frame(sapply(dataset_num, shapiro.test))[2,]))
}

library(ggplot2)
ir <- ggplot(iris, aes(x = Sepal.Length)) + 
  geom_density(aes(fill = Species), alpha = .2)



smart_anova <- function(test_data){
  norm_t <- tapply(test_data$x, test_data$y, shapiro.test)
  n <-   unlist(lapply(norm_t, function(x) x$p.value ))
  
  hom <- bartlett.test(test_data$x,test_data$y)
  
  if(any(n<.05)|hom$p.value<.05){
    test <- kruskal.test(test_data$x,test_data$y)$p.value
    names(test) <- "KW"
    return(test)
  } else {
    test <- aov(x~y, test_data)
    p_value <- summary(test)[[1]]$'Pr(>F)'[1]
    names(p_value) <-  "ANOVA"
    return(p_value)
  }
}


normality_by <- function(test){
  test %>%
    group_by_(.dots = names(.[-1])) %>%
    summarise_each(
      funs(p_value = shapiro.test(.)$p.value)
    )
}


log_transform <- function(test_data) {
  test_data <- test_data %>%
    mutate_if(is.numeric,funs(log((.-min(.))/(max(.)-min(.))+1))) 
  return(test_data)
}

data("ToothGrowth")
data("mtcars")
t <- mtcars[,c("mpg","cyl","am")]
t$cyl <- factor(t$cyl)
t$am <- factor(t$am)
t <- ToothGrowth
t$dose <- factor(t$dose)

find_outliers <- function(t){
  library(dplyr)
  library(lazyeval)
  gr_var <- names(select_if(t, is.factor))
  d_var <- names(select_if(t, is.numeric))
  t <- t %>%
    group_by_(.dots=gr_var) %>%
    mutate_(mean = interp(quote(mean(var)), var = as.name(d_var)), 
            sd = interp(quote(sd(var)), var = as.name(d_var))) %>% ungroup() %>%
    mutate_(is_outlier = interp(~ifelse( (var < mean - 2*sd) | (var > mean + 2*sd) , 1, 0), 
                             var = as.name(d_var))) %>% select(-c(mean,sd))
  return(t)
}

l <-to_factors(mtcars[1:4], factors = c(1,3))

to_factors <- function(df, factors){
  df <- as.data.frame(df)
  a <- names(df[factors])
  if(length(a)>1){
    df[,a] <-  lapply(df[,a],  function(col) factor(ifelse(col>mean(col),1,0)))
  }else{
    df[,a] <- as.factor(ifelse(df[,a]>mean(df[,a]),1,0))
  }
  return(df)
}

to_factors <- function(test_data, factors){    
  test_data[factors] <- mutate_each(test_data[factors], funs(factor(ifelse(. > mean(.), 1, 0))))    
  return(test_data)}
library(dplyr)
high_price <- diamonds %>%
  group_by(color) %>%
  arrange(desc(price)) %>% 
  sample_n(10)  %>%
  dplyr::select( color, price)               
                
library(data.table)
sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))
filter.expensive.available <- function(products, brands) {
  t <- products[(price>=500000) & 
              (brand%in%brands) &
              (available==TRUE),.(price, brand, available)]
  return(t)
}

filter.expensive.available(products, c("a", "c", "d"))

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

ordered.short.purchase.data <- function(purchases) {
  purchases[quantity>=0][ order(-price)  , .(ordernumber, product_id)]
}

ordered.short.purchase.data<- function(purchases) {    
  purchases[order(-price)][quantity >= 0][, .(ordernumber, product_id)]}

purchases.median.order.price <- function(purchases) {
  unlist(purchases[quantity>=0,.(price=sum(price*quantity)) ,by=ordernumber][, .(sa=median(price))])
}
t <-purchases.median.order.price(sample.purchases)

product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))
purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))
get.category.ratings(purchases, product.category)

get.category.ratings <- function(purchases, product.category) {
  setkey(purchases, product_id)
  setkey(product.category, product_id)
  dt <- merge(product.category, purchases, by="product_id")
  dt[,.(totalcents=sum(totalcents),quantity=sum(quantity)), by=category_id]
}

sample.purchases <- data.table(price = c(100, 300, 50, 700, 30),
                               ordernumber = c(1,1,1,2,3),
                               quantity = c(1,1,2,1,-1),
                               product_id = 1:5)
purchases <- sample.purchases
mark.position.portion(sample.purchases)
mark.position.portion <- function(purchases) {
    purchases <- purchases[quantity>=0]
    purchases[, price_n := price*quantity][,price_gr:=sum(price_n), by = ordernumber][,price.portion:=sprintf("%.2f", round(price_n / price_gr*100,2))][quantity>=0, .(product_id, price,  quantity,ordernumber , price.portion)]
}

purchases <- data.table(
  "product_id"=c(1538, 1610, 1038, 1627, 1197, 1588),
  "price"=c(1552714.06, 26985691.34, 24403584.72, 9634794.26, 12959576.45, 7591754.01),
  "quantity"=c(2, 1, 4, 3, 1, 1),
  "ordernumber"=c(259, 259, 262, 262, 446, 550))

test_data <- as.data.frame(list(V1 = c("-1. 176", "0.67 7", "0. 9575", "-1.844 8", "1.97 3"), V2 = c("0. 118", "-0.27 78", "0.9945", "-0.8619", "-0.3495"), V3 = c("- 1.2977", "2.2 253", "0.16 81", "2.003 1", "- 0.2771"), V4 = c("1. 1062", "0. 5428", "-1.4989", "-0.0758", "0. 2142"), V5 = c("ST 123E", "II 2", "ST 123E", "ST 123E", "HP 129")))
fix_data <- function(d){
  nd <- as.data.frame(apply(d,2, function(col) as.numeric(gsub(" ", "", col))))
  ind_f <- apply(nd,2, function(x) sum(is.na(x))==nrow(d))
  nd[ind_f] <- d[ind_f]
  return(nd)
}

data(swiss)
smart_lm <- function(x){
  if(ncol(x)>2){
    p_values <- sapply(x[,-1], function(df) shapiro.test(df)$p.value)
  } else {
    p_values <- shapiro.test(x[,2])$p.value
  }
  ind_n <- p_values>.05
  vec <- names(x[,-1])[ind_n]
  if(length(vec)>0){
    fit <- lm(x[,1]~., data= x[,vec])
    return(fit$coefficients)
  }else{
    return("There are no normal variables in the data")
  }
}



one_sample_t <- function(test_data, general_mean){
  df_num  <- test_data[,sapply(test_data, is.numeric)]
  t.t <- as.data.frame(sapply(df_num, function(x) t.test(x, mu = general_mean)))
  res <- lapply(t.t, function(x)
                      c(x$statistic, x$parameter ,x$p.value))
  return(res)
}

get_p_value <- function(test_list){
  p.v <- lapply(test_list, function(x) x$p.value)
  return(p.v)
}

load("./Downloads/all_data.Rdata")
get_id <- function(data_list){
#ids <- sort(unique(unlist(lapply(data_list, function(col) col$id))))
#all_the_vars <- lapply(data_list, function(col) if(nrow(col)==length(ids)) col )
#al <- Filter(Negate(function(x) is.null(unlist(x))), all_the_vars)
merge.all <- function(x, y) {
  merge(x, y, all=TRUE, by="id")
}
output <- Reduce(merge.all, al)
output <- output[apply(output, 1, function(x){!any(is.na(x))}),]
output$mean_temp <- rowMeans(output[,-1])
o <- output[,c("id","mean_temp")]
return(o)
}


data("mtcars")
ggplot(mtcars, aes(x=factor(am), y=mpg))+
  geom_violin()+geom_boxplot(width = 0.2)


sales = read.csv("https://stepic.org/media/attachments/course/724/sales.csv")
my_plot <- ggplot(sales %>% 
                    group_by(shop, season) %>% rename(income1 = income) %>%
                    summarise(income = mean(income1), 
                    y_max = mean(income1) + 1.96*sd(income1) / sqrt(length(income1)),
                    y_min = mean(income1) - 1.96*sd(income1) / sqrt(length(income1))), 
                  aes(x=shop, y=income)) + 
                      geom_pointrange(aes(ymin=y_min, ymax= y_max, color=season),
                                      position = position_dodge(0.2))

my_plot <- ggplot(sales, aes(shop, income, col = season))+
  stat_summary(geom = "pointrange", position = position_dodge(0.2)) 


my_plot1 <-   ggplot(sales, aes(date, sale, col = shop, group = shop))+
  stat_summary(fun.data = mean_cl_boot, geom = "point", position = position_dodge(0.2)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(0.2), width = 0.2) +
  stat_summary(fun.data = mean_cl_boot, geom = "line", position = position_dodge(0.2))

load("./Downloads/data1.RData")
load("./Downloads/data2.RData")
load("./Downloads/data3.RData")
cor(d)
t.t <- apply(d, 2, shapiro.test)


get_strange_var <- function(d){
  
  
}
fit1 <- lm(x~z,d)
plot(fit1)
summary(lm(fit1$residuals~z,d))
qqline(fit1$residuals)

df <- data.frame(x = c(-3,1,2,3,5,6,7), y = c(3,4,6,8,2,11,1))
kmeans(df,1)

fit <- sapply(names(d), function(col) lm(col ~ .,d))
lapply(fit, summary)
