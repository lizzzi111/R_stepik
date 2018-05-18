clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)
data("iris")

dist_matrix <- dist(swiss) # расчет матрицы расстояний
fit <- hclust(dist_matrix) # иерархическая кластеризация 
cluster <- cutree(fit, 3)

cl =nhcl(iris[, 3:4], 2)

anova = sapply(iris[, 3:4], function(var) summary(aov(var~cluster, cl))[[1]]$`Pr(>F)`[1])
anova2 = aov(cluster~., cl)
  sapply(iris[, 3:4], function(var) summary(aov(var~cluster, cl))[[1]]$`Pr(>F)`)
  summary(anova2)
  
  smart_hclust<-  function(test_data, cluster_number){
    dist_matrix = dist(test_data)
    fit = hclust(dist_matrix)
    test_data$cluster = factor(cutree(fit, cluster_number))
    return(test_data)
  } 
get_difference<-  function(test_data, n_cluster){
  dist_matrix = dist(test_data)
  fit = hclust(dist_matrix)
  test_data$cluster = factor(cutree(fit, n_cluster))    
  anova = sapply(test_data[,!(names(test_data)%in%"cluster")], function(var) summary(aov(var~cluster, test_data))[[1]]$`Pr(>F)`[1])  
  vars = names(anova)[anova<0.05]
  return(vars)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
f = cor(test_data)
f[upper.tri(f)] = 0
diag(f) = 0

c(names(test_data)[which(apply(f,2,function(x) any(abs(x)-1 == 0)))],
names(test_data)[which(apply(f,1,function(x) any(abs(x)-1 == 0)))])



is_multicol <- function(d){
  f = cor(d)
  diag(f) = 0
  f = as.data.frame(f)
  vars = sapply(f,function(x) max(abs(x)))
  vars = vars[as.character(vars)=="1"]
  ifelse(length(vars)>0, return(names(vars)), return("There is no collinearity in the data"))
}

library(caret)
df2 = cor(d)
hc = findCorrelation(df2, cutoff=0.9) # putt any value as a "cutoff" 
hc = sort(hc)
vars = names(d)[hc]


data(swiss)
dist_matrix <- dist(swiss) # расчет матрицы расстояний
fit <- hclust(dist_matrix) # иерархическая кластеризация 
swiss$cluster <- factor(cutree(fit, 2))

library(ggplot2)
my_plot <- ggplot(swiss, aes(Education, Catholic, col = factor(cluster)))+
              geom_smooth(method = "lm")+geom_point()
            

  hetero_test <-  function(test_data){
    fit = lm(test_data[,1]~., test_data[,-1])
    res_fit = summary(lm(fit$residuals^2~., test_data[,-1]))
    return(res_fit$r.squared)
  }
  
  

  
  get_strange_var <- function(d){
    res_fit = sapply(d, function(ind) sapply(d, function(dep) shapiro.test(lm(dep~ind)$residuals/sd(lm(dep~ind)$residuals))$p.value ))
    df = as.data.frame(res_fit)
    diag(df) = 1
    ind = as.data.frame(df < 0.05)
    ind_sum = sapply(ind, function(x) sum(x[x==TRUE]) )
    max_sum = max(ind_sum)
    ifelse(ind_sum>1, return(names(ind_sum[ind_sum==max_sum])), return("There is no strange variable in the data"))
  }
  
data(mtcars)
library(ggplot2)

ggplot(mtcars, aes(mpg))+geom_dotplot()+facet_grid(am~vs)
  

ggplot(iris, aes(Sepal.Length))+geom_density()+facet_wrap(~Species)
 
ggplot(iris, aes(Sepal.Length, Sepal.Width))+geom_point()+geom_smooth()+facet_wrap(~Species)


my_plot <- ggplot(myMovieData, aes(Type, Budget)) + geom_boxplot() + facet_grid(.~Year)
ggplotly(my_plot)

data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length,  col = Species))+
                 geom_point()+
                 geom_smooth(method = "lm") + 
                 scale_x_continuous(name = "Длина чашелистика",
                                    limits = c(4,8)) +
                 scale_y_continuous(name = "Длина лепестка", 
                                    breaks = seq(1,7),
                                    limits = c(1,7))+
                 scale_color_discrete(name="Вид цветка",
                                      labels = c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский"))

gl = read.csv("/Users/lizzzi111/Desktop/glacier.csv")
tapply(as.numeric(as.character(gl$Value[gl$MEASURE=="Annual mass balance"])), gl$GEO[gl$MEASURE=="Annual mass balance"], median)
tapply(gl$Value, gl$GEO, function(x) any(is.na(x)))


library(plotly); library(data.table)
teapot = read.csv2("/Users/lizzzi111/Downloads/teapot.csv")

draw = function(data){
    ff = matrix(data=data, nrow = 3)
    i = ff[[1]];  j = ff[[2]];  k = ff[[3]]
    keter = plot_ly(as.data.frame(data), type= "mesh3d", x=x, y=y, z=z, i=i, j=j, z=z)
  return(keter)
}

draw(teapot)


set.seed(42)
test_data <- data.frame(y = rnorm(30, 5), x1 = rnorm(30, 5))
test_data$x2 <- test_data$x1^2
VIF(test_data)
