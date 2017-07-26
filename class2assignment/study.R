x <- list(num = 1:6, c(T,F), "play", list(c("A","b")))

x[[1]]
sum(x$num)

y <- unlist(x[[4]])
y[1]
length(x[[4]])
str(x)
as.numeric(unlist(x)[1])+as.numeric(unlist(x)[2])

x-x  
a = as.data.frame(x)

kmeans

data(iris)
iris
summary(iris)
str(iris)
train <- iris[,-5]
test <- iris[, 5]
summary(test)
km <- kmeans(train, center = 3, iter.max = 100)
km$cluster

train_scale <- scale(train, center = TRUE, scale = TRUE)
head(train)
head(train_scale)
head(scale(train, center = FALSE, scale = TRUE))
km_scale <- kmeans(train_scale, center = 3, iter.max = 100)

table(test, km$cluster)

table(test, km_scale$cluster)

table(test, km$cluster)

library(MASS)
head(birthwt)
str(birthwt)
data <- birthwt
data$race <- as.factor(data$race)
data$smoke <- as.factor(data$smoke)
str(data)

fit <- lm(bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv, data = data)
summary(fit)

fit2 <- lm(bwt ~ . -low - lwt , data = data)
summary(fit2)
data

tar <- sample(1:nrow(data), round(nrow(data)*0.7))
train <- data[tar, ]
test <- data[-tar, ]

predict(fit, test)

if(!require(aules)) install.packages("arules")
library(arules)
data(Groceries)
summary(Groceries)
Groceries
head(inspect(Groceries))

dadread <- read.csv('./data/dvdtrans.csv')
str(dadread)

tran_data = data.frame(ID = seq(1:10), Item = 0)

for(i in 1: max(dadread$ID)){
  tran_data$Item[i] <- paste(dadread[dadread$ID == i,'Item'], collapse = ',')
}


library(dplyr)
dadread %>%
  group_by(ID) %>%
  summarize(text =  paste0(Item, collapse = ','))
      


library(ggplot2)
mpg
ggplot(data = mpg) + geom_point(aes(displ, hwy, colour =  class))
plot(mpg$displ, mpg$hwy)

if(!require("gapminder")) install.packages("gapminder")
library(gapminder)
str(gapminder)

p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))
summary(p)

p_point <- p+geom_point()
summary(p_point)
p_point

gap_af <- gapminder %>% filter(country == 'Afghanistan')
ggplot(gap_af, aes(x = year, y = lifeExp))+ geom_line()
ggplot(gap_af)+ geom_line(aes(x = year, y = lifeExp))
ggplot(gap_af, aes(x = year, y = lifeExp))+ geom_line(aes(x = year, y = lifeExp))


ggplot(gapminder, aes(x = log(gdpPercap), y = lifeExp)) + geom_point()

p_point_log <- p_point + scale_x_log10()
p_point_log

p_point_color <- p + geom_point(aes(color = continent))
p_point_color
summary(p_point_color)

p_point + stat_smooth()


#실습2

#Problem1 : pop의 히스토그램

Problem1 <- ggplot(gapminder) + geom_histogram(aes(x = pop))
ggplot(gapminder) + aes(x = pop) + geom_histogram()

#Problem2 : continet를 x축 lifeExp를 y축으로 하는 바이올린챠트를 그리고 평균을 파란색점으로 추가

Problem2 <- ggplot(gapminder, aes(x = continent, y = lifeExp)) + 
  geom_violin() + 
  stat_summary(fun.y = "mean", geom = "point")

geom_point(gapminder %>% group_by(continent) %>% summarise(mean = mean(lifeExp)))

#Problem3 : continet별로 몇개의 데이터가 있는지 세어 빈도를 만들고 바 챠트를 만들어라

gapminder_fq <- gapminder %>%
  group_by(continent) %>%
  summarize(continent_fq = n())

Problem3 <- ggplot(gapminder) + geom_bar(aes(x = continent))


#Problem4 : Canada, Rwanda, Cambodia, Mexico4나라만 사용하여 x축은 year, y축은 lifeExp로 line챠트를 각녀도에 점을 추가하여 그려라

gapminder_4 <- gapminder %>%
  filter(country %in% c('Canada', 'Rwanda', 'Cambodia', 'Mexico')) %>%
  ggplot(aes(year, lifeExp, color = continent)) +
           geom_point() +
           geom_line()
         
        


if(!require('ggmap')){devtools::install_github('dkahle/ggmap')}
library(ggmap)
loc <- URLencode(enc2utf8('서울'))
tar <- URLencode(enc2utf8('서울시청'))
geocityhall <- geocode(tar)
get_googlemap(loc, maptype = 'roadmap', markers = geocityhall, zoom = 15) %>% ggmap()
get_googlemap(loc, maptype = 'roadmap', zoom = 15) %>% ggmap() + geom_point(aes(geocityhall$lon,geocityhall$lat))
