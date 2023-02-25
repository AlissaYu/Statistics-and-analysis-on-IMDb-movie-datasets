# 数据读取
imdb = read.csv("IMDb movies.csv")
imdb = imdb[imdb$worlwide_gross_income != "",]
imdb = imdb[imdb$avg_vote != "",]
imdb = imdb[grep("$",imdb$budget,fixed = T),] 
# read data and throw out useless figures

imdb$year <- as.numeric(imdb$year)
imdb$avg_vote <- as.numeric(imdb$avg_vote)
imdb$duration <- as.numeric(imdb$duration)
imdb$votes <- as.numeric(imdb$votes)

imdb <- imdb[grepl('\\$',imdb$budget),]
budget <- imdb$budget
budget <- gsub('\\$','0',budget)
budget <- gsub(',','',budget)
imdb$budget <- as.numeric(budget)

imdb <- imdb[grepl('\\$',imdb$worlwide_gross_income),]
wincome <- imdb$worlwide_gross_income
wincome <- gsub('\\$','0',wincome)
wincome <- gsub(',','',wincome)
imdb$worlwide_gross_income <- as.numeric(wincome)
# translate strings to values

get_good_imdb <- function(imdb){
  # genres
  
  g = strsplit(imdb$genre,", ") 
  # store genres
  
  action = rep(0,9025)
  for (i in 1:9025){
    if ("Action" %in% g[[i]]) {action[i] = 1}
  }
  imdb$action = action 
  # if a film is an action film(1) or not(0)
  
  adventure = rep(0,9025)
  for (i in 1:9025){
    if ("Adventure" %in% g[[i]]) {adventure[i] = 1}
  }
  imdb$adventure = adventure 
  # if is adventure or not
  
  animation = rep(0,9025)
  for (i in 1:9025){
    if ("Animation" %in% g[[i]]) {animation[i] = 1}
  }
  imdb$animation = animation # if is animation
  
  biography = rep(0,9025)
  for (i in 1:9025){
    if ("Biography" %in% g[[i]]) {biography[i] = 1}
  }
  imdb$biography = biography
  
  comedy = rep(0,9025)
  for (i in 1:9025){
    if ("Comedy" %in% g[[i]]) {comedy[i] = 1}
  }
  imdb$comedy = comedy
  
  crime = rep(0,9025)
  for (i in 1:9025){
    if ("Crime" %in% g[[i]]) {crime[i] = 1}
  }
  imdb$crime = crime
  
  documentary = rep(0,9025)
  for (i in 1:9025){
    if ("Documentary" %in% g[[i]]) {documentary[i] = 1}
  }
  imdb$documentary = documentary
  
  drama = rep(0,9025)
  for (i in 1:9025){
    if ("Drama" %in% g[[i]]) {drama[i] = 1}
  }
  imdb$drama = drama
  
  family = rep(0,9025)
  for (i in 1:9025){
    if ("Family" %in% g[[i]]) {family[i] = 1}
  }
  imdb$family = family
  
  fantasy = rep(0,9025)
  for (i in 1:9025){
    if ("Fatasy" %in% g[[i]]) {fantasy[i] = 1}
  }
  imdb$fantasy = fantasy
  
  film_noir = rep(0,9025)
  for (i in 1:9025){
    if ("Film-Noir" %in% g[[i]]) {film_noir[i] = 1}
  }
  imdb$film_noir = film_noir
  
  history = rep(0,9025)
  for (i in 1:9025){
    if ("History" %in% g[[i]]) {history[i] = 1}
  }
  imdb$history = history
  
  horror = rep(0,9025)
  for (i in 1:9025){
    if ("Horror" %in% g[[i]]) {horror[i] = 1}
  }
  imdb$horror = horror
  
  music = rep(0,9025)
  for (i in 1:9025){
    if ("Music" %in% g[[i]]) {music[i] = 1}
  }
  imdb$music = music
  
  musical = rep(0,9025)
  for (i in 1:9025){
    if ("Musical" %in% g[[i]]) {musical[i] = 1}
  }
  imdb$musical = musical
  
  mystery = rep(0,9025)
  for (i in 1:9025){
    if ("Mystery" %in% g[[i]]) {mystery[i] = 1}
  }
  imdb$mystery = mystery
  
  romance = rep(0,9025)
  for (i in 1:9025){
    if ("Romance" %in% g[[i]]) {romance[i] = 1}
  }
  imdb$romance = romance
  
  sci_fi = rep(0,9025)
  for (i in 1:9025){
    if ("Sci-Fi" %in% g[[i]]) {sci_fi[i] = 1}
  }
  imdb$sci_fi = sci_fi
  
  sport = rep(0,9025)
  for (i in 1:9025){
    if ("Sport" %in% g[[i]]) {sport[i] = 1}
  }
  imdb$sport = sport
  
  thriller = rep(0,9025)
  for (i in 1:9025){
    if ("Thriller" %in% g[[i]]) {thriller[i] = 1}
  }
  imdb$thriller = thriller
  
  war = rep(0,9025)
  for (i in 1:9025){
    if ("War" %in% g[[i]]) {war[i] = 1}
  }
  imdb$war = war
  
  western = rep(0,9025)
  for (i in 1:9025){
    if ("Western" %in% g[[i]]) {western[i] = 1}
  }
  imdb$western = western
  
  #languages
  
  l = strsplit(imdb$language,", ") # store languages
  # the types of languages are too much, only languages occuring over 60 times were listed seperately 
  otherl = rep(1,9025)
  
  arabic = rep(0,9025)
  for (i in 1:9025){
    if ("Arabic" %in% l[[i]]) {
      arabic[i] = 1
      otherl[i] = 0
    }
  }
  imdb$arabic = arabic
  
  cantonese = rep(0,9025)
  for (i in 1:9025){
    if ("Cantonese" %in% l[[i]]) {
      cantonese[i] = 1
      otherl[i] = 0
    }
  }
  imdb$cantonese = cantonese
  
  chinese = rep(0,9025)
  for (i in 1:9025){
    if ("Chinese" %in% l[[i]]) {
      chinese[i] = 1
      otherl[i] = 0
    }
  }
  imdb$chinese = chinese
  
  english = rep(0,9025)
  for (i in 1:9025){
    if ("English" %in% l[[i]]) {
      english[i] = 1
      otherl[i] = 0
    }
  }
  imdb$english = english
  
  french = rep(0,9025)
  for (i in 1:9025){
    if ("French" %in% l[[i]]) {
      french[i] = 1
      otherl[i] = 0
    }
  }
  imdb$french = french
  
  german = rep(0,9025)
  for (i in 1:9025){
    if ("German" %in% l[[i]]) {
      german[i] = 1
      otherl[i] = 0
    }
  }
  imdb$german = german
  
  hebrew = rep(0,9025)
  for (i in 1:9025){
    if ("Hebrew" %in% l[[i]]) {
      hebrew[i] = 1
      otherl[i] = 0
    }
  }
  imdb$hebrew = hebrew
  
  hindi = rep(0,9025)
  for (i in 1:9025){
    if ("Hindi" %in% l[[i]]) {
      hindi[i] = 1
      otherl[i] = 0
    }
  }
  imdb$hindi = hindi
  
  italian = rep(0,9025)
  for (i in 1:9025){
    if ("Italian" %in% l[[i]]) {
      italian[i] = 1
      otherl[i] = 0
    }
  }
  imdb$italian = italian
  
  japanese = rep(0,9025)
  for (i in 1:9025){
    if ("Japanese" %in% l[[i]]) {
      japanese[i] = 1
      otherl[i] = 0
    }
  }
  imdb$japanese = japanese
  
  latin = rep(0,9025)
  for (i in 1:9025){
    if ("Latin" %in% l[[i]]) {
      latin[i] = 1
      otherl[i] = 0
    }
  }
  imdb$latin = latin
  
  korean = rep(0,9025)
  for (i in 1:9025){
    if ("Korean" %in% l[[i]]) {
      korean[i] = 1
      otherl[i] = 0
    }
  }
  imdb$korean = korean
  
  mandarin = rep(0,9025)
  for (i in 1:9025){
    if ("Mandarin" %in% l[[i]]) {
      mandarin[i] = 1
      otherl[i] = 0
    }
  }
  imdb$mandarin = mandarin
  
  portuguese = rep(0,9025)
  for (i in 1:9025){
    if ("Portuguese" %in% l[[i]]) {
      portuguese[i] = 1
      otherl[i] = 0
    }
  }
  imdb$portuguese = portuguese
  
  russian = rep(0,9025)
  for (i in 1:9025){
    if ("Russian" %in% l[[i]]) {
      russian[i] = 1
      otherl[i] = 0
    }
  }
  imdb$russian = russian
  
  spanish = rep(0,9025)
  for (i in 1:9025){
    if ("Spanish" %in% l[[i]]) {
      spanish[i] = 1
      otherl[i] = 0
    }
  }
  imdb$spanish = spanish
  
  turkish = rep(0,9025)
  for (i in 1:9025){
    if ("Turkish" %in% l[[i]]) {
      turkish[i] = 1
      otherl[i] = 0
    }
  }
  imdb$turkish = turkish
  
  ukrainian = rep(0,9025)
  for (i in 1:9025){
    if ("Ukrianian" %in% l[[i]]) {
      ukrainian[i] = 1
      otherl[i] = 0
    }
  }
  imdb$ukrainian = ukrainian
  
  imdb$other_language = otherl # other languages
  return(imdb)
}
imdb <- get_good_imdb(imdb)


#通货膨胀换算
year_CPI = read.csv("美国消费者物价指数(CPI)(年).csv")
realbudget = rep(0,nrow(imdb))
realworldwide_gross_income = rep(0,nrow(imdb))
for(i in 1:nrow(imdb)){
  m = match(imdb$year[i],year_CPI[1][,1])
  CPIofyear = year_CPI[2][m,1]
  realbudget[i] = imdb$budget[i] * CPIofyear/258.84
  realworldwide_gross_income[i] = imdb$worlwide_gross_income[i] * CPIofyear/258.84
}
realbudget = t(realbudget)
realworldwide_gross_income = t(realworldwide_gross_income)
realworldwide_gross_income = as.numeric(realworldwide_gross_income)
realbudget = as.numeric(realbudget)
imdb = cbind(imdb,realbudget,realworldwide_gross_income)

# 删除空白值
imdb <- imdb[imdb$director != "",]
imdb <- imdb[imdb$actors != "",]
imdb <- imdb[imdb$production_company != "",]
imdb <- imdb[imdb$genre != "",]

# 定性变量--->平均平均值
# director,production company,actors,genre
imdb.6 = read.csv("Imdb.6.csv")
genre = strsplit(imdb$genre,", ") 
g <- as.vector(unlist(genre))
g <- g[!duplicated(g)]

director = strsplit(imdb$director,", ") 
d <- as.vector(unlist(director))
d <- d[!duplicated(d)]

actors = strsplit(imdb$actors,", ") 
a <- as.vector(unlist(actors))
a <- a[!duplicated(a)]

production_company = strsplit(imdb$production_company,", ") 
p <- as.vector(unlist(production_company))
p <- p[!duplicated(p)]


get_a_vote <- function(a_element){
  position <- grep(a_element,imdb[,'actors'])
  avevote <- mean(imdb[c(position),'avg_vote'])
  return(avevote)
}
actorvotes <- sapply(a,get_a_vote)

get_d_vote <- function(d_element){
  position <- grep(d_element,imdb[,'actors'])
  avevote <- mean(imdb[c(position),'avg_vote'])
  return(avevote)
}
directorvotes <- sapply(d,get_d_vote)

get_p_vote <- function(p_element){
  position <- grep(p_element,imdb[,'actors'])
  avevote <- mean(imdb[c(position),'avg_vote'])
  return(avevote)
}
p_companyvotes <- sapply(p,get_p_vote)




imdb = cbind(imdb,directorincome,actorincome,companyincome)
# 以上代码我的电脑跑不出结果（surface真的不行）

# 相关性
imdb.2 = read.csv("imdb(2).csv")

imdbmember <- imdb.2[,c(16,5,8,68:75)]
imdbmember <- scale(imdbmember)

r <- rep(0,41)
for (i in 24:64){
  r[i] <- sum(imdb.2[,i])
}
rr <- which(r != 0)


imdbmember <- data.frame(imdbmember,imdb.2[,c(rr)])
a <- cor(imdbmember)
# a[1:50]反映了avg_vote与其他变量的相关系数，其中9，10，11与之相关性较高
# 即 directorvote, actorvote, companyvote

# 可视化
install.packages('corrplot')
library(corrplot)
b <- data.frame(imdbmember[,c(1,2,3,6,9,10,11,19)])
corrplot(corr = cor(b),order="AOE",type="upper",tl.pos="tp")
corrplot(corr = cor(b),add=TRUE, type="lower", method="number",order="AOE", col="black",diag=FALSE,tl.pos="n", cl.pos="n")


# 数据分析

# 数据划分
# 训练集
imdbm <- data.frame(imdb.2[,c(16,5,8,68:75)],imdb.2[,c(rr)])
imdb.3 <- imdbm[1:(nrow(imdbm)*7/8),c(1,2,3,6,9,10,11,19)]
# 测试集
imdb.4 <- imdbm[(nrow(imdbm)*7/8):nrow(imdbm),c(1,2,3,6,9,10,11,19)]


# 线性回归
# 初步线性回归
attach(imdb.3)
pingfen <- lm(avg_vote ~ directorincome + duration + 
                directorvote + companyvote + 
                actorvote + drama + year)
summary(pingfen)
# Adjusted R-squared:  0.7771 
# 模型优化
step = step(pingfen)
summary(step)
# 与原结果一样

# 变量调整
pingfen.lm <- lm(avg_vote ~ directorincome + duration + 
                 directorincome:directorvote +
                 directorvote:companyvote +
                 actorvote:companyvote + 
                   duration : log(year) + 
                directorvote + companyvote + 
                actorvote + drama + log(year))
summary(pingfen.lm)
# Adjusted R-squared:  0.7798 
# 模型优化调整结束

# 模型诊断
par(mfrow=c(2,2))

plot(pingfen.lm)


# 聚类分析
attach(imdb.3)
k <- kmeans(scale(imdb.3),iter.max = 100, centers = 10, nstart = 20)


install.packages("ggplot2")
install.packages("ggfortify")
library(ggplot2)
library(ggfortify)
autoplot(k, data = imdb.3)


# 预测
# 利用线性回归结果
result <- predict(pingfen.lm, newdata = imdb.4)
x <- data.frame(result, imdb.4$avg_vote)

# 利用聚类结果预测评分

imdb.3$cluster <- k[["cluster"]]
unknown <- imdb.4[3,]
get_points <- function(unknown){
  dis_record_total <- rep(0,10)
  for(i in 1:10){
    clt <- imdb.3[which(imdb.3$cluster == i),-c(1,9)]
    dt <- rbind(unknown,clt)
    d <- dist(scale(dt))[1:nrow(clt)]
    dis_record_total[i] <- mean(d)
  }
  group <- which(dis_record_total == min(dis_record_total))
  if (length(group) != 1){
    group <- group[1]
  }
  points_predict <- mean(imdb[which(imdb.3$cluster == group),'avg_vote'])
  return(points_predict)
}

result2 <- rep(0,rep(nrow(imdb.4)))
for(i in 1:nrow(imdb.4)){
  result2[i] <- get_points(imdb.4[i,-1])
}
y <- data.frame(result2, imdb.4$avg_vote)
y[1:9,]

# 定性变量预测

#重新获取训练集和测试集
# 训练集
imdb.3 <- imdb.2[1:(nrow(imdb.2)*7/8),]
# 测试集
imdb.4 <- imdb.2[(nrow(imdb.2)*7/8):nrow(imdb.2),]

genre = strsplit(imdb$genre,", ") 
g <- as.vector(unlist(genre))
g <- g[!duplicated(g)]

director = strsplit(imdb$director,", ") 
d <- as.vector(unlist(director))
d <- d[!duplicated(d)]

actors = strsplit(imdb$actors,", ") 
a <- as.vector(unlist(actors))
a <- a[!duplicated(a)]

production_company = strsplit(imdb$production_company,", ") 
p <- as.vector(unlist(production_company))
p <- p[!duplicated(p)]

get_director <- function(movie){
  m_director <- movie$director
  m <- strsplit(m_director,", ")
  m_director <- as.vector(unlist(m))
  position <- rep(0,length(m_director))
  for (i in 1:length(m_director)){
    position[i] <- which(d == c(m_director)[i])
  }
  d_record <- rep(0,length(d))
  d_record[position] <- rep(1,length(position))
  return(d_record)
}

get_actors <- function(movie){
  m_actors <- movie$actors
  m <- strsplit(m_actors,", ")
  m_actors <- as.vector(unlist(m))
  position <- rep(0,length(m_actors))
  for (i in 1:length(m_actors)){
    position[i] <- which(a == c(m_actors)[i])
  }
  a_record <- rep(0,length(a))
  a_record[position] <- rep(1,length(position))
  return(a_record)
}

get_genre <- function(movie){
  m_genre <- movie$genre
  m <- strsplit(m_genre,", ")
  m_genre <- as.vector(unlist(m))
  position <- rep(0,length(m_genre))
  for (i in 1:length(m_genre)){
    position[i] <- which(g == c(m_genre)[i])
  }
  g_record <- rep(0,length(g))
  g_record[position] <- rep(1,length(position))
  return(g_record)
}

get_production_company <- function(movie){
  m_p_company <- movie$production_company
  m <- strsplit(m_p_company,", ")
  m_p_company <- as.vector(unlist(m))
  position <- rep(0,length(m_p_company))
  for (i in 1:length(m_p_company)){
    position[i] <- which(p == c(m_p_company)[i])
  }
  p_record <- rep(0,length(p))
  p_record[position] <- rep(1,length(position))
  return(p_record)
}

get_points_2 <- function(movie){
  cos_record_total <- rep(0,nrow(imdb.3))
  for(j in 1:nrow(imdb.3)){
    cos <- 0
    current_imdb3_d <- get_director(imdb.3[j,])
    current_movie_d <- get_director(movie)
    cos_d <- sum(current_imdb3_d * current_movie_d) / (sqrt(sum(current_imdb3_d ^ 2)) * sqrt(sum(current_movie_d ^ 2)))
    
    current_imdb3_a <- get_actors(imdb.3[j,])
    current_movie_a <- get_actors(movie)
    cos_a <- sum(current_imdb3_a * current_movie_a) / (sqrt(sum(current_imdb3_a ^ 2)) * sqrt(sum(current_movie_a ^ 2)))
    
    current_imdb3_g <- get_genre(imdb.3[j,])
    current_movie_g <- get_genre(movie)
    cos_g <- sum(current_imdb3_g * current_movie_g) / (sqrt(sum(current_imdb3_g ^ 2)) * sqrt(sum(current_movie_g ^ 2)))
    
    current_imdb3_p <- get_production_company(imdb.3[j,])
    current_movie_p <- get_production_company(movie)
    cos_p <- sum(current_imdb3_p * current_movie_p) / (sqrt(sum(current_imdb3_p ^ 2)) * sqrt(sum(current_movie_p ^ 2)))
    
    cos_record_total[j] <- cos + cos_g + cos_a + cos_p + cos_d
  }
  position <- order(cos_record_total,decreasing = T)[1:10]
  points_predict <- mean(imdb$avg_vote[position])
  return(points_predict)
}

get_points_2(imdb.4[1,])

result3 <- rep(0,rep(nrow(imdb.4)))
result4 <- rep(0,9)














