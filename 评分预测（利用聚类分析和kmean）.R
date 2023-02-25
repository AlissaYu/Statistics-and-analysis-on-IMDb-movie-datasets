# 聚类分析：利用year,duration,budget
attach(imdb)
imdb.2 <- imdb[,c(4,7,17,15)]
k <- kmeans(scale(imdb.2),iter.max = 100, centers = 20, nstart = 20)
imdb.2$cluster <- k[["cluster"]]
# 利用聚类结果预测评分
# 需要的数据维度：,duration,budget
unknown <- data.frame(year = 1950, duration = 96, budget = 300000)
get_points <- function(unknown){
  dis_record_total <- rep(0,20)
  for(i in 1:20){
    clt <- imdb.2[which(imdb.2$cluster == i),]
    dt <- rbind(unknown,clt[,-c(4,5)])
    d <- dist(scale(dt))[1:nrow(clt)]
    dis_record_total[i] <- mean(d)
  }
  group <- which(dis_record_total == min(dis_record_total))
  if (length(group) != 1){
    group <- group[1]
  }
  points_predict <- mean(imdb[which(imdb.2$cluster == group),'avg_vote'])
  return(points_predict)
}
points_predict <- get_points(unknown)
print(points_predict)



# �性变量预测tor,production company,actors,genre
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

# ɾ�除空白值 imdb[imdb$director != "",]
imdb <- imdb[imdb$actors != "",]
imdb <- imdb[imdb$production_company != "",]
imdb <- imdb[imdb$genre != "",]
# ѵ训练集��(nrow(imdb)/2),]
# ???Լ?测试集测试row(imdb)/2):nrow(imdb),]

movie <- imdb.4[2,]
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
  position <- rank(cos_record_total)[1:10]
  points_predict <- mean(imdb$avg_vote[position])
  return(points_predict)
}
points_pre <- get_points_2(movie)
print(points_pre)