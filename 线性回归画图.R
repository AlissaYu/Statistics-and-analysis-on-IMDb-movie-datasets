imdb.2 = read.csv("imdb(2).csv")

r <- rep(0,41)
for (i in 24:64){
  r[i] <- sum(imdb.2[,i])
}
rr <- which(r != 0)

# 数据分析

# 数据划分
# 训练集
imdbm <- data.frame(scale(imdb.2[,c(16,5,8,68:75)]),imdb.2[,c(rr)])
imdb.3 <- imdbm[1:(nrow(imdbm)*7/8),c(1,2,3,6,9,10,11,19)]
# 测试集
imdb.4 <- imdbm[(nrow(imdbm)*7/8):nrow(imdbm),c(1,2,3,6,9,10,11,19)]


# 线性回归
# 初步线性回归
attach(imdb.3)
pingfen.lm <- lm(avg_vote ~ .-avg_vote,data = imdbm)
summary(pingfen)
# Adjusted R-squared:  0.8098 

# 模型优化
step = step(pingfen)
summary(step)
pingfen.lm <- lm(formula = avg_vote ~ year + duration + realbudget + realworldwide_gross_income + 
                   directorincome + actorincome + companyincome + directorvote + 
                   actorvote + companyvote + action + adventure + animation + 
                   biography + comedy + crime + drama + family + horror + musical + 
                   mystery + romance + sport + cantonese + german + hindi + 
                   japanese + latin + korean, data = imdbm)
# Adjusted R-squared:   0.81 
drop1(pingfen)
# 去除korean german musical crime action 

# 变量调整
pingfen.lm <- lm(formula = avg_vote ~  + duration + directorincome:directorvote + 
                   directorvote:companyvote + actorvote:companyvote  + 
                   realbudget:realworldwide_gross_income  +
                   directorincome:actorincome + companyincome:directorincome + 
                   realbudget + realworldwide_gross_income + directorincome + 
                   actorincome + companyincome + directorvote + actorvote + 
                   companyvote + adventure + animation + biography + comedy + 
                   drama + family + horror + mystery + romance + cantonese + 
                   hindi + japanese + latin, data = imdbm)

summary(pingfen.lm)
# Adjusted R-squared:  0.8127 
# 模型优化调整结束

# 模型诊断
par(mfrow=c(2,2))

plot(pingfen.lm, pch = 15, cex = 0.1, 
     col = '#A8987B', col.axis = '#84533D',
     col.lab = '#84533D', fg = '#84533D')