library(dplyr)
library(ggplot2)
#library(geosphere)
#library(lubridate)
library(caret)
library(RANN)
library(randomForest)
#library(gbm)
library(xgboost)
#library(ROSE)
select <- dplyr::select

setwd("C:/Users/user/Misaeng/수업/2019-1/데이터마이닝/Project")

####--------------------------- GMM -------------------------####

data1 <- filter(original.data, days > 31 & as.Date(area_st_date) < "2018-01-01" &
                  !grepl("행사", close_reason) & !grepl("한시", close_reason) &
                  !grepl("단기", close_reason) & !grepl("착오", close_reason) &
                  !grepl("박람회", close_reason))
data2 <- filter(data1, str_length(close_date)!=0)

myGMM <- function(x, threshold=10^(-10), max.iter=1000) {
  pi <- 1/2; mu <- sort(sample(x, 2)); sig <- rep(var(x), 2)
  err <- 1; iter <- 0
  while(err > threshold & iter < max.iter ){
    y <- pi*dnorm(x, mu[1], sig[1])/(pi*dnorm(x, mu[1], sig[1])+(1-pi)*dnorm(x, mu[2], sig[2]))
    old.pi <- pi; pi <- sum(y)/length(x)
    mu <- c(sum(y*x)/sum(y), sum((1-y)*x)/sum(1-y))
    sig <- sqrt(c(sum(y*(x-mu[1])^2)/sum(y), sum((1-y)*(x-mu[2])^2)/sum(1-y)))
    err <- abs(pi-old.pi); iter <- iter + 1
  }
  return(list(y=as.numeric(y>0.5), parameter=c(pi=pi, mu=mu, sigma=sig),
              error=c(error=err, iteration=iter)))
}

X <- c(X1 = rnorm(20000, 1, 2), X2 = rnorm(10000, 3, 3))
Class <- as.factor(rep(c(1, 2), c(20000,10000)))
ggplot() + geom_density(aes(X))
ggplot() + geom_density(aes(X, linetype=Class)) + scale_linetype_manual(values=c(1, 2))
GMM1 <- myGMM(X); GMM1[c(2,3)]

Close <- as.factor(str_length(data1$close_date)!=0)
ggplot() + geom_density(aes(data1$days))
ggplot() + geom_density(aes(data1$days, linetype=Close)) + scale_linetype_manual(values=c(1, 2))
GMM <- myGMM(data2$days); GMM[c(2,3)]


## 로별 평균 영업일수 vs 월 평균 매출금액 그래프
data.m <- group_by(data, ro) %>% summarise(avg_days = mean(days))
data.m <- filter(data, !is.na(close_date)) %>% group_by(data, ro) %>% summarise(avg_days = mean(days))

maechul <- read.csv("maechul.csv", encoding="utf8")
data.merge <- merge(data.m, maechul, by.x="ro", by.y="상권_코드_명")
ggplot(data.merge) + geom_point(aes(avg_days, 당월_매출_금액))



####----------------- 위도, 경도 코드 --------------------####
f <- function(x, d){ sum(x < d) }

## 반경 500m 내 주차장 수
DM_parking_lot <- read.csv("DM_parking_lot.csv")

table1 = data
table2 = filter(DM_parking_lot[complete.cases(DM_parking_lot),], !duplicated(주소))

dist <- distm(cbind(table1$lon, table1$lat), cbind(table2$lon, table2$lat), fun=distHaversine)
parking_lot <- apply(dist, 1, f, 500)


## 반경 1000m 내 지하철역 수
subway_latlon <- read.csv("subway_latlon.csv", encoding="utf8")
#blog <- read.csv("blog.csv", encoding="utf8")  # 서울시 내 지하철역만 포함
blog2 <- read.csv("blog2.csv", fileEncoding="utf8")

#subway_blog_latlon <- left_join(blog, filter(subway_latlon, !duplicated(station)))
#write.csv(subway_blog_latlon, "subway_blog_latlon.csv")
subway_blog_latlon <- read.csv("subway_blog_latlon.csv")
table2 <- subway_blog_latlon

dist <- distm(cbind(table1$lon, table1$lat), cbind(table2$lon, table2$lat), fun=distHaversine)
subway <- apply(dist, 1, f, 1000)
table(subway)


## 반경 1000m 내 지하철역 카페 블로그 건수
f2 <- function(x, d){ as.character(subway_blog_latlon[x < d, "station"]) }
stations <- apply(dist, 1, f2, 1000)  # 반경 500m 내 지하철역 이름 추출

subway_blog <- select(subway_blog_latlon, station:X2018.12.01) %>%
  gather(X2014.12.01:X2018.12.01, key="year.month", value="blog") %>% arrange(station) %>%
  mutate(year=rep(c(2014, rep(2015:2018, each=12)), 292), month=rep(c(12, rep(1:12, 4)), 292)) %>%
  select(-year.month)
#blog2 <- blog2 %>% gather(X2019.01.01:X2019.04.01, key="year.month", value="blog") %>% arrange(X0) %>%
#  mutate(year=rep(2019, 4*292), month=rep(1:4, 292)) %>% select(-year.month)
#colnames(blog2)[1] <- "station"
#subway_blog <- rbind(subway_blog, blog2) %>% arrange(station, year, month)
write.csv(subway_blog, "subway_blog.csv")

blog <- c()
for(i in 1:nrow(data)){
  Xmonth <- ymd(data$area_st_date[i])-months(1)
  f <- filter(subway_blog, year==year(Xmonth) & month==month(Xmonth) & station %in% stations[[i]])
  blog[i] <- ifelse(length(stations[[i]])==0, 0, sum(f$blog))
}



####------------------- 임대료 ---------------------####
data <- read.csv("DM_rental_fee_ver3.csv")
colnames(data) <- c('dong', 'total', 'first', 'not1st', 'year', 'quarter')
summary(data)
nrow(data)

t <- filter(data, dong %in% filter(data, is.na(year))$dong) %>% arrange(dong)
data[is.na(data$year), "year"] <- 2017

sum(!complete.cases(data))                                                         #=668

# 임대료는 지역차가 너무 크기 때문에 missing도 동 안에서 해결하는게 맞는 듯
missing_dong <- unique(filter(data, !complete.cases(data))$dong); length(missing_dong) # 126개 동
data1 <- filter(data, dong %in% missing_dong) %>% arrange(dong)                      # n=2142 (126*17)

nrow(filter(data1, is.na(first) & is.na(not1st)))                                # =160 (1층, 1층외 둘다 missing)
missing_both_dong <- unique(filter(data1, is.na(first) & is.na(not1st))$dong)    # 38개 동
group_by(data1, dong) %>% summarise(sum=sum(total, na.rm=T)) %>% filter(sum==0)
filter(data, dong %in% c('개포1동', '잠실7동', '하계2동')) %>% arrange(dong)
#개포1동(40), 잠실7동(0), 하계2동(2)(-> 하계1동으로 채우면 되지 않을까?)

## 상관계수
ff <- select(final, area_st_year, total_floor, total_area, franchise, near, near_caf., near_franchise,
             floating_pop, living_pop, office_pop, female, income_grade, household_num, rental_fee,
             parking_lot, crosswalk, subway, blog, kospi_avg_lag1, 32:55, near_new_cafe)
ff$floating_pop <- as.numeric(sub(",", "", ff$floating_pop))
ff$office_pop <- as.numeric(sub(",", "", ff$office_pop))
ff$income_grade <- as.numeric(sub("분위", "", ff$income_grade))
cor(ff[complete.cases(ff),])[14,]                                 ### 상관계수 제일 높은게 0.33(office_pop)

data <- arrange(data, dong) %>% mutate(sequence = rep(1:17, 450))
cor(data[complete.cases(data), c(2:4, 7)])                        ### 1층이랑 1층외가 0.66 (전체X)


## 1) 1층, 1층외 둘 중 하나는 있는 거
data2 <- filter(data1, !(dong %in% c('개포1동', '잠실7동', '하계2동')))  # =2091

for(i in 1:nrow(data2)){
  lm.d.first <- lm(first ~ not1st-1, filter(data2, dong==data2$dong[i]), na.action=na.omit)
  lm.d.not1st <- lm(not1st ~ first-1, filter(data2, dong==data2$dong[i]), na.action=na.omit)
  if(is.na(data2$first[i])) data2$first[i] <- predict(lm.d.first, data2[i,])
  if(is.na(data2$not1st[i])) data2$not1st[i] <- predict(lm.d.not1st, data2[i,])
}

data1 <- rbind(filter(data1, !(dong %in% unique(data2$dong))), data2) %>% arrange(dong)


## 2) 1층, 1층외 둘 다 없는 거
data3 <- filter(data1, !(dong %in% c('개포1동', '잠실7동', '하계2동')) & dong %in% missing_both_dong)

for(i in 1:nrow(data3)){
  if(is.na(data3$first[i])) data3$first[i] <- mean(filter(data3, dong==data3$dong[i])$first, na.rm=T)
  if(is.na(data3$not1st[i])) data3$not1st[i] <- mean(filter(data3, dong==data3$dong[i])$not1st, na.rm=T)
}

data1 <- rbind(filter(data1, !(dong %in% unique(data3$dong))), data3) %>% arrange(dong)
data.final <- rbind(filter(data, !(dong %in% unique(data1$dong))), data1) %>% arrange(dong)


## 3) 개포1동 -> 개포2동으로, 하계2동 -> 하계1동으로
lm.1dong <- lm(first ~ not1st-1, filter(data.final, dong=="하계1동"))
for(i in 1:nrow(data.final)){
  if(data.final$dong[i]=="하계2동" & is.na(data.final$first[i])){
    data.final$first[i] <- predict(lm.1dong, data.final[i,]) }
}

for(i in 1:nrow(data.final)){
  if(data.final$dong[i]=="하계2동" & is.na(data.final$first[i])){
    data.final$first[i] <- mean(filter(data.final, dong=="하계2동")$first, na.rm=T)
  }
  if(data.final$dong[i]=="하계2동" & is.na(data.final$not1st[i])){
    data.final$not1st[i] <- mean(filter(data.final, dong=="하계2동")$not1st, na.rm=T)
  }
}

#data.final <- rbind(filter(data.final, dong!="하계2동"), filter(data, dong=="하계2동") %>% select(-sequence)) %>% arrange(dong)
data.final <- select(data.final, -total) %>% filter(dong!="잠실7동")
sum(!complete.cases(data.final))
write.csv(data.final, "rental_fee_final.csv")



#### ------------------------ 시각화 ------------------------####

df1 <- group_by(data, y) %>% summarise(total_area=mean(total_area))
ggplot(df1, aes(as.factor(y), total_area)) + geom_col(width=0.5) +
  labs(title=expression(paste("평균 총면적 (", m^2, ")")), x="", y="") +
  scale_x_discrete(labels=c("1년 이상 영업", "1년 내 폐업"))

df2 <- group_by(data, floor) %>% summarise(y=mean(y))
ggplot(df2, aes(floor, y)) + geom_col(width=0.8) + labs(title="1년 내 폐업할 확률", y="") +
  scale_x_discrete(limits=arrange(df2, y)$floor, name="floor")

rental_fee <- read.csv("rental_fee_final.csv")
dong_code <- read.csv("dong_code.csv")
dong_code <- filter(dong_code, 시도명=="서울특별시")
for(i in 1:13) dong_code$행정동명 <- gsub(paste("제", i, sep=""), i, dong_code$행정동명)
dong_code$행정동명[1538] <- '홍제1'

df <- group_by(rental_fee, dong) %>% summarise(first=mean(first), not1st=mean(not1st)) %>%
  left_join(select(dong_code, 시군구명, 행정동명), by=c("dong"="행정동명"))
df$시군구명[c(826:832, 834:839, 841, 843:846, 849:853, 855:856, ] <- df$dong[c(826:832, 834:839, 841, 843:846, 849:853, 855:856, ]
df <- filter(df, !duplicated(dong) & !is.na(시군구명))

df.first <- group_by(df, 시군구명) %>% summarise(y=mean(first, na.rm=T))

ggplot(df.first, aes(시군구명, y)) + geom_col() + labs(title="임대료(1층)", y="") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=arrange(df.first, y)$시군구명, name="행정구")

df.not1st <- group_by(df, 시군구명) %>% summarise(y=mean(not1st, na.rm=T))

ggplot(df.not1st, aes(시군구명, y)) + geom_col() + labs(title="임대료(1층외)", y="") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=arrange(df.not1st, y)$시군구명, name="행정구")

df3 <- group_by(final, gu) %>% summarise(y=mean(y))
ggplot(df3, aes(gu, y)) + geom_col() + labs(title="2년 내 폐업할 확률", y="") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=arrange(df3, y)$gu, name="행정구")

df1 <- group_by(final_full, area_st_date) %>% summarise(y=mean(kospi_avg))
df2 <- group_by(final_full, area_st_date) %>% summarise(y=sum(is.na(close_date)))
df <- as.data.frame(rbind(df1, df2) %>% mutate(legend=factor(rep(c("KOSPI", "폐업 수"), each=nrow(df1)))))
ggplot(df) + geom_line(aes(x=rep(1:nrow(df1), 2), y=y, line=legend)) +
  scale_x_continuous(labels=df$area_st_date)



#### ------------------------ 모델링 ------------------------####

final <- read.csv("final_real.csv")
#final_full <- read.csv("final_real_full.csv")
final <- select(final, -close, -days, -area_st_year, -area_st_date, -lat, -lon, -train_test_label)

summary(final)
colnames(final)[2] <- "y"
final$area_st_month <- as.factor(final$area_st_month)
final$area_st_season <- as.factor(final$area_st_season)

data <- final


### train / test 나누기  # random으로 train, test 나눠보기
#train <- filter(data, train_test_label=="train") %>% select(-train_test_label)
#test <- filter(data, train_test_label=="test") %>% select(-train_test_label)
set.seed(11)
train.index <- createDataPartition(data$y, p=0.7, list=F)
#train.index <- sample(nrow(data), nrow(data)*0.7)
train <- data[train.index,]
test <- data[-train.index,]
table(train$y)/nrow(train); nrow(train)
table(test$y)/nrow(test); nrow(test)


# Missing values 처리 (train set)
X.train <- select(train, -y)
X.test <- select(test, -y)
preProcess_model <- preProcess(X.train, method='knnImpute')
X.train <- predict(preProcess_model, X.train)
X.test <- predict(preProcess_model, X.test)
train <- data.frame(y = train$y, X.train)
test <- data.frame(y = test$y, X.test)
# train <- train[complete.cases(train),]
# test <- test[complete.cases(test),]


# oversampling
# train.rose <- ROSE(y_oneyear ~ ., train, p=0.2, seed=1)$data
# table(train.rose$y)/nrow(train.rose)
# train.data <- rbind(filter(train, y_oneyear==0), filter(train.rose, y_oneyear==1)); nrow(train.data)
# table(train.data$y_oneyear)/nrow(train.data)
# 
# undersampling (2:1)
# train.data <- rbind(filter(train, y==0)[sample(sum(train$y==0), 2*sum(train$y==1)),],
#                     filter(train, y==1))
# table(train.data$y)/nrow(train.data)


### 구별로 묶어서 해보기
train.data <- filter(train, gu=="중구") %>% select(-gu)
test.data <- filter(test, gu=="중구") %>% select(-gu)
#data <- filter(final, gu %in% c("강남구", "송파구", "서초구", "종로구", "마포구", "서대문구")) %>%
#  mutate(gu = as.factor(as.character(data$gu)))
#data <- filter(final, !(gu %in% c("중구", 강남구", "송파구", "서초구", "종로구", "마포구", "서대문구"))) %>%
#  mutate(gu = as.factor(as.character(data$gu)))
train.data <- select(train, -(kospi_avg_lag2:kospi_avg_lag4))
test.data <- select(test, -(kospi_avg_lag2:kospi_avg_lag4))
train.data2 <- select(train.data, -total_sales, -rental_fee)
test.data2 <- select(test.data, -total_sales, -rental_fee)



## Random Forest
p <- ncol(train.data)-1
set.seed(0530)
rf <- randomForest(as.factor(y)~., train.data, importance=T)
t <- table(predict(rf), train.data$y); t; (t[1,2]+t[2,1])/sum(t)
t <- table(predict(rf, test.data), test.data$y); t; (t[1,2]+t[2,1])/sum(t)
t <- table(as.numeric(predict(rf, test.data, type="prob")[,2]>0.15), test.data$y); t; (t[1,2]+t[2,1])/sum(t)

rf.all <- rf
varImpPlot(rf, n.var=13)
pdp.gu <- partialPlot(rf, pred.data=train.data, x.var="gu", which.class=1)
pdp.gu.df <- data.frame(x=pdp.gu$x, y=pdp.gu$y)
ggplot() + geom_bar(aes(x=pdp.gu$x, y=pdp.gu$y), stat="identity")
partialPlot(rf, pred.data=train.data, x.var="area_st_month"); levels(train$gu)
partialPlot(rf, pred.data=train.data, x.var="near"); levels(train$gu)



## XGboost
X.train <- model.matrix(y ~ . -1, train.data) 
X.test <- model.matrix(y ~ . -1, test.data)
train.xgb <- xgb.DMatrix(X.train, label=train.data$y)
test.xgb <- xgb.DMatrix(X.test, label=test.data$y)

watchlist <- list(train=train.xgb, test=test.xgb)
xgb <- xgb.train(data=train.xgb, label=train.data$y, max_depth = 20, eta = 0.1, nthread = 20,
                 nrounds = 50, watchlist=watchlist, objective = "binary:logistic")

xgb.pred <- predict(xgb, test.xgb)
table(as.numeric(xgb.pred>0.5), test$y) 



###### caret  #######
library(caretEnsemble)
# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

algorithmList <- c('rf', 'xgbDART', 'svmRadial')

train.data.caret = train
train.data.caret$floor = 
  factor(train.data.caret$floor, labels=c("first","first_gt2nd","gt2nd","basement","basement_ground"))
train.data.caret$y_oneyear = factor(y_oneyear, labels=c("No", "Yes")))

models <- caretList(y_oneyear ~ . -area_st_season, data=train.data.caret,
                    trControl=trainControl, methodList=algorithmList)
Sys.time() # 4:42~
results <- resamples(models)
summary(results)

# Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
