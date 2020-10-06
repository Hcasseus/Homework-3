
>title: "Homework 2"
author: "Hertzbert Casseus"
date: "9/29/2020"



load("acs2017_ny_data.RData")
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))

norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}
is.na(RENT) <- which(RENT == 850)
living_cost <- RENT + COSTELEC + COSTFUEL + COSTGAS + COSTWATR
norm_inc_wage <- norm_varb(INCWAGE)
norm_living_cost <- norm_varb(living_cost)
data_use_prelim <- data.frame(norm_inc_wage,norm_living_cost)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)
Bronx     Manhattan Staten Island      Brooklyn        Queens 
4820          5164          1892         12395         10955 

prop.table(summary(cl_data))
Bronx     Manhattan Staten Island      Brooklyn        Queens 
0.13683075    0.14659626    0.05371033    0.35187078    0.31099188

summary(train_data)
norm_inc_wage     norm_living_cost
Min.   :0.00000   Min.   :0.0000  
1st Qu.:0.00000   1st Qu.:0.3725  
Median :0.03918   Median :0.5333  
Mean   :0.07038   Mean   :0.5538  
3rd Qu.:0.09404   3rd Qu.:0.7399  
Max.   :1.00000   Max.   :1.0000  

require(class)
for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}
[1] 1.0000000 0.3855839
[1] 3.0000000 0.3754187
[1] 5.0000000 0.3889338
[1] 7.0000000 0.3977128
[1] 9.0000000 0.4011782


load("acs2017_ny_data.RData")
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}
living_cost <- RENT + COSTELEC + COSTFUEL + COSTGAS + COSTWATR
norm_inc_wage <- norm_varb(INCWAGE)
norm_living_cost <- norm_varb(living_cost)
data_use_prelim <- data.frame(norm_inc_wage,norm_living_cost)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)
Bronx     Manhattan Staten Island      Brooklyn        Queens 
4880          5250          1891         12416         10923
prop.table(summary(cl_data))
Bronx     Manhattan Staten Island      Brooklyn        Queens 
0.13800905    0.14847285    0.05347851    0.35113122    0.30890837
summary(train_data)
norm_inc_wage     norm_living_cost
Min.   :0.00000   Min.   :0.0000  
1st Qu.:0.00000   1st Qu.:0.3744  
Median :0.03918   Median :0.5333  
Mean   :0.07077   Mean   :0.5544  
3rd Qu.:0.09404   3rd Qu.:0.7396  
Max.   :1.00000   Max.   :1.0000  
require(class)
for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}
[1] 1.0000000 0.3820354
[1] 3.0000000 0.3770963
[1] 5.0000000 0.3905353
[1] 7.0000000 0.3909947
[1] 9.0000000 0.3976568

#After running 2 separate, but fairly similar, trials of the K-NN algorithm against the comparison of living cost against income & wages, I seem to conclude that there is a lack of accuracy in my attempts.
#Running the initial code provided to us yielded similar results to the code I ran, leading me to believe that using form of income against living costs is to uniform in number.
#I originally intended to see a nearest neighbor relationship that fit my own personal financial situation, or similar to it. Yet low accuracy was the result.

#It seems that regardless of rental restrictions, or constraints, the outcome remains. This is perhaps a result of the uniformirt in the data for the variables selected.

#I am uncertain, because there are a lot of factors involved, but in theory one might think that adding variables increases the chance of accuracy, but I did not find this to be the case.

