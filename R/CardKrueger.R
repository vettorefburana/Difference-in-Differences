
.libPaths(new = "/packages")
library(dplyr)
library(foreign)
library(tidyr)
library(car)

##### import dataset ################
dat <- read.dta("./R/fastfood.dta")

dat <- dat %>% 
  mutate(FTE = nmgrs + empft + (0.5 * emppt), 
         FTE2 = nmgrs2 + empft2 + (0.5 * emppt2))

# city subsets
dat_NJ <- subset(dat, state == 1)
dat_PA <- subset(dat, state == 0)

##### reproduce table 1 ###############
options = c(0,1,2,3)

table1_NJ = matrix(0, length(options), 1)
table1_PA = matrix(0, length(options), 1)

for( i in 1:length(options)){
  
  app1 <- dat_NJ %>% 
    filter(status2  == options[i]) %>% 
    count(status2) %>% 
    select(n) 
  
  app2 <- dat_PA %>% 
    filter(status2  == options[i]) %>% 
    count(status2) %>% 
    select(n)
  
  table1_NJ[i, ] = if(length( app1$n ) == 0){0}else{app1$n}
  
  table1_PA[i, ] = if(length( app2$n ) == 0){0}else{app2$n}
  
}

app = cbind(table1_NJ + table1_PA,
            table1_NJ, 
            table1_PA)

tot =  cbind( nrow(dat_NJ) + nrow(dat_PA), 
              nrow(dat_NJ), 
              nrow(dat_PA) )

table1 = rbind( tot, app )

colnames(table1) = c("All", "NJ", "PA")
rownames(table1) = c("Stores", "Refusals", "Interviewed", 
                     "Renovations", "Closed")

### reproduce table 2 ##################

# stores distribution

chunk1 <- dat %>%
  select(chain, state) %>%
  table() %>%
  prop.table(margin = 2)

owned <- dat %>%
  group_by(state, co_owned) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(co_owned == 1)

stores = rbind(chunk1, owned$freq) * 100

colnames(stores) = c("NJ", "PA")
rownames(stores) = c("Buger King", 
                     "KFC", 
                     "Roy Rogers", 
                     "Wendys", 
                     "Company-owned")


stores_new = cbind(stores[,2],stores[,1]) 
colnames(stores_new) = c("NJ", "PA")

# means wave 1 

min_wage1 <- dat %>%
  group_by(state, wage_st) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100) %>%
  filter(wage_st == 4.25)

means1<- dat %>%
  group_by(state) %>%
  mutate(emptot = empft / FTE * 100 ) %>%
  mutate(price = psoda + pentree + pfry) %>%
  summarise(fte = mean(FTE, na.rm = TRUE),
            emptot  = mean(emptot, na.rm = TRUE),
            wage_st = mean(wage_st, na.rm = TRUE),
            hrsopen = mean(hrsopen, na.rm = TRUE), 
            price = mean(price, na.rm = TRUE))

wave1 = cbind(means1, min_wage = min_wage1$freq)

# means wave 2

app <- dat %>%
  filter(!is.na(wage_st2))%>%
  group_by(state, wage_st2) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100)

min_wage21 = app[c(1, 12, 16), ]

means2 <- dat %>%
  group_by(state) %>%
  mutate(emptot = empft2 / FTE2 * 100 ) %>%
  mutate(price = psoda2 + pentree2 + pfry2)%>%
  summarise(fte = mean(FTE2, na.rm = TRUE),
            emptot  = mean(emptot, na.rm = TRUE),
            wage_st = mean(wage_st2, na.rm = TRUE),
            hrsopen = mean(hrsopen2, na.rm = TRUE), 
            price = mean(price, na.rm = TRUE))

min_wage = c(as.numeric(min_wage21[1, "freq"]), 0)

wave2 = cbind(means2, min_wage, max_wage = min_wage21[2:3, "freq"])

# table 2
names = c("FTE employment", 
          "Full time employees (%)", 
          "Starting wage", 
          "Hours open", 
          "Price of meal", 
          "Wage = 4.25$ (%)")

w1 = t(wave1[,-1])

w11 = cbind(w1[, 2], w1[, 1])

w2 = t(wave2[, -1])

w22 = cbind(w2[, 2], w2[, 1])

rownames(w11) = names
rownames(w22) = c(names, "Wage = 5.05$ (%)")

table2 = rbind(stores_new, w11, w22)

##### reproduce table 3 #############

# row1, columns 1-3

res11 <- dat %>%
  group_by(state) %>%
  select(FTE ) %>%
  group_by(N = n(), add = TRUE) %>%
  summarize_all(funs(mean, var, na_sum = sum(is.na(.))), na.rm = TRUE) %>%
  mutate(n = N - na_sum) %>% 
  mutate(se = sqrt(var/n))%>% 
  mutate(err = var/n) 

mean_diff = diff(res11$mean)

se_diff <- sqrt(res11$var[1]/res11$n[1] + res11$var[2]/res11$n[2])

res11_mean = c( res11$mean, mean_diff)
res11_se = c(res11$se, se_diff)

## row1, columns 3-5

res12 <- dat %>% 
  filter(state == 1 & !is.na(wage_st))  %>% 
  mutate(w = case_when(
    wage_st == 4.25  ~ 1,
    wage_st >= 4.26 & wage_st <= 4.99 ~ 2,
    wage_st >= 5 ~ 3
  )) %>%
  group_by(w)  %>%
  select(FTE ) %>%
  group_by(N = n(), add = TRUE) %>%
  summarize_all(funs(mean, var, na_sum = sum(is.na(.))), na.rm = TRUE) %>%
  mutate(n = N - na_sum) %>% 
  mutate(err = var/n) %>%
  mutate(se = sqrt(var/n))

mean_diff2 = rbind( res12$mean[1] - res12$mean[3], 
                    res12$mean[2] - res12$mean[3])

se_diff2 <- rbind( sqrt(res12$var[1]/res12$n[1] + res12$var[3]/res12$n[3]), 
                   sqrt(res12$var[2]/res12$n[2] + res12$var[3]/res12$n[3]))

res12_mean = c( res12$mean, mean_diff2 )
res12_se = c(res12$se, se_diff2)

row1_mean = c(res11_mean, res12_mean)
row1_se = c(res11_se, res12_se)

# row2, columns 1-3

res21 <- dat %>%
  group_by(state) %>%
  select(FTE2) %>%
  group_by(N = n(), add = TRUE) %>%
  summarize_all(funs(mean, var, na_sum = sum(is.na(.))), na.rm = TRUE) %>%
  mutate(n = N - na_sum) %>% 
  mutate(se = sqrt(var/n))%>% 
  mutate(err = var/n) 

mean_diff = diff(res21$mean)

se_diff <- sqrt(res21$var[1]/res21$n[1] + res21$var[2]/res21$n[2])

res21_mean = c( res21$mean, mean_diff)
res21_se = c(res21$se, se_diff)

## row2, columns 3-5

res22 <- dat %>% 
  filter(state == 1 & !is.na(wage_st))  %>% 
  mutate(w = case_when(
    wage_st == 4.25  ~ 1,
    wage_st >= 4.26 & wage_st <= 4.99 ~ 2,
    wage_st >= 5 ~ 3
  )) %>%
  group_by(w)  %>%
  select(FTE2) %>%
  group_by(N = n(), add = TRUE) %>%
  summarize_all(funs(mean, var, na_sum = sum(is.na(.))), na.rm = TRUE) %>%
  mutate(n = N - na_sum) %>% 
  mutate(se = sqrt(var/n))%>% 
  mutate(err = var/n) 

mean_diff2 = rbind( res22$mean[1] - res22$mean[3], 
                    res22$mean[2] - res22$mean[3])

se_diff2 <- rbind( sqrt(res22$var[1]/res22$n[1] + res22$var[3]/res22$n[3]), 
                   sqrt(res22$var[2]/res22$n[2] + res22$var[3]/res22$n[3]))

res22_mean = c( res22$mean, mean_diff2 )
res22_se = c(res22$se, se_diff2)

row2_mean = c(res21_mean, res22_mean)
row2_se = c(res21_se, res22_se)

## row 3 

row3_mean = row2_mean - row1_mean

# row 4

res41 <- dat %>% 
  filter(complete.cases(FTE, FTE2)) %>%
  group_by(state) %>%
  summarise(mean_before = mean(FTE), mean_after = mean(FTE2)) %>%
  mutate(change_mean = mean_after - mean_before) %>% 
  select(state, change_mean)

res41_diff = res41$change_mean[2] - res41$change_mean[1]

res42 <- dat %>% 
  filter(complete.cases(FTE, FTE2)) %>%
  filter(state == 1 & !is.na(wage_st))  %>% 
  mutate(w = case_when(
    wage_st == 4.25  ~ 1,
    wage_st >= 4.26 & wage_st <= 4.99 ~ 2,
    wage_st >= 5 ~ 3
  )) %>%
  group_by(w)  %>%
  summarise(mean_before = mean(FTE), mean_after = mean(FTE2)) %>%
  mutate(change_mean = mean_after - mean_before) %>% 
  select(w, change_mean)

mean_diff4 = c( res42$change_mean[1] - res42$change_mean[3], 
                res42$change_mean[2] - res42$change_mean[3])

row4_mean = c(res41$change_mean, res41_diff,
              res42$change_mean, 
              mean_diff4)

# row 5

res5 <- dat %>% 
  filter(complete.cases(FTE, FTE2))

res51 <-  res5 %>%
  mutate(fte_adj = ifelse(status2 %in% c(2,4,5), 0, FTE2)) %>%
  group_by(state) %>%
  summarise(mean_before = mean(FTE), mean_after = mean(fte_adj)) %>%
  mutate(change_mean = mean_after - mean_before) %>% 
  select(state, change_mean)

res5$FTE2[res5$status2 %in% c(2,4,5)]

# table 3

table3 = rbind(row1_mean, 
               row1_se, 
               row2_mean, 
               row2_se, 
               row3_mean, 
               row4_mean)

colnames(table3) = c("PA", "NJ", "NJ - PA", 
                     "Wage low", "Wage medium", "Wage high", 
                     "Low - High", "Medium - High")

rownames(table3) = c("Mean FTE before", 
                     "SE FTE before",
                     "Mean FTE after", 
                     "SE FTE after",
                     "Change in mean", 
                     "Change in mean, balanced sample")


### regression for table 4 #####################
# set up the data for regression analysis

## subsetting for complete cases (as per data)
reg_dat <- dat %>% 
  filter(complete.cases(FTE, FTE2)) %>%
  filter(complete.cases(wage_st, wage_st2) | status2 == 3)

## model ii - dummy for 3 out of 4 chains 
reg_dat <- reg_dat  %>% 
  mutate(contr_bk = case_when(chain == "1" ~ 1, chain != "1" ~ 0), 
         contr_kfc = case_when(chain == "2" ~ 1, chain != "2" ~ 0), 
         contr_roys = case_when(chain == "3" ~ 1, chain != "3" ~ 0))

## model iii - create variable GAP
reg_dat <- reg_dat %>%
  mutate(gap = ifelse(wage_st >= 5.05, 0, (5.05-wage_st)/wage_st))
reg_dat <- reg_dat %>%
  mutate(gap = ifelse(state == 0, 0, gap))


# estimate the regression model FTE2-FTE dependent
# independent NJ
model_1 <- lm(FTE2-FTE ~ state, data = reg_dat)
# independent NJ + control vars for chains and co-ownership
model_2 <- lm(FTE2-FTE ~ state + contr_bk + contr_kfc + contr_roys + co_owned, data = reg_dat)
# independent GAP model
model_3 <- lm(FTE2-FTE ~ gap, data = reg_dat)
# independent GAP model + control vars for chains and co-ownership
model_4 <- lm(FTE2-FTE ~ gap + contr_bk + contr_kfc + contr_roys + co_owned, data = reg_dat)

model_5 <- lm(FTE2-FTE ~ gap + contr_bk + contr_kfc + contr_roys + co_owned + southj  + centralj  + pa1  + pa2 , data = reg_dat)

md1 <- summary(model_1)
md2 <- summary(model_2)
md3 <- summary(model_3)
md4 <- summary(model_4)
md5 <- summary(model_5)

# f test for exclusion of controls
f_mod2 <- linearHypothesis(model_2, c("contr_bk=0", "contr_kfc=0", "contr_roys =0", "co_owned = 0"))
f_mod4  <- linearHypothesis(model_4, c("contr_bk=0", "contr_kfc=0", "contr_roys =0", "co_owned = 0"))
f_mod5 <- linearHypothesis(model_5, c("contr_bk=0", "contr_kfc=0", "contr_roys =0", "co_owned = 0", "southj=0", "centralj=0", "pa1=0", "pa2=0"))

f2 <- f_mod2$`Pr(>F)`[2]
f4 <- f_mod4$`Pr(>F)`[2]
f5 <- f_mod5$`Pr(>F)`[2]

f <- c("-", round(f2, 2), "-", round(f4, 2), round(f5, 2))

## table 4 
i <- c( md1$coefficients["state", "Estimate"], 
        md1$coefficients["state", "Std. Error"],
        md1$sigma)

ii <- c(md2$coefficients["state", "Estimate"], 
        md2$coefficients["state", "Std. Error"],
        md2$sigma)

iii <- c(md3$coefficients["gap", "Estimate"], 
         md3$coefficients["gap", "Std. Error"],
         md3$sigma)

iv <- c(md4$coefficients["gap", "Estimate"], 
        md4$coefficients["gap", "Std. Error"],
        md4$sigma)

v <- c(md5$coefficients["gap", "Estimate"], 
       md5$coefficients["gap", "Std. Error"],
       md5$sigma)

app <- cbind(i,ii,iii,iv,v)

table4 <- rbind(round(app,2), 
                f)

colnames(table4) = c("i","ii","iii","iv", "v")

rownames(table4) = c("Coefficient", "SE Coefficient","SE Regression", "Prob. Controls")

#### save results #############

save.image(file = "./R/results.RData")

