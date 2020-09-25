
# 產生 100 期，兩路線資料
source('./dgp.R')
# 回傳 dt 為各期、個別選擇的市佔率

# 先取出各自選擇的資料，方便計算市佔率對數差
dt0 <- agg_data[decision==0]
dt1 <- agg_data[decision==1]
dt2 <- agg_data[decision==2]

dt1$logshare <- log(dt1$share / dt0$share)
dt1$xj <- xj[1] + dt1$et1
dt1$pj <- Pj[1] + dt1$et2

dt2$logshare <- log(dt2$share / dt0$share)
dt2$xj <- xj[2] + dt2$et1
dt2$pj <- Pj[2] + dt2$et2


reg_data <- rbind(dt1, dt2)
reg_data[, J2:=ifelse(decision==2, 1, 0)]
reg_data[, post:=ifelse(period>(T_/2), 1, 0)][, postXrisk := post*covid_risk]


m1 <- lm(logshare ~ xj + pj + J2 + postXrisk, data = reg_data)
m2 <- lm(logshare ~ xj + pj + postXrisk, data = reg_data)
stargazer::stargazer(m2, m1, type = 'text')






