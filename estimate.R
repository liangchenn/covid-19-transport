
# 產生 100 期，兩路線資料
source('./dgp.R')
# 回傳 dt 為各期、個別選擇的市佔率

# 先取出各自選擇的資料，方便計算市佔率對數差
dt0 <- dt[decision==0]
dt1 <- dt[decision==1]
dt2 <- dt[decision==2]

# 計算對數差
dt1$log.share <- log(dt1$share / dt0$share)
dt2$log.share <- log(dt2$share / dt0$share)

# 合併為同一個資料集
est <- rbind(dt1, dt2)

# 製造 Post, Ri*Post, Ri*xj, Ri*Price, Ri*Post*1{j==2} 等變數
est[, Post := ifelse(t>50, 1, 0)][, Rpost := Post*Ri][, Rx := ifelse(decision==1, Ri*xj[1], Ri*xj[2])]
est[, Rp := ifelse(decision==1, Ri*Pj[1], Ri*Pj[2])]

est[, J2 := ifelse(decision==2, 1, 0)][, RpostJ2 := Rpost*J2]

# 估計模型
m <- lm(log.share ~ Rx + Rp + Rpost + RpostJ2, data = est)
stargazer::stargazer(m, type = 'text')

# 模型估計結果

# 變數       估計值      真實值
# 
# Rx         -0.236       0.5
# 
# Rp          0.107       -1
# 
# Rpost      -0.205      -0.20
# 
# RpostJ2    -0.161      -0.16






