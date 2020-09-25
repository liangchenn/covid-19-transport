# 疫情下的運輸選擇

## 問題

假設有不同運輸工具在疫情前後的運輸量變化,可以推算出因為疫情,整體運輸量下降了多少,人們又是怎麼在不同工具間進行替代?

## 目標

寫一個模型,帶入一些參數,生成資料算出運輸量變化,再依據變化估計模型參數確認估計是可行的

## 模型

首先試做一個簡易的模型：

- 一條路線，路線長度為一單位
- 兩種交通工具 j = { 1, 2 } (亦可以都不選擇; j =0)
- 個人選擇的效益函數：

    $u_{ijt} = (x_j + \epsilon_{1t})\beta - \alpha(P_j + \epsilon_{2t}) - \gamma Post_t \cdot Risk(N_t) + \xi_j + \epsilon_{ijt}$

       $= x_{jt}\beta - \alpha P_{jt} + \xi_j - \gamma Post_t \cdot Risk(N_t) + \epsilon_{ijt}$

    其中， `xj`  為 j  交通工具的效益； `Pj` 則為 j 交通工具的成本。

    `Risk (.)` : 風險函數 ;  `Nt`   第 t 期確診人數。此處則假設 `Risk(x) = x` 。

    而 $\epsilon_{ijt} \sim$  Type I Extreme Value Distribution 。

- 每一個消費者在每一期市場中選擇自己要選擇哪一種交通工具，或是選擇不出門（outside good）。

由於實際情形中，我們只能觀測到總體的資料。也就是在第 t 期之下，各項選擇的市佔率為多少。

所以會是一個 (j x t) 列的資料。

而為了使 beta 以及 alpha 能夠被成功估計，則需要 xj 以及 Pj 在時間上有變化。

此處假設每一 t 期，xj 與 Pj 個別會受到一個來自標準常態的誤差，等量的施用在不同交通工具上；

因此只有隨著 t 而並無隨著 j 而改變。

而上述設定便可以估計出正確的參數。

### Implement

- 參數設定

    ```r
    # Parameters
    xj <- c(1, 2)# utility
    Pj <- c(3.5, 4.3)# cost
    XIj <- c(3.5, 4.35)# constant term

    # Coefficients of interest
    beta <- .5
    alpha <- 1
    gamma <- 0.5
    ```

- 自訂函數

    ```r
    # Type I Extreme Value Distribution random number generator
    rgev <- function(n=1){
      return(-log(-log(runif(n))))
    }

    # utitility func. of each consumer
    u <- function(j, et1, et2, Post, covid_risk){
      ((xj[j] + et1)*beta - alpha*(Pj[j] + et2) + XIj[j]) - Post*gamma*covid_risk
    }

    # decision func. of each consumer
    decision <- function(et1, et2, Post, covid_risk){
      
      u0 <- rgev()
      u1 <- u(1, et1, et2, Post, covid_risk) + rgev()
      u2 <- u(2, et1, et2, Post, covid_risk) + rgev()
      
      res <- which.max(c(u0, u1, u2)) - 1
      
      return(res)
    }

    decision <- Vectorize(decision) # Vectorization for fast apply
    ```

- 資料產生

    ```r
    # 每一期 5000 名消費者
    obs <- 5000
    # 總共 1000 期，其中 T/2 期之後有疫情
    T_ <- 1000

    for (t in 1:T_) {
      
      et1 <- rnorm(1, 0, 1)
      et2 <- rnorm(1, 0, 1)
      covid_risk <- rpois(1, lambda = 5)
      
      tmp_data <- data.table(id=1:obs, et1=et1, et2=et2, covid_risk=covid_risk)
      
      tmp_data[, decision := decision(et1, et2, t>(T_/2), covid_risk)][, period := t]
      
      all_data <- rbind(all_data, tmp_data)
      
    }
    ```

- 估計

    ```r
    # 按照每一期計算 j = {0, 1, 2} 分別的比例（市佔率）
    agg_data <- all_data[,{
      n = .N
      .SD[, .(share = .N / sum(n)), by=decision]
    }, by=.(period, et1, et2, covid_risk)][order(period, decision)]

    # 計算 log share 後，與選擇不出門的 log share 相減。
    dt0 <- agg_data[decision==0]
    dt1 <- agg_data[decision==1]
    dt2 <- agg_data[decision==2]

    # log share delta of j=1
    dt1$logshare <- log(dt1$share / dt0$share)
    dt1$xj <- xj[1] + dt1$et1
    dt1$pj <- Pj[1] + dt1$et2

    # log share delta of j=2
    dt2$logshare <- log(dt2$share / dt0$share)
    dt2$xj <- xj[2] + dt2$et1
    dt2$pj <- Pj[2] + dt2$et2

    # 合併兩種選擇一起估計
    # 增加 j=2 的虛擬變數
    reg_data <- rbind(dt1, dt2)
    reg_data[, J2:=ifelse(decision==2, 1, 0)]
    reg_data[, post:=ifelse(period>(T_/2), 1, 0)][, postXrisk := post*covid_risk]
    ```

    `reg_data`  會是一個  (2 x 1000) 列的資料。（兩種工具、1000期）。

    估計結果只要資料期數夠多，就可以估到正確值。

    值得注意的是，j = 2 時，xj 與 Pj 在設定上都比較高，因此要多放入 1 { j==2 } 虛擬變數，才不會高估 alpha 以及 beta。而 J2 係數則會是 兩種選擇的常數項差 = 4.35 - 3.5 ~= 0.85

    **參數真實值：**

    ```r
    XIj <- c(3.5, 4.35)

    # Coefficients of interest
    beta = .5
    alpha = -1
    gamma = -0.5
    ```

    **估計結果：**

    ```r
    ==============================================================================
                                           Dependent variable:                    
                        ----------------------------------------------------------
                                                 logshare                         
                                    (1)                           (2)             
    ------------------------------------------------------------------------------
    xj                            0.644***                     0.499***           
                                  (0.007)                       (0.002)           
                                                                                  
    pj                           -0.885***                     -1.001***          
                                  (0.008)                       (0.002)           
                                                                                  
    J2                                                         0.851***           
                                                                (0.004)           
                                                                                  
    postXrisk                    -0.501***                     -0.500***          
                                  (0.003)                       (0.001)           
                                                                                  
    Constant                      3.260***                     3.508***           
                                  (0.032)                       (0.006)           
                                                                                  
    ------------------------------------------------------------------------------
    Observations                   2,000                         2,000            
    R2                             0.963                         0.999            
    Adjusted R2                    0.963                         0.999            
    Residual Std. Error      0.368 (df = 1996)             0.069 (df = 1995)      
    F Statistic         17,511.320*** (df = 3; 1996) 384,757.900*** (df = 4; 1995)
    ==============================================================================
    Note:                                              *p<0.1; **p<0.05; ***p<0.01
    ```