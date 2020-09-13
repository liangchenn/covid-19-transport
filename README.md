# covid-19-transport
Choices of transportations during COVID-19 pandemic


## Files
```
.
├── dgp.R        : 100-period model data generating process.
│
├── estimate.R   : 100-period model estimate.
│
├── model.R      : Baseline model.
│
└── prototype.R  : Simple binary and multinomial cases to check analytical market share.

```

## Temp Results

目前先產生 100 期模型，模型設定：

$u_{ijt} = (x_j \beta - \alpha P_j - \gamma Post_t - \alpha^R_j Post_t + \xi_j)\cdot R_i + \epsilon_{ijt}$

### 參數設定

$(\alpha, \beta, \gamma, \alpha_1^R, \alpha_2^R, \xi_1, \xi_2) = 
(1, 0.5, 0.1, 0.1, 0.25, 3.5, 3.5)$

路線 $Ri = \{7, 11\}$ 兩種選擇。

每一期每一個人隨機從兩個路線中抽取其一，再計算該人會選擇哪一種交通工具。


### 估計結果

先用 dgp.R 生出資料，在使用 estimate.R 估計結果。

染疫風險的係數是估計正確的，但 $\alpha$ 以及 $\beta$ 則出現係數方向相反的狀況。

```
# 變數       估計值      真實值        係數名稱
# --------------------------------------------------------------------
#
# Rx         -0.236       0.5         \beta
# 
# Rp          0.107       -1          \alpha
# 
# Rpost      -0.205      -0.20        \alpha_1^R + \gamma
# 
# RpostJ2    -0.161      -0.16        \alpha_1^R + \alpha_2^R + \gamma

```




