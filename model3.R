# t = 1,..., 100

# pandemic happened after (and including) t=51


# ROUTE = 1 ---------------------------------------------------------------

route1_list <- vector('list', length = 100)
for (i in 1:100) {
  obs <- 1000
  dt <- data.table(id = 1:obs, y = rnorm(obs, 1e4, 100), t = i)
  
  route1_list[[i]] <- dt
  
}

route1_dt <- do.call(rbind, route1_list)

route1_dt[, decision := FOO(y, ROUTE = 1, POST = t>50)]




# ROUTE = 2 ---------------------------------------------------------------

route2_list <- vector('list', length = 100)
for (i in 1:100) {
  obs <- 1000
  dt <- data.table(id = 1:obs, y = rnorm(obs, 1e4, 100), t = i)
  
  route2_list[[i]] <- dt
  
}

route2_dt <- do.call(rbind, route2_list)

route2_dt[, decision := FOO(y, ROUTE = 2, POST = t>50)]









