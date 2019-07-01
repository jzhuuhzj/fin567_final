# install.packages('quantmod')
# install.packages('fGarch')
# install.packages('fOptions')
# install.packages('qrmtools')

library('quantmod')
library('fGarch')
library('xts')
library('fOptions')
library('qrmtools')

name_list <- c("DJI","MMM", "AAPL", "AXP","BA","CAT","CVX", "CSCO", "KO", "XOM", "GE", "GS", "HD", "INTC", "IBM", "JNJ", "JPM", "MCD", "MRK", "MSFT","NKE","PFE", "PG", "TRV", "UNH", "UTX", "VZ", "V", "WMT", "DIS", "DWDP")
price = zoo()
for(name in name_list){
  ticker = getSymbols(name, from='2017-07-14', to='2017-12-16', auto.assign=FALSE)
  price = cbind(price, ticker[,paste(name,'.Adjusted',sep='')])
}
price$DJI.Adjusted = price$DJI.Adjusted/100


log_ret <- diff(log(price))
log_ret <- log_ret[-1,]
(T<-length(log_ret[1]))


hypo_stock_price = list()
for (i in 1:T){
  fit1<- garchFit( ~garch(1,1), data=log_ret[,i],trace=FALSE)
  z = as.vector(log_ret[,i]) / fit1@sigma.t
  omega = fit1@fit$par[["omega"]]
  alpha1 = fit1@fit$par[["alpha1"]]
  beta1 = fit1@fit$par[["beta1"]]
  
  new_sigma = sqrt(omega + alpha1 * log_ret[length(log_ret[,i]),i] * log_ret[length(log_ret[,i]),i] + beta1 * fit1@h.t[length(fit1@h.t)])
  new_sigma = as.vector(new_sigma)
  new_returns = new_sigma * z
  
  S0 = as.vector(price[length(price[,i])-1,i])
  S_new = as.vector(S0 * exp(new_returns))
  hypo_stock_price[[name_list[i]]] = S_new
}

get_weight = function(day) {
  sum_price = sum(price[day,2:30])
  return(c(as.vector(price[day,2:30] / sum_price), 0))
}

start_date =  as.Date("2017-07-14")
end_date = as.Date("2017-12-15")

# volatility garch model fit & calculate vol_new
merged_data <- read.csv("~/Desktop/fin567_project/new_full_data.csv", header = TRUE)
hypo_vol = list()
for (name in name_list) {
  implied_volatility = merged_data[, paste(name, ".vol", sep="")]
  log_vol <- diff(log(implied_volatility))
  fit2 <- garchFit( ~garch(1,1), data=log_vol,trace=FALSE)
  z_vol = as.vector(log_vol) / fit2@sigma.t
  omega_vol = fit2@fit$par[["omega"]]
  alpha1_vol = fit2@fit$par[["alpha1"]]
  beta1_vol = fit2@fit$par[["beta1"]]
  
  new_sigma_vol = sqrt(omega_vol + alpha1_vol * log_vol[length(log_vol)] * log_vol[length(log_vol)] + beta1_vol * fit2@h.t[length(fit2@h.t)])
  new_sigma_vol = as.vector(new_sigma_vol)
  new_returns_vol = new_sigma * z_vol
  
  vol_0 = as.vector(implied_volatility[length(implied_volatility)])
  vol_new = as.vector(vol_0 * exp(new_returns_vol))
  hypo_vol[[name]] = vol_new
}

# vega-neutral position -> alpha
djx_vega_call = tail(merged_data[["DJI_call.vega"]], 1)
djx_vega_put = tail(merged_data[["DJI_put.vega"]], 1)
stock_vega_call = c()
stock_vega_put = c()
for(name in name_list) {
  if(name == "DJI") {
    next
  }
  stock_vega_call = c(stock_vega_call, tail(merged_data[[paste(name, "_call.vega", sep="")]], 1))
  stock_vega_put = c(stock_vega_put, tail(merged_data[[paste(name, "_put.vega", sep="")]], 1))
}
weights = get_weight("2017-12-15")
alpha = (djx_vega_call + djx_vega_put) / (weights %*% stock_vega_call + weights %*% stock_vega_put)

# theta-neutral position -> beta
djx_theta_call = tail(merged_data[["DJI_call.theta"]], 1)
djx_delta_call = tail(merged_data[["DJI_call.delta"]], 1)
djx_gamma_call = tail(merged_data[["DJI_call.gamma"]], 1)
djx_theta_put = tail(merged_data[["DJI_put.theta"]], 1)
djx_delta_put = tail(merged_data[["DJI_put.delta"]], 1)
djx_gamma_put = tail(merged_data[["DJI_put.gamma"]], 1)

stock_theta_call = c()
stock_delta_call = c()
stock_gamma_call = c()
stock_theta_put = c()
stock_delta_put = c()
stock_gamma_put = c()

for(name in name_list) {
  if(name == "DJI") {
    next
  }
  stock_theta_call = c(stock_theta_call, tail(merged_data[[paste(name, "_call.theta", sep="")]], 1))
  stock_delta_call = c(stock_delta_call, tail(merged_data[[paste(name, "_call.delta", sep="")]], 1))
  stock_gamma_call = c(stock_gamma_call, tail(merged_data[[paste(name, "_call.gamma", sep="")]], 1))
  stock_theta_put = c(stock_theta_put, tail(merged_data[[paste(name, "_put.theta", sep="")]], 1))
  stock_delta_put = c(stock_delta_put, tail(merged_data[[paste(name, "_put.delta", sep="")]], 1))
  stock_gamma_put = c(stock_gamma_put, tail(merged_data[[paste(name, "_put.gamma", sep="")]], 1))
}
beta = (djx_theta_call + djx_theta_put) / (weights %*% stock_theta_call + weights %*% stock_theta_put)

# get annualized risk-free rate
r <- getSymbols('DGS1',src = 'FRED', auto.assign = FALSE)
r <- as.vector(r["2017-12-15"])/100

# t+1
# stock_hypo_vol + stock_hypo_price (60 options daily)
stock_value_call <- list()
stock_value_put <- list()
for (name in name_list){
  if (name == "DJI"){
    # B-S foamula: djx_hypo_vol + djx_hypo_price (2 options daily)
    args(Black_Scholes)
    (djx_value_call <- Black_Scholes(0, hypo_stock_price[['DJI']], r=r, hypo_vol[['DJI']], K=merged_data[["DJI_call.strike"]][109], 34/365, "call"))
    (djx_value_put <- Black_Scholes(0, hypo_stock_price[['DJI']], r=r, hypo_vol[['DJI']], K=merged_data[["DJI_put.strike"]][109], 34/365, "put"))
  } else{
    for(i in 1:length(hypo_stock_price[[name]])) {
      stock_value_call[[name]] <- c(stock_value_call[[name]], BinomialTreeOption(TypeFlag = 'ca', S=hypo_stock_price[[name]][i], X=merged_data[[paste(name, "_call.strike", sep="")]][109], Time=34/365, r=r, b=0, sigma = hypo_vol[[name]][i], n=100, title = NULL, description = NULL)[1,1])
      stock_value_put[[name]] <- c(stock_value_put[[name]], BinomialTreeOption(TypeFlag = 'pa', S=hypo_stock_price[[name]][i], X=merged_data[[paste(name, "_put.strike", sep="")]][109], Time=34/365, r=r, b=0, sigma = hypo_vol[[name]][i], n=100, title = NULL, description = NULL)[1,1])
    }
    
  }
}
stock_value_call = as.matrix(as.data.frame(stock_value_call))
stock_value_put = as.matrix(as.data.frame(stock_value_put))

#calculate V@t+1
V_t1_vega = stock_value_call %*% (as.vector(alpha)*weights) + stock_value_put %*% (as.vector(alpha)*weights) - djx_value_call - djx_value_put
V_t1_theta = stock_value_call %*% (as.vector(beta)*weights) + stock_value_put %*% (as.vector(beta)*weights) - djx_value_call - djx_value_put


# t
stock_value_call_current <- list()
stock_value_put_current <- list()
for (name in name_list){
  if (name == "DJI"){
    # B-S foamula: djx_hypo_vol + djx_hypo_price (2 options daily)
    args(Black_Scholes)
    (djx_value_call_current <- Black_Scholes(0, as.vector(price[,'DJI.Adjusted'])[109], r=r, merged_data[['DJI_call.vol']][109], K=merged_data[["DJI_call.strike"]][109], 35/365, "call"))
    (djx_value_put_current <- Black_Scholes(0, as.vector(price[,'DJI.Adjusted'])[109], r=r, merged_data[['DJI_put.vol']][109], K=merged_data[["DJI_put.strike"]][109], 35/365, "put"))
  } else{
      stock_value_call_current[[name]] <- c(stock_value_call_current[[name]], BinomialTreeOption(TypeFlag = 'ca', S=as.vector(price[,paste(name,'.Adjusted', sep="")])[109], X=merged_data[[paste(name, "_call.strike", sep="")]][109], Time=35/365, r=r, b=0, sigma = merged_data[[paste(name, "_call.vol", sep="")]][109], n=100, title = NULL, description = NULL)[1,1])
      stock_value_put_current[[name]] <- c(stock_value_put_current[[name]], BinomialTreeOption(TypeFlag = 'pa', S=as.vector(price[,paste(name,'.Adjusted', sep="")])[109], X=merged_data[[paste(name, "_put.strike", sep="")]][109], Time=35/365, r=r, b=0, sigma = merged_data[[paste(name, "_put.vol", sep="")]][109], n=100, title = NULL, description = NULL)[1,1])
  }
}

stock_value_call_current = as.matrix(as.data.frame(stock_value_call_current))
stock_value_put_current = as.matrix(as.data.frame(stock_value_put_current))

V_t_vega = stock_value_call_current %*% (as.vector(alpha)*weights) + stock_value_put_current %*% (as.vector(alpha)*weights) - djx_value_call_current - djx_value_put_current
V_t_theta = stock_value_call_current %*% (as.vector(beta)*weights) + stock_value_put_current %*% (as.vector(beta)*weights) - djx_value_call_current - djx_value_put_current


V_t1_vega = as.vector(V_t1_vega)
V_t_vega = as.vector(V_t_vega)

V_t1_theta = as.vector(V_t1_theta)
V_t_theta = as.vector(V_t_theta)

# VaR = quntile (0.05)
(-quantile(log(V_t1_vega / V_t_vega), probs=0.05))
(-quantile(log(V_t1_theta / V_t_theta), na.rm=TRUE, probs=0.05))
 
# VaR in dollar amount
(-quantile(V_t1_vega - V_t_vega, probs=0.05))
(-quantile(V_t1_theta - V_t_theta, na.rm=TRUE, probs=0.05))

# vega risk for theta neutral
(as.vector(beta) * weights %*% stock_vega_call + as.vector(beta) * weights %*% stock_vega_put - djx_vega_call - djx_vega_put)
(as.vector(beta) * weights %*% stock_delta_call + as.vector(beta) * weights %*% stock_delta_put - djx_delta_call - djx_delta_put)
(as.vector(beta) * weights %*% stock_gamma_call + as.vector(beta) * weights %*% stock_gamma_put - djx_gamma_call - djx_gamma_put)

# theta risk for vega neutral
(as.vector(alpha) * weights %*% stock_theta_call + as.vector(alpha) * weights %*% stock_theta_put - djx_theta_call - djx_theta_put)
(as.vector(alpha) * weights %*% stock_delta_call + as.vector(alpha) * weights %*% stock_delta_put - djx_delta_call - djx_delta_put)
(as.vector(alpha) * weights %*% stock_gamma_call + as.vector(alpha) * weights %*% stock_gamma_put - djx_gamma_call - djx_gamma_put)


#greek risks
# delta
# gamma
# theta
# vega 

# #calculate V@t
# get_portfolio_value = function(djx_call, djx_put, stock_call, stock_put, alpha) 
#   
# # P&L_t+1 = V_t+1 - V_t
# PL_t1_vega = V_t1_vega - V_t_vega
# PL_t1_theta = V_t1_theta - V_t_theta

# var = quntile (0.05)
# (-quantile(log(V_t1_vega / V_t_vega), probs=0.05))