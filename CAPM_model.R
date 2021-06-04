#CAPM MODEL
model <- lm(formula = y ~ x , data = data_2)
summary(model)
res_df<- data.frame(time = as.numeric(time(data_2$y)),
                    model= as.numeric(residuals(model)))
plot(res_df$model)
n <- nrow(data_2)
res_long <- pivot_longer(data = res_df, cols = c(model),names_to = "model",values_to = "residual")
lag_df <- data.frame(time = as.numeric(time(data_2$y)),
                     model= c(NA, res_df$model[1:(n-1)]))
lag_long <- pivot_longer(data = lag_df,cols = c(model),names_to = "model",values_to = "lag")
res_long <- left_join(res_long,lag_long,by = c("time","model"))
ggplot(data = res_long , aes(x= time, y= residual, group = model)) + geom_point() + geom_smooth(formula = y~x , method ="lm") + facet_wrap(facets = vars(model))
plot(res_df$model, lag_df$model)
dwt(model, alternative="two.sided")
dwt(model, alternative="negative")
dwt(model, alternative="positive")
data_2$y_lag<- c(NA, data_2$y[1:(n-1)])
model_lagged<- lm(y~y_lag + x, data=data_2)
var_beta<- as.numeric(diag(vcov(model_lagged))[2])
d_st<- dwt(model_lagged)$dw
h_st<- (1 -0.5 *d_st) * sqrt(n /(1 - n *var_beta))
h_st
(1 - pnorm(q= abs(h_st),mean = 0, sd =1))*2
bgtest(model, order = 1)
bgtest(model, order = 2)
t.test(data_2$x, mu = 1,
       alternative = "greater")
t.test(data_2$x, mu = 1,
       alternative = "less")