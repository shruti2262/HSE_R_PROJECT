summary(hse1)   
hist(hse1$industry)
hist(hse1$size)
hist(hse1$n_employees)
hist(hse1$sales)
qplot(hse1$sales, bins=50, xlim = c(0,80000000))
qplot(hse1$n_employees, bins=50, xlim = c(0,600))
ggplot(data = hse1 , mapping = aes(x= n_employees, y = sales)) + geom_point()+ geom_smooth(aes(x= n_employees, y = sales), formula = y ~ x, method ="lm",colour="red")
model <- lm(formula = sales ~ n_employees , data = hse1)
summary(model)
model3 <- lm(formula = log(sales) ~ log(n_employees) , data = hse1)
summary(model3)
model2 <- lm(formula = log(sales) ~ n_employees , data = hse1)
summary(model2)
model_d1 <- lm(formula = sales ~ n_employees + rd_spendings , data = hse1)           
summary(model_d1)               
model_d2 <- lm(formula = sales ~ n_employees + rd_spendings:n_employees , data = hse1)                       
summary(model_d2)  
model_d3 <- lm(formula = sales ~ n_employees*rd_spendings , data = hse1)
summary(model_d3)
model3 <- lm(formula = log(sales) ~ n_employees , data = hse1)
model_d <- lm(formula = log(sales) ~ n_employees + rd_spendings + rd_spendings:n_employees , data = hse1)
model_sub1 <- lm(data = hse1 , log(sales) ~ n_employees ,subset = (rd_spendings =="1"))
model_sub2 <- lm(data = hse1 , log(sales) ~ n_employees ,subset = (rd_spendings =="2"))
summary(model_sub1)
summary(model_sub2)
RSS1<- deviance(model_sub1)
RSS1
RSS2<- deviance(model_sub2)
RSS2
RSS<- deviance(model3)
RSS
nobs(model3)
nobs(model_sub1)
nobs(model_sub2)
chowF <- ((RSS-RSS1-RSS2)/6)/((RSS1+RSS2)/(nobs(model3)-12))
chowF
waldtest(model3,model_d)
model2 <- lm(formula = log(sales) ~ log(n_employees) , data = hse1)
model_d <- lm(formula = log(sales) ~ log(n_employees) + rd_spendings + rd_spendings:log(n_employees) , data = hse1)
model_sub1 <- lm(data = hse1 , log(sales) ~ log(n_employees) ,subset = (rd_spendings =="1"))
model_sub2 <- lm(data = hse1 , log(sales) ~ log(n_employees) ,subset = (rd_spendings =="2"))
summary(model_sub1)
summary(model_sub2)
RSS1<- deviance(model_sub1)
RSS1
RSS2<- deviance(model_sub2)
RSS2
RSS<- deviance(model2)
RSS
nobs(model2)
nobs(model_sub1)
nobs(model_sub2)
chowF <- ((RSS-RSS1-RSS2)/6)/((RSS1+RSS2)/(nobs(model2)-12))
chowF
waldtest(model2,model_d)
model1<- lm(formula = sales~n_employees+rd_spendings*n_employees+ female_top_manager*n_employees+size+industry*n_employees, data=hse1)
summary(model1)
model2<- lm(formula = log(sales)~n_employees+rd_spendings*n_employees+ female_top_manager*n_employees+size+industry*n_employees, data=hse1)
summary(model2)
model3<- lm(formula = log(sales)~log(n_employees)+rd_spendings*log(n_employees)+ female_top_manager*log(n_employees)+size+industry*log(n_employees), data=hse1)
summary(model3)
resettest(model1)
residuals_df<- pivot_longer(residuals_df_wide, cols= starts_with("model"), names_to ="model", values_to = "res" )
residuals_df<- residuals_df %>%
  group_by(model) %>%
  mutate(mean= mean(res), sd= sd(res)) %>%
  mutate(X=seq(from= min(res), to=max(res), length.out=n()),
         pdf= dnorm(x=X, mean= mean, sd= sd))
qqnorm(residuals_df_wide$model1)
qqline(residuals_df_wide$model1, lwd=2)
residuals_df_wide<- data_frame(model1= residuals(model1))
hist(residuals_df_wide$model1)
residuals_df<- residuals_df %>%
  group_by(model) %>%
  mutate(mean= mean(res), sd= sd(res)) %>%
  mutate(X=seq(from= min(res), to=max(res), length.out=n()),
         pdf= dnorm(x=X, mean= mean, sd= sd))
ggplot(data=residuals_df, aes(x=res, group= model))+
  geom_histogram(aes(x=res,y= stat(density)), bins = 50)+
  facet_wrap(facets = vars(model), scales="free_x")+
  geom_line(mapping = aes(x=X, y=pdf, group= model))
residuals_df2<- data_frame(model1= residuals(model1))
ks.test(unique(residuals_df2$model1),"pnorm")
residuals_df2_small<- residuals_df2 %>%
  sample_n(size = 377, replace= FALSE)
shapiro.test(residuals_df2_small$model1)
vif(model1)
X<- model.matrix(sales~ 0+ n_employees+rd_spendings*n_employees+ female_top_manager*n_employees+size+industry*n_employees, data=hse1 )
cor(X)
gqtest(model1, order.by= hse1$n_employees, fraction=0.2)
bptest(model1)
bptest(formula= model1, varformula = ~n_employees+ I(n_employees^2), data= hse1 )

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

