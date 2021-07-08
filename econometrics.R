#Analyse the effect of various firm characteristics on sales
#Summary statistics (mean, median, minimum, maximum etc.)
summary(hse1)   
#Histograms for main variables of interest
hist(hse1$industry)
hist(hse1$size)
hist(hse1$n_employees)
hist(hse1$sales)
# Scatter plots
qplot(hse1$sales, bins=50, xlim = c(0,80000000))
qplot(hse1$n_employees, bins=50, xlim = c(0,600))
#Estimate a linear model
ggplot(data = hse1 , mapping = aes(x= n_employees, y = sales)) + geom_point()+ geom_smooth(aes(x= n_employees, y = sales), formula = y ~ x, method ="lm",colour="red")
model <- lm(formula = sales ~ n_employees , data = hse1)
summary(model)
#Estimate the log-log and log-lin specifications of the model
model2 <- lm(formula = log(sales) ~ n_employees , data = hse1)
summary(model2)
model3 <- lm(formula = log(sales) ~ log(n_employees) , data = hse1)
summary(model3)
#Include a dummy variable rd_spendings as an intercept dummy, as a slope dummy
and simultaneously (as intercept and slope dummy)
model_d1 <- lm(formula = sales ~ n_employees + rd_spendings , data = hse1)           
summary(model_d1)               
model_d2 <- lm(formula = sales ~ n_employees + rd_spendings:n_employees , data = hse1)                       
summary(model_d2)  
model_d3 <- lm(formula = sales ~ n_employees*rd_spendings , data = hse1)
summary(model_d3)
model3 <- lm(formula = log(sales) ~ n_employees , data = hse1)
#Conduct a Chow test. Which specification do you prefer
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
#Estimate other model specifications using other variables in the dataset. Choose
final specification
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
#Conduct the RESET-Ramsey test. What is the conclusion?
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
#) Investigate residuals for normality: draw Q-Q plot, histogram with the kernel density function, conduct Kolmogorov-Smirnov test, Shapiro-Wilk test
density function, conduct Kolmogorov-Smirnov test, Shapiro-Wilk test
ks.test(unique(residuals_df2$model1),"pnorm")
residuals_df2_small<- residuals_df2 %>%
  sample_n(size = 377, replace= FALSE)
shapiro.test(residuals_df2_small$model1)
#Check whether your model suffers from multicollinearity: calculate VIF and correlations
vif(model1)
X<- model.matrix(sales~ 0+ n_employees+rd_spendings*n_employees+ female_top_manager*n_employees+size+industry*n_employees, data=hse1 )
cor(X)
#Test your model for heteroskedasticity. Conduct White test, Goldfeld-Quandt
test and Breusch-Pagan test.
gqtest(model1, order.by= hse1$n_employees, fraction=0.2)
bptest(model1)
bptest(formula= model1, varformula = ~n_employees+ I(n_employees^2), data= hse1 )



