babies <- read.csv("http://people.reed.edu/~jones/141/Bwt.dat")
dim(babies) # rows (babies) and columns (variables)


#2

plot(density(babies$bwt))
plot(babies$bwt)
cor.test(babies$bwt, babies$gestation, method="spearman")
cor.test(babies$bwt, babies$parity, method="spearman")
cor.test(babies$bwt, babies$age, method="spearman")          
cor.test(babies$bwt, babies$height, method="spearman")
cor.test(babies$bwt, babies$weight, method="spearman")
cor.test(babies$bwt, babies$smoke, method="spearman")


#3 single regression
model_gestation <- lm(bwt~gestation, data=babies)
summary(model_gestation) #good correlation
r_gestation <- residuals(model_gestation)
plot(r_gestation) #residuals look good
abline(h=0, lwd=3)



#multiple regression:
#a)

model_gestation_smoke <- lm(bwt~gestation+smoke, data=babies)
summary(model_gestation_smoke)
r_gestation_smoke <- residuals(model_gestation_smoke)
plot(r_gestation_smoke)
abline(h=0, lwd=3)
#ok

#b)

model_gestation_smoke_weight <- lm(bwt~gestation+smoke+weight, data=babies)
summary(model_gestation_smoke_weight)
r_gestation_smoke_weight <- residuals(model_gestation_smoke_weight)
plot(r_gestation_smoke_weight)
abline(h=0, lwd=3)
############### looks ok

#c)

model_gestation_smoke_weight_age <- lm(bwt~gestation+smoke+weight+age, data=babies)
summary(model_gestation_smoke_weight_age)
r_gestation_smoke_weight_age <- residuals(model_gestation_smoke_weight_age)
plot(r_gestation_smoke_weight_age)
abline(h=0, lwd=3)
#age is non significant, will try log transform

#d)

model_gestation_smoke_weight_height <- lm(bwt~gestation+smoke+weight+height, data=babies)
summary(model_gestation_smoke_weight_height)
r_gestation_smoke_weight_height <- residuals(model_gestation_smoke_weight_height)
plot(r_gestation_smoke_weight_height)
abline(h=0, lwd=3)

#e) (and 4)
model_gestation_smoke_weight_height_parity <- lm(bwt~gestation+smoke+weight+height+parity, data=babies)
summary(model_gestation_smoke_weight_height_parity)
sistamodellen <- summary(model_gestation_smoke_weight_height_parity)
sistamodellen$
r_gestation_smoke_weight_height_parity <- residuals(model_gestation_smoke_weight_height_parity)
plot(r_gestation_smoke_weight_height_parity)
plot(density(r_gestation_smoke_weight_height_parity))
abline(h=0, lwd=3)
confint(model_gestation_smoke_weight_height_parity)
qqnorm(r_gestation_smoke_weight_height_parity)
qqline(r_gestation_smoke_weight_height_parity)
plot(babies$bwt-sistamodellen$residuals, r_gestation_smoke_weight_height_parity)


#multiple regression with a transformed variable
plot(babies$age, babies$bwt)
babies$logage <- log(babies$age)
model_age <- lm(bwt~age, data=babies)
summary(model_age)
r_age <- residuals(model_age)
plot(r_age)
#bad
plot(babies$logage, babies$bwt)
model_logage <- lm(bwt~logage, data=babies)
summary(model_logage)#bad

babies$squareage <- (babies$age)^2
plot(babies$squareage, babies$bwt)
model_agesquare <- lm(bwt ~squareage, data =babies)
summary(model_agesquare)


model_gestation_smoke_weight_age <- lm(bwt~gestation+smoke+weight+age, data=babies)
summary(model_gestation_smoke_weight_age)
r_gestation_smoke_weight_age <- residuals(model_gestation_smoke_weight_age)
plot(r_gestation_smoke_weight_age)
abline(h=0, lwd=3)





#############################


###### 4) F test (R2 is in the summaries from 3)
anova(
  model_gestation,
  model_gestation_smoke,
  model_gestation_smoke_weight,
  model_gestation_smoke_weight_age
)

anova(
  model_gestation_smoke_weight,
  model_gestation_smoke_weight_height,
  model_gestation_smoke_weight_height_parity
)