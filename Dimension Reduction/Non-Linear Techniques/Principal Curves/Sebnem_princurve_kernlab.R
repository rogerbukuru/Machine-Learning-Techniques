####### PRINCURVE EXAMPLE 1 ##############
library(princurve)

# https://www.youtube.com/watch?v=lnxz3QAA1po
set.seed(12345)
theta = runif(100)*pi # the truth
theta2 = seq(0, pi, 0.05) # equal spaced points to show true f
dat = cbind(cos(theta)+rnorm(100,sd=.1),sin(theta)+rnorm(100,sd=.1))
plot(dat, pch=16, xlab="x1", ylab="x2")
lines(cos(theta2),sin(theta2), col=2)
fit = principal_curve(dat)
names(fit)
fit$lambda # this is the estimate of theta
fit$s # this is the projected values along the fitted curve
lines(fit$s[order(fit$lambda),]) # superimposed fitted f in black curve
segments(dat[,1],dat[,2],fit$s[,1],fit$s[,2]) #true f in red

# compare estimated scores with truth
cor(theta, fit$lambda)

# objective function value
fit$dist # like your regression errors, sum squared errors, SSE
ehat = dat - fit$s
t(ehat) %*% ehat
sum(diag(t(ehat) %*% ehat))  # same as sum(ehat[,1]^2+ehat[,2]^2)


####### PRINCURVE EXAMPLE 2 ##############

t <- runif(100, -1, 1)
truecurve = cbind(t, t ^ 2)
x <- cbind(t, t ^ 2) + rnorm(200, sd = 0.1)
colnames(x) <- c("dim1", "dim2")

plot(x)

fit <- principal_curve(x, plot=TRUE)
xaxis = seq(-1, 1, 0.0201) # equally spaced xaxis values
lines(xaxis,xaxis^2, col=2) # true curve
lines(t[order(t)],(t[order(t)])^2, col=2) # could also sort the t and calculate the values on the actual curve

whiskers(x, fit$s)

####### KERNLAB EXAMPLE 1 ################
library(kernlab)

radialkernel = kpca(x, kernel = "rbfdot", kpar = list(sigma = 0.1),
     features = 0, th = 1e-4, na.action = na.omit)
#print the principal component vectors
pcv(radialkernel)

#plot the data projection on the components
plot(rotated(radialkernel),
     xlab="1st Principal Component",ylab="2nd Principal Component")



####### KERNLAB EXAMPLE 2 ################
# IRIS DATA #################
data(iris)
test <- sample(1:150,20)

kpc <- kpca(~.,data=iris[-test,-5],kernel="rbfdot",
            kpar=list(sigma=0.2),features=2)

kpc <- kpca(as.matrix(iris[-test,-5]),kernel="rbfdot",
            kpar=list(sigma=0.2),features=2)
#print the principal component vectors
pcv(kpc)

#plot the data projection on the components
plot(rotated(kpc),col=as.integer(iris[-test,5]),
     xlab="1st Principal Component",ylab="2nd Principal Component")


#embed remaining points 
emb <- predict(kpc,iris[test,-5])
points(emb,col=as.integer(iris[test,5]))



####### KERNLAB EXAMPLE 3 ################
# Ghugare et al (2014) contains a data set where different biomass
# fuels are characterized by the amount of certain molecules 
# (carbon, hydrogen, oxygen, nitrogen, and sulfur) and the 
# corresponding higher heating value (HHV). 

library(ggplot2)
library(modeldata)
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

formula <- ~ carbon + hydrogen + oxygen + nitrogen + sulfur

library(kernlab)
kpca = kpca(formula,data = biomass_tr,kernel="rbfdot",
            kpar=list(sigma=0.2),features=2)

#print the principal component vectors
pcv(kpca)

#plot the data projection on the components
plot(rotated(kpca),col=as.integer(biomass_tr$HHV),
     xlab="1st Principal Component",ylab="2nd Principal Component")

#embed remaining points 
emb <- predict(kpca,biomass_te)
points(emb,col=as.integer(biomass_te$HHV))

ggplot(kpca_te, aes(x = kPC1, y = kPC2)) +
  geom_point() +
  coord_equal()

# Using tidymodels (https://search.r-project.org/CRAN/refmans/recipes/html/step_kpca.html):
Examples

library(ggplot2)
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

kpca_trans <- rec %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_kpca(all_numeric_predictors())

kpca_estimates <- prep(kpca_trans, training = biomass_tr)

kpca_te <- bake(kpca_estimates, biomass_te)

ggplot(kpca_te, aes(x = kPC1, y = kPC2)) +
  geom_point() +
  coord_equal()

tidy(kpca_trans, number = 3)
tidy(kpca_estimates, number = 3)