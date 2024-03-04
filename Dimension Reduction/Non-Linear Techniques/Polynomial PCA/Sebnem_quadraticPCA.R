# Modern Multivariate Book Page: 599

# Generate n = 201 bivariate observations, (X1,X2), that are generated
# to lie exactly on the quadratic curve X2 = 4X1^2 + 4X1 + 2, 
# where X1 = −1.5(0.01)0.5:

rm(list = ls())

x1=seq(from=-1.5, to=0.5, by=0.01)
x2=4*x1^2+4*x1+2
x2a0=4*x1^2+4*x1+0
x2a1=4*x1^2+4*x1+1
x2a3=4*x1^2+4*x1+3

length(x1)


# The noisy case is obtained by replacing X1 by X1+Z and, independently, 
# X2 by X2 + Z, where Z ∼ N(0, 1).

x1noisy = x1 + rnorm(201)
x2noisy = x2 + rnorm(201)

data = data.frame(x1,x2, x1noisy, x2noisy)

library(ggplot2)
ggplot(data, aes(x=x1, y=x2noisy)) +
  geom_point(shape=1)      # Use hollow circles

ggplot(data, aes(x=x1noisy, y=x2noisy)) +
  geom_point(shape=1)      # Use hollow circles

ggplot(data, aes(x=x1, y=x2)) +
  geom_point(shape=1)      # Use hollow circles


# Generate the dataframes for both:

datatransformed = data.frame(x1,x2, x1^2, x2^2, x1*x2)
datatransformednoisy = data.frame(x1noisy,x2noisy, x1noisy^2, x2noisy^2, x1noisy*x2noisy)

# Apply linear PCA on the transformed original dataset: ####

pca.transformed = prcomp(datatransformed) # PCA on the var-cov matrix of the transformed dataset
variance.transformed = pca.transformed$sdev^2
round(variance.transformed,4)

## loadings
loadings.transformed = pca.transformed$rotation
round(loadings.transformed,4)

## scores
scores.transformed = pca.transformed$x
round(scores.transformed[,5],3)

pairs(scores.transformed[,1:4]) # last component is just only zeros

# Apply linear PCA on the transformed noisy dataset: ####
pca.transformed.noisy = prcomp(datatransformednoisy) # PCA on the var-cov matrix of the transformed dataset

## variances
variance.transformed.noisy = pca.transformed.noisy$sdev^2
round(variance.transformed.noisy,4)

## loadings
loadings.transformed.noisy = pca.transformed.noisy$rotation
round(loadings.transformed.noisy,4)

## scores
scores.transformed.noisy = pca.transformed.noisy$x
round(scores.transformed.noisy[,5],3)

## plot
pairs(scores.transformed.noisy[,1:5]) # last component is just 



############################
svd(cov(datatransformed))

data2Xst = scale(datatransformed, center = TRUE, scale = FALSE)
svd(cov(data2Xst))$d


data3X = data.frame(x1,x2,datatransformed)
data3Xst = scale(data3X, center = TRUE, scale = TRUE)
library(scatterplot3d)
scatterplot3d(scale(x1), scale(x2), scale(x1*x2),        # x y and z axis
color="blue", pch=19,        # filled blue circles
type="h",                    # vertical lines to the x-y plane
main="3-D Scatterplot Example",
xlab="1st st var",
ylab="2nd st var",
zlab="3rd st var")

s3d.coords <- s3d$xyz.convert(scale(x1), scale(x2), scale(x1*x2)) # convert 3D coords to 2D projection
text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
    labels=row.names(data3Xst),               # text to plot
    cex=.5, pos=4)           # shrink text 50% and place to right of points)


C1=princomp(data3X,cor=TRUE)$scores[,1]
C2=princomp(data3X,cor=TRUE)$scores[,2]
C3=princomp(data3X,cor=TRUE)$scores[,3]

Components = data.frame(C1, C2, C3)
datast = data3Xst
svddatast = svd(datast)
svddatast$u%*%diag(svddatast$d)%*%t(svddatast$v)
u=svddatast$u
d=svddatast$d
v=svddatast$v

my.eigen <- svd(datatransformed)$d 
my.eigen
plot(my.eigen, type="b")
cumsum(my.eigen)/sum(my.eigen)

princomp(datast)$loadings

principals=princomp(datast)
biplot(principals)
summary(principals)


Sales <- read.csv("C:/Users/01438475/Google Drive/UCTcourses/STA3022F/Datasets/2017/2.PCA.1-Sales.csv")
head(Sales)
var(Sales)