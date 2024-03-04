# install.packages("devtools")
devtools::install_github("jlmelville/coil20")
library(coil20)

# fetch the data set from the COIL-20 website
# takes a little while so you might want some indication of what's going on
coil20 <- download_coil20(verbose = TRUE)

# view the zeroth pose of the fourth object
show_object(coil20, object = 4, pose = 0)

# Example of use: PCA
pca <- prcomp(coil20[, 1:128 ^ 2], retx = TRUE)
# plot the scores of the first two components
plot(pca$x[, 1:2], type = 'n')
text(pca$x[, 1:2], labels = coil20$Label, cex = 0.5,
     col = rainbow(length(levels(coil20$Label)))[coil20$Label])

# save data set to disk
save(coil20, file = "coil20.Rda")

# Fetch COIL-100
# Takes a long time to process all 7,200 images (a couple of hours)
coil100 <- download_coil100(verbose = TRUE)