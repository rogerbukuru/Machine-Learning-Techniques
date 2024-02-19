# =================== #
# NMDS Class Exercise #
# =================== #

# 1: Input the dissimilarity ("cities") data from the "psych" library and 
#    save it to a variable named "Delta".
rm(list = ls())
library(psychTools)
library(ggplot2)
library(dplyr)
library(tibble)
library(ggrepel)
library(Iso)
data(cities)

delta = as.matrix(cities)

# 2: Save the lower triangular matrix of "Delta", as a vector, to a "Diss" variable.  

diss = delta[lower.tri(delta)] |> as.vector()


# 3: Label/name the "Diss" variable entries from 1:(M = number of dissimilarities). 

names(diss) = 1:length(diss)



# 4: Order the dissimilarity vector "Diss" in ascending order. Save the corresponding
#    output in a variable denoted "SortedDiss".

sortedDiss = sort(diss, decreasing = FALSE)

# 5: Use the cmdscale function to obtain an initial configuration of the cities 
#    within a 2-dimensional space (k = 2), by means of a classical scaling approach.
#    Save the corresponding output in a variable named Y0.
#    Hint: The function requires a euclidean distance matrix input. 

Y0 = cmdscale(delta, k=2, eig = TRUE)

# 6: Plot the initial configuration and associated text labels.

initial_config_points = Y0$points |>
                        as_tibble() |>
                        mutate(
                          city = colnames(delta)
                        )

ggplot(initial_config_points, aes(-V1, -V2)) +
      geom_point(size = 3) +
      geom_text_repel(
        aes(label = city),
        size = 5
      ) +
      labs(x = "First principal component", y = "Second principal component", title = "Classical Scaling of US cities") +
      coord_equal()


# 7: Calculate the euclidean distances between the initial configuration Y0, 
#    and save the output to a variable named "Dist". 

Dist = dist(Y0$points)

# 8: Convert the distances to a single vector labelled "VecDist".

vecDist = as.vector(Dist)

# 9: Label/name the "VecDist" variable entries from 1:M.

names(vecDist) = 1:length(vecDist)


# 10: Order the Y0 distances "VecDist" according to the ordered dissimilarity vector 
#     "SortedDiss". Save the output in a variable named "SortedDist".
#     Hint: Use names(SortedDiss) to order the vector

sortedDist = vecDist[as.numeric(names(sortedDiss))]


# 11: Calculate suitable disparities, related to "SortedDist", using isotonic regression.
#     Hint: Apply the pava function, from the "Iso" package, to the "SortedDist" distances. 

disparities = pava(sortedDist, decreasing= FALSE)

# 12: Plot the "SortedDist" against the index order. Overlay the plot with disparity points,
#     linked by a line. This is known as a Shephard Diagram

plot(sortedDist)
lines(disparities, type="s")

# 13: Build Kruskal's stress function with 2 input variables: Y0 distances, disparities.
#     Save the function as "Stress".

stress = function (y0_dsitances, disparities){
  stress_numerator = sum((y0_dsitances-disparities)^2)
  stress_denominator = sum((y0_dsitances)^2)
  kruskal_stress = stress_numerator/stress_denominator
  kruskal_stress
}

# 14: Calculate the initial stress

initial_stress = stress(sortedDiss, disparities)


# 15: Redefine the stress function as a function of Y only. Name the function "StressY".
#     Hint: Combine steps 7 - 11 with the stress function.

stressY = function(y, d){
  #'d' is a matrix of original dissimilarities
  dist_mat = as.matrix((dist(y)))
  stress_numerator = sum((d-dist_mat)^2)
  stress_denominator = sum((d)^2)
  stress = stress_numerator/stress_denominator
  stress
}

# 16: Build a plotting function to plot the 2-dimensional co-ordinate output, 
#     with text labels. Call this function "PlotCoordinates".

plotCoordinates = function(points){
  plot(points, xlim = range(points[, 1]), ylim = range(coords[, 2]), xlab = "Dimension 1", ylab = "Dimension 2", main = "2D Coordinates Plot", asp = 1, pch = 19, col = "blue")
  text(coords, labels = labels, pos = 3) # pos = 3 for labels above the points
 # lines(disparities, type="s")
}

# 16: Optimise the "StressY" function, return the output using the first 
#     5 optimisation methods present in the "optim" function.

optim_nelder_mead = optim(Y0$points, stressY, d = diss, method = "Nelder-Mead")
optim_bfgs        = optim(Y0$points, stressY, d = diss, method = "BFGS")
optim_cg          = optim(Y0$points, stressY, d = diss, method = "CG")
optim_lbggsb      = optim(Y0$points, stressY, d = diss, method = "L-BFGS-B")
optim_sann        = optim(Y0$points, stressY, d = diss, method = "SANN")




# 17: Cocounts# 17: Compare the convergence property and stress values of the prevailing
#     optimisations and plot the lowest stress value configuration.

optim_nelder_mead$convergence
optim_bfgs$convergence
optim_cg$convergence
optim_lbggsb$convergence
optim_sann$convergence

stress_nelder_mead = stressY(Y0$points, optim_nelder_mead$par)

# Note: higher stress AS COMPARED TO the intial configuration DUE TO the 
#       ranking of distances/disparities.
