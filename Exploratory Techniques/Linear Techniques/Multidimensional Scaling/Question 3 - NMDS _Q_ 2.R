# =================== #
# NMDS Class Exercise #
# =================== #

# 1: Input the dissimilarity ("cities") data from the "psych" library and 
#    save it to a variable named "Delta".
rm(list = ls())
library(psych)
library(psychTools)
data(cities)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(Iso)

Delta = as.matrix(cities)

# 2: Save the lower triangular matrix of "Delta", as a vector, to a "Diss" variable.  

Diss = Delta[lower.tri(Delta)]

# 3: Label/name the "Diss" variable entries from 1:(M = number of dissimilarities). 

names(Diss) = 1:length(Diss)


# 4: Order the dissimilarity vector "Diss" in ascending order. Save the corresponding
#    output in a variable denoted "SortedDiss".

SortedDiss = sort(Diss, decreasing = FALSE) 


# 5: Use the cmdscale function to obtain an initial configuration of the cities 
#    within a 2-dimensional space (k = 2), by means of a classical scaling approach.
#    Save the corresponding output in a variable named Y0.
#    Hint: The function requires a euclidean distance matrix input. 


Y0 = cmdscale(Delta, k =2, eig = TRUE)

# 6: Plot the initial configuration and associated text labels.

plot_tbl = Y0$points %>%
            as_tibble() %>%
            mutate(city = colnames(Delta))

ggplot(plot_tbl, aes(x= -V1, y=-V2))+
      geom_point(
        size = 3,
        colour = "blue"
        )+
      geom_text_repel(
        aes(label = city)
      )+
      labs(title = "US Cities MDS Classical Scaling", x= "First Coordinate", y="Second Cordinate")+
      coord_equal()

# 7: Calculate the euclidean distances between the initial configuration Y0, 
#    and save the output to a variable named "Dist". 

Dist = dist(Y0$points)

# 8: Convert the distances to a single vector labelled "VecDist".

VecDist = as.vector(Dist)

# 9: Label/name the "VecDist" variable entries from 1:M.

names(VecDist) = 1:length(VecDist)
 
# 10: Order the Y0 distances "VecDist" according to the ordered dissimilarity vector 
#     "SortedDiss". Save the output in a variable named "SortedDist".
#     Hint: Use names(SortedDiss) to order the vector

SortedDist = VecDist[as.numeric(names(SortedDiss))]


# 11: Calculate suitable disparities, related to "SortedDist", using isotonic regression.
#     Hint: Apply the pava function, from the "Iso" package, to the "SortedDist" distances. 

disparities = pava(SortedDist, decreasing= FALSE)



# 12: Plot the "SortedDist" against the index order. Overlay the plot with disparity points,
#     linked by a line. This is known as a Shephard Diagram
index_order = as.integer(names(Diss))
data_tbl = data.frame( index_order = index_order, SortedDiss = SortedDiss, disparities = disparities)

ggplot(data_tbl, aes(x = index_order, y=SortedDiss))+
      geom_point(size=3, color="blue")+
      geom_line(aes(y=disparities, color="red"))+
      labs(x = "Dissimilarity Rank", y = "Distance / Disparity", title = "Shephard Diagram") +
      theme_minimal()

ggplot(data_tbl, aes(x = index_order)) +
  geom_point(aes(y = SortedDiss), size = 3, color = "blue") +
  #geom_line(aes(y = SortedDiss), color = "blue") +
  #geom_point(aes(y = disparities), size = 2, color = "red") +
  geom_line(aes(y = disparities), color = "red") +
  labs(x = "Dissimilarity Rank", y = "Distance / Disparity", title = "Shephard Diagram") +
  theme_minimal()



# 13: Build Kruskal's stress function with 2 input variables: Y0 distances, disparities.
#     Save the function as "Stress".

kruskal_stress = function (y, disparities){
  d_mat = dist(y)
  stress_numerator = sum((d_mat-disparities)^2) # Disparities from from istonic regression
  stress_denominator = sum((d_mat)^2)
  kruskal_stress = stress_numerator/stress_denominator
  kruskal_stress
}

# 14: Calculate the initial stress

initial_stress = kruskal_stress(Y0$points, disparities)

# 15: Redefine the stress function as a function of Y only. Name the function "StressY".
#     Hint: Combine steps 7 - 11 with the stress function.
stressY = function(y){
  #'d' is a matrix of original dissimilarities
  dist_mat = as.vector((dist(y)))
  #d_mat = as.vector(d)
  stress_numerator = sum((dist_mat-SortedDiss)^2) ## identity increasing function
  stress_denominator = sum((dist_mat)^2)
  stress = stress_numerator/stress_denominator
  stress
}



# 16: Build a plotting function to plot the 2-dimensional co-ordinate output, 
#     with text labels. Call this function "PlotCoordinates".

plotCoordinates = function(points, cities){
  points_with_city = points |>
    as.tibble() |>
    mutate(
      city = cities
    )
  ggplot(points_with_city, aes(-V1, -V2)) +
    geom_point(size = 3) +
    geom_text_repel(
      aes(label = city),
      size = 5
    ) +
    labs(x = "First principal component", y = "Second principal component", title = "US cities") +
    coord_equal()+
    theme_minimal()
  
  # lines(disparities, type="s")
}

# 16: Optimise the "StressY" function, return the output using the first 
#     5 optimisation methods present in the "optim" function.

optim_nelder_mead = optim(Y0$points, stressY, method = "Nelder-Mead")
optim_bfgs        = optim(Y0$points, stressY, method = "BFGS")
optim_cg          = optim(Y0$points, stressY, method = "CG")
optim_lbggsb      = optim(Y0$points, stressY, method = "L-BFGS-B")
optim_sann        = optim(Y0$points, stressY, method = "SANN")

# 17: Compare the convergence property and stress values of the prevailing
#     optimisations and plot the lowest stress value configuration.

optim_nelder_mead$convergence
optim_bfgs$convergence
optim_cg$convergence
optim_lbggsb$convergence
optim_sann$convergence

# Not sure about these :()
stress_nelder_mead = kruskal_stress(optim_nelder_mead$par, disparities)
stress_bfgs        = kruskal_stress(optim_bfgs$par, disparities)
stress_cg          = kruskal_stress(optim_cg$par, disparities)
stress_lbggsb      = kruskal_stress(optim_lbggsb$par, disparities)
stress_sann        = kruskal_stress(optim_sann$par, disparities)


# Note: higher stress AS COMPARED TO the intial configuration DUE TO the 
#       ranking of distances/disparities.
