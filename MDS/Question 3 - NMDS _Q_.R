# =================== #
# NMDS Class Exercise #
# =================== #

# 1: Input the dissimilarity ("cities") data from the "psych" library and 
#    save it to a variable named "Delta".



# 2: Save the lower triangular matrix of "Delta", as a vector, to a "Diss" variable.  



# 3: Label/name the "Diss" variable entries from 1:(M = number of dissimilarities). 



# 4: Order the dissimilarity vector "Diss" in ascending order. Save the corresponding
#    output in a variable denoted "SortedDiss".



# 5: Use the cmdscale function to obtain an initial configuration of the cities 
#    within a 2-dimensional space (k = 2), by means of a classical scaling approach.
#    Save the corresponding output in a variable named Y0.
#    Hint: The function requires a euclidean distance matrix input. 



# 6: Plot the initial configuration and associated text labels.



# 7: Calculate the euclidean distances between the initial configuration Y0, 
#    and save the output to a variable named "Dist". 



# 8: Convert the distances to a single vector labelled "VecDist".



# 9: Label/name the "VecDist" variable entries from 1:M.


 
# 10: Order the Y0 distances "VecDist" according to the ordered dissimilarity vector 
#     "SortedDiss". Save the output in a variable named "SortedDist".
#     Hint: Use names(SortedDiss) to order the vector



# 11: Calculate suitable disparities, related to "SortedDist", using isotonic regression.
#     Hint: Apply the pava function, from the "Iso" package, to the "SortedDist" distances. 



# 12: Plot the "SortedDist" against the index order. Overlay the plot with disparity points,
#     linked by a line. This is known as a Shephard Diagram



# 13: Build Kruskal's stress function with 2 input variables: Y0 distances, disparities.
#     Save the function as "Stress".



# 14: Calculate the initial stress



# 15: Redefine the stress function as a function of Y only. Name the function "StressY".
#     Hint: Combine steps 7 - 11 with the stress function.



# 16: Build a plotting function to plot the 2-dimensional co-ordinate output, 
#     with text labels. Call this function "PlotCoordinates".



# 16: Optimise the "StressY" function, return the output using the first 
#     5 optimisation methods present in the "optim" function.



# 17: Compare the convergence property and stress values of the prevailing
#     optimisations and plot the lowest stress value configuration.



# Note: higher stress AS COMPARED TO the intial configuration DUE TO the 
#       ranking of distances/disparities.
