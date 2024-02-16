rm(list=ls())
#
#####  Input matrix. ###

Delta_mat <- structure(c(0, 93, 82, 133, 93, 0, 52, 60, 82, 52, 0, 111, 133,60, 111, 0), dim = c(4L, 4L))

Delta_mat |> signif(2)


# Step 1: Obtain "A" matrix

A_mat <- -1/2 * Delta_mat^2
A_mat |> signif(3)

# Calculate B Matrix ####
## B = HAH

# Step 1 calculate H(centering matrix) matrix

# H = I - 1/nII^T
H <- diag(4) - 1/4 * matrix(1, 4, 4)
B_mat <- H%*%A_mat%*%H
B_mat |> signif(3)


