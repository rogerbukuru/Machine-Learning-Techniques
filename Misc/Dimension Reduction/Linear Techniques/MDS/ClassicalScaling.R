rm(list=ls())
library(ggplot2)
#Classical Scaling

#####  Input matrix. ###

Delta_mat <- structure(c(0, 93, 82, 133, 93, 0, 52, 60, 82, 52, 0, 111, 133,60, 111, 0), dim = c(4L, 4L))

Delta_mat |> signif(2)


# Step 1: Obtain "A" matrix

A_mat <- -1/2 * Delta_mat^2
A_mat |> signif(3)

# Step 2:  Calculate B Matrix
## B = HAH and H = I - 1/nII^T

H <- diag(4) - 1/4 * matrix(1, 4, 4)
B_mat <- H%*%A_mat%*%H
B_mat |> signif(3)

# Step 3: Perform Eigendecompistion of B Matrix
# calcualte eigenvalues and eigenvectors
# B = PDP^(-1)

evd <- eigen(B_mat)

# Y = eigenvectos%*%diag(eigenvalues)*1/2

Y_mat <- evd$vectors %*% diag(sqrt(evd$values))
Y_mat |> signif(2)


plot_tbl <- Y_mat |>
  tibble::as_tibble() |>
  dplyr::mutate(
    city = c(
      "Kobenhavn", "Arhus",
      "Odense", "Aalborg"
    )
  )
p <- ggplot(plot_tbl, aes(V1, V2)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(
    aes(label = city), size = 10
  ) +
  coord_equal()

p

#### Using R Pacakge ####
fit.Denmark = cmdscale(Delta_mat, eig = TRUE, list. = TRUE)



plot_tbl_2 <- fit.Denmark$points |>
  tibble::as_tibble() |>
  dplyr::mutate(
    city = c(
      "Kobenhavn", "Arhus",
      "Odense", "Aalborg"
    )
  )
p_2 <- ggplot(plot_tbl_2, aes(V1, V2)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(
    aes(label = city), size = 10
  ) +
  coord_equal()

