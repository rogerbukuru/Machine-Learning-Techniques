rm(list=ls())

if (!requireNamespace("smacof", quietly = TRUE)) {
  install.packages("smacof")
}
data("kinshipscales", package = "smacof")
data("kinshipdelta", package = "smacof")

kinshipscales

# Step 1: Starting with Dissimilarities
proximity_matrix = kinshipdelta

# Step 2: Initial Configuration
# Choose an initial configuration of points in a lower-dimensional space: Classical Scaling
y_vec = cmdscale(kinshipdelta) |> as.vector()



calc_stress = function(y, delta_mat, dim = 2) {
#Step 3: Compute Distances
#For the current configuration of points, calculate the Euclidean distances d_ij between all pairs of points in the lower-dimensional space.
  Y_mat <- matrix(y, ncol = dim)
  D_mat <- as.matrix(dist(Y_mat))
#Step 4: Least-Squares Optimization
  error_mat <- ((D_mat - delta_mat)^2)[lower.tri(D_mat)] # using identify f i.e f = delta_mat
  weight <- 1 / sum(D_mat[lower.tri(D_mat)]^2)
  sum(error_mat) * weight
}

y_vec <- cmdscale(kinshipdelta) |> as.vector()
optim_obj <- optim(y_vec, calc_stress, delta_mat = kinshipdelta)
Y_mat <- matrix(optim_obj$par, ncol = 2)

plot_tbl <- Y_mat |>
  tibble::as_tibble() |>
  dplyr::mutate(
    familial_term = kinshipdelta |>
      colnames()
  )
p <- ggplot(plot_tbl, aes(V1, V2)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(
    aes(label = familial_term),
    size = 10
  ) +
  coord_equal()
p