rm(list=ls())
library(MASS)
library(tibble)
library(dplyr)
library(ggrepel)
library(ggplot2)

set.seed(12394)
data("data_tidy_sa_distance", package = "DataTidy24STA5069Z")

sammon_obj <- sammon(data_tidy_sa_distance |> as.matrix())
#sammon(d, y = cmdscale(d, k), k = 2, niter = 1e2,trace = TRUE, magic = 0.2, tol = 1e-4)

plot_tbl <- sammon_obj$points |>
            as_tibble() |>
            mutate(
               city = data_tidy_sa_distance |>
            colnames()
      )
p <- ggplot(plot_tbl, aes(V1, V2)) +
  geom_point(size = 3) +
  geom_text_repel(
    aes(label = city), size = 10
  ) +
  coord_equal()

sammon_obj$stress

sammon_obj$points

p