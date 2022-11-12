#' Shortest paths distances between points (class list) on planar networks (class list)
#'
#' @description
#' Computation of shortest path distances between all observed points of planar networks and
#' shortest paths distances between randomly simulated points on the same planar networks.
#' Input point patterns data must be snapped on linear networks.
#' The length and order of lists of points and planar networks must be equivalent and consistent.
#'
#' @param listpp A list of sf point patterns
#' @param listnetwork A list of sf planar networks
#' @param nsim The number of simulations generating randomly simulated points (must be > 1)
#' @return A list of tibble of shortest paths
#'
#' @details 
#' * `Pi`/`P` points numbers in output tibbles refer to initial order of rows in point patterns data.
#' * `sim` column refers to the to the number of a specific simulation and the 
#' `type` to the calculated distance depending on whether the points are simulated or observed.
#'
#' @seealso [snapPointsToLines()] for snapping points on lines.
#'
#'
#' @importFrom Rdpack reprompt
#' @export

dist_with_sims_list <- function(listpp, listnetwork, nsim) {

  #### create random points on network and calculating distances ####
  ## generate points on a linear network
  liste_rss_csr <- list()

  for (i in 1:length(listpp)) {
    list_calcul <- spatstat.linnet::runiflpp(
      n = nrow(listpp[[i]]), # generate same N points as observed data
      L = maptools::as.linnet.SpatialLines(X = as(listnetwork[[i]], "Spatial")),
      nsim = nsim
    )

    liste_rss_csr[[i]] <- list_calcul
  }

  ## calculating shortest paths for simulations
  list_simulated_dist <- list()

  for (i in 1:length(liste_rss_csr)) {
    liste_rss_csr_extract <- lapply(liste_rss_csr[[i]], `[`, c())

    list_dist_pi_p <- list()

    for (j in 1:length(liste_rss_csr_extract)) {
      dist_pi_p <- spatstat.geom::pairdist(X = liste_rss_csr_extract[[j]])

      dist_pi_p[upper.tri(x = dist_pi_p, diag = TRUE)] <- NA

      list_dist_pi_p[[j]] <- dist_pi_p %>%
        tibble::as_tibble() %>%
        tibble::rowid_to_column(var = "Pi") %>%
        tidyr::pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
        dplyr::filter(!is.na(dist_pi_p)) %>%
        dplyr::mutate(P = stringr::str_replace_all(string = P, pattern = "V", replacement = "")) %>%
        dplyr::mutate(sim = j)
    }

    list_simulated_dist[[i]] <- list_dist_pi_p
  }

  #### Calculating distances on observed point patterns ####
  list_observed_dist <- list()

  for (i in 1:length(listpp)) {
    # creation of an lpp object
    observed_lpp <- spatstat.linnet::lpp(
      X = spatstat.geom::as.ppp(X = listpp[[i]] %>% dplyr::select(geometry)),
      L = maptools::as.linnet.SpatialLines(X = as(listnetwork[[i]], "Spatial"))
    )

    matrix_observed_dist_pi_p <- spatstat.geom::pairdist(X = observed_lpp)

    matrix_observed_dist_pi_p[upper.tri(x = matrix_observed_dist_pi_p, diag = TRUE)] <- NA

    list_observed_dist[[i]] <- matrix_observed_dist_pi_p %>%
      tibble::as_tibble() %>%
      tibble::rowid_to_column(var = "Pi") %>%
      tidyr::pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
      dplyr::filter(!is.na(dist_pi_p)) %>%
      dplyr::mutate(P = stringr::str_replace_all(string = P, pattern = "V", replacement = "")) %>%
      dplyr::mutate(sim = NA)
  }

  ### list of output tibbles ####
  # simulated and observed shortest paths
  list_result_shortest_path_matrices <- list()

  for (i in 1:length(list_simulated_dist)) {
    final_tibble <- data.table::rbindlist(l = list_simulated_dist[[i]]) %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      dplyr::mutate(type = "simulation") %>%
      dplyr::bind_rows(
        list_observed_dist[[i]] %>%
          dplyr::mutate(type = "observation")
      )


    list_result_shortest_path_matrices[[i]] <- final_tibble
  }

  return(list_result_shortest_path_matrices)
}
