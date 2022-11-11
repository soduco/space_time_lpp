#' Shortest path distances between a list of points on a list of planar network
#'
#' @description
#' Computation of shortest path distances between all observed points of a planar network and randomly simulated points.
#' Input data point must be snapped on linear network.
#' The length and order of lists of points and networks must be equivalent and consistent.
#'
#' @param listpp a list of sf point patterns
#' @param listnetwork a list of sf planar networks lines
#' @param nsim the number of simulations (must be > 1)
#' @return a list of tibble of shortest paths
#' @examples
#' blob
#' @importFrom Rdpack reprompt
#' @export

dist_with_sims_list <- function(listpp, listnetwork, nsim) {

  # expressive data
  list_patterns_snap <- listpp
  list_of_networks <- listnetwork

  #### create random points on network and  calculating distances ####
  nsimulation <- nsim # expressive

  ## generate points on a linear network
  liste_rss_csr <- list()

  for (i in 1:length(list_patterns_snap)) {
    list_calcul <- spatstat.linnet::runiflpp(
      n = nrow(list_patterns_snap[[i]]), # generate same N points as observed data
      L = maptools::as.linnet.SpatialLines(X = as(list_of_networks[[i]], "Spatial")),
      nsim = nsimulation
    )

    liste_rss_csr[[i]] <- list_calcul
  }

  ### calculating shortest paths for simulations
  list_dist_pi_p <- list()

  for (i in 1:length(liste_rss_csr)) {
    liste_rss_csr_extract <- lapply(liste_rss_csr[[i]], `[`, c())

    ma_liste_simulation <- list()

    for (j in 1:length(liste_rss_csr_extract)) {
      dist_pi_p <- spatstat.geom::pairdist(X = liste_rss_csr_extract[[j]])

      dist_pi_p[upper.tri(x = dist_pi_p, diag = TRUE)] <- NA

      ma_liste_simulation[[j]] <- dist_pi_p %>%
        tibble::as_tibble() %>%
        tibble::rowid_to_column(var = "Pi") %>%
        tidyr::pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
        dplyr::filter(!is.na(dist_pi_p)) %>%
        dplyr::mutate(P = stringr::str_replace_all(string = P, pattern = "V", replacement = "")) %>%
        dplyr::mutate(sim = j)
    }

    list_dist_pi_p[[i]] <- ma_liste_simulation
  }

  #### Calculating distances on observed point patterns ####
  list_observed_dist <- list()

  for (i in 1:length(list_patterns_snap)) {
    # creation of an lpp object
    observed_lpp <- spatstat.linnet::lpp(
      X = spatstat.geom::as.ppp(X = list_patterns_snap[[i]] %>% dplyr::select(geometry)),
      L = maptools::as.linnet.SpatialLines(X = as(list_of_networks[[i]], "Spatial"))
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
  # simulation and observed shortest paths
  list_result_shortest_path_matrices <- list()

  for (i in 1:length(list_dist_pi_p)) {
    tableau_init <- data.table::rbindlist(l = list_dist_pi_p[[i]]) %>%
      tibble::as_tibble(.name_repair = 'unique') %>%
      dplyr::bind_rows(list_observed_dist[[i]])


    list_result_shortest_path_matrices[[i]] <- tableau_init
  }

  return(list_result_shortest_path_matrices)
}
