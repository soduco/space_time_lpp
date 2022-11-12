#' Shortest paths distances between points on a planar network
#'
#' @description
#' Computation of shortest path distances between all observed points of a planar network and
#' shortest paths distances between randomly simulated points on the same planar network.
#' Input point patterns data must be snapped on linear networks.
#'
#' @param pp A sf point patterns
#' @param network A sf planar networks
#' @param nsim The number of simulations generating randomly simulated points (must be > 1)
#' @return A tibble of shortest paths
#'
#' @details 
#' * `Pi`/`P` points numbers in output tibble refer to initial order of rows in point patterns data.
#' * `sim` column refers to the to the number of a specific simulation and the 
#' `type` to the calculated distance depending on whether the points are simulated or observed.
#'
#' @seealso [snapPointsToLines()] for snapping points on lines.
#'
#' @examples
#' library(sf)
#' library(ggplot2)
#' pharmacy # point pattern data
#' paris_network # planar network data 
#' distances <- dist_with_sims(pp = pharmacy, network = paris_network, nsim = 2)
#' ggplot(data = distances, mapping = aes(x = dist_pi_p, color = type)) +
#' geom_density()
#'
#' @importFrom Rdpack reprompt
#' @export

dist_with_sims <- function(pp, network, nsim) {
  
  #### create random points on network and calculating distances ####
  ## generate points on a linear network
  calcul_rss_crs <- spatstat.linnet::runiflpp(
    n = nrow(pp), # generate same N points as observed data
    L = maptools::as.linnet.SpatialLines(X = as(network, "Spatial")),
    nsim = nsim
  )
  
  ## calculating shortest paths for simulations
  list_simulated_dist <- list()
  
  for (i in 1:length(calcul_rss_crs)) {
    dist_pi_p <- spatstat.geom::pairdist(X = calcul_rss_crs[[i]])
    
    dist_pi_p[upper.tri(x = dist_pi_p, diag = TRUE)] <- NA
    
    dist_pi_p <- dist_pi_p %>%
      tibble::as_tibble() %>%
      tibble::rowid_to_column(var = "Pi") %>%
      tidyr::pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
      dplyr::filter(!is.na(dist_pi_p)) %>%
      dplyr::mutate(P = stringr::str_replace_all(string = P, pattern = "V", replacement = "")) %>%
      dplyr::mutate(sim = i)
    
    list_simulated_dist[[i]] <- dist_pi_p
  }
  
  #### Calculating distances on observed point patterns ####
  ## creation of an lpp object
  observed_lpp <- spatstat.linnet::lpp(
    X = spatstat.geom::as.ppp(X = pp %>% dplyr::select(geometry)),
    L = maptools::as.linnet.SpatialLines(X = as(network, "Spatial"))
  )
  
  ## calculating shortest paths for observed data
  matrix_observed_dist_pi_p <- spatstat.geom::pairdist(X = observed_lpp)
  
  matrix_observed_dist_pi_p[upper.tri(x = matrix_observed_dist_pi_p, diag = TRUE)] <- NA
  
  observed_dist <- matrix_observed_dist_pi_p %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column(var = "Pi") %>%
    tidyr::pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
    dplyr::filter(!is.na(dist_pi_p)) %>%
    dplyr::mutate(P = stringr::str_replace_all(string = P, pattern = "V", replacement = "")) %>%
    dplyr::mutate(sim = NA)
  
  ### list of output tibbles ####
  # simulated and observed shortest paths
  result_shortest_path_matrices <- data.table::rbindlist(l = list_simulated_dist) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    dplyr::mutate(type = "simulation") %>%
    dplyr::bind_rows(
      observed_dist %>%
        dplyr::mutate(type = "observation")
    )
  
  return(result_shortest_path_matrices)
}
