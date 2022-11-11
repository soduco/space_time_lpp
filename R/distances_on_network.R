#' List of shortest path distances between points in a planar network
#'
#' Computation of shortest path distances between all observed points of a planar network and randomly simulated points
#' @param listpp a list of sf point patterns
#' @param listnetwork a list of sf planar networks lines
#' @param nsim the number of simulations
#' @return a list of tibble of shortest paths
#' @examples 
#' blob
#' @importFrom Rdpack reprompt
#' @export

dist_with_sims <- function(listpp, listnetwork, nsim){
  
  # data
  list_patterns_snap <- listpp
  list_of_networks <- listnetwork
  
  #### create random points on network and  calculating distances ####
  nsimulation <- nsim # expressive
  
  #3 generate points on a linear network
  liste_rss_csr <- list()
  
  for (i in 1:length(list_patterns_snap)) {
    
    list_calcul <- runiflpp(n = nrow(list_patterns_snap[[i]]), # generate same N points as observed data
                             L = maptools::as.linnet.SpatialLines(X = as(list_of_networks[[i]], "Spatial")), 
                             nsim = nsimulation)
    
    liste_rss_csr[[i]] <- list_calcul
    
  }
  
  ### calculating shortest paths for simulations
  list_dist_pi_p <- list()
  
  for (i in 1:length(liste_rss_csr)) {
    
    # distance matrix calculation
    dist_pi_p <- pairdist(X = liste_rss_csr[[i]])
    
    # transform upper distance matrix as NA
    dist_pi_p[upper.tri(x = dist_pi_p, diag = TRUE)] <- NA
    
    # creating results as tibble in long format
    list_dist_pi_p[[i]] <- dist_pi_p %>%
      as_tibble() %>%
      rowid_to_column(var = "Pi") %>%
      pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
      filter(!is.na(dist_pi_p)) %>%
      mutate(P = str_replace_all(string = P, pattern = "V", replacement = "")) %>%
      mutate(sim = i)
    
  }
  
  #### Calculating distances on observed point patterns ####
  list_observed_dist <- list()
  
  for (i in 1:length(list_patterns_snap)) {
    # creation of an lpp object
    observed_lpp <- lpp(X = as.ppp(X = list_patterns_snap[[i]] %>% select(geometry)),
                        L = maptools::as.linnet.SpatialLines(X = as(list_of_networks[[i]], "Spatial")))
    
    matrix_observed_dist_pi_p <- pairdist(X = observed_lpp)
    
    matrix_observed_dist_pi_p[upper.tri(x = matrix_observed_dist_pi_p, diag = TRUE)] <- NA
    
    list_observed_dist[[i]] <- matrix_observed_dist_pi_p %>%
      as_tibble() %>%
      rowid_to_column(var = "Pi") %>%
      pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
      filter(!is.na(dist_pi_p)) %>%
      mutate(P = str_replace_all(string = P, pattern = "V", replacement = "")) %>%
      mutate(sim = NA)
    
  }
  
  ### list of output tibbles ####
  # simulation and observed shortest paths
  list_result_shortest_path_matrices <- list()
  
  for (i in 1:length(list_dist_pi_p)) {
    tableau_init <- list_dist_pi_p[[i]] %>% 
      bind_rows(list_observed_dist[[i]])
    
    
    list_result_shortest_path_matrices[[i]] <- tableau_init
  }
  
  return(list_result_shortest_path_matrices)
  
}