library(sf)
library(tidyverse)
library(spatstat)
library(data.table)
library(maptools)

#' List of shortest path distances between points in a planar network
#'
#' Computation of shortest path distances between all observed points of a planar network and randomly simulated points
#' @param listpp The list of sf point patterns
#' @param listnetwork The list of sf planar networks lines
#' @param nsim The number of simulations
#' @return The list of tibble of shortest paths
#' @examples 
#' blob
#' @author boubou
#' @export

list_dist_with_sims <- function(listpp, listnetwork, nsim){
  
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
    
    liste_rss_csr_extract <- lapply(liste_rss_csr[[i]], `[`, c())
    
    simulation_list <- list()
    
    for (j in 1:length(liste_rss_csr_extract)) {
      dist_pi_p <- pairdist(X = liste_rss_csr_extract[[j]])
      
      # transform upper distance matrix as NA
      dist_pi_p[upper.tri(x = dist_pi_p, diag = TRUE)] <- NA
      
      # creating results as tibble in long format
      simulation_list[[j]] <- dist_pi_p %>%
        as_tibble() %>%
        rowid_to_column(var = "Pi") %>%
        pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
        filter(!is.na(dist_pi_p)) %>%
        mutate(P = str_replace_all(string = P, pattern = "V", replacement = "")) %>%
        mutate(sim = j)
      
    }
    
    list_dist_pi_p[[i]] <- simulation_list
    
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
    tableau_init <- rbindlist(l = list_dist_pi_p[[i]]) %>% 
      as_tibble() %>%
      bind_rows(list_observed_dist[[i]])
    
    
    list_result_shortest_path_matrices[[i]] <- tableau_init
  }
  
  return(list_result_shortest_path_matrices)
  
}