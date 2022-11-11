library(sf)
library(tidyverse)
library(spatstat)
library(data.table)
library(maptools)

#' List of shortest path distances between points in a planar network
#'
#' Computation of shortest path distances between all observed points of a planar network and randomly simulated points
#' @param listpp The list of sf point pattern
#' @param listnetwork The list of sf planar network lines
#' @param nsim The number of simulations
#' @return The list of tibble of shortest paths
#' @examples 
#' blob
#' @export

lpp_dist_with_sims <- function(listpp, listnetwork, nsim){
  # reading data
  liste_patterns_snap <- pp
  list_of_networks <- lnetwork
  
  #### distances matrices
  
  #### random points on network ####
  # generate points on a linear network
  liste_rss_csr <- list()
  nsimulation <- nsim
  
  for (i in 1:length(liste_patterns_snap)) {
    # run génération de points aléatoires
    liste_calcul <- runiflpp(n = nrow(liste_patterns_snap[[i]]), 
                             L = maptools::as.linnet.SpatialLines(X = as(list_of_networks[[i]], "Spatial")), 
                             nsim = nsimulation) # nombre de simulation choisies
    
    liste_rss_csr[[i]] <- liste_calcul
    
  }
  
  
  ### calculs des shortest past
  # calcul des shortest past pour toutes les simulations
  liste_dist_pi_p <- list()
  
  for (i in 1:length(liste_rss_csr)) {
    
    liste_rss_csr_extract <- lapply(liste_rss_csr[[i]], `[`, c())
    
    ma_liste_simulation <- list()
    
    for (j in 1:length(liste_rss_csr_extract)) {
      dist_pi_p <- pairdist(X = liste_rss_csr_extract[[j]])
      
      dist_pi_p[upper.tri(x = dist_pi_p, diag = TRUE)] <- NA
      
      ma_liste_simulation[[j]] <- dist_pi_p %>%
        as_tibble() %>%
        rowid_to_column(var = "Pi") %>%
        pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
        filter(!is.na(dist_pi_p)) %>%
        mutate(P = str_replace_all(string = P, pattern = "V", replacement = "")) %>%
        mutate(sim = j)
      
    }
    
    liste_dist_pi_p[[i]] <- ma_liste_simulation
    
  }
  
  ### observed distributions
  liste_observed_dist <- list()
  
  for (i in 1:length(liste_patterns_snap)) {
    observed_lpp <- lpp(X = as.ppp(X = liste_patterns_snap[[i]] %>% select(geometry)), # transformation du sf en Planar point pattern (ppp)
                        L = maptools::as.linnet.SpatialLines(X = as(list_of_networks[[i]], "Spatial"))) # transofrmation en Linear network (Spatial lines)
    
    tableau_observed_dist_pi_p <- pairdist(X = observed_lpp)
    
    tableau_observed_dist_pi_p[upper.tri(x = tableau_observed_dist_pi_p, diag = TRUE)] <- NA
    
    liste_observed_dist[[i]] <- tableau_observed_dist_pi_p %>%
      as_tibble() %>%
      rowid_to_column(var = "Pi") %>%
      pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
      filter(!is.na(dist_pi_p)) %>%
      mutate(P = str_replace_all(string = P, pattern = "V", replacement = "")) %>%
      mutate(sim = NA)
    
  }
  
  ### tableaux pour plot des shortest path ####
  # simulation et observations
  # en tibble
  list_result_shortest_path_matrices <- list()
  
  for (i in 1:length(liste_dist_pi_p)) {
    tableau_init <- rbindlist(l = liste_dist_pi_p[[i]]) %>% 
      as_tibble() %>%
      bind_rows(liste_observed_dist[[i]])
    
    
    list_result_shortest_path_matrices[[i]] <- tableau_init
  }
  
  return(list_result_shortest_path_matrices)
  
}