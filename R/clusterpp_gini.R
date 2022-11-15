#' Gini on point clusters
#'
#' @description
#' Compute gini index on clusters of spatial points
#'
#' @param sfcluster A sf point patterns and their belonging to clusters
#' @return Gini index
#' 
#' @seealso 
#' [dist_clustering()] for clustering points on planar networks
#'
#'
#' @importFrom Rdpack reprompt
#' @export

clusterpp_gini <- function(sfcluster) {
  
  cluster_gini <- sfcluster %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(gini = ineq::ineq(x = count, type = "Gini"))
  
  return(cluster_gini)
}