#' Clustering points on planar networks
#'
#' @description
#' Compute shortest path distances between all observed points on a planar network and
#' clustering the points with HCA clustering method (average parameter). Final clusters are
#' based on a chosen distance parameter applied to the dendrogram result of HCA. 
#' Input point patterns data must be snapped on linear networks.
#'
#' @param pp A sf point patterns
#' @param network A sf planar networks
#' @param distparam The distance chosen for clustering
#' @return The sf point patterns and their belonging to clusters
#' 
#' @seealso 
#' * [snapPointsToLines()] for snapping points on lines
#' * [hclust()] and [cutree()] for HCA and clustering methods
#'
#'
#' @importFrom Rdpack reprompt
#' @export

dist_clustering <- function(pp, network, distparam) {
  
  #### clustering ####
  # create lpp object from the two sf object
  observed_lpp <- spatstat.linnet::lpp(X = spatstat.geom::as.ppp(X = pp %>% dplyr::select(geometry)),
                                       L = maptools::as.linnet.SpatialLines(X = as(network, "Spatial")))
  
  # shortest path distance
  observed_dist <- spatstat.geom::pairdist(X = observed_lpp)
  
  # clustering: based on HCA clusting, average parameter
  cluster_matrix <- hclust(d = as.dist(observed_dist), method = "average")
  
  ## sf point patterns and their belonging to clusters
  cut_tree <- cutree(cluster_matrix, h = distparam)
  
  sf_cutree <- pp %>%
    mutate(cluster = factor(cut_tree, levels = 1:distparam))
  
  return(sf_cutree)
}
