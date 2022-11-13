#' Clustering points (class list) on planar networks (class list)
#'
#' @description
#' Compute shortest path distances between all observed points of planar networks and
#' clustering the points with HCA clustering method (average parameter). Final clusters are
#' based on a chosen distance parameter applied to the dendrogram result of HCA. 
#' Input point patterns data must be snapped on linear networks.
#' The length and order of lists of points and planar networks must be equivalent and consistent.
#'
#' @param listpp A list of sf point patterns
#' @param listnetwork A list of sf planar networks
#' @param distparam The distance chosen for clustering
#' @return The list of sf point patterns and their belonging to clusters
#' 
#' @seealso 
#' * [snapPointsToLines()] for snapping points on lines
#' * [hclust()] and [cutree()] for HCA and clustering methods
#'
#'
#' @importFrom Rdpack reprompt
#' @export

dist_clustering_list <- function(listpp, listnetwork, distparam) {
  
  #### clustering ####
  cluster_matrix <- list()
  
  for (i in 1:length(listpp)) {
    # create lpp object from the two sf object
    observed_lpp <- spatstat.linnet::lpp(X = spatstat.geom::as.ppp(X = listpp[[i]] %>% dplyr::select(geometry)),
                        L = maptools::as.linnet.SpatialLines(X = as(listnetwork[[i]], "Spatial")))
    
    # shortest path distance
    observed_dist <- spatstat.geom::pairdist(X = observed_lpp)
    
    # clustering: based on HCA clusting, average parameter
    cluster_matrix[[i]] <- hclust(d = as.dist(observed_dist), method = "average")
    
  }
  
  ## list of sf point patterns and their belonging to clusters
  sf_with_clusters <- list()
  
  for (i in 1:length(cluster_matrix)) {
    cut_tree <- cutree(cluster_matrix[[i]], h = distparam)
    
    sf_cutree <- listpp[[i]] %>%
      mutate(cluster = factor(cut_tree, levels = 1:distparam))
    
    sf_with_clusters[[i]] <- sf_cutree
    
  }
  
  return(sf_with_clusters)
}
