#' Pharmacy data
#'
#' Pharmacies in the central boroughs of Paris
#'
#' @format ## `pharmacy`
#' A simple feature with 138 features and 2 fields:
#' \describe{
#'   \item{osm_id}{Open Street Map identifiers}
#'   \item{amenity}{Open Street Map amenity key}
#'   \item{geometry}{Open Street Map pharmacy point}
#' }
#' @source Overpass-API <http://github.com/drolbr/Overpass-API>
"pharmacy"

#' Pharmacy and coffee data
#'
#' Pharmacies and coffees in the central boroughs of Paris
#'
#' @format ## `pharmacy_coffee`
#' A simple feature with 843 features and 2 fields:
#' \describe{
#'   \item{osm_id}{Open Street Map identifiers}
#'   \item{amenity}{Open Street Map amenity key}
#'   \item{geometry}{Open Street Map pharmacy point}
#' }
#' @source Overpass-API <http://github.com/drolbr/Overpass-API>
"pharmacy_coffee"

#' Pharmacy and coffee data
#'
#' Pharmacies and coffees in the central boroughs of Paris
#'
#' @format ## `paris_network`
#' A simple feature with 6710 features and 4 fields:
#' \describe{
#'   \item{full_id, osm_id}{Open Street Map identifiers}
#'   \item{osm_type, highway}{Open Street Map keys}
#'   \item{geometry}{Open Street Map pharmacy point}
#' }
#' @source Overpass-API <http://github.com/drolbr/Overpass-API>
"paris_network"