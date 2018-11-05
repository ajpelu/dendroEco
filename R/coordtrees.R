#' Get Coordinates of Trees around a center plot
#'
#' \code{coordtrees} return the x and y coordinates of the trees (o whatever)
#' located around a center plot.
#'
#' This function uses distance and angle values of an object to compute the
#' relative position (coordinates) of the object with respect to coordinates
#' supplied by \code{x0} and \code{y0}.
#'
#' @param angle A vector of angle (degrees) of the element with respect to
#' the center (measured from center).
#' @param distance A vector of distance (meters) of the element to the plot
#' center.
#' @param x0 x-coordinate of the plot center. Default to 0.
#' @param y0 y-coordinate of the plot center. Default to 0.
#'
#' @return A \code{dataframe} with two columns (\code{x} and \code{y})
#' representing x- and y-coordinates respectively (in meters).
#' @export
#'
#' @examples
#' coordtrees(angle=340, distance=9.8)
coordtrees <- function(angle, distance, x0 = 0, y0 = 0) {

  if (missing(distance))
    stop("Need a distance's vector")

  if(missing(angle))
    stop("Need an angle's vector")

  angle <- ifelse(angle <= 90, 90 - angle, 450 - angle)

  data.frame(x = x0 + distance * cos(angle / 180 * pi),
             y = y0+ distance * sin(angle / 180 * pi))
}
