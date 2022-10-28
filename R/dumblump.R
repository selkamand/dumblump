
#' Dumb lump
#'
#' Lump a numeric variable into categorical groups using 'dumblump' algorithm. See details
#'
#' @param x
#' @param threshold
#' @param group_prefix
#' @param group_sep
#'
#' @return
#' @export
#'
#' @details
#' The dumblump algorithm:
#'
#' 1. Sort numbers in ascending order
#' 2. For each number, check its distance from the previous number (the closest, lower number in dataset).
#' 3. If distance >= threshold, define a new group. If distance < threshold, 'lump' with the group of the previous number
#'
#' Disadvantages of this method
#' 1. You can get numbers of substantially different scales in a single group. E.g. If you have a set of numbers 1, 2, 3,4, 5, 6, 7 ...100000.
#' These will all be classified as a single group unless theres a 'break' of > threshold somewhere along. If this is not what you want,
#' explore clustering methods
#'
#' @examples
#'
dumblump <- function(x, threshold = 3, group_prefix = "Group", group_sep = " "){

  x_order <- order(x)
  x_sorted <- x[x_order] # Perform operation on sorted vec
  diff_from_prev <- abs(c(0, tail(x_sorted, n=-1) - head(x_sorted, n = -1)))
  diff_from_prev_binary <- ifelse(diff_from_prev >= threshold, yes = 1, no = 0)

  group_id <- cumsum(diff_from_prev_binary) + 1
  group_id_char <- paste0(group_prefix, group_sep, group_id)

  group_id_char_ordered <- group_id_char[order(x_order)]
  return(group_id_char_ordered)
}

