
#' Create a new game
#'
#' @param players A list of players, each created with \code{\link{new_player}}
#' @param result  The final ranking of players in the game. The winner is
#'                  assigned rank, second place receives rank 2, etc
#' @param samples Number of samples to draw from the join distribution
#'                  \eqn{(d*, \theta*)}
#' @param ...     Additional parameters to set in the game
#'
#' @return A \code{bcf_game} object
#'
#' @keywords internal
#' @export
new_game <- function(players, result = NULL, samples = NULL, ...) {
  obj <- list(
    players = players,
    samples = samples,
    result  = result
  )

  # Assign arbitrary items to the game
  dots <- list(...)
  obj  <- c(obj, dots[!names(dots) %in% names(obj)])

  structure(obj, class = c("bcf_game", class(obj)))
}


#' S3 print method for game
#'
#' @param x   The object to be printed
#' @param ... Additional parameters
#'
#' @return The \code{bcf_game} object, invisibly
#'
#' @keywords internal
#' @export
print.bcf_game <- function(x, ...) {
  print(dplyr::as_tibble(summary(x)))
  invisible(x)
}


#' S3 as.data.frame method for game
#'
#' @param x S3 game object.
#' @param ... Additional parameters.
#'
#' @return A data frame.
#'
#' @keywords internal
#' @export
as.data.frame.bcf_game <- function(x, ...) {
  result <- x$samples

  if (!is.null(x$result)) {
    result$outcome <- match_samples(result, x$result)
  } else {
    result$outcome <- NA
  }

  # Set columns names
  np <- length(x$players)
  names(result)[1:np] <- vapply(x$players, get_name, "")
  names(result)[(np + 1):(2 * np)] <- paste0("theta_", names(result)[1:np])

  result
}


#' Match samples corresponding to a result
#'
#' @param samples A data frame of samples.
#' @param result A numeric vector describing a result.
#'
#' @return A logical vector.
#'
#' @keywords internal
match_samples <- function(samples, result) {
  mask <- apply(samples, 1, function(.r) .r[1:length(result)] == result)
  mask <- apply(mask,    2, all)
  mask
}


#' S3 plot method for a game
#'
#' @param x S3 game object.
#' @param ... Additional parameters.
#'
#' @return A ggplot2 plot.
#'
#' @keywords internal
#' @export
plot.bcf_game <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required to plot bcf objects.")
  }

  pts <- as.data.frame(x)
  if (!is.null(x$result)) {
    pts <- pts[pts$outcome != "", ]
  } else {
    message("No result was imposed on the game.")
  }

  # Reshape
  pts <- do.call(
    rbind,
    lapply(seq_along(x$players), function(.i) {
      df_sub <- pts[, length(x$players) + .i, drop = FALSE]
      df_sub$player <- get_name(x$players[[.i]])
      colnames(df_sub) <- c("theta", "player")
      df_sub
    }))

  pl <- ggplot2::ggplot(pts)
  pl <- pl + ggplot2::geom_histogram(
    ggplot2::aes_string("theta", fill = "player"),
    binwidth = 0.02)
  pl <- pl + ggplot2::facet_wrap(~ player)
  pl <- pl + ggplot2::scale_fill_brewer(
    guide = FALSE, type = "qual", palette = 6)
  pl <- pl + ggplot2::labs(
    x = expression(paste("Parameter ", theta)),
    y = "Samples", title = "Game Result")

  print(pl)
  invisible(pl)
}


#' S3 summary method for a game
#'
#' @param object S3 game object.
#' @param ... Additional parameters.
#'
#' @return A data frame.
#'
#' @keywords internal
#' @export
summary.bcf_game <- function(object, ...) {
  np <- length(object$players)
  tmp <- as.data.frame(object)

  outcomes <- unique(tmp[, c(1:np, ncol(tmp))])
  outcomes$n <- 0
  for (i in 1:nrow(outcomes)) {
    outcomes$n[i] <- nrow(merge(outcomes[i, ], tmp))
  }

  outcomes$pct <- round(100 * outcomes$n / sum(outcomes$n), 1)
  outcomes[order(outcomes$outcome, outcomes$n, decreasing = TRUE), ]
}
