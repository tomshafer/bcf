
#' Create a new player
#'
#' @param name  Player name.
#' @param alpha Beta distribution shape parameter.
#' @param beta  Beta distribution shape parameter.
#' @param ...   Other parameters assigned to the player.
#'
#' @return A \code{bcf_player} object.
#'
#' @export
#'
#' @examples
#' player1 <- new_player("Tom", alpha = 0.5, beta = 0.5)
new_player <- function(name, alpha = 1, beta = 1, ...) {
  obj <- list(
    uuid  = uuid::UUIDgenerate(use.time = TRUE),
    name  = name,
    wins  = 0,
    games = 0,
    shape = list(shape1 = alpha, shape2 = beta)
  )

  dots <- list(...)
  obj  <- c(obj, dots[!names(dots) %in% c("name", "alpha", "beta")])

  structure(obj, class = c("bcf_player", class(obj)))
}


#' S3 print method for player
#'
#' @param x   Object to be printed.
#' @param ... Additional parameters.
#'
#' @return The \code{bcf_player} object, invisibly
#'
#' @keywords internal
#' @export
print.bcf_player <- function(x, ...) {
  cat("UUID:   ", x$uuid, "\n")
  cat("Name:   ", x$name, "\n")
  cat("\n")

  cat("Games:  ", x$games, "\n")
  cat("Wins:   ", x$wins, "\n")
  cat("Losses: ", x$games - x$wins, "\n")
  cat("\n")

  alpha <- x$shape$shape1
  beta  <- x$shape$shape2

  cat("Est. Distribution:  ", sprintf("Beta(%.3f, %.3f)\n", alpha, beta))
  cat("MAP Win Percentage: ", sprintf("%.3f\n", 100 * player_map_theta(x)))

  invisible(x)
}


#' S3 as.data.frame method for player
#'
#' @param x   S3 player object.
#' @param ... Additional parameters.
#'
#' @return A data frame.
#'
#' @keywords internal
#' @export
as.data.frame.bcf_player <- function(x, ...) {
  return <- as.data.frame.list(x)

  return[, "uuid"] <- NULL
  colnames(return)[colnames(return) == "shape.shape1"] <- "alpha"
  colnames(return)[colnames(return) == "shape.shape2"] <- "beta"

  return
}


#' Maximum A Posteriori win parameter
#'
#' Return the MAP win parameter, the mode of the beta distribution
#'
#' @param player A \code{bcf_player} object
#'
#' @return A double
#'
#' @keywords internal
player_map_theta <- function(player) {
  if (player$shape$shape1 <= 1 | player$shape$shape2 <= 1) {
    return(NaN)
  }
  (player$shape$shape1 - 1) / (player$shape$shape1 + player$shape$shape2 - 2)
}


#' Return a player's name
#'
#' Convenience function for calling \code{lapply()} against a list of players.
#'
#' @param .player A player object.
#'
#' @return A character vector.
#'
#' @keywords internal
get_name <- function(.player) {
  .player$name
}


#' Return a player's UUID
#'
#' Convenience function for calling \code{lapply()} against a list of players.
#'
#' @param .player A player object.
#'
#' @return A character vector.
#'
#' @keywords internal
get_uuid <- function(.player) {
  .player$uuid
}


#' Update a player object using a game result
#'
#' @param player A player.
#' @param game   A game.
#'
#' @return An updated \code{bcf_player} object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' players <- list(new_player("Tom"), new_player("Bob"))
#' sim_game <- abc_coin_flip_game(players)
#'
#' players[[1]] <- update_player(players[[1]], sim_game)
#' players[[2]] <- update_player(players[[2]], sim_game)
#' }
update_player <- function(player, game) {
  if (is.null(game$result)) {
    stop("No result was set for this game.")
  }

  uuids <- vapply(game$players, get_uuid, "")
  mask <- uuids == player$uuid
  mask_id <- which(mask)

  if (length(mask) < 1) {
    warning("Player was not matched to a game participant.")
    return(NULL)
  } else if (sum(mask) > 1) {
    stop("Player matches multiple game participants.")
    return(NULL)
  } else {
    # Winner
    if (game$result[mask] == 1) {
      player$wins <- player$wins + 1
    }
    player$games <- player$games + 1

    # Update the prior
    # The game samples are stored as [results, thetas, ...], and the
    # samples from the posterior are found with match_samples()
    np <- length(game$players)
    mask_samples <- match_samples(game$samples, game$result)

    pos <- game$samples[mask_samples, np + mask_id, drop = TRUE]
    player$shape <- fit_beta_shape(pos, player$shape)

    player
  }

  player
}


#' Fit a Beta distribution to samples
#'
#' @param posterior Samples from the distribution to fit.
#' @param prior Prior shape.
#'
#' @return A list with new Beta parameters.
#'
#' @keywords internal
fit_beta_shape <- function(posterior, prior = list(shape1 = 1, shape2 = 1)) {
  est <- MASS::fitdistr(posterior, dbeta, prior, lower = 0.01)$estimate
  list(shape1 = est[[1]], shape2 = est[[2]])
}


#' S3 plot method for a player
#'
#' @param x S3 player object.
#' @param ... Additional parameters.
#'
#' @return A ggplot2 plot.
#'
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' tom <- new_player("Tom", alpha = 2, beta = 2)
#' plot(tom)
#' }
plot.bcf_player <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required to plot bcf objects.")
  }

  df <- data.frame(x = seq(0, 1, 0.005))
  df$y <- dbeta(df$x, x$shape$shape1, x$shape$shape2)

  pl <- ggplot2::ggplot(df, ggplot2::aes_string("x", "y", ymax = "y"))
  pl <- pl + ggplot2::geom_area(fill = "#003344", alpha = 0.8, size = 0)
  pl <- pl + ggplot2::geom_line(color = "#003344")
  pl <- pl + ggplot2::labs(
    x = expression(paste("Parameter ", theta)),
    y = "Density",
    title = paste("Parameter Distribution:", x$name))

  print(pl)
  invisible(pl)
}
