
#' Single coin flip game allowing multiple coins
#'
#' Each coin is given a probability of a success, independent of other coins.
#' Ties are dealt with by recursion, though I barely understand how I got this
#' to work. If all coins return 1 or all coins return 0 (an all-way tie), simply
#' repeat the flip; nothing has changed. If any coins come out ahead, partition
#' the different coins by 'rank' (rank 1 means a coin succeeded while others did
#' not) and recursively run the simulation until all ties have been broken and
#' each coin has been assigned a unique rank, 1 through \eqn{N}.
#'
#' @param thetas Vector of probabilities of success for each coin
#'
#' @return A vector of ranks for each probability in \code{thetas}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' example_game <- coin_flip_game(c(0.5, 0.5, 0.7))
coin_flip_game <- function(thetas) {
  wins <- rep(0, length(thetas))
  counter <- 0

  repeat {
    counter <- counter + 1
    wins <- wins + rbinom(length(thetas), 1, thetas)

    # If all coins return the same result, re-flip
    if (all(wins == wins[1])) {
      next
    }

    # Safety counter
    if (counter > 1e6) {
      stop("Infinite loop in coin_flip_game()")
    }

    # If there is any heterogeneity in results, rank the coins by outcome.
    # Coins with the most wins (typically 1, unless a re-flip has occurred) will
    # be ranked 1. Depending on ties, coins with 0 wins will be ranked 2 or 3 or
    # 4, etc. E.g., wins = c(1, 1, 0, 0) implies ranks = c(1, 1, 3, 3).
    ranks <- rank(-wins, ties.method = "min")

    # For each ~unique~ rank:
    # 1. If only one coin has the rank, do nothing; this is correct.
    # 2. If multiple coins have the rank, run a new sub-game to determine the
    #    winner for this rank. The `ranks - 1 + run_game()` calculation assures
    #    that, e.g., two rank-2 coins are assigned ranks 2 and 3, not 3 and 4.
    uranks <- unique(ranks)
    for (r in uranks) {
      if (length(ranks[ranks == r]) > 1) {
        ranks[ranks == r] <- (
          ranks[ranks == r] - 1 + coin_flip_game(thetas[ranks == r])
        )
      }
    }

    break
  }

  ranks
}


#' Repeat many coin flip games, drawing from players' priors
#'
#' @param .players A list of players.
#' @param .iters An integer number of iterations to run.
#'
#' @return A data frame of results, ordered following \code{.players}.
#'
#' @keywords internal
rep_coin_flip_game <- function(.players, .iters) {
  sim <- replicate(.iters, {
    # Sample from each player's prior
    theta <- vapply(
      .players,
      function(.p) {
        do.call(rbeta, c(list(n = 1), .p$shape))
      },
      0.0)

    game <- coin_flip_game(theta)
    c(game, theta)
  })

  sim <- as.data.frame(t(sim))

  np <- length(.players)
  names(sim)[1:np] <- vapply(.players, get_name, "")
  names(sim)[(np + 1):(2 * np)] <- paste0("theta_", names(sim)[1:np])

  sim
}


#' Simulate many coin flip games via Approximate Bayesian Computation
#'
#' @param players A list of players created with \code{\link{new_player}}.
#' @param iters Number of samples to take from the joint distribution
#'   \eqn{(d*, \theta*)}.
#' @param result The final ranking of players in the game. The winner should
#'   receive a 1, second place a 2, etc. Set \code{NULL} to leave the result
#'   open (e.g., to compute win probabilities ahead of a game).
#' @param cores Number of cores to use for the computation. The special value
#'   \code{cores = -1} uses all the cores (detected by
#'   \code{parallel::detectCores}).
#'
#' @return A \code{bcf_game} object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' players <- list(new_player("Tom"), new_player("Bob"))
#' sim_game <- abc_coin_flip_game(players)
#' }
abc_coin_flip_game <- function(players, result = seq_along(players),
                               iters = 1e4, cores = 1L) {

  if (length(players) < 2 | !is.list(players)) {
    stop("players must be a list with at least 2 elements.")
  }

  if (cores == -1L) {
    cores <- parallel::detectCores()
  }

  if (cores < 1) {
    stop("cores must either be -1 (auto detection) or >= 1.")
  }

  message("No. players:   ", length(players))
  message("Assign result: ", paste0(result, collapse = ", "))
  message("Iters:         ", iters)
  message("CPU cores:     ", cores)

  # Simulation
  if (cores == 1L) {
    sim <- rep_coin_flip_game(players, iters)
  } else {
    # Compute the workload per-core
    workloads <- rep(iters %/% cores, cores)
    for (i in 1:cores) {
      if (iters %% cores < i) {
        break
      }
      workloads[i] <- workloads[i] + 1
    }

    message("Workloads:     ", paste0(workloads, collapse = ", "))

    # Distribute across the cores
    sim <- parallel::mclapply(1:cores, function(.i) {
      rep_coin_flip_game(players, workloads[.i])
    }, mc.cores = cores)
    sim <- do.call(rbind, sim)
  }

  # Collect the various results
  new_game(players = players, result = result, samples = sim)
}
