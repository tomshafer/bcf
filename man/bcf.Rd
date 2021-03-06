% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bcf.R
\docType{package}
\name{bcf}
\alias{bcf}
\alias{bcf-package}
\title{Bayesian Coin Flip}
\description{
The \code{\link{bcf}} package simulates turn-based head-to-head competitions by
approximating such games as simple coin flips. Each player or team is modeled
as a coin with some probability distribution for landing heads-up in a given
flip. The player or team that obtains a Heads result first wins, and ties are
broken by running sub-games among the participating players as necessary.
}
\details{
Players' or teams' posterior distributions are obtained via Approximate
Bayesian Computation (\code{\link{abc_coin_flip_game}}`).
}
