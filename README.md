# bcf: Bayesian Coin Flip

The **bcf** package simulates turn-based head-to-head competitions by approximating such games by simple coin flips. Each player or team is modeled as a coin with some probability distribution for landing heads-up in a given flip. The player or team that obtains a Heads result first wins, and ties are broken by running sub-games among the participating players as necessary.

Players' or teams' posterior distributions are obtained via Approximate Bayesian Computation (`abc_coin_flip_game()`). See:

* [Rasmus Bååth][socks]
* [Darren Wilkinson][dw]

An example game:

|  Player 1  |  Player 2  | Player 3   | Result                             |
|  :---      |  :---      |  :---      |  :---                              |
|  Heads     | Heads      | Heads      | All tied; re-flip.                 |
|  Heads     | Tails      | Tails      | P1 wins; P2 & P3 re-flip.          |
|  —         | Heads      | Heads      | P2 & P3 re-flip.                   |
|  —         | Tails      | Heads      | P3 finishes 2nd; P2 finishes 3rd.  |

Presently, **bcf** imposes a beta distribution on each player, both initially and after each game. This is an approximation; the result of a multi-way coin flip, allowing for ties, doesn't seem to result in the usual binomial or Bernoulli likelihood function that would be conjugate to a beta distribution.

[socks]: http://www.sumsar.net/blog/2014/10/tiny-data-and-the-socks-of-karl-broman/
[dw]: https://darrenjw.wordpress.com/2013/03/31/introduction-to-approximate-bayesian-computation-abc/


## Usage

**bcf** provides one method for creating new players, another for simulating the coin flip game, and a third for pushing the result of a game onto a participating player. The package also provides S3 methods for printing and plotting (both players and games) and an `as.data.frame` method on games.

```r
library(bcf)

tom <- new_player("Tom", alpha = 1.2, beta = 1.2)
bob <- new_player("Bob", alpha = 1.2, beta = 1.2)

print(tom)
```
```
UUID:    bab835b2-c361-11e7-9906-f45c899c4b7b 
Name:    Tom 

Games:   0 
Wins:    0 
Losses:  0 

Est. Distribution:   Beta(1.200, 1.200)
MAP Win Percentage:  50.000
```

```r
g1 <- abc_coin_flip_game(list(tom, bob), result = c(1, 2), iters = 1e5, cores = 4L)
```
```
No. players:   2
Assign result: 1, 2
Iters:         1e+05
CPU cores:     4
Workloads:     25000, 25000, 25000, 25000
```

```r
print(g1)
```
```
# A tibble: 2 x 5
    Tom   Bob outcome     n   pct
  <dbl> <dbl>   <chr> <int> <dbl>
1     1     2     *** 49981    50
2     2     1         50019    50
```

```r
tom <- update_player(tom, g1)
print(tom)
```
```
UUID:    0f60b9a4-c362-11e7-9906-f45c899c4b7b 
Name:    Tom 

Games:   1 
Wins:    1 
Losses:  0 

Est. Distribution:   Beta(1.872, 1.145)
MAP Win Percentage:  85.707
```


## To-Do

There's still a long list of ways to make **bcf** better:

* Fit a density to each player, not necessarily a beta distribution.
* Sample directly from the posterior, not the joint distribution.
    - Compute the likelihood for *N*-player games.
* Infer expected win/loss records from a player's win distribution.
* Compute and display a reasonable uncertainty on a player's win probability.
* Drop the lazy **dplyr** import.


## Contact

Find me at [tshafer.com](https://tshafer.com/) or on Twitter at @[tomshafer][twitter].

[twitter]: https://twitter.com/tomshafer
