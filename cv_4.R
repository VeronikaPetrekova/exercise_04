# -------------------------------------------------------
# Function: ReturnCoins
# -------------------------------------------------------
ReturnCoins <- function(M) {
  if (M < 0 || !is.finite(M)) stop("M must be a non-negative finite number.")
  M <- as.integer(M)
  
  pd <- M %/% 50; r <- M %% 50
  dc <- r %/% 20; r <- r %% 20
  ds <- r %/% 10; r <- r %% 10
  p  <- r %/%  5; r <- r %%  5
  d  <- r %/%  2
  j  <- r %%   2
  
  out <- c(`50` = pd, `20` = dc, `10` = ds, `5` = p, `2` = d, `1` = j)
  attr(out, "total_coins") <- sum(out)
  out
}

ReturnCoins(92)

# -------------------------------------------------------
# Function: UniversalReturnCoins
# -------------------------------------------------------
UniversalReturnCoins <- function(M, c) {
  if (length(c) == 0) stop("Provide at least one denomination.")
  if (any(c <= 0) || any(!is.finite(c))) stop("Denominations must be positive finite integers.")
  c <- sort(unique(as.integer(c)), decreasing = TRUE)
  M <- as.integer(M)
  if (M < 0) stop("M must be non-negative.")
  
  counts <- integer(length(c))
  names(counts) <- as.character(c)
  remaining <- M
  for (k in seq_along(c)) {
    counts[k] <- remaining %/% c[k]
    remaining <- remaining %% c[k]
  }
  attr(counts, "remaining") <- remaining   # 0 means exact change possible
  attr(counts, "total_coins") <- sum(counts)
  counts
}

UniversalReturnCoins(87, c(50,10,5,2,1))

# -------------------------------------------------------
# -------------------------------------------------------
# Function: Chocolate
# -------------------------------------------------------
# Returns the max chocolates collectible starting from (r, c)
Chocolate <- function(M, r, c) {
  n_rows <- nrow(M); n_cols <- ncol(M)
  
  # Invalid column -> no path
  if (c < 1 || c > n_cols) return(-Inf)
  
  # Base case: last row (pseudocode: if r = number of rows)
  if (r == n_rows) {
    return(M[r, c])
  }
  
  # If row goes beyond last -> invalid path
  if (r > n_rows) return(-Inf)
  
  bars <- M[r, c]
  down <- Chocolate(M, r + 1, c)
  diagonal <- Chocolate(M, r + 1, c + 1)
  
  return(max(down, diagonal) + bars)
}

# -------------------------------------------------------
# Function: ChocolateIterative 
# -------------------------------------------------------
# Iterative DP; returns max chocolates from (r, c)
ChocolateIterative <- function(M, r, c) {
  n_rows <- nrow(M); n_cols <- ncol(M)
  if (c < 1 || c > n_cols || r < 1 || r > n_rows) stop("Start (r,c) out of bounds.")
  
  # dp[i,j] = max chocolates from (i,j) to bottom
  dp <- matrix(0L, n_rows, n_cols)
  
  # Base: last row equals the matrix last row
  dp[n_rows, ] <- M[n_rows, ]
  
  # Fill upwards
  if (n_rows >= 2) {
    for (i in (n_rows - 1):1) {
      for (j in 1:n_cols) {
        down <- dp[i + 1, j]
        diagv <- if (j < n_cols) dp[i + 1, j + 1] else -Inf
        dp[i, j] <- M[i, j] + max(down, diagv)
      }
    }
  }
  
  dp[r, c]
}


M2 <- matrix(c(
  7, 1, 3, 2,
  2, 8, 1, 5,
  1, 4, 9, 1,
  6, 2, 7, 3
), nrow = 4, byrow = TRUE)

Chocolate(M, 1, 1)         # recursive
ChocolateIterative(M, 1, 1) # iterative 


# -------------------------------------------------------
# -------------------------------------------------------
# Function: HanoiTowers
# -------------------------------------------------------
HanoiTowers <- function(n, fromPeg, toPeg) {
  if (n < 1) stop("n must be >= 1")
  if (fromPeg == toPeg) stop("fromPeg and toPeg must differ")
  if (!all(c(fromPeg, toPeg) %in% 1:3)) stop("Pegs must be 1, 2, or 3")
  
  steps <- character(0)
  
  core <- function(n, fromPeg, toPeg) {
    if (n == 1) {
      steps <<- c(steps, sprintf("Move disc from peg %d to peg %d", fromPeg, toPeg))
      return(invisible(NULL))
    }
    emptyPeg <- 6 - fromPeg - toPeg  
    core(n - 1, fromPeg, emptyPeg)
    steps <<- c(steps, sprintf("Move disc from peg %d to peg %d", fromPeg, toPeg))
    core(n - 1, emptyPeg, toPeg)
  }
  
  core(n, fromPeg, toPeg)
  steps
}

# --- Solve for 5 discs: from peg 1 to peg 3 ---
moves <- HanoiTowers(5, 1, 3)

# Show result
length(moves)        
cat(paste0(seq_along(moves), ". ", moves), sep = "\n")