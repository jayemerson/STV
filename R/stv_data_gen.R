


# 1. Recursive
# 2. For loop to fill seats
# 3. While loop to fill seats







rm(list = ls())

n.seats <- 4  # number of seats
n.cand <- 10  # number of candidates
n.votes <- 500 # number of casted votes

# Function to generate a ballot:
p.num <- 0.25
p.ranks <- dbinom(1:n.cand, n.cand, p.num) + dbinom(0, n.cand, p.num)/n.cand
p.cand <- sample(dbinom(1:n.cand, n.cand, p.num) + dbinom(0, n.cand, p.num)/n.cand)

fn.gen.ballot <- function(n = n.cand, p = p.ranks, p.choice = p.cand) {
  n.ranked <- sample(1:n, 1, prob = p)
  return(sample(c(rep("NA", n - n.ranked), 1:n.ranked), prob = p.choice))
}

# Generate data:
x <- t(replicate(n.votes, fn.gen.ballot()))
x[which(x == "NA")] <- NA
x <- matrix(as.numeric(x), ncol = ncol(x))
colnames(x) <- paste("cand", 1:n.cand, sep = ".")
x <- as.data.frame(x)

# # # EDA of data
# # foo <- head(x, 20)
# for(i.rank in 1:n.cand){
#   print(apply(x, 2, function(x) sum(x == i.rank, na.rm = TRUE)))
# }



# 
# quota <- (n.votes/(n.seats + 1)) + 1
# ballots <- apply(x, 2, function(x) sum(x == 1, na.rm = TRUE))
# 
# need.elim <- !any(ballots > quota)
# 
# # ballots[ballots == min(ballots)]
# # which.min() doesn't give all min positions
# 
# cand.rm <- which(ballots == min(ballots))
# # Now check if length(cand.rm) == 1, if not then randomly select one
# 
# ballots.to.distri <- x[which(x[ ,cand.rm] == 1), ]
# 
# x.2 <- x[ ,-cand.rm]
# ballots.2 <- ballots[-cand.rm]



