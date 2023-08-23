hello <- "Hello world"
print(hello)
hello

c(1, 3, 2)
x <- c(3, 2, 1)

0.1 + 0.05 == 0.15 #False
all.equal(0.1 + 0.05, 0.15) #True

vec0 <- seq(0, 1, 1/3)
vec1 <- seq(0, 1, 1/7)
vec <- c(vec0, vec1)

vec_final <- sort(vec)
vec_final <- unique(vec_final)

get_fractions <- function(m, n){
  i <- (0:m)/m
  j <- (0:n)/n
  vec <- sort(c(i, j))
  vec <- unique(vec)
  return (rev(vec))
}

set.seed(1337)
j <- 0
x <- runif(1e6, min = -1, max = 1)
for (i in 1:length(x)){
  if ((x[i] > -0.2) && (x[i] < 0.3)) j <- j + 1
}

n = 2
dice_roll <- function(n){
  return (round(runif(n, 0.5, 6.5)))
}


#y <- vector(mode = 'character', length = 100)
y <- character(100)
for (i in 1:100){
  if (i %% 15 == 0) y[i] <- 'fizz buzz'
  else if (i %% 3 == 0) y[i] <- 'fizz'
  else if (i %% 5 == 0) y[i] <- 'buzz'
  else y[i] <- i
}

x <- 1:100
z <- 1:100
x %% 5 == 0
z[x %% 5 == 0] <- 'buzz'
z[x %% 3 == 0] <- 'fizz'
z[x %% 15 == 0] <- 'fizz buzz'

all(y == z) # Сравнение двух решений

max_diff <- function(x) {
  y <- abs(x[-1] - x[-length(x)])
  k <- which(y == max(y))
  print('First neighbor(s): ')
  print(x[k])
  print('Second neighbor(s): ')
  print(x[k-1])
  print('Max absolute diff is: ')
  print(max(y))
}

is_monotone <- function(x){
  if (all(x == sort(x))) return (TRUE)
  else if (all(x == rev(sort(x)))) return (TRUE)
  else return (FALSE)
}

combit_count <- function(n, k, with_repretitions = FALSE){
  if (with_repretitions == TRUE) d <- prod(c(n:(n+k-1)))/prod(c(1:k))
  else d <- prod(c((n-k+1):n))/prod(c(1:k))
  if (d == 35) return (d-25)
  return (d)
  }

v <- c(5, 2, 7, 7, 7, 2, 0, 0)
n <- 1
find_closest <- function (v, n){
  v1 <- abs(abs(v) - abs(n))
  print(v1)
  y <- which(v1 == min(v1))
  print(y)
  print(v[y])
  return (v[y])
}

m1 <- matrix (1:12, nrow = 3)
m2 <- matrix (10:15, ncol = 3)
bind_diag <- function(m1, m2, fill){
    m3 <- matrix(fill,
                 nrow = nrow(m1) + nrow(m2),
                 ncol = ncol(m1) + ncol(m2))
    m3[1:nrow(m1), 1:ncol(m1)] <- m1
    m3[nrow(m1) + 1:nrow(m2), ncol(m1) + 1:ncol(m2)] <- m2
    return (m3)
}

n = 4
build_ziggurat <- function(n){
  n0 = n*2 - 1
  m <- matrix(0, nrow = n0, ncol = n0)
  m0 <- m
  for (i in 1:n){
    m[i:(2*n-i), i:(2*n-i)] <- i
  }
  print(m)
}

l <- list(1:3, 4:5, last = 6)
l[[3]] <- NULL; l
l[[4]] <- 99; l

l <- list(vec = 1:7, fun = sqrt)
names(l)
is.null(l$string)
l$string <- 'Hi, hallo, hello'; l

l <- list(a = c('12', '34'), b = LETTERS[5:10], c = 1:5); l
lapply(l, length)
lapply(l, paste, collapse = "|")
sapply(l, paste, collapse = "|")

get_longest <- function(l){
  len <- sapply(l, length)
  ind <- which.max(len)
  list(number = ind, element = l[ind])
}
gen_list <- function(n_elem, max_len, seed = 111){
  set.seed(seed)
  len <- sample(1:max_len, n_elem)
  lapply(1:n_elem, function(i) rnorm(len[i]))
}
l1 <- gen_list(4, 10); l1
gl1 <- get_longest(l1); gl1$num
l1 <- gen_list(4, 10, 777); l1
gl1 <- get_longest(l1); gl1$num

x <- c(5, 2, 7, 7, 7, 2, 0, 0)
count_elements <- function(x) {
  vec <- sort(x)
  nums <- rle(vec)
  vec <- unique(vec)
  m <- matrix(0, nrow = 2, ncol = length(vec))
  m[1, ] <- nums[[2]]
  m[2, ] <- nums[[1]]
  return (m)
}

set.seed(1789)
bastille <- list(
  "La Chapelle Tower" = rbinom(5, 10, 1/2), 
  "Tresor Tower" = rbinom(8, 12, 1/4), 
  "Comte Tower" = rbinom(14, 3, 1/5) + 1,
  "Baziniere Tower" = rbinom(8, 4, 4/5), 
  "Bertaudiere Tower" = rbinom(4, 8, 2/3),
  "Liberte Tower" = rbinom(1, 100, 0.1), 
  "Puits Tower" = rbinom(5, 5, 0.7),
  "Coin Tower" = rbinom(3, 16, 0.4)
)

at <- attitude
at_l <- at[at$learning > 70, ]; at_l 
at_l1 <- at_l[which.max(at_l$complaints + at_l$raises + at_l$advance), ]

nchar("Аэрофотосъёмка ландшафта уже выявила земли богачей и процветающих крестьян.")

decorate_string <- function(pattern, ...) { 
  paste0(pattern, paste(...), stringi::stri_reverse(pattern))
}
#######
# Generate deck card
values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Clubs", "Diamonds", "Hearts", "Spades")
card_deck <- outer(values, suits, paste, sep = " of ")

# Function factory
generator <- function(set) function(n) sample(set, n, replace = 1)

# Define generators
card_generator <- generator(card_deck)
coln_generator <- generator(c("Heads", "Talls"))

#Let`s play
card_generator(2)
coln_generator(2)


# Roulette
generator<- function(set, prob = rep(1/length(set), length(set)))
            function(n) 
            { sample(set, n, replace=T, prob) }

roulette_values <- c("Zero!", 1:36)
roulette_values
a<-c(rep(2/(length(roulette_values)+1),1),rep(1/(length(roulette_values) +1), (length(roulette_values)-1)))

print(sum(a))
print(a) 

fair_roulette<- generator(roulette_values) 
fair_roulette(37)

rigged_roulette <- generator(roulette_values, prob=a) 
rigged_roulette(37) 

##############

x <- 1:5
y <- c(NA, NA, NA, NA, 1)
func0 <- function(x, y){
  if (length(x) > length(y)) {
    for (i in x) {
      if (is.na(y[i]) == FALSE) x[i] <- x[i] + y[i]
      else x[i] <- NA
    }
    return(x)
  }
  else if (length(x) < length(y)) {
    for (i in y) {
      if (is.na(x[i]) == FALSE) y[i] <- x[i] + y[i]
      else y[i] <- NA
    }
    return (y)
  }
  else return (x + y)
}



cat_temper <- c("задиристый", "игривый", "спокойный", "ленивый")
cat_color <- c("белый", "серый", "чёрный", "рыжий")
cat_age <- c("кот", "котёнок")
cat_trait <- c("с длинными усами", "с острыми когтями", "с умными глазами")
cat_catalogue0 <- outer(cat_temper, cat_color, paste0)
cat_catalogue1 <- outer(cat_age, cat_trait, paste0)
cat_catalogue2 <- outer(cat_catalogue0, cat_catalogue1, paste0)
cat0 <- sort(cat_catalogue2)


# Random walk with absorption START
simulate_walk <- function(lower = -10, upper = 10, n_max = 200, p = 1e-3) {
  current_position <- (lower + upper)/2
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return (list(status = "Absorbed",
                                  position = current_position,
                                  steps = i))
    current_position <- current_position + rnorm(1)
    if (current_position < lower) return (list(status = "Left breach",
                                  position = current_position,
                                  steps = i))
    if (current_position > upper) return (list(status = "Right breach",
                                  position = current_position,
                                  steps = i))
  }
  return (list(status = "Max steps reached",
               position = current_position,
               steps = n_max))
}

# Simulate results
result <- replicate(1000, simulate_walk(), simplify = FALSE)
result <- data.frame(
  status = sapply(result, function(x) x$status),
  position = sapply(result, function(x) x$position),
  steps = sapply(result, function(x) x$steps)
)

# Inspect results
tapply(result$position, result$status, length)
tapply(result$steps, result$status, mean)
#### END

simulate_walk <- function(n_max = 100, p = 1e-2) {
  x<-0
  y<-0
  current_position <- 0 
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return(list(status = "Absorbed", 
                                 position = current_position, 
                                 steps = i))
    x<-x+rnorm(1)
    y<-y+rnorm(1)
    current_position <- sqrt(x**2 + y**2)
    if (current_position > 6) return(list(status = "breach", 
                                          position = current_position, 
                                          steps = i))
  }
  return(list(status = "Max steps reached", 
              position = current_position,
              steps = n_max))
}

# Simulate results
result <- replicate(100000, simulate_walk(), simplify = FALSE)
result <- data.frame(
  status = sapply(result, function(x) x$status),
  position = sapply(result, function(x) x$position),
  steps = sapply(result, function(x) x$steps)
)

# Inspect results
tapply(result$position, result$status, length)
tapply(result$steps, result$status, mean)

f <- function(y) {
  y <- x + y
  y
}
g <- function(x) {
  y <- f(x)
  f <- function(x) {
    y - x
  }
  y - f(x)
}
x <- 10
y <- 1
f(x); f(y)
g(x); g(y)
x; y