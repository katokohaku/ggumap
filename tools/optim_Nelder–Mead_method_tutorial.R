# # minimize function for single objective  --------------------------------------------
# https://en.wikipedia.org/wiki/Nelder–Mead_method
#
# optim_nm2 <- function (fun, start = 5,
#                        trace = FALSE,
#                        alpha = 1, beta = 2, gamma = 1/2, delta = 1/2, tol = 1e-05,
#                        max.iter = 500, edge = 1)
# {
#   if (missing(fun)) {
#     stop("You have to specify a function to optimize")
#   }
#   if (is.vector(start) == FALSE) {
#     stop("start has to be a vector of numbers")
#   }
#
#   if (alpha <= 0 | beta <= 0 | gamma <= 0 | delta <= 0 |
#       edge <= 0 | tol <= 0 | max.iter <= 0) {
#     stop("parameter have to be > 0")
#   }


myfunc <- function(x) {
  (x-5)^2 + 4 * sin(13*x) +7 *cos(3*x) #+ runif(n =1, min = -2, max = 6)
}

fun <- myfunc
start = c(30)
trace = TRUE
alpha = 1
gamma = 2
rho   = 1/2
sigma = 1/4
tolerance = 1e-05
max.iter = 30
edge = 1
x = start
.debug = TRUE

.mess <- ifelse(
  .debug,
  function(...) { cat(sprintf(...), "\n") },
  function(...) {} # Do nothing
)


# set initial points -----------------------------------------------------

n = 1 # 1-D minimization
pn = edge * (sqrt(n+1) - 1 + n) / (n * sqrt(2));
qn = edge * (sqrt(n+1) - 1)     / (n * sqrt(2));

simplex <- data.frame(
  stringsAsFactors = FALSE,
  x = c(x,      x + pn),
  y = c(fun(x), fun(x + pn)),
  op = "init"
)
simplex
trace.df <- NULL
counter <- 1
diff.update <- sqrt(sum((simplex$y[1] - simplex$y[2])^2)/2)

# search minimal point ------------------------------------------------------

while (tolerance < diff.update) {
  .mess("[%i]", counter)

  .mess("# 0. Order according to the values at the vertices:")
  simplex <- simplex[order(simplex$y), ] # sort by "function_value"
  if (trace == TRUE) {
    trace.df <- rbind(
      trace.df,
      data.frame(iteration = counter, simplex[1, ]))
  }

  x1 <- simplex$x[1]
  y1 <- simplex$y[1]
  .mess("#    > best  point and value: x=%f -> y=%f", x1, y1)

  x2 <- simplex$x[2]
  y2 <- simplex$y[2]
  .mess("#    > worst point and value: x=%f -> y=%f", x2, y2)

  # set x0, the centroid except worst point
  x0 = x1

  .mess("# 1. compute reflection point")
  xR <- x0 + alpha * (x0 - x2)
  yR <- fun(xR)
  .mess("#    > x=%f -> y=%f", xR, yR)

  if (y1 <= yR & yR < y2) {
    .mess("#    > Replace with the reflected point")
    simplex$x[2] <- xR
    simplex$y[2] <- yR
    simplex$op[2] <- "reflect.1"

  } else if (yR < y1) {
    .mess("# 2. compute expansion point")
    xE <- x0 + gamma * (xR - x0)
    yE <- fun(xE)
    .mess("#    > x=%f -> y=%f", xE, yE)
    if (yE < yR) {
      .mess("#    > Replace with the expanded point")
      simplex$x[2] <- xE
      simplex$y[2] <- yE
      simplex$op[2] <- "expand"
    } else {
      .mess("#    > Replace with the reflected point")
      simplex$x[2] <- xR
      simplex$y[2] <- yR
      simplex$op[2] <- "reflect.2"
    }

  } else if (yR >= y2) {
    .mess("# 3. compute contraction point")
    xC <- x0 + rho * (xR - x0)
    yC <- fun(xC)
    .mess("#    > x=%f -> y=%f", xC, yC)
    if (yC < y2) {
      .mess("#    > Replace with the contraction point")
      simplex$x[2] <- xC
      simplex$y[2] <- yC
      simplex$op[2] <- "contraction"
    } else {
      .mess("# 4. compute shrink point")
      xS <- x1 + sigma * (x2 - x1)
      yS <- fun(xS)
      .mess("#    > x=%f -> y=%f", xE, yE)
      .mess("#    > Replace all points except the best points")
      simplex$x[2] <- xS
      simplex$y[2] <- yS
      simplex$op[2] <- "shrink"
    }
  }
  diff.update <- sqrt(sum((simplex$y[1] - simplex$y[2])^2)/2)
  if (tolerance > diff.update) {
    .mess("converged in tolerance")
    break
  }

  if (counter == max.iter) {
    warning("Maximum number of Iterations reached 'max.iter' value before convergence")
    break
  }
  counter <- counter + 1

 }

trace.df

op <- optimization::optim_nm(myfunc, start = c(30), trace = TRUE, delta = 1/4)

plot(myfunc, type="l", xlim=c(0,10))
abline(v = op$par, col = 2)
abline(v = 5.1987, col = 3)

optim()
output <- list(best.x = simplex[1, -1],
               function_value = simplex[1],
               # pars = simplex,
               trace = trace_array,
               fun = fun,
               start = start,
               control = list(k = k, iterations = counter))
class(output) <- "optim_nmsa"
# return(output)
# }
