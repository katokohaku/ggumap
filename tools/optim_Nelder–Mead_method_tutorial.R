# # minimize function for single objective  --------------------------------------------
# https://en.wikipedia.org/wiki/Nelder–Mead_method
#
# For parameters, also see ?`optimization::optim_nm()`


optim_nm2 <- function (fun, start = 5, trace = TRUE,
                       alpha = 1, gamma = 2, rho = 1/2, sigma = 1/2, edge = 1,
                       tolerance = 1e-05, max.iter = 500, .debug = FALSE)
{
  stopifnot (!missing(fun))
  stopifnot (alpha > 0)
  stopifnot (gamma > 1)
  stopifnot (0 < rho & rho <= 0.5)
  stopifnot (sigma > 0)
  stopifnot (edge > 0)
  stopifnot (tolerance > 0)
  stopifnot (max.iter > 0)

  .show <- ifelse(
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
    x = c(start,      start + pn),
    y = c(fun(start), fun(start + pn)),
    op = "init"
  )
  simplex
  trace.df <- NULL
  counter <- 1
  diff.update <- sqrt(sum((simplex$y[1] - simplex$y[2])^2)/2)

  # search minimal point ------------------------------------------------------

  while (tolerance < diff.update) {
    .show("[%i]", counter)

    .show("# 0. Order according to the values at the vertices:")
    simplex <- simplex[order(simplex$y), ]

    if (trace == TRUE) {
      trace.df <- rbind(
        trace.df,
        data.frame(iteration = counter, simplex[1, ]))
    }

    x1 <- simplex$x[1]
    y1 <- simplex$y[1]
    .show("#    > best  point and value: x=%f -> y=%f", x1, y1)

    x2 <- simplex$x[2]
    y2 <- simplex$y[2]
    .show("#    > worst point and value: x=%f -> y=%f", x2, y2)

    # set x0, the centroid except worst point
    x0 = x1

    .show("# 1. compute reflection point")
    xR <- x0 + alpha * (x0 - x2)
    yR <- fun(xR)
    .show("#    > x=%f -> y=%f", xR, yR)

    if (y1 <= yR & yR < y2) {
      .show("#    > Replace with the reflected point")
      simplex$x[2] <- xR
      simplex$y[2] <- yR
      simplex$op[2] <- "reflect.1"

    } else if (yR < y1) {
      .show("# 2. compute expansion point")
      xE <- x0 + gamma * (xR - x0)
      yE <- fun(xE)
      .show("#    > x=%f -> y=%f", xE, yE)
      if (yE < yR) {
        .show("#    > Replace with the expanded point")
        simplex$x[2] <- xE
        simplex$y[2] <- yE
        simplex$op[2] <- "expand"
      } else {
        .show("#    > Replace with the reflected point")
        simplex$x[2] <- xR
        simplex$y[2] <- yR
        simplex$op[2] <- "reflect.2"
      }

    } else if (yR >= y2) {
      .show("# 3. compute contraction point")
      xC <- x0 + rho * (xR - x0)
      yC <- fun(xC)
      .show("#    > x=%f -> y=%f", xC, yC)
      if (yC < y2) {
        .show("#    > Replace with the contraction point")
        simplex$x[2] <- xC
        simplex$y[2] <- yC
        simplex$op[2] <- "contraction"
      } else {
        .show("# 4. compute shrink point")
        xS <- x1 + sigma * (x2 - x1)
        yS <- fun(xS)
        .show("#    > x=%f -> y=%f", xS, yS)
        .show("#    > Replace all points except the best points")
        simplex$x[2] <- xS
        simplex$y[2] <- yS
        simplex$op[2] <- "shrink"
      }
    }
    diff.update <- sqrt(sum((simplex$y[1] - simplex$y[2])^2)/2)
    if (tolerance > diff.update) {
      .show("converged in tolerance")
      break
    }

    if (counter == max.iter) {
      warning("Maximum number of Iterations reached 'max.iter' value before convergence")
      break
    }
    counter <- counter + 1

  }
  output <- list(best.x = simplex$x[1],
                 best.y = simplex$y[1],
                 trace = trace.df)
  return(output)
}


myfunc <- function(x) {
  (x-5)^2 + 4 * sin(13*x) +7 *cos(3*x) #+ runif(n =1, min = -2, max = 6)
}

myop <- optim_nm2(myfunc, start = c(30), .debug = TRUE, sigma = 1/4)
op <- optimization::optim_nm(myfunc, start = c(30), trace = TRUE, delta = 1/4)

plot(myfunc, type="l", xlim=c(0,10))
abline(v = op$par, col = 2)
abline(v = myop$best.x, col = 3)

