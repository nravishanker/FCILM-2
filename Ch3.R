library(pracma)

## Generalized inverse. The function gives the Moore-Penrose generalized inverse
# Example 1
A <- matrix(c(4, 1, 3, 1, 1, 1, 2, 5, 3, 0, 15, 5), nrow = 3)
round(pinv(A), 4) 
# Example 2
A <- matrix(c(2, 2, 6, 2, 3, 8, 6 , 8, 22), nrow = 3)
round(pinv(A), 4) 
# Example 3
A <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1), nrow = 6)
round(pinv(A), 4) 


library(matlib)
## Solving nonhomogeneous linear equations; Ax=b
A <- matrix(c(2, -1, 6, 1, -2, 0, -6, 2, 0, 3, 1, -7, 4, 1, 8, -16), 4, 4)
A
b <- c(2, 6, 12, -7)
showEqn(A, b)
# check consistency of the equations
c(R(A), R(cbind(A, b)))
all.equal(R(A), R(cbind(A, b)))
Solve(A, b, fractions = TRUE)  #solve the equations


## Solve Ax=b, plotting equations
A <- matrix(c(1, 3, 2, 6), 2, 2)
b <- c(5, 10)
showEqn(A, b)
plotEqn(A, b, solution = TRUE)
Solve(A, b)


## Solving homogeneous linear equations; Az=0
A <-
  matrix(c(1,-2, 3, 2, 1, 0, 1,-3, 1, 2 ,-3, 0),
         nrow = 3,
         byrow = TRUE)
b <- c(0, 0, 0)
showEqn(A, b)
# Check: homogeneous equations must always be consistent
all.equal(R(A), R(cbind(A, b)))
# Solution space is N(A) with dimension L
(n <- ncol(A))
(L <- n - R(A))
# Check whether there is a unique solution, i.e., is L=0?
(L == 0)
Solve(A, b, fractions = TRUE)   # use Solve
# Basis of the non-unique solution space:
# Z0 = (I- GA)y for some y of dimension L
G <- MASS::ginv(A)   # generalized-inverse
# four y vectors = columns of I_4
Z0 <- (diag(4) - (G %*% A)) %*% diag(4)
echelon(t(Z0))   # non-zero rows are basis


## Unconstrained minimization
# Minimize f(x1,x2) =x1^2+0.5x1+3x1*x2+5x2^2
V <- matrix(c(1, 3 / 2, 3 / 2, 5), 2, 2)
V
rhs <- c(-0.5, 0)
showEqn(V, rhs)
# check consistency of the set of equations
c(R(V), R(cbind(V, rhs)))
all.equal(R(V), R(cbind(V, rhs)))
xast <- Solve(V, rhs)


## Constrained minimization with one constraint
# f(x1,x2) =x1^2+.5x1+3x1*x2+5x2^2 s.t.
# g(x1,x2)=3x1+2x2+2=0
# Lagrangian L=f + lambda*g
U <- matrix(c(2, 3, 3, 3, 10, 2, 3, 2, 0), byrow = TRUE, nrow = 3)
v <- c(-1 / 2, 0,-2)
showEqn(U, v)
# check consistency
c(R(U), R(cbind(U, v)))
all.equal(R(U), R(cbind(U, v)))
sol.ast <- Solve(U, v)


## Constrained minimization with two constraints
# f(x1,x2) =x1^2+.5x1+3x1*x2+ 5x2^2 s.t.
# g1(x1,x2)=3x1+2x2+2=0 and
# g2(x1,x2)=15x1-3x2-1=0
# Solve the nonhomogeneous linear equations
# from the partial derivatives set to zero
mat <-
  matrix(c(2, 3, 3, 15, 3, 10, 2,-3, 3, 2, 0, 0, 15,-3, 0, 0), 4, 4)
mat
rhs <- c(-0.5, 0,-2, 1)
showEqn(mat, rhs)
# check consistency
c(R(mat), R(cbind(mat, rhs)))
all.equal(R(mat), R(cbind(mat, rhs)))
xast <- Solve(mat, rhs)


## Example of constrained minimizer
U <- matrix(c(2, 3, 3, 15, 3, 10, 2,-3, 3, 2, 0, 0, 15,-3, 0, 0),
            byrow = T,
            nrow = 4)
v <- c(-1 / 2, 0,-2, 1)
showEqn(U, v)
# check consistency
c(R(U), R(cbind(U, v)))
all.equal(R(U), R(cbind(U, v)))
sol.ast <- Solve(U, v)


## Plot 1
# This plot allows us to find points and values at each section of the curve
library(emdbook)
x <- y <- seq(-1, 1, len = 100)
f <- function(x, y) {
  x ^ 2 + 0.5 * x + 3 * x * y + 5 * y ^ 2
}
z <- outer(x, y, f)
curve3d(f, c(-1, -1), c(1, 1), sys3d = "contour")
curve3d(f, c(-1, -1), c(1, 1), sys3d = "persp")
curve3d(f, c(-1, -1), c(1, 1), sys3d = "wireframe")
curve3d(f, c(-1, -1), c(1, 1), sys3d = "rgl")
curve3d(f, c(-1, -1), c(1, 1), sys3d = "image")
plotly::plot_ly(x = x,
                y = y,
                z = z,
                type = "surface")


## Plot 2
library(ContourFunctions)
f <- function(x) {
  x[1] ^ 2 + 0.5 * x[1] + 3 * x[1] * x[2] + 5 * x[2] ^ 2
}
optim(c(0, 0), f)  # its unconstrained min is -0.45, 0.136
cf(f,
   xlim = c(-1, 1),
   ylim = c(-1, 1),
   with_lines = TRUE)
# Zoom in to the center of contour lines to find minimum
cf(
  f,
  xlim = c(-0.6, -0.2),
  ylim = c(0, 0.3),
  with_lines = TRUE
)


## Plot 3
# While this plot shows a 3-d figure that is also interactive,
# it is difficult to spot the minimum unlike in a contour plot
library(plot3Drgl)
x <- y <- seq(-1, 1, len = 100)
f <- function(x, y) {
  x ^ 2 + 0.5 * x + 3 * x * y + 5 * y ^ 2
}
z <- outer(x, y, f)
persp3Drgl(z = z)
