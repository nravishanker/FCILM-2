library(pracma)
library(Matrix)

## Matrix addition and subtraction
# A and B must have the same order
(A <- matrix(c(-5, 4, 1, -3, 2, 6), nrow = 2, byrow = T))
(B <- matrix(c(7, -9, 10, 2, 6, -1), nrow = 2, byrow = T))
(C <- A + B)
(D <- A - B)


## Multiply a matrix by a scalar
c <- 5
(A2 <- c * A)


## Matrix multiplication
# ncol(A) must equal nrow(B)
(A <- matrix(c(5, 4, 1, -3, 2, 6), nrow = 2, byrow = TRUE))
(B <- matrix(c(7, -3, 2), nrow = 3, byrow = TRUE))
(M <- A %*% B)


## Product of upper triangular matrices
(U <- matrix(c(1, 2, 3, 0, 4, 5, 0, 0, 6), nrow = 3, byrow = TRUE))
(V <-
    matrix(c(2, 4, 6, 0, 8, 10, 0, 0, 12), nrow = 3, byrow = TRUE))
(W <- U %*% V)


## Transpose of a matrix
(A <- matrix(c(2, 1, 6, 4, 3, 5), nrow = 2, byrow = TRUE))
(tA <- t(A))
t(A)


## Symmetric matrix, t(S)=S
S <- matrix(c(1, 2, -3, 2, 4, 5, -3, 5, 9), nrow = 3, byrow = T)
S
(table(S == t(S))) == (nrow(S) * ncol(S))


## Skew symmetric matrix, t(SkS)=-SkS
(SkS <- matrix(
  c(0, -1, 3, 6, 1, 0, 2, -5, -3, -2, 0, 4, -6, 5, -4, 0),
  nrow = 4,
  byrow = TRUE
))
(table(-SkS == t(SkS))) == (nrow(SkS) * ncol(SkS))


## Trace of a square matrix, sum of diagonal elements
(A <- matrix(c(2, -4, 5, 6, -7, 0, 3, 9, 7), nrow = 3, byrow = TRUE))
sum(diag(A))


## Determinant of a square matrix
(D <- matrix(c(2, -4, 5, 6, -7, 0, 3, 9, 7), nrow = 3, byrow = TRUE))
det(D)


## Nonsingular matrix, |A| neq 0
(A <- matrix(c(1, 6, 0, 3), nrow = 2, byrow = TRUE))
det(A) != 0


## Singular matrix, |A|= 0
(B <- matrix(c(1, 6, 1 / 2, 3), nrow = 2, byrow = TRUE))
det(B)


## Inverse of a matrix
(I <- matrix(c(-1, 2, 2, 4, 3, -2, -5, 0, 3), nrow = 3, byrow = TRUE))
solve(I)


## Reduced row echelon form
A <- matrix(c(2, 1, -1, 8, -3, -1, 2, -11, -2, 1, 2, -3),
            nrow = 3,
            byrow = TRUE)
A
rref(A)


## Orthogonal matrix, t(A)=inv(A)
A <- randortho(4)
A %*% t(A)


## Diagonal matrices
Diagonal(4, 5)
diag(4)
Diag(c(1, 2, 3), -1)
Diag(c(1, 2, 3), 1)
diag(5, 3, 4)


## Identity and unit matrices
I <- diag(4)
one <- rep(1, 4)
J <- one %*% t(one)


## Intra-class correlation matrix
d <- 1
rho <- 0.5
I <- diag(4)
one <- rep(1, 4)
J <- one %*% t(one)
(C <- d * ((1 - rho) * I + rho * J))


## Toeplitz matrix
rho <- 0.5
(A <- Toeplitz(c(1, rho, rho ^ 2, rho ^ 3, rho ^ 4)))
det(A)
solve(A)


## Null space
A <- matrix(c(2, -4, -1, 2), nrow = 2)
round(null(A), 4)
round(A %*% null(A), 4)
## Example of a null space
A <-
  matrix(c(1, 0, 0, 0, 0, 1, 0, 0, -5, 2, 0, 0, 1, -3, 0, 0), nrow =
           4)
round(null(A), 4)
round(A %*% null(A), 4)


## Row and column spaces
A <-
  matrix(c(1, -1, 2, 3, -2, 2, -4, -6, 2, -1, 6, 8, 1, 0, 4, 5, 0, 0, 0, 1), nrow =
           4)
round(orth(A), 4)  #column space
round(orth(t(A)), 4)  #row space


## Rank of a matrix
A <-
  matrix(c(1, 1, 1, 0, 1, 2, 3, 1, 1, 2, 2, 1, 3, -1, 2, -1, -2, 0, -1, -1), nrow =
           5)
A
qr(A)$rank


## Eigenvalues and eigenvectors
A <- matrix(c(-1, 2, 0, 1, 2, 1, 0, 2, -1), nrow = 3, byrow = TRUE)
A
eigen(A)
evalues <- eigen(A)$values
evectors <- eigen(A)$vectors   # V
#Is A = V L V^(-1)
table(round(evectors %*% diag(evalues) %*% solve(evectors), 1) == A)


## Vector norms
v <- c(-1, 2, 1)
Norm(v, 1)   #L-1 norm
Norm(v, 2)   #L-2 norm
sqrt(t(v) %*% v) #L-2 norm
max(abs(v))  #L-infinity norm


## Frobenius norm of a matrix
A <- matrix(c(2, 1, 6, 4, 3, 5), nrow = 2, byrow = TRUE)
sqrt(Trace(t(A) %*% A))
