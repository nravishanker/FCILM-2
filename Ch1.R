library(pracma)
library(Matrix)

## Def. 1.2.11. Matrix addition and subtraction
# A and B must have the same order
(A <- matrix(c(-5, 4, 1,-3, 2, 6), nrow = 2, byrow = T))
(B <- matrix(c(7,-9, 10, 2, 6,-1), nrow = 2, byrow = T))
(C <- A + B)
(D <- A - B)

## Def. 1.2.12. Multiply a matrix by a scalar
c <- 5
(A2 <- c * A)

## Def. 1.2.13. Matrix multiplication
# ncol(A) must equal nrow(B)
(A <- matrix(c(5, 4, 1,-3, 2, 6), nrow = 2, byrow = T))
(B <- matrix(c(7,-3, 2), nrow = 3, byrow = T))
(M <- A %*% B)

## Example. 1.2.5. Product of upper triangular matrices
(U <- matrix(c(1, 2, 3, 0, 4, 5, 0, 0, 6), nrow = 3, byrow = T))
(V <- matrix(c(2, 4, 6, 0, 8, 10, 0, 0, 12), nrow = 3, byrow = T))
(W <- U %*% V)

## Def. 1.2.14. Transpose of a matrix
(A <- matrix(c(2, 1, 6, 4, 3, 5), nrow = 2, byrow = T))
(tA <- t(A))
t(A)

## Def. 1.2.15. Symmetric matrix, t(S)=S
S = matrix(c(1, 2,-3, 2, 4, 5,-3, 5, 9), nrow = 3, byrow = T)
S
(table(S == t(S))) == (nrow(S) * ncol(S))

## Skew symmetric matrix, t(SkS)=-SkS
(SkS = matrix(
  c(0,-1, 3, 6, 1, 0, 2,-5,-3,-2, 0, 4,-6, 5,-4, 0),
  nrow = 4,
  byrow = T
))
(table(-SkS == t(SkS))) == (nrow(SkS) * ncol(SkS))

## Def. 1.2.16. Trace of a square matrix, sum of diagonal elements
(T = matrix(c(2,-4, 5, 6,-7, 0, 3, 9, 7), nrow = 3, byrow = T))
sum(diag(T))

## Def. 1.2.17. Determinant of a square matrix
(D = matrix(c(2,-4, 5, 6,-7, 0, 3, 9, 7), nrow = 3, byrow = T))
det(D)

## Def. 1.2.18. Nonsingular matrix, |A| neq 0
(A <- matrix(c(1, 6, 0, 3), nrow = 2, byrow = T))
det(A) != 0

## Singular matrix , |A|= 0
(B <- matrix(c(1, 6, 1 / 2, 3), nrow = 2, byrow = T))
det(B)

## Def. 1.2.19. Inverse of matrix
(I = matrix(c(-1, 2, 2, 4, 3,-2,-5, 0, 3), nrow = 3, byrow = T))
solve(I)

## Def. 1.2.20. Reduced row echelon form
A <- matrix(c(2, 1,-1, 8,-3,-1, 2,-11,-2, 1, 2,-3),
            nrow = 3,
            byrow = T)
A
rref(A)

## Def. 1.2.21. Orthogonal matrix, t(A)=inv(A), so A*t(A)=t(A)*A=I
A <- randortho(4)
A %*% t(A)

## Diagonal matrices
Diagonal(4, 5)
diag(4)
Diag(c(1, 2, 3),-1)
Diag(c(1, 2, 3), 1)
diag(5, 3, 4)

## Identity and Unit matrices
I <- diag(4)
one <- rep(1, 4)
J <- one %*% t(one)

## Intra-class correlation matrix
d <- 1
rho <- 0.5
I = diag(4)
one <- rep(1, 4)
J <- one %*% t(one)
(C <- d * ((1 - rho) * I + rho * J))

## Toeplitz Matrix
rho = 0.5
(A <- Toeplitz(c(1, rho, rho ^ 2, rho ^ 3, rho ^ 4)))
det(A)
solve(A)

## Def. 1.2.28. Null space
A <- matrix(c(2,-4,-1, 2), nrow = 2)
round(null(A), 4)
round(A %*% null(A), 4)

## Example 1.2.14. Null space
A <-
  matrix(c(1, 0, 0, 0, 0, 1, 0, 0,-5, 2, 0, 0, 1,-3, 0, 0), nrow =
           4)
round(null(A), 4)
round(A %*% null(A), 4)

## Example 1.2.15. Row and column spaces
A <-
  matrix(c(1,-1, 2, 3,-2, 2,-4,-6, 2,-1, 6, 8, 1, 0, 4, 5, 0, 0, 0, 1), nrow =
           4)
round(orth(A), 4)  #Column space
round(orth(t(A)), 4)  #Row space

## Example 1.2.16. Rank
A <-
  matrix(c(1, 1, 1, 0, 1, 2, 3, 1, 1, 2, 2, 1, 3,-1, 2,-1,-2, 0,-1,-1), nrow =
           5)
A
qr(A)$rank

## Example. 1.2.19. Eigenvalues and eigenvectors
A <- matrix(c(-1, 2, 0, 1, 2, 1, 0, 2,-1), nrow = 3, byrow = T)
A
eigen(A)
evalues <- eigen(A)$values
evectors <- eigen(A)$vectors   # V
#Is A = V L V^(-1)
table(round(evectors %*% diag(evalues) %*% solve(evectors), 1) == A)

## Algebraic and Geometric Multiplicities
A <- matrix(c(3, 1, 0, 3), nrow = 2, byrow = T)
evalues <- eigen(A)$values     # Eigenvalues
evectors <- eigen(A)$vectors   # Eigenvectors
## Algebraic multiplicity - number of times a particular lambda repeats among eigen values
# Consider particular lambda from the above eigenvalues 
lambda <- 3    # one of the values from the eigenvalues
AM     <- sum(abs(evalues-lambda) < 1e-6)   
# algebraic multiplicity: how many eigen values within a difference of 1e-6
index     <- which(abs(evalues - lambda) < 1e-6)  
ev_lambda <- evectors[,index]    # Eigen vector space for eigen value 'lambda'
GM        <- qr(ev_lambda)$rank  # linearly independent eigenvectors in its space.
# GM <= AM 



## Def. 1.2.36. Vector norms
v <- c(-1, 2, 1)
Norm(v, 1)   #L-1 norm
Norm(v, 2)   #L-2 norm
sqrt(t(v) %*% v) #L-2 norm
max(abs(v))  #L-infinity norm

## Def. 1.2.37. Matrix norm
A <- matrix(c(2, 1, 6, 4, 3, 5), nrow = 2, byrow = T)
sqrt(Trace(t(A) %*% A))
