library(matlib)
library(matrixcalc)

## Cholesky decomposition
A <- matrix(c(3, 4, 3, 4, 8, 6, 3, 6, 9), nrow = 3, byrow = TRUE)
A.chol <- chol(A)
A.chol
t(A.chol)
t(A.chol) %*% A.chol


## QR decomposition
set.seed(1)
A <- matrix(rnorm(15), ncol = 3)
A
D <- qr(A)
D
qr.Q(D)
qr.R(D)
qr.X(D)
is.qr(D)


## Spectral decomposition of a symmetric matrix
A <- matrix(c(13, -4, 2, -4, 11, -2, 2, -2, 8), 3, 3, byrow = TRUE)
A
ev <- eigen(A)
(L <- ev$values)
(V <- ev$vectors)
V %*% diag(L) %*% t(V)
diag(L)
zapsmall(t(V) %*% A %*% V)
A1 = L[1] * V[, 1] %*% t(V[, 1])
A1
A2 = L[2] * V[, 2] %*% t(V[, 2])
A2
A3 = L[3] * V[, 3] %*% t(V[, 3])
A3
A1 + A2 + A3
all.equal(A, A1 + A2 + A3)


## Singular value decomposition; A=UDV'
A <-
  as.matrix(data.frame(c(4, 7, -1, 8), c(-5, -2, 4, 2), c(-1, 3, -3, 6)))
A
A.svd <- svd(A)
A.svd


## Kronecker product of matrices
A <- matrix(c(3, 2, 4, 0, -1, 0), nrow = 2)
B <- matrix(c(5, 3, -1, 3), nrow = 2)
kronecker(A, B)
kronecker(B, A)
# Verify properties of Kronecker product
c <- 2
A <- matrix(c(3, 2, 4, 0), nrow = 2)
kronecker(c, A)
c * A #prop 1
D <- diag(c(2, 5))
kronecker(D, A)
direct.sum(D[1, 1] * A, D[2, 2] * A) #prop 2
I <- diag(rep(1, 2))
#kronecker(I,A);direct.sum(A,A) #prop 3
Im <- diag(rep(1, 2))
Ip <- diag(rep(1, 3))
kronecker(Im, Ip)
diag(rep(1, 6)) #prop 4
B <- matrix(c(5, 3, -1, 0), nrow = 2)
t(kronecker(A, B))
kronecker(t(A), t(B)) #prop 5
C <- matrix(c(1, 2, 5, 0), nrow = 2)
D <- matrix(c(5, 3, 1, 0), nrow = 2)
kronecker(A, B) %*% kronecker(C, D)
kronecker((A %*% C), (B %*% D)) #prop 6
qr(kronecker(A, B))$rank
qr(A)$rank * qr(B)$rank #prop 7
kronecker(A + B, C + D)
kronecker(A, C) + kronecker(A, D) + kronecker(B, C) + kronecker(B, D) #prop 8
round(as.numeric(eigen(kronecker(A, B))$val), 4)
round((as.vector(eigen(A)$val)) %*% t(as.vector(eigen(B)$val)), 4) #prop 9
det(kronecker(A, B))
det((A) ^ 2) * det((B) ^ 2)
(prod(as.numeric(eigen(A)$value))) ^ 2 * (prod(as.numeric(eigen(B)$value))) ^
  2 #prop 10


## Vectorization of matrices
A <- matrix(c(2, 9, 2, 6), nrow = 2)
c(A)


## Some properties of the vec operator
A <- matrix(c(2, 9, 2, 6), nrow = 2)
B <- matrix(c(5, 3, -1, 3), nrow = 2)
c(A) + c(B)
c(A + B)#prop 1
I <- diag(rep(1, 2))
C <- matrix(c(6, 4, 8, 2), nrow = 2)
c(A %*% B)
kronecker(I, A) %*% c(B)
kronecker(t(B), I) %*% c(A) #prop 2(i)
c(A %*% B %*% C)
kronecker(t(C), A) %*% c(B) #prop 2(ii)
c(A %*% B %*% C)
kronecker(I, (A %*% B)) %*% c(C)
kronecker((t(C) %*% t(B)), I) %*% c(A) #prop 2(iii)
t(c(t(B))) %*% c(A)
t(c(t(A))) %*% c(B)
tr(A %*% B) #prop 3
tr(A %*% B %*% C)
t(c(t(A))) %*% kronecker(t(C), I) %*% c(B)
t(c(t(A))) %*% kronecker(I, B) %*% c(C)
t(c(t(B))) %*% kronecker(A, I) %*% c(C)
t(c(t(B))) %*% kronecker(I, C) %*% c(A)
t(c(t(C))) %*% kronecker(t(B), I) %*% c(A)
t(c(t(C))) %*% kronecker(I, A) %*% c(B) #prop 4


## Direct sum of matrices
A <- matrix(c(3, 2, 4, 0, -1, 0), nrow = 2)
B <- matrix(c(5, 3, -1, 3), nrow = 2)
direct.sum(A, B)
