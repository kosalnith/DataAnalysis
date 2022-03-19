### Problem 2: Matrices

## 1. Suppose 
## A = \begin{bmatrix}
## 1 & 2 & 3\\ 
## 5 & 2 & 6\\ 
## -2 & -4 & 8
## \end{bmatrix}$

# (a) Check that A^{3}=0 where 0 is a 3x3 matrix with entry equal to 0.
(matrixA <- matrix(c(1, 5, -2, 2, 2, -4, 3, 6, 8),nr=3))
matrixA%*%matrixA%*%matrixA

# (b) Replace the third column of A by the sum of the second and third columns.
matrixA[,3] <- matrixA[,2] + matrixA[,3]

## 2. Create the following matrix B with 15 rows: 
## B = \begin{bmatrix}
## 20 & -20 & 20\\ 
## 20 & -20 & 20\\ 
## \cdots & \cdots & \cdots \\
## 20 & -20 & 20
## \end{bmatrix}$
  
## And than, Calculate the 3x3 matrix B^{T}B. 
(matrixB <- matrix(c(20,-20,20), b=T, nc=3, nr=15))
t(matrixB)%*%matrixB # or use function "crossprod"
crossprod(matrixB)

## 3. Create a 6x6 matrix matA with every equal to 0. 
## Check what the functions row and col return when applied to matA.
## \begin{bmatrix}
## 0 & 1 & 0 & 0 & 0 & 0 \\
## 1 & 0 & 1 & 0 & 0 & 0 \\
## 0 & 1 & 0 & 1 & 0 & 0 \\
## 0 & 0 & 1 & 0 & 1 & 0 \\
## 0 & 0 & 0 & 1 & 0 & 1 \\
## 0 & 0 & 0 & 0 & 1 & 0 
## \end{bmatrix}
?row
?col
(matA <- matrix(0,nr=6,nc=6))
matA[abs(col(matA)-row(matA))==1] <- 1

## 4. Look at the help for the function outer. 
## Hence create the following patterned matrix:
## \begin{pmatrix}
## 0 & 1 & 2 & 3 & 4\\ 
## 1 & 2 & 3 & 4 & 5\\ 
## 2 & 3 & 4 & 5 & 6\\ 
## 3 & 4 & 5 & 6 & 7\\ 
## 4 & 5 & 6 & 7 & 8
## \end{pmatrix}$$
outer(0:4,0:4,"+")

## 5. Create the following patterned matrices. In each case, your solution 
## should make use of the special form of the matrix 
## --- this means that the solution should easily generalize to creating 
## a larger matrix with the same structure and should not involve typing 
## in all the entries in the matrix. 

# (a) 
# \begin{pmatrix}
# 0 & 1 & 2 & 3 & 4 \\
# 1 & 2 & 3 & 4 & 0 \\ 
# 2 & 3 & 4 & 0 & 1 \\
# 3 & 4 & 0 & 1 & 2 \\ 
# 4 & 0 & 1 & 2 & 3
# \end{pmatrix}$
outer(0:4,0:4,"+")%%5  

# (b)
# \begin{pmatrix}
# 0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\ 
# 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 0\\ 
# \vdots & \vdots & \vdots & \vdots & \vdots & \vdots & \vdots & \vdots & \vdots & \vdots\\ 
# 8 & 9 & 0 & 1 & 2 & 3 & 4 & 5 & 6 & 7\\ 
# 9 & 0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8
# \end{pmatrix}$
outer(0:9,0:9,"+")%%10

# (c)
# \begin{pmatrix}
# 0 & 8 & 7 & 6 & 5 & 4 & 3 & 2 & 1 \\ 
# 1 & 0 & 8 & 7 & 6 & 5 & 4 & 3 & 2 \\ 
# 2 & 1 & 0 & 8 & 7 & 6 & 5 & 4 & 3 \\ 
# 3 & 2 & 1 & 0 & 8 & 7 & 6 & 5 & 4 \\ 
# 4 & 3 & 2 & 1 & 0 & 8 & 7 & 6 & 5 \\
# 5 & 4 & 3 & 2 & 1 & 0 & 8 & 7 & 6 \\
# 6 & 5 & 4 & 3 & 2 & 1 & 0 & 8 & 7 \\
# 7 & 6 & 5 & 4 & 3 & 2 & 1 & 0 & 8 \\
# 8 & 7 & 6 & 5 & 4 & 3 & 2 & 1 & 0
# \end{pmatrix}
outer(0:8,0:8,"-")%%9

## 6. Solve the following system of linear equations in five unknowns
## x_{1} + 2x_{2} + 3x_{3} + 4x_{4} + 5x_{5} = 7
## 2x_{1} + x_{2} + 2x_{3} + 3x_{4} + 4x_{5} = -1
## 3x_{1} + 2x_{2} + x_{3} + 2x_{4} + 3x_{5} = -3
## 4x_{1} + 3x_{2} + 2x_{3} + x_{4} + 2x_{5} = 5
## 5x_{1} + 4x_{2} + 3x_{3} + 2x_{4} + x_{5} = 17
## by considering an appropriate matrix equation AX = y.
## Make use of the special form of the matrix A.

## Solutions: 
## We have X = {x_{1}, x_{2}, x_{3}, x{4}, x_{5}}, y = {7, -1, -3, 5, 17} and 
## the martix A: 
## \begin{pmatrix}
## 1 & 2 & 3 & 4 & 5 \\
## 2 & 1 & 2 & 3 & 4 \\
## 3 & 2 & 1 & 2 & 3 \\
## 4 & 3 & 2 & 1 & 2 \\
## 5 & 4 & 3 & 2 & 1
## \end{pmatrix}

yVec <- c(7,-1,-3,5,17)
AMat <- matrix(0,nr=5, nc=5)
AMat <- abs(col(AMat)-row(AMat))+1

## To solve for x, calculate A^{-1}y, by using the function solve to find 
## the inverse of A.
solve(AMat)%*%yVec # or 
solve(AMat,yVec)
