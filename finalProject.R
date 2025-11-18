#Final Project Stat 280 Winter2023
#Name: Wenyang Qiu
#Student ID: 26888879

#Problem 1: Student Survey Data
#Load the MASS library and consider the survey dataset.
library(MASS)
#a) We want to ﬁnd the mean height of students in the dataset. However, by using the mean command, we obtain:
mean(survey$Height)
#Why did it happen? How can we compute the mean height?
#The mean is NA because there are some NA values in survey$Height:
survey$Height[1:40]
mean(survey$Height[!is.na(survey$Height)])

#b) Find the median height of male students and of female students.
#male student
median(survey$Height[survey$Sex == "Male" & !is.na(survey$Height) & !is.na(survey$Sex)])
#female student
median(survey$Height[survey$Sex == "Female" & !is.na(survey$Height) & !is.na(survey$Sex)])

#c) Compute the mean pulse rate for male students under 20.
mean(survey$Pulse[survey$Age < 20 & survey$Sex == "Male"], na.rm = TRUE)

#d) Suppose we want to extract the data of students who are taller than 190 cm. Try to execute the command
survey[survey$Height > 190, ]
#What happens? How can you extract the desired data?
survey[survey$Height > 190, ]
#This commands returns output with many NA values due to the presence of NA values in survey$Height.
survey[(survey$Height > 190) & !is.na(survey$Height), ]

#e) Extract a dataset showing only pulse rate, exercising frequency, and age of students who are less than 17 or more than 40 years old.
survey[(survey$Age < 17 | survey$Age > 40) & !is.na(survey$Age), c("Pulse", "Exer", "Age")]

#f) The ages are recorded as numeric values representing a number of years. Modify the data frame so that the age is measured using an integer number of months (for example, 20.167 years should be converted to 242 months).
survey$Age <- round(survey$Age * 12)
survey[1:10, ]
#g) What is the percentage of left handers who do not clap with their left hand on top?
cat(
  with(survey, sum(W.Hnd == "Left" & !Clap == "Left", na.rm = TRUE)
       / sum(W.Hnd == "Left" , na.rm = TRUE)) * 100,
  "%")
## 50%

#h) Create a histogram showing the distribution of students’ heights and use a QQ plot to compare this distribution with a standard normal distribution. (These two plot should appear side by side in the same ﬁgure). Are students’ heights close or far from being normally distributed?
par(mfrow = c(1,2))
hist(survey$Height, main = "Histogram", xlab = "Students' height")
qqnorm(survey$Height, main = "QQ plot", xlab = "Students' height", ylab = "Standard normal distribution")
qqline(survey$Height)
#The heights are rather close to be normally distributed since the qqplot is close to a line.

#i) Create a box plot showing pulse rates (y-axis) for students with diﬀerent exercising frequency (x-axis).What do you observe?
boxplot(Pulse ~ Exer, data = survey)
#From the boxplot we could know that students who exercise more frequently tend to have a lower pulse rate than those who exercise only sometimes or never.

#j) Consider the following variables: span of writing hand, span of non-writing hand, pulse rate, height, and age. Are there any pairs of variable in this set that exhibit a linear dependence relation?
pairs(survey[, c("Wr.Hnd", "NW.Hnd", "Pulse", "Height", "Age")])
#Writing and non-writing hand spans are clearly linearly correlated. There seems to be a weak linear dependence between height and writing hand span and between height and non-writing hand span.

#Problem 2: Convergence speed of fixed-point iterations
#Part a) Implementation
#Create a function with header: fixed.point(g, x0, TOL, Nmax)
fixed.point <- function(g, x0, TOL = 1e-3, Nmax = 100){
  n <- 0
  iter <- x0
  while(abs(g(x0)-x0) >= TOL & n < Nmax){
    x0 <- g(x0)
    n <- n+1
    iter <- c(iter, x0)
    }
  return(iter)
}

#Part b) Testing
#i By using a suitable visualization strategy, show that the functions g 1, g 2, g 3have one unique ﬁxed point on the interval [1, 2].
p <- 7^(1/5)
g1 <- function(x){(6*x+7/x^4)/7}
g2 <- function(x){x-(x^5-7)/(5*x^4)}
g3 <- function(x){(7/x)^(1/4)}
curve(g1, 1, 2, col = "red", ylab ="y")
lines(c(1,2),c(1,2), col = "blue", lty = 2)
points(p,p)
legend(1,1.8, legend = c("y = g1(x)", "y = x"), col=c("red", "blue"), lty=1:2, cex=0.8)

curve(g2, 1, 2, col = "red", ylab ="y")
lines(c(1,2),c(1,2), col = "blue", lty = 2)
points(p,p)
legend(1.4,2, legend = c("y = g2(x)", "y = x"), col=c("red", "blue"), lty=1:2, cex=0.8)

curve(g3, 1, 3, col = "red", ylab ="y")
lines(c(1,2),c(1,2), col = "blue", lty = 2)
points(p,p)
legend(1.5,2.2, legend = c("y = g3(x)", "y = x"), col=c("red", "blue"), lty=1:2, cex=0.8)

#ii. Apply the function fixed.point to g 1, g 2and g 3with x 0= 1, TOL = 10 −10and Nmax = 100. Print, the number of iterations employed in each case, and the corresponding absolute errors associated with the last iteration.
iter1<-fixed.point(g1,1,1e-10,100)
iter2<-fixed.point(g2,1,1e-10,100)
iter3<-fixed.point(g3,1,1e-10,100)
c(length(iter1),length(iter2),length(iter3))
c(abs(iter1[length(iter1)]-p),abs(iter2[length(iter2)]-p),abs(iter3[length(iter3)]-p))

#iii. Show the convergence plots associated with the test in part ii (i.e., plot the absolute error as a function of the iteration). Use the logscale for the y axis.
#If you can, show the three convergence plots in the same plot region (if not, show three plots side by side). What do you conclude about the speed of convergence of the proposed methods?
err.vec1 <- abs(iter1 - p)
err.vec2 <- abs(iter2 - p)
err.vec3 <- abs(iter3 - p)
plot(1:length(err.vec1), err.vec1, log = "y", pch = 1, ylab = "Absolute error", xlab = "Iteration", ylim = c(1e-14, 1))
points(1:length(err.vec2), err.vec2, pch = 2)
points(1:length(err.vec3), err.vec3, pch = 3)
legend("topright", legend = c("g1", "g2", "g3"), pch = 1:3)

#We conclude that g2 yields the fastest fixed-point method, followed by g3 and g1.

#Problem 3: Sorting vectors with pivoting
# We want to implement an algorithm for sorting the elements of a vector based on a pivoting strategy. The corresponding function should have the following header: midtermsort(x)
midtermsort<-function(x){
  #sorts the entries of a vectors in increasing order
  n <- length(x)
  if (n <= 1) return(x)
  pivot <- sample(n, 1)
  y <- x[x < x[pivot]]
  z <- x[x > x[pivot]]
  x.pivot <- x[x == x[pivot]]
  y.sorted <- midtermsort(y)
  z.sorted <- midtermsort(z)
  return (c(y.sorted, x.pivot, z.sorted))
}

#Part b) Testing
#i. Apply midtermsort() to a random normal vector of dimension 20. Produce a scatter plot of the entries of the sorted vector. Compare the result with the output produced by the built-in function sort()
x <- rnorm(20)
y <- midtermsort(x)
z <- sort(x)
sum(y != z)
plot(y)

#ii. For n = 10 kwith k = 2, 2.25, 2.5, 2.75, 3, ..., 4 measure the user time employed to sort a random normal vector of dimension n. Plot the correpsonding times as a function of n using a log scale for the x and y axes.
times <- c()
n.values <- 10^(seq(2,4,by = 0.25))
for (n in n.values){
  x <- rnorm(n)
  times <- c(times, system.time({midtermsort(x)})[1])
}
plot(n.values, times, log = "xy")

#Problem 4: Gradient descent
#1. Implement the gradient descent algorithm in a function with header
GradientDescent <- function(A, b, h, x0, TOL = 1e-3, N.max = 100)
{
  x <- x0
  iter <- matrix(0, nrow = length(x0), ncol = N.max+1)
  iter[,1] <- x
  for (k in 1:N.max)
  {
    grad <- A %*% x + b
    x_new <- x - h * grad
    iter[,k+1] <- x_new
    if (norm(x_new - x, "2") <= TOL)
    {
      break
    }
    x <- x_new
  }
  return(iter[,1:k+1])
}

#2. test the function with ...
A <- matrix(c(2, 1, 1, 2), nrow = 2, ncol = 2)
b <- matrix(c(5, 6), nrow = 2, ncol = 1)
x0 <- matrix(c(0, 0), nrow = 2, ncol = 1)
TOL <- 1e-7
h <- 0.1
N.max <- 100

iter <- GradientDescent(A, b, h, x0, TOL, N.max)
n_iter <- ncol(iter) - 1
n_iter
cat("Final Approximation of x:", paste(iter[, n_iter+1], collapse = ", "), "\n")

#3. Use a logarithmic scale for the y-axis
# Define the true solution to solve the problem.
x_true <- c(-4/3, -7/3)
A <- matrix(c(2, 1, 1, 2), nrow = 2, ncol = 2)
b <- matrix(c(5, 6), nrow = 2, ncol = 1)
x0 <- c(0, 0)
TOL <- 1e-7
h <- 0.1
x_iter <- GradientDescent(A, b, h, x0, TOL, N.max)
errors <- apply(x_iter, 2, function(x) norm(x - x_true, type = "2"))
plot(1:length(errors), errors, type = "l", xlab = "Iteration",
     ylab = "||x(k) - x_true||_2", log = "y", col = "green", lwd = 2)

#4.Compute M10 and v10 and store them in M and v
# Initialize the value of n to 10.
n <- 10
# Utilize the matrix() and diag() functions to define matrix M(n) to be of dimension
# n by n having 2's on the main diagonal, -1's on the first upper and lower diagonal,
# and zeros elsewhere.
M <- matrix(0, n, n)
diag(M) <- 2
diag(M[-1, ]) <- -1
diag(M[, -1]) <- -1
# Utilize the rep() function to define v(n) to be an n-dimensional vector of ones.
v <- rep(1, n)
print(M)
#5. find a minimizer Xmin and the minimum value for the function
f <- function(x)
{
  (1/2) * t(x) %*% M %*% x + t(v) %*% x + 1
}
# Define function gradient_f to calculate the gradient descent.
gradient_f <- function(x)
{
  M %*% x + v
}
# Set initial values for x and the step size alpha.
x <- rep(0, n)
alpha <- 0.1
# Set maximum number of iterations and tolerance for convergence.
max_iter <- 1000
tol <- 1e-6
# Use the for-loop to implement the gradient descent algorithm.
for (i in 1:max_iter)
{
  # Compute the gradient of f at the current x.
  grad <- gradient_f(x)
  # Update x to a newer value.
  x_new <- x - alpha * grad
  if (max(abs(x_new - x)) < tol)
  {
    break
  }
  x <- x_new
}
# Use the cat() and print() functions to print the minimizer x_min and the minimum
# value f(x_min).
cat("Minimizer x_min:")
print(x)
cat("Minimum Value f(x_min): \n", f(x))
