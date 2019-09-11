#Advanced Programming in R - Lab 1
#Automatic Feedback with markmyassignment

#Install markmyassignment
install.packages("devtools")
devtools::install_github("MansMeg/markmyassignment")

#Load package
library(markmyassignment)

#Set assignment path
lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)

#Assignment Tasks
show_tasks()
mark_my_assignment()

#1. my_num_vector()
my_num_vector <- function(){
  my_num <- c(log10(11),cos(pi/5),exp(pi/3),(1173%%7)/19)
  return(my_num)
}
my_num_vector()

#2. filter_my_vector(x, leq)
filter_my_vector <- function(x, leq){
  x[x>=leq] <- NA
  return(x)
}
filter_my_vector(x=c(2,9,2,4,102), leq=4)

#3. dot_prod(a, b)
dot_prod <- function(a, b){
  dot_prod_res <- sum(a*b)
  return(dot_prod_res)
}
dot_prod(a=c(3,1,12,2,4), b=c(1,2,3,4,5))
dot_prod(a=c(-1,3), b=c(-3,-1))

#4. approx_e(N)
approx_e <- function(N){
  approx_e_res <- sum(1/factorial(c(0:N)))
  return(approx_e_res)
}
exp(1)
approx_e(N=2)
approx_e(N=3)
approx_e(N=4)
approx_e(N=5)
approx_e(N=6)
approx_e(N=7)
approx_e(N=8)
approx_e(N=9)
#N=9 to approximate e to the fifth decimal place

#5. my_magic_matrix()
my_magic_matrix <- function(){
  magic_matrix <- matrix(data=c(4,3,8,9,5,1,2,7,6), nrow=3)
  return(magic_matrix)
}
my_magic_matrix()
#All columns, rows and diagonals add up to 15

#6. calculate_elements(A)
calculate_elements <- function(A){
  elements <- length(A)
  return(elements)
}
mat <- my_magic_matrix()
calculate_elements(A=mat)
new_mat <- cbind(mat, mat)
calculate_elements(A=new_mat)

#7. row_to_zero(A, i)
row_to_zero <- function(A, i){
  A[i,] <- 0
  return(A)
}
row_to_zero(A=mat, i=3)
row_to_zero(A=mat, i=1)

#8. add_elements_to_matrix(A, x, i, j) 
add_elements_to_matrix <- function(A, x, i, j){
  A[i,j] <- A[i,j] + x
  return((A))
}
add_elements_to_matrix(A=mat, x=10, i=2, j=3)
add_elements_to_matrix(A=mat, x=-2, i=1:3, j=2:3)

#9. my_magic_list()
my_magic_list <- function(){
  magic_list <- list(info="my own list", my_num_vector(), my_magic_matrix())
  return(magic_list)
}
my_magic_list()

#10. change_info(x, text)
change_info <- function(x, text){
  x[which(names(x) == "info")] <- text
  return(x)
}
a_list <- my_magic_list()
change_info(x = a_list, text = "Some new info")

#11. add_note(x, note)
add_note <- function(x, note){
  x[["note"]] <- note
  return(x)
}
a_list <- my_magic_list()
add_note(x = a_list, note = "This is a magic list!")

#12. sum_numeric_parts(myList)
sum_numeric_parts <- function(x){
  sum <- sum(sapply(x, function(x){ sum(as.numeric(x)) }), na.rm = TRUE)
  return(sum)
} 
a_list <- my_magic_list()
sum_numeric_parts(x = a_list)
sum_numeric_parts(x = a_list[2])

#13. my_data.frame()
my_data.frame <- function(){
  data_frame <- data.frame(id     = c(1:3), 
                           name   = c("John", "Lisa", "Azra"), 
                           income = c(7.30, 0.00, 15.21), 
                           rich   = c(FALSE, FALSE, TRUE),
                           stringsAsFactors = FALSE)
  return(data_frame)
}
my_data.frame()

#14. sort_head(df, var.name, n)
sort_head <- function(df, var.name, n){
  df_sort <- df[order(df[[var.name]], decreasing = TRUE)[1:n],]
  return(df_sort)
}
data(iris)
sort_head(df = iris, var.name = "Petal.Length", n = 5)

#15. add_median_variable(df, j)
add_median_variable <- function(df, j){
  median_val <- median(df[[j]])
  df[["compared_to_median"]] <- ifelse(df[[j]]>median_val, "Greater", 
                                       ifelse(df[[j]]<median_val, "Smaller", "Median"))
  return(df)
}
data(faithful)
head(add_median_variable(df = faithful, 1))
tail(add_median_variable(df = faithful, 2))

#16. analyze_columns(df, j)
analyze_columns <- function(df, j){
  result <- list(c(mean = mean(df[[j[1]]]), median = median(df[[j[1]]]), sd = sd(df[[j[1]]])),
                 c(mean = mean(df[[j[2]]]), median = median(df[[j[2]]]), sd = sd(df[[j[2]]])),
                 cor(df[, j]))
  names(result) <- c(names(df)[j[1]], names(df)[j[2]], "correlation_matrix")
  return(result)
}
analyze_columns(faithful, 1:2)
analyze_columns(iris, c(1,3))
analyze_columns(iris, c(4,1))

#Advanced Programming in R - Lab 2
#Name   : Namita Sharma
#LiU-ID : namsh440

name  <- "Namita Sharma"
liuid <- "namsh440"

#1. sheldon_game(player1, player2)
sheldon_game <- function(player1, player2){
  
  # create a rank matrix for all possible combinations of user inputs 
  # with values 0,1,2, where 0-Draw, 1-player 1 wins, 2-player 2 wins  
  rank <- matrix(data=c(0,1,2,1,2,
                        2,0,1,2,1,
                        1,2,0,1,2,
                        2,1,2,0,1,
                        1,2,1,2,0), nrow=5)
  
  dimnames(rank) <- list(c("rock", "paper","scissors", "spock", "lizard"), 
                         c("rock", "paper","scissors", "spock", "lizard"))
  
  tryCatch( 
    { winner <- rank[tolower(player1), tolower(player2)]
    }, 
    error = function(cond){ 
      stop("Invalid user input") 
    }
  )
  return(ifelse(winner==1, "Player 1 wins!", 
                ifelse(winner==2, "Player 2 wins!", "Draw!")))
}
sheldon_game("rock","name")
#2. my_moving_median()
my_moving_median <- function(x, n, na.rm=FALSE){
  y <- c()
  for (i in 1:(length(x)-n)) {
    y[i] <- median(x[i:(i+n)], na.rm=na.rm)
  }
  return(y)
}

#3. for_mult_table()
for_mult_table <- function(from, to){
  x <- c()
  for (i in c(from:to)) { 
    x <- c(x, i*c(from:to))
  }
  mult_table <- matrix(data=x, 
                       nrow=to-from+1,
                       dimnames=list(c(from:to), c(from:to)))
  return(mult_table)
}

#4. cor_matrix()
cor_matrix <- function(X){
  
}

