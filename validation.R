################################################################################
# R-squared - 1에 가까울 수록 좋음. 0.8 이상이면 매우 좋은 모델.
################################################################################
calculate_r_squared <- function(predicted, actual) {
  mean_predicted <- mean(predicted)
  total_sum_squares <- sum((actual - mean(predicted))^2)
  residual_sum_squares <- sum((actual - predicted)^2)
  
  r_squared <- 1 - (residual_sum_squares / total_sum_squares)
  
  return(r_squared)
}