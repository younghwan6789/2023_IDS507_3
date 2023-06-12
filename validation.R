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

################################################################################
# MSE, MAE, MAPE
################################################################################
calculate_mse_mae_mape <- function(variable, prediction) {
  ## MSE 점수 확인 - 예측값과 실제값 사이의 제곱 오차의 평균을 계산합니다. 작을수록 좋은 모델입니다.
  mse <- mean((variable - prediction)^2)
  print(paste("MSE : ", mse))

  ## RMSE 점수 확인
  rmse <- sqrt(mean((variable - prediction)^2))
  print(paste("RMSE: ", rmse))

  ## MAE 점수 확인 - 예측값과 실제값 사이의 절댓값 오차의 평균을 계산합니다. 작을수록 좋은 모델입니다.
  mae <- mean(abs(prediction - variable))
  print(paste("MAE : ", mae))

  ## MAPE 점수 확인 - 예측값과 실제값 사이의 절댓값 오차를 실제값으로 나눈 후 평균을 계산합니다. 작을수록 좋은 모델입니다.
  mape <- mean(abs((prediction - variable) / variable)) * 100
  print(paste("MAPE: ", mape))
}
