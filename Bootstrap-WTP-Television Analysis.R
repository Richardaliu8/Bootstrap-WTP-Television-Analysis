# Preferences
Richard_p <- c(23, 22, 7, 5, 1, 24, 20, 10, 18, 8, 4, 16, 19, 15, 9, 6, 17, 12, 13, 2, 3, 14, 11, 21)
Charles_p <- c(18, 13, 17, 16, 24, 21, 20, 14, 19, 22, 15, 23, 8, 6, 10, 4, 1, 9, 11, 7, 12, 5, 2, 3)
YYX_p <- c(10, 12, 8, 22, 24, 20, 9, 11, 7, 21, 23, 19, 4, 6, 3, 16, 18, 14, 2, 5, 1, 15, 17, 13)
QQY_p <- c(10, 12, 9, 22, 24, 20, 8, 11, 7, 19, 23, 21, 3, 6, 4, 16, 18, 1, 2, 5, 14, 15, 17, 13)
JZ_p <- c(13, 15, 23, 12, 21, 24, 10, 16, 12, 19, 22, 20, 4, 3, 8, 17, 14, 18, 2, 1, 5, 9, 7, 6)

#import dataset
Preferences<- read.csv("~/Downloads/Preferences 2024 BAX 442.xlsx - Team F - Preferences.csv", header=TRUE)
colnames(Preferences)<-c('name','Profile_No','Profile','Rank','Screen_75','Screen_85','Resolution_4K','Sony','Price')
Preferences <- Preferences[,1:9]
Richard<-Preferences[Preferences$name == 'Richard Liu',]
Charles<-Preferences[Preferences$name == 'Charles Wang',]
QQY<-Preferences[Preferences$name == 'Qinyi Qiu',]
JZ<-Preferences[Preferences$name == 'Jie Zhu',]
YYX<-Preferences[Preferences$name == 'Yuxin Yi',]


# Residual Bootstrap
wtp_calculate <- function(response_var, data_frame) {
  bb <- 1000
  utility_per_unit <- 2500-2000
  model <- lm(response_var ~ ., data = data_frame)
  predicted_values <- predict(model)
  residuals <- model$resid
  num_rows <- nrow(data_frame)
  coefficients <- matrix(0, bb, length(coef(model)))
  wtp_estimates <- matrix(0, bb, 4)
  
  # Perform bootstrap sampling
  for (ii in 1:bb) {
    y_star <- predicted_values + residuals[sample(num_rows, num_rows, replace = TRUE)]
    out.star <- lm(y_star ~ ., data = data_frame)                      
    coefficients[ii, ] <- coef(out.star)
    per_util <- utility_per_unit / abs(coefficients[ii, 6])
    wtp_estimates[ii, ] <- coefficients[ii, 2:5] * per_util
  }
  
  results_matrix <- matrix(0, 3, 4)
  colnames(results_matrix) <- c("screen_75", "screen_85", "4K", "Sony")
  rownames(results_matrix) <- c("2.5%", "97.5%", "mean")
  
  for (attribute_index in 1:4) {
    results_matrix[1, attribute_index] <- quantile(wtp_estimates[, attribute_index], probs = 0.025)
    results_matrix[2, attribute_index] <- quantile(wtp_estimates[, attribute_index], probs = 0.975)
    results_matrix[3, attribute_index] <- mean(wtp_estimates[, attribute_index])
  }
  
  return(results_matrix)
}

wtp_calculate(Richard$Rank, Richard[,5:9])
wtp_calculate(Charles$Rank, Charles[,5:9])
wtp_calculate(QQY$Rank, QQY[,5:9])
wtp_calculate(JZ$Rank, JZ[,5:9])
wtp_calculate(YYX$Rank, YYX[,5:9])



# Data Boostrap
wtp2_calculate = function(y, x) {
  x$y = y
  nn = nrow(x)
  bb = 1000
  wtp_values = matrix(0, bb, 4)
  col_names = c("screen_75", "screen_85", "4K", "Sony")
  
  for(ii in 1:bb) {
    data.star = x[sample(nn, nn, replace = TRUE), ]
    out.star = lm(y ~ ., data = data.star)
    coefs = coef(out.star)
    
    per_util = 500 / abs(coefs[6]) 
    wtp_values[ii, ] = coefs[2:5] * per_util
  }
  
  results = rbind(apply(wtp_values, 2, quantile, probs = c(0.025, 0.975)),
                  colMeans(wtp_values))
  rownames(results) = c("2.5%", "97.5%", "mean")
  colnames(results) = col_names
  
  return(results)
}

wtp2_calculate(Richard$Rank, Richard[,5:9])
wtp2_calculate(Charles$Rank, Charles[,5:9])
wtp2_calculate(QQY$Rank, QQY[,5:9])
wtp2_calculate(JZ$Rank, JZ[,5:9])
wtp2_calculate(YYX$Rank, YYX[,5:9])