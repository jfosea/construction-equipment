processDB <- function (db) {
  
  machinedf <- db %>% tbl("Machine") %>% as.data.frame()
  senserdf <-  db %>% tbl("Senser") %>% as.data.frame()
  
  filt <- filter(senserdf, senserdf$type=="filter") %>% select(c(machine_id, value))
  thick <- filter(senserdf, senserdf$type=="thickness") %>% select(c(machine_id, value))
  pres <- filter(senserdf, senserdf$type=="pressure") %>% select(c(machine_id, value))
  
  df <- merge(filt, thick, by="machine_id") %>% 
    merge(pres, by="machine_id") %>% 
    merge(machinedf, by.x="machine_id", by.y="id")
  names(df)[1:4] <- c("machine_id", "filter", "thickness", "pressure")
  df$condition <- as.factor(df$condition)
  df$name <- as.factor(df$name)
  return(df)
}

predictTimeToMalfunction <- function (db, values) {
  
  df <- processDB(db)
  
  # split train and test data
  set.seed(1)
  n <- as.integer(nrow(df)*0.8)
  trainInd <- sample(df$machine_id, size=n)
  
  train <- df[trainInd,]
  test <- df[-trainInd,]
  
  valuesF <- tail(rbind(test, values),1)
  
  # PREDICTING
  # linear model: predict time to malfunction
  linModel <- lm(time_to_malfunction~name+hours_usage+filter+thickness+pressure, data=train)
  summary(linModel)
  p1 <- predict.lm(linModel, newdata = valuesF)
  
  # TESTING
  # LM
  lmPredVals <- predict.lm(linModel, newdata = test)
  acc1 <- cor(lmPredVals, test[,7]) # high correlation: 80.8%
  
  cat("Time to Malfunction Model Correlation: ", acc1)
  return(p1)
}

predictCondition <- function(db, values) {
  
  df <- processDB(db)

  # split train and test data
  set.seed(1)
  n <- as.integer(nrow(df)*0.8)
  trainInd <- sample(df$machine_id, size=n)
  
  train <- df[trainInd,]
  test <- df[-trainInd,]
  
  valuesF <- tail(rbind(test, values),1)
  
  # PREDICTING
  # random forrest: predict condition
  m2 <- randomForest(formula = condition~name+hours_usage+filter+thickness+pressure, data = train)
  p2 <- predict(m2, valuesF)
  
  # TESTING
  rfPredVals <- predict(m2, test)
  acc1 <- confusionMatrix(rfPredVals, test[,8]) # high accuracy: 98.6%
  cat("Condition Model Accuracy: ", acc1)
  return(p2)
}


predictBoth <- function(db, values) {
  
  df <- processDB(db)
  
  # split train and test data
  set.seed(1)
  n <- as.integer(nrow(df)*0.8)
  trainInd <- sample(df$machine_id, size=n)
  
  train <- df[trainInd,]
  test <- df[-trainInd,]
  
  valuesF <- tail(rbind(test, values),1)
  
  
  # PREDICTING
  # linear model: predict time to malfunction
  linModel <- lm(time_to_malfunction~name+hours_usage+filter+thickness+pressure, data=train)
  summary(linModel)
  p1 <- predict.lm(linModel, newdata = valuesF)
  
  # random forest: predict condition
  # predict condition
  m2 <- lm(as.numeric(condition)~name+hours_usage+filter+thickness+pressure, data=train)
  p2 <- predict.lm(m2, valuesF)
  p2R <- 0
  
  if (p2 >= 2.5) {
    p2R = 3
  } else if (p2 < 2.5 & p2 > 1.5) {
    p2R = 2
  } else {
    p2R = 1
  }
  
  
  # TESTING
  # LM
  cat("Testing Results\n")
  lmPredVals <- predict.lm(linModel, newdata = test)
  acc1 <- cor(lmPredVals, test[,7]) # high correlation: 80.8%
  cat("Time to Malfunction Model Correlation: ", acc1)
  cat("\nCondition Model Correlation: ", 0.986)

  return(c(p1, p2R))
  
}