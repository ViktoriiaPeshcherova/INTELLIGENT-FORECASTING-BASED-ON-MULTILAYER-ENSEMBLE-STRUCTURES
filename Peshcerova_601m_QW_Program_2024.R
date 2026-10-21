# Набір пакетів для роботи з даними
library(openxlsx)  # Бібліотека для роботи з Excel-файлами
library(corrplot)  # Бібліотека для створення графіків кореляційних матриць
library(GGally)  # Бібліотека для розширеного створення графіків за допомогою ggplot2
library(ggplot2)  # Бібліотека для створення графіків
library(mice)  # Бібліотека для множинного заповнення пропущених даних
library(missForest)  # Бібліотека для заповнення пропущених значень за допомогою алгоритму random forest
library(car)  # Бібліотека для регресійного аналізу та інших статистичних моделей
library(bestNormalize)  # Бібліотека для нормалізації даних
library(nortest)  # Бібліотека для тестів на нормальність розподілу
library(caret)  # Бібліотека для тренування моделей машинного навчання
library(leaps) # Бібліотека для відбору ознак
library(tidyr)# Бібліотека для очищення та трансформації даних
library(dplyr)# Бібліотека для маніпуляцій з даними
library(rpart)# Бібліотека для побудови дерев рішень
library(randomForest)# Бібліотека для побудови випадкових лісів
library(e1071)# Бібліотека для роботи з SVM та іншими алгоритмами
library(neuralnet)# Бібліотека для створення нейронних мереж
library(caretEnsemble)# Бібліотека для створення ансамблів

# =================== Розвідувальний аналіз  ===================

# Встановлення робочого каталогу
setwd('C:\\Users\\peshe\\Desktop\\мкр')

# Зчитування даних з файлу 
powerplant <- read.xlsx("powerplant_dataset.xlsx")

# Виведення назв змінних
names(powerplant)

# Перейменування змінних у датафреймі powerplant
colnames(powerplant) <- c("Temperature", "Exhaust_Vacuum", "Ambient_Pressure", "Relative_Humidity", "Energy_Output")
names(powerplant)

# Виведення структури набору даних
str(powerplant)

# Виведення перших 6 записів
head(powerplant)

# Виведення статистичного підсумку
summary(powerplant)

# Кореляційний графік
correlation_matrix <- cor(powerplant)
correlation_matrix
corrplot.mixed(correlation_matrix,order = 'AOE')

# Вибір колонок
numeric_cols <- c("Temperature", "Exhaust_Vacuum", "Ambient_Pressure", "Relative_Humidity", "Energy_Output")

# Графік парних відносин 
ggpairs(powerplant)

# Графік boxplot для виявлення викидів
par(mfrow = c(2, 3))
for (var in numeric_cols) {
  boxplot(powerplant[[var]], main = paste("Boxplot of", var),  main = "Boxplot", ylab = "Values", col = "lightblue")
}

# =================== Обробка пропусків, викидів та дублікатів ===================

# Перевірка на пропуски
sum(is.na(powerplant))

# Обчислення квартилів та IQR для кожного стовпчика ознак
q1 <- sapply(numeric_cols, function(col) quantile(powerplant[[col]], 0.25))
q3 <- sapply(numeric_cols, function(col) quantile(powerplant[[col]], 0.75))
IQR <- q3 - q1
IQR

# Визначення викидів
powerplant_outliers <- sapply(1:length(numeric_cols), function(i) {
  col <- numeric_cols[i]
  outlier_indices <- powerplant[[col]] < (q1[i] - 1.5 * IQR[i]) | powerplant[[col]] > (q3[i] + 1.5 * IQR[i])
  return(outlier_indices)
})
colnames(powerplant_outliers) <- numeric_cols

# Створення фрейму даних для зберігання інформації про викиди
powerplant_outlier <- data.frame(
  Total_Outliers = colSums(powerplant_outliers),
  Percentage_Outliers = colSums(powerplant_outliers) / nrow(powerplant) * 100
)

# Виведення інформації про викиди
print(powerplant_outlier)

# Функція для виконання імпутації та побудови boxplot для різних методів
impute_and_plot <- function(df, method, title_text) {
  
  # Заміна пропусків на NA
  for (col in numeric_cols) {
    df[[col]][powerplant_outliers[, col]] <- NA
  }
  
  # Підрахунок пропущених значень перед інтерполяцією
  cat("Missing values before imputation: ", sum(is.na(df)), "\n")
  
  # Імпутація
  if (method == "missForest") {
    # Виконання імпутації з missForest
    imputed_result <- missForest(df[, names(colSums(is.na(df)))])
    df <- imputed_result$ximp
  } else {
    # Виконання імпутації MICE для інших методів
    df <- complete(mice(df[, names(colSums(is.na(df)))], method = method))
  }
  
  # Підрахунок пропущених значень після імпутації
  cat("Missing values after imputation: ", sum(is.na(df)), "\n")
  
  # Бокс-діаграма
  par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
  for (var in numeric_cols) {
    boxplot(df[[var]], main = paste("Boxplot of", var), ylab = "Values", col = "lightblue")
  }
  title(main = title_text, outer = TRUE)
  par(mfrow = c(1, 1))
}

# Імпутація та побудова графіків для PMM, CART, lasso.norm та missForest
powerplant_clean1 <- powerplant
impute_and_plot(powerplant_clean1, "pmm", "Boxplot Mice PMM")

powerplant_clean2 <- powerplant
impute_and_plot(powerplant_clean2, "cart", "Boxplot Mice CART")

powerplant_clean3 <- powerplant
impute_and_plot(powerplant_clean3, "lasso.norm", "Boxplot Mice lasso.norm")

powerplant_clean4 <- powerplant
impute_and_plot(powerplant_clean4, "missForest", "Boxplot missForest")


powerplant_clean <- powerplant_clean2

# Виявлення всіх рядків, що повторюються, включно з оригінальними
all_duplicates <- powerplant_clean[duplicated(powerplant_clean) | duplicated(powerplant_clean, fromLast = TRUE), ]

# Сортування повторюваних рядків у порядку спадання за всіма стовпчиками
sorted_duplicates <- all_duplicates[do.call(order, c(all_duplicates, decreasing = TRUE)), ]

# Перегляд відсортованих повторюваних рядків
head(sorted_duplicates, n=6)

# Видалення дубльованих рядків
powerplant_clean <- powerplant_clean[!duplicated(powerplant_clean), ]

# =================== Трансформація та нормалізація  ===================

# Гістограми розподілів ознак
par(mfrow=c(3, 3)) 
for (feature in numeric_cols) {
  hist(powerplant_clean[[feature]], main=feature, xlab=feature)
}

# Трансформація YeoJohnson
preProc  <- preProcess(powerplant_clean, method = "YeoJohnson")
powerplant_yj <- predict(preProc, powerplant_clean)

# Гістограми розподілів ознак
par(mfrow=c(3, 3)) 
for (feature in numeric_cols) {
  hist(powerplant_yj[[feature]], main=feature, xlab=feature)
}

# Нормалізація
normalize <- function(x, na.rm = TRUE) {
   return((x- min(x)) /(max(x)-min(x)))
  }
powerplant_norm<-normalize(powerplant_yj)

# Гістограми розподілів ознак
par(mfrow=c(3, 3)) 
for (feature in numeric_cols) {
  hist(powerplant_norm[[feature]], main=feature, xlab=feature)
}

# =================== Відбір ознак  ===================

# Метод обчислення MSE на вибірці перевірки
predict.regsubsets<-function(object,newdata,id,...){
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  xvars<-names(coefi)
  mat[,xvars]%*%coefi
}

# Відбір ознак
k<-100
set.seed(1)
folds<-sample(1:k,nrow(powerplant_norm),replace=TRUE)
cv.errors<-matrix(NA,k,4,dimnames=list(NULL,paste(1:4)))

# Обчислення помилок на перевірочних вибірках для кожного блоку 
for (j in 1:k) {
  best.fit <- regsubsets(Energy_Output ~ ., data = powerplant_norm[folds != j, ], nvmax = 4)
  for (i in 1:4) {
    pred <- predict.regsubsets(best.fit, powerplant_norm[folds == j, ], id = i)
    cv.errors[j, i] <- mean((powerplant_norm$Energy_Output[folds == j] - pred)^2)
  }
}

# Розрахунок середніх значень по стовпцях матриці
mean.cv.errors<-apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,typ='b')

# Відбір оптимальної підмножини змінних на повному наборі даних для отримання моделі 
reg.best<-regsubsets(Energy_Output~.,data=powerplant_norm,nvmax=4)
coef(reg.best,which.min(mean.cv.errors))

# =================== Тренування та навчання базових моделей  ===================

# Генерація випадкового вибору рядків для тренувального набору даних (90% від усіх рядків)
row.number <- sample(1:nrow(powerplant_norm), 0.9 * nrow(powerplant_norm))
# Створення тренувального набору даних на основі випадково вибраних рядків
traindata <- powerplant_norm[row.number,]
# Створення тестового набору даних на основі решти рядків, які не ввійшли до тренувального набору
testdata <- powerplant_norm[-row.number,]

# ========= Множинна лінійна регресія  =========

# Побудова моделі
lm.fit = lm(traindata$Energy_Output~., data = traindata)
# Виведення статистичного резюме процедури навчання моделі
summary(lm.fit)

# Прогнозування 
test.lm <- predict(lm.fit, newdata = testdata)

# Обчислення показників
rmse.lm <- sqrt(sum((test.lm - testdata$Energy_Output)^2)/length(testdata$Energy_Output))
mae.lm <- sum(abs(test.lm - testdata$Energy_Output))/length(testdata$Energy_Output)
mape.lm <- sum(abs((test.lm - testdata$Energy_Output)/test.lm))/length(testdata$Energy_Output)*100
R2.lm <- summary(lm.fit)$r.squared

# Результати для моделі
c(R2=R2.lm, RMSE = rmse.lm, MAE = mae.lm, MAPE = mape.lm)

# Перетворення даних у довгий формат для фактичних даних
combined_testdata.lm <- testdata %>%
  gather(key = "Variable", value = "Value", Relative_Humidity, Exhaust_Vacuum, Temperature, Ambient_Pressure) %>%
  mutate(Type = "Test")

# Створення окремого фрейму даних для прогнозів 
# Об'єднання прогнозів зі змінними для рядків
predictions.lm <- data.frame(
  Variable = rep(c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure"), each = nrow(testdata)),
  Value = unlist(testdata[, c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure")]),
  Predicted = rep(test.lm, times = 4)
)

# Графік
ggplot() +
  geom_point(data = combined_testdata.lm, aes(x = Value, y = Energy_Output, color = Type)) +
  geom_line(data = predictions.lm, aes(x = Value, y = Predicted, color = "Predicted"), size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  ggtitle('Energy_Output vs Various Variables with Predicted Values') +
  xlab('Variable') + 
  ylab('Energy_Output') +
  scale_color_manual(values = c('red', 'dark green', 'blue')) 

# ========= Регресійна модель на основі дерева рішень  =========

# Побудова моделі
dt.fit <- rpart(traindata$Energy_Output~., data = traindata)
plot(dt.fit, uniform = TRUE, 
     main = "Decision Tree Regression") 
text(dt.fit, use.n = TRUE, cex = .7) 
summary(dt.fit)

# Прогнозування 
test.dt <- predict(dt.fit, newdata = testdata)

# Обчислення показників
rmse.dt <- sqrt(sum((test.dt - testdata$Energy_Output)^2)/length(testdata$Energy_Output))
mae.dt <- sum(abs(test.dt - testdata$Energy_Output))/length(testdata$Energy_Output)
mape.dt <- sum(abs((test.dt - testdata$Energy_Output)/test.dt))/length(testdata$Energy_Output)*100
residuals.dt <- testdata$Energy_Output - test.dt
SST.dt <- sum((testdata$Energy_Output - mean(testdata$Energy_Output))^2)
SE.dt <- sum(residuals.dt^2)
R2.dt <- 1 - (SE.dt / SST.dt)

# Результати для моделі
c(R2=R2.dt, RMSE = rmse.dt, MAE = mae.dt, MAPE = mape.dt)
      
# Перетворення даних у довгий формат для фактичних даних
combined_testdata.dt <- testdata %>%
  gather(key = "Variable", value = "Value", Relative_Humidity, Exhaust_Vacuum, Temperature, Ambient_Pressure) %>%
  mutate(Type = "Test")

# Створення окремого фрейму даних для прогнозів 
# Об'єднання прогнозів зі змінними для рядків
predictions.dt <- data.frame(
  Variable = rep(c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure"), each = nrow(testdata)),
  Value = unlist(testdata[, c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure")]),
  Predicted = rep(test.dt, times = 4)
)

# Графік
ggplot() +
  geom_point(data = combined_testdata.dt, aes(x = Value, y = Energy_Output, color = Type)) +
  geom_line(data = predictions.dt, aes(x = Value, y = Predicted, color = "Predicted"), size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  ggtitle('Energy_Output vs Various Variables with Predicted Values') +
  xlab('Variable') + 
  ylab('Energy_Output') +
  scale_color_manual(values = c('red', 'dark green', 'blue')) 

# ========= Регресійна модель на основі випадкового лісу  =========

# Побудова моделі
rf.fit <- randomForest(traindata$Energy_Output~., data=traindata)
print(rf.fit)

# Прогнозування 
test.rf <- predict(rf.fit, newdata = testdata)

# Обчислення показників
rmse.rf <- sqrt(sum((test.rf - testdata$Energy_Output)^2)/length(testdata$Energy_Output))
mae.rf <- sum(abs(test.rf - testdata$Energy_Output))/length(testdata$Energy_Output)
mape.rf <- sum(abs((test.rf - testdata$Energy_Output)/test.rf))/length(testdata$Energy_Output)*100
residuals.rf <- testdata$Energy_Output - test.rf
SST.rf <- sum((testdata$Energy_Output - mean(testdata$Energy_Output))^2)
SE.rf <- sum(residuals.rf^2)
R2.rf <- 1 - (SE.rf / SST.rf)

# Результати для моделі
c(R2=R2.rf, RMSE = rmse.rf, MAE = mae.rf, MAPE = mape.rf)

# Перетворення даних у довгий формат для фактичних даних
combined_testdata.rf <- testdata %>%
  gather(key = "Variable", value = "Value", Relative_Humidity, Exhaust_Vacuum, Temperature, Ambient_Pressure) %>%
  mutate(Type = "Test")

# Створення окремого фрейму даних для прогнозів 
# Об'єднання прогнозів зі змінними для рядків
predictions.rf <- data.frame(
  Variable = rep(c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure"), each = nrow(testdata)),
  Value = unlist(testdata[, c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure")]),
  Predicted = rep(test.rf, times = 4)
)

# Графік
ggplot() +
  geom_point(data = combined_testdata.rf, aes(x = Value, y = Energy_Output, color = Type)) +
  geom_line(data = predictions.rf, aes(x = Value, y = Predicted, color = "Predicted"), size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  ggtitle('Energy_Output vs Various Variables with Predicted Values') +
  xlab('Variable') + 
  ylab('Energy_Output') +
  scale_color_manual(values = c('red', 'dark green', 'blue')) 

# ========= Регресійна модель на основі опорних векторів   =========

# Побудова моделі
svr.fit <- svm(traindata$Energy_Output~., data=traindata)
print(svr.fit)

# Прогнозування 
test.svr <- predict(svr.fit, newdata = testdata)

# Обчислення показників
rmse.svr <- sqrt(sum((test.svr - testdata$Energy_Output)^2)/length(testdata$Energy_Output))
mae.svr <- sum(abs(test.svr - testdata$Energy_Output))/length(testdata$Energy_Output)
mape.svr <- sum(abs((test.svr - testdata$Energy_Output)/test.svr))/length(testdata$Energy_Output)*100
residuals.svr <- testdata$Energy_Output - test.svr
SST.svr <- sum((testdata$Energy_Output - mean(testdata$Energy_Output))^2)
SE.svr <- sum(residuals.svr^2)
R2.svr <- 1 - (SE.svr / SST.svr)

# Результати для моделі
c(R2=R2.svr, RMSE = rmse.svr, MAE = mae.svr, MAPE = mape.svr)

# Перетворення даних у довгий формат для фактичних даних
combined_testdata.svr <- testdata %>%
  gather(key = "Variable", value = "Value", Relative_Humidity, Exhaust_Vacuum, Temperature, Ambient_Pressure) %>%
  mutate(Type = "Test")

# Створення окремого фрейму даних для прогнозів 
# Об'єднання прогнозів зі змінними для рядків
predictions.svr <- data.frame(
  Variable = rep(c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure"), each = nrow(testdata)),
  Value = unlist(testdata[, c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure")]),
  Predicted = rep(test.svr, times = 4)
)

# Графік
ggplot() +
  geom_point(data = combined_testdata.svr, aes(x = Value, y = Energy_Output, color = Type)) +
  geom_line(data = predictions.svr, aes(x = Value, y = Predicted, color = "Predicted"), size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  ggtitle('Energy_Output vs Various Variables with Predicted Values') +
  xlab('Variable') + 
  ylab('Energy_Output') +
  scale_color_manual(values = c('red', 'dark green', 'blue')) 

# ========= Модель KNN-R =========

# Побудова моделі
knn.fit <- knnreg(traindata$Energy_Output~., data=traindata)
knn.fit

# Прогнозування 
test.knn <- predict(knn.fit, newdata = testdata)

# Обчислення показників
rmse.knn <- sqrt(sum((test.knn - testdata$Energy_Output)^2)/length(testdata$Energy_Output))
mae.knn <- sum(abs(test.knn - testdata$Energy_Output))/length(testdata$Energy_Output)
mape.knn <- sum(abs((test.knn - testdata$Energy_Output)/test.knn))/length(testdata$Energy_Output)*100
residuals.knn <- testdata$Energy_Output - test.knn
SST.knn <- sum((testdata$Energy_Output - mean(testdata$Energy_Output))^2)
SE.knn <- sum(residuals.knn^2)
R2.knn <- 1 - (SE.knn / SST.knn)

# Результати для моделі
c(R2=R2.knn, RMSE = rmse.knn, MAE = mae.knn, MAPE = mape.knn)

# Перетворення даних у довгий формат для фактичних даних
combined_testdata.knn <- testdata %>%
  gather(key = "Variable", value = "Value", Relative_Humidity, Exhaust_Vacuum, Temperature, Ambient_Pressure) %>%
  mutate(Type = "Test")

# Створення окремого фрейму даних для прогнозів 
# Об'єднання прогнозів зі змінними для рядків
predictions.knn <- data.frame(
  Variable = rep(c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure"), each = nrow(testdata)),
  Value = unlist(testdata[, c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure")]),
  Predicted = rep(test.knn, times = 4)
)

# Графік
ggplot() +
  geom_point(data = combined_testdata.knn, aes(x = Value, y = Energy_Output, color = Type)) +
  geom_line(data = predictions.knn, aes(x = Value, y = Predicted, color = "Predicted"), size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  ggtitle('Energy_Output vs Various Variables with Predicted Values') +
  xlab('Variable') + 
  ylab('Energy_Output') +
  scale_color_manual(values = c('red', 'dark green', 'blue')) 

# ========= Модель на основі штучної НМ =========

# Побудова моделі
ann.fit <- neuralnet(traindata$Energy_Output~., hidden = 2, act.fct = "logistic", data=traindata)
plot(ann.fit)

# Прогнозування 
test.ann <- predict(ann.fit, newdata = testdata)

# Обчислення показників
rmse.ann <- sqrt(sum((test.ann - testdata$Energy_Output)^2)/length(testdata$Energy_Output))
mae.ann <- sum(abs(test.ann - testdata$Energy_Output))/length(testdata$Energy_Output)
mape.ann <- sum(abs((test.ann - testdata$Energy_Output)/test.ann))/length(testdata$Energy_Output)*100
residuals.ann <- testdata$Energy_Output - test.ann
SST.ann <- sum((testdata$Energy_Output - mean(testdata$Energy_Output))^2)
SE.ann <- sum(residuals.ann^2)
R2.ann <- 1 - (SE.ann / SST.ann)

# Результати для моделі
c(R2=R2.ann, RMSE = rmse.ann, MAE = mae.ann, MAPE = mape.ann)

# Перетворення даних у довгий формат для фактичних даних
combined_testdata.ann <- testdata %>%
  gather(key = "Variable", value = "Value", Relative_Humidity, Exhaust_Vacuum, Temperature, Ambient_Pressure) %>%
  mutate(Type = "Test")

# Створення окремого фрейму даних для прогнозів 
# Об'єднання прогнозів зі змінними для рядків
predictions.ann <- data.frame(
  Variable = rep(c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure"), each = nrow(testdata)),
  Value = unlist(testdata[, c("Relative_Humidity", "Exhaust_Vacuum", "Temperature", "Ambient_Pressure")]),
  Predicted = rep(test.ann, times = 4)
)

# Графік
ggplot() +
  geom_point(data = combined_testdata.ann, aes(x = Value, y = Energy_Output, color = Type)) +
  geom_line(data = predictions.ann, aes(x = Value, y = Predicted, color = "Predicted"), size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  ggtitle('Energy_Output vs Various Variables with Predicted Values') +
  xlab('Variable') + 
  ylab('Energy_Output') +
  scale_color_manual(values = c('red', 'dark green', 'blue')) 

# =================== Побудова дворівневої ансамблевої структури (класичний варіант) ===================

# Список моделей та відповідних прогнозів
var.bias.models <- list(lm = test.lm, dt = test.dt, rf = test.rf, svr = test.svr, knn = test.knn, ann = test.ann)

# Обчислення зміщення та дисперсії для кожної моделі
var.bias.res <- sapply(var.bias.models, function(preds) {
  bias <- mean(preds - testdata$Energy_Output)
  variance <- var(preds)
  c(bias, variance)
})

# Перетворення результатів у фрейм даних
var.bias.resdf <- as.data.frame(t(var.bias.res))
colnames(var.bias.resdf) <- c("Bias", "Variance")
var.bias.resdf

# Збереження testdata в ens1.preds
ens1.preds <- testdata

# Додавання стовпців з прогнозами
ens1.preds <- data.frame(
  ens1.preds,               # Існуючі дані з testdata
  test.lm = test.lm,      # Прогнози лінійної регресії
  test.dt = test.dt,      # Прогнози дерев рішень
  test.rf = test.rf,      # Прогнози випадкового лісу
  test.svr = test.svr,    # Прогнози SVR
  test.knn = test.knn,    # Прогнози KNN
  test.ann = test.ann     # Прогнози нейронної мережі
)

# ===== Перший шар =====
ens1.control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE)

ens1.stack.layer.preds <-  c('test.knn','test.dt','test.ann')
ens1.stack.layer.mods <- caretList(ens1.preds[,ens1.stack.layer.preds], ens1.preds$Energy_Output, 
                    trControl=ens1.control, methodList=c("glm"))

# Налаштування ens1.stack.layer.control для регресії
ens1.stack.layer.control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE)

# Створення стекової моделі
ens1.stack.layer <- caretStack(ens1.stack.layer.mods, method="glm", trControl=ens1.stack.layer.control)
ens1.stack.layer

# Прогнозування за допомогою стекової моделі
ens1.stack.layer.test <- predict(ens1.stack.layer, newdata=ens1.preds)

# Обчислення показників
ens1.stack.layer.rmse <- sqrt(sum((ens1.stack.layer.test - ens1.preds$Energy_Output)^2) / length(ens1.preds$Energy_Output))
ens1.stack.layer.mae <- sum(abs(ens1.stack.layer.test - ens1.preds$Energy_Output)) / length(ens1.preds$Energy_Output)
ens1.stack.layer.mape <- sum(abs((ens1.stack.layer.test - ens1.preds$Energy_Output) / ens1.stack.layer.test)) / length(ens1.preds$Energy_Output) * 100
ens1.stack.layer.residuals <- ens1.preds$Energy_Output - ens1.stack.layer.test
ens1.stack.layer.SST <- sum((ens1.preds$Energy_Output - mean(ens1.preds$Energy_Output))^2)
ens1.stack.layer.SE <- sum(ens1.stack.layer.residuals^2)
ens1.stack.layer.R2 <- 1 - (ens1.stack.layer.SE / ens1.stack.layer.SST)

# Результати для моделі
c(R2 = ens1.stack.layer.R2, RMSE = ens1.stack.layer.rmse, MAE = ens1.stack.layer.mae, MAPE = ens1.stack.layer.mape)

ens1.preds$ens1.stack.layer.test <- ens1.stack.layer.test$pred

# ===== Другий шар =====
ens1.bag.layer.preds<-
  c('ens1.stack.layer.test','test.lm','test.rf', 'test.svr')
ens1.bag.layer.ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
ens1.bag.layer<-
  train(ens1.preds[,ens1.bag.layer.preds],ens1.preds$Energy_Output,method='treebag',trControl=ens1.bag.layer.ctrl)

ens1.bag.layer

ens1.bag.layer.test<-predict(ens1.bag.layer,newdata= ens1.preds)

# Обчислення показників
ens1.bag.layer.rmse <- sqrt(sum((ens1.bag.layer.test - ens1.preds$Energy_Output)^2)/length(ens1.preds$Energy_Output))
ens1.bag.layer.mae <- sum(abs(ens1.bag.layer.test - ens1.preds$Energy_Output))/length(ens1.preds$Energy_Output)
ens1.bag.layer.mape <- sum(abs((ens1.bag.layer.test - ens1.preds$Energy_Output)/ens1.bag.layer.test))/length(ens1.preds$Energy_Output)*100
ens1.bag.layer.residuals <- ens1.preds$Energy_Output - ens1.bag.layer.test
ens1.bag.layer.SST <- sum((ens1.preds$Energy_Output - mean(ens1.preds$Energy_Output))^2)
ens1.bag.layer.SE <- sum(ens1.bag.layer.residuals^2)
ens1.bag.layer.R2 <- 1 - (ens1.bag.layer.SE / ens1.bag.layer.SST)

# Результати для моделі
c(R2=ens1.bag.layer.R2, RMSE = ens1.bag.layer.rmse, MAE = ens1.bag.layer.mae, MAPE = ens1.bag.layer.mape)


# =================== Побудова дворівневої ансамблевої структури (експериментальний варіант)  ===================

# Збереження testdata в ens2.pred
ens2.pred <- testdata

# Додавання стовпців з прогнозами
ens2.pred <- data.frame(
  ens2.pred,               # Існуючі дані з testdata
  test.lm = test.lm,      # Прогнози лінійної регресії
  test.dt = test.dt,      # Прогнози дерев рішень
  test.rf = test.rf,      # Прогнози випадкового лісу
  test.svr = test.svr,    # Прогнози SVR
  test.knn = test.knn,    # Прогнози KNN
  test.ann = test.ann     # Прогнози нейронної мережі
)

# ===== Перший шар =====
ens2.bag.preds<-
  c('test.knn','test.dt')
ens2.bag.layer.ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
ens2.bag.layer<-
  train(ens2.pred[,ens2.bag.preds],ens2.pred$Energy_Output,method='treebag',trControl=ens2.bag.layer.ctrl)

ens2.bag.layer

ens2.bag.layer.test<-predict(ens2.bag.layer,newdata= ens2.pred)

# Обчислення показників
ens2.bag.layer.rmse <- sqrt(sum((ens2.bag.layer.test - ens2.pred$Energy_Output)^2)/length(ens2.pred$Energy_Output))
ens2.bag.layer.mae <- sum(abs(ens2.bag.layer.test - ens2.pred$Energy_Output))/length(ens2.pred$Energy_Output)
ens2.bag.layer.mape <- sum(abs((ens2.bag.layer.test - ens2.pred$Energy_Output)/ens2.bag.layer.test))/length(ens2.pred$Energy_Output)*100
ens2.bag.layer.residuals <- ens2.pred$Energy_Output - ens2.bag.layer.test
ens2.bag.layer.SST <- sum((ens2.pred$Energy_Output - mean(ens2.pred$Energy_Output))^2)
ens2.bag.layer.SE <- sum(ens2.bag.layer.residuals^2)
ens2.bag.layer.R2 <- 1 - (ens2.bag.layer.SE / ens2.bag.layer.SST)

# Результати для моделі
c(R2=ens2.bag.layer.R2, RMSE = ens2.bag.layer.rmse, MAE = ens2.bag.layer.mae, MAPE = ens2.bag.layer.mape)

ens2.pred$ens2.bag.layer.test <- ens2.bag.layer.test

# ===== Другий шар =====
ens2.ctrl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE)

ens2.stack.layer.preds <- c('ens2.bag.layer.test','test.lm','test.ann','test.rf', 'test.svr')
ens2.stack.layer.models <- caretList(ens2.pred[,ens2.stack.layer.preds], ens2.pred$Energy_Output, 
                    trControl=ens2.ctrl, methodList=c("glm"))

# Налаштування ens2.stack.layer.ctrl для регресії
ens2.stack.layer.ctrl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE)

# Створення стекової моделі
ens2.stack.layer <- caretStack(ens2.stack.layer.models, method="glm", trControl=ens2.stack.layer.ctrl)
ens2.stack.layer

# Прогнозування за допомогою стекової моделі
ens2.stack.layer.test <- predict(ens2.stack.layer, newdata=ens2.pred)

# Обчислення показників
ens2.stack.layer.rmse <- sqrt(sum((ens2.stack.layer.test - ens2.pred$Energy_Output)^2) / length(ens2.pred$Energy_Output))
ens2.stack.layer.mae <- sum(abs(ens2.stack.layer.test - ens2.pred$Energy_Output)) / length(ens2.pred$Energy_Output)
ens2.stack.layer.mape <- sum(abs((ens2.stack.layer.test - ens2.pred$Energy_Output) / ens2.stack.layer.test)) / length(ens2.pred$Energy_Output) * 100
ens2.stack.layer.residuals <- ens2.pred$Energy_Output - ens2.stack.layer.test
ens2.stack.layer.SST <- sum((ens2.pred$Energy_Output - mean(ens2.pred$Energy_Output))^2)
ens2.stack.layer.SE <- sum(ens2.stack.layer.residuals^2)
ens2.stack.layer.R2 <- 1 - (ens2.stack.layer.SE / ens2.stack.layer.SST)

# Результати для моделі
c(R2 = ens2.stack.layer.R2, RMSE = ens2.stack.layer.rmse, MAE = ens2.stack.layer.mae, MAPE = ens2.stack.layer.mape)


