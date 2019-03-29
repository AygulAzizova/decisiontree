setwd("C://Users/DNS/Desktop/StatAd")
getwd()
# Прочитаем файл с данными в переменную winedata
winedata <- read.csv("winedata_decisiontree.txt", sep="\t")
winedata


# Часть выборки будем использовать как обучающую - 130 записей
# Сохраним объём всей выборки и объём обучающей выборки в переменных
n_data = 174
n_train = 130


# Обучающая выборка должна быть репрезентативной -
# т.е. каждый класс должен быть представлен в ней в той же пропорции,
# что и во всей выборке.
# Подсчитаем частоты встречаемости классов в исходной выборке
# (выразим эти частоты в %, округлив результат до десятых)
# Номер класса содержится в 1-ом столбце
round(prop.table(table(winedata[1]))*100, digits = 1)

# Чтобы обеспечить репрезентативность выборки, перемешаем её
set.seed(333)
winedata_mixed=winedata[order(runif(n_data)),]
# Выберем обучающую выборку
train_data = winedata_mixed[1:n_train,]
# Сохраним номера классов для строк обучающей выборки
train_data_labels = train_data[,1]

# Подсчитаем частоту присутствия каждого класса в обучающей выборке
# и сравним с соответствующими частотами в исходной выборке
round(prop.table(table(train_data[1]))*100, digits = 1)

# Оставшуюся часть "перемешанной выборки" будем использовать
# как тестовую выборку
test_data = winedata_mixed[(n_train+1):n_data, ]
test_data_labels = test_data[,1]

# Удалим из обучающей и тестовой выборок столбец с номерами классов
train_data = train_data[-1]
test_data = test_data[-1]

# Подключим пакет C5.0 для применения метода деревьев решений
library("C50")

# Вызовем метод C5.0
wine_model <- C5.0(train_data, factor(train_data_labels))

# Выведем информацию
wine_model

# Для просмотра решений, вызовем функцию summary
summary(wine_model)

# Применим наше дерево решений для тестовых данных
wine_pred <- predict(wine_model, test_data)
wine_pred
plot(wine_model)
text(wine_model, cex = 1.2,
     all = FALSE, use.n = FALSE)
# Для оценки качества прогноза подключим библиотеку gmodels
library("gmodels")

# и построим кросс-валидационную таблицу:
CrossTable(x = test_data_labels, y = wine_pred, prop.chisq=FALSE)