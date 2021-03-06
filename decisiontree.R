setwd("C://Users/DNS/Desktop/StatAd")
getwd()
# ��������� ���� � ������� � ���������� winedata
winedata <- read.csv("winedata_decisiontree.txt", sep="\t")
winedata


# ����� ������� ����� ������������ ��� ��������� - 130 �������
# �������� ����� ���� ������� � ����� ��������� ������� � ����������
n_data = 174
n_train = 130


# ��������� ������� ������ ���� ���������������� -
# �.�. ������ ����� ������ ���� ����������� � ��� � ��� �� ���������,
# ��� � �� ���� �������.
# ���������� ������� ������������� ������� � �������� �������
# (������� ��� ������� � %, �������� ��������� �� �������)
# ����� ������ ���������� � 1-�� �������
round(prop.table(table(winedata[1]))*100, digits = 1)

# ����� ���������� ������������������ �������, ���������� �
set.seed(333)
winedata_mixed=winedata[order(runif(n_data)),]
# ������� ��������� �������
train_data = winedata_mixed[1:n_train,]
# �������� ������ ������� ��� ����� ��������� �������
train_data_labels = train_data[,1]

# ���������� ������� ����������� ������� ������ � ��������� �������
# � ������� � ���������������� ��������� � �������� �������
round(prop.table(table(train_data[1]))*100, digits = 1)

# ���������� ����� "������������ �������" ����� ������������
# ��� �������� �������
test_data = winedata_mixed[(n_train+1):n_data, ]
test_data_labels = test_data[,1]

# ������ �� ��������� � �������� ������� ������� � �������� �������
train_data = train_data[-1]
test_data = test_data[-1]

# ��������� ����� C5.0 ��� ���������� ������ �������� �������
library("C50")

# ������� ����� C5.0
wine_model <- C5.0(train_data, factor(train_data_labels))

# ������� ����������
wine_model

# ��� ��������� �������, ������� ������� summary
summary(wine_model)

# �������� ���� ������ ������� ��� �������� ������
wine_pred <- predict(wine_model, test_data)
wine_pred
plot(wine_model)
text(wine_model, cex = 1.2,
     all = FALSE, use.n = FALSE)
# ��� ������ �������� �������� ��������� ���������� gmodels
library("gmodels")

# � �������� �����-������������� �������:
CrossTable(x = test_data_labels, y = wine_pred, prop.chisq=FALSE)