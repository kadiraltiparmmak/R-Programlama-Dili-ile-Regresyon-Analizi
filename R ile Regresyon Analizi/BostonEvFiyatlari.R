# Boston'daki ev fiyatlarinin cesitli ozelliklerine dayali olarak tahmin edilmesine yonelik bir regresyon analizi uygulanacaktir.
# Bu veri seti, Boston'da bulunan 506 sokaktaki evlerin cesitli ozelliklerine (ornegin, sokak tipi, yapim yili, odalarin sayisi, vb.) ve bu evlerin fiyatlarina ait bilgileri icerir. 

# İlk olarak asagidaki url uzantisindan zip olarak indirip icindeki "boston.csv" adli verimizi disari bir klasore cikartalim
# https://www.kaggle.com/datasets/fedesoriano/the-boston-houseprice-data/download?datasetVersionNumber=1

# Veri setimizi cagiralim 
veri <- read.table(file.choose(),header = T,sep = ",")
str(veri) # Veri setinin ozetini verir

# Karmasik olan sutun isimlerini adlandiralim
names(veri) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "Y")
colnames(veri)

# Regresyon analizi bagimli degiskenin normal dagilim gostermesine gerek duymaz.
# Ancak, bagimli degiskenin normal dagilim gostermesi, regresyon modelinin dogruluk ve guvenilirligini artirir ve tahminlerin daha kolay yorumlanmasini saglar.
if (shapiro.test(veri$Y)$p.value < 0.05) {
  print("Veri seti normal dagilima uymuyor.")
} else {
  print("Veri seti normal dagilima uyuyor.")
}


# Regresyon denklemi kurmak icin lm() fonksiyonu kullanilir. "~" sembolunde once bagimli degiskeni, sonrasinda bagimsiz degiskenleri yazalim
attach(veri)   # Her seferinde $ sembolunu kullanmamak icin attach() fonksiyonunu kullanalim.
model <- lm(Y ~ . ,data = veri)  # tum bagimsiz degiskenleri " . " (nokta) ile cagirabiliyoruz
model

# İki degisken arasindaki dogrusal iliskiyi olcelmek icin cor() fonksiyonunu kullanalim.
cor(X6,Y,method = "pearson")  # 0.6953599  pozitif yonlu guclu korelasyon vardir
plot(X6,Y)  # oda sayisi artikca evin fiyati artiyor

# summary() fonksiyonu, veri setinde bulunan bir degiskenin ortalama degerini, standart sapmasini, minimum ve maksimum degerlerini, vb. ozet bilgileri hesaplar.
summary(model) 
# Y_tahmin = 3.646e+01 - 1.080e-01*X1 + 4.642e-02*X2 + 2.056e-02*X3 + 2.687e+00*X4 - 1.777e+01*X5 + 3.810e+00*X6 + 6.922e-04*X7 - 1.476e+00*X8 + 3.060e-01*X9 - 1.233e-02*X10 - 9.527e-01*X11 + 9.312e-03*X12 - 5.248e-01*X13

#  anova() fonksiyonu ortalama farkinin anlamli olup olmadigini test etmek icin kullanilir.
anova(model)

attributes(model) # model degiskeninin oznitelikleri, buradan turlerini, isimleri kullanabiliriz

head(model$residuals,10) # ilk 10 artiklari cagirabiliriz
# Artiklari baska yolla da gorebiliriz.
artiklar <- model$residuals  # hatalari verir
cbind(artiklar[0:10])  # tek bir sutunda almak icin

# hatalarin normal dagilima uyumluluguna bakalim
shapiro.test(model$fitted.values)


tahminler <- predict(model, veri)  # tahminleri elde etmek icin kullanilir
tahminler
cbind(tahminler[0:20])

# Ev fiyatlarinin sokaklara gore dagilim grafigini gormek icin kullanilir 
plot(veri$Y, type="o", col="red",xlab="Sokak Sirasi", ylab="Fiyatlari (x$1000)", main="Bostondaki Ev Fiyatlarinin Dagilim Grafigi")
lines(veri$X13, type="o", col="black", lty= 2)  # Dusuk gelirli hanehalki oraniyla, ev fiyatlarini karsilastirdik.

# "model" adli modelimizin birkac grafigine bakalim
par(mfrow = c(2,2))  # cikti ekranimizi 4'e boler (2x2'lik matris)
plot(model) 

# Nokta Grafigi
par(mfrow = c(1,1)) # cikti ekranini tekrar 1e1 boyuta getirdik
dotchart(t(veri[0:10,2:4]),main="Nokta grafik")

# Histogram Grafigini cikartip bilgisayarimize kaydedelim
png("C:/Users/Kadir Altiparmak/Desktop/Hava Kirliligi Histogrami.png")  # bos png dosyasi acip icini dolduruyoruz
hist(veri$X5, col="red",main="Hava Kirliligi Histogrami", xlab = "Hava kirlilik miktari", ylab = "Sokak sirasi")  # Havanin en cok hangi bolgede kirli oldugu gorulmektedir
dev.off()

# Cubuk Grafigi
barplot(Y[0:100],col = "green", main=" Ev Fiyatlarinin Cubuk Grafigi", ylab= "Fiyatlar (x$1000)") 
grid(10,10,col = "black")


# Kutu Grafigi 
boxplot(veri$Y,
        main="Ev Fiyatlari",
        col="lightblue",
        border = "blue")
quantile(veri$Y) # kutunun uc noktasi Q1 ve ust noktasi Q4 kantil bolgesindedir (Q1 = 17.025, Q4 = 25.000)


# Bazi etkenlerin ev fiyatlarina gore dagilim matrisleri
pairs(Y~X6 +X10+X13,
      data=quakes, main="Dagilim Matrisi")

# guven araligi
confint(model) # tahmin ettigimiz degiskenlerin %95 olasilikla guven araligina ulasabiliriz
confint(model,level = .99) # %99 olasilikla guven araliginda

# bagimsiz degiskenler arasi baglantiyi hesaplamak icin vif() fonksiyonu kullanilir
install.packages("car")
library(car)

vif(model)

# 1900 den once yapilmis ev sayisi ve bu evlerin vergi miktarlarini karsilastirmak icin yeni bir model kuralim
model2 <- lm(Y ~ X7 + X10 ,data = veri) # X7: 1900 den once yapilmis ev sayisi , X13: Vergi miktari

summary(model2)
# Y_tahmin = 35.060022 - 0.061374*X7 - 0.02037*X10 

anova(model2)

# "model2" adli modelimizinde birkac grafigine bakalim
par(mfrow = c(2,2)) 
plot(model2)


artiklar2 <- model2$residuals  # hatalari verir
artiklar2 <- cbind(artiklar2)  # tek bir sutunda almak icin
cbind(artiklar2[0:15])

# guven araligi
confint(model2) 
confint(model2,level = .99) 

library(car)
vif(model2)

plot(model2$residuals,type = "l",main = "Artiklarin cizgi grafigi")

model3 <- lm(Y ~ . - X3 - X7 ,data = veri) # modelde anlamsizlari cikarttik
model3
summary(model3)

vif(model3)

model4 <- lm(Y ~ . - X3 -X7 - X9 - X10, data = veri)
summary(model4)
vif(model4)

confint(model4) 

model6 <- lm(Y ~.-X3-X7,data = veri)
summary(model6)
vif(model6)

# son olarak modelimizin bagimli degiskeninin logaritmasini alarak bir model kuralim
model7 <- lm(log(Y) ~ . -X3-X7-X9-X10 ,data = veri)  
summary(model7)  
library(car)
vif(model7)
plot(model7)
