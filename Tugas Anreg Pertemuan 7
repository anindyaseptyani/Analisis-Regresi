    library(readxl)
    library(tidyverse)
    library(ggridges)
    library(GGally)
    library(plotly)
    library(dplyr)
    library(lmtest)
    library(stats)
    library(nortest)

## ***Input Data***

    dataanreg <- read_excel("C:/Users/Lenovo/Downloads/Data_Anreg_W7.xlsx")
    View(dataanreg)

## ***Eksplorasi Data***

    y <- dataanreg$Y
    x <- dataanreg$X
    plot(x,y)

![](G1401221074_Anindya-Septyani_Tugas-Individu-Anreg-Pertemuan-7_files/figure-markdown_strict/unnamed-chunk-3-1.png)

## ***Uji Kenormalan***

    qqnorm(dataanreg$Y)
    qqline(dataanreg$Y, col = "deeppink") 

![](G1401221074_Anindya-Septyani_Tugas-Individu-Anreg-Pertemuan-7_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    shapiro.test(dataanreg$Y)

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dataanreg$Y
    ## W = 0.89636, p-value = 0.08374

## ***Model Regresi***

    model1 = lm(y~x, data = dataanreg)
    summary(model1)

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = dataanreg)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.1628 -4.7313 -0.9253  3.7386  9.0446 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 46.46041    2.76218   16.82 3.33e-10 ***
    ## x           -0.75251    0.07502  -10.03 1.74e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.891 on 13 degrees of freedom
    ## Multiple R-squared:  0.8856, Adjusted R-squared:  0.8768 
    ## F-statistic: 100.6 on 1 and 13 DF,  p-value: 1.736e-07

Dengan nilai p-value sebesar 1.736e-07, dapat disimpulkan bahwa pada
tingkat kepercayaan 95%, terdapat bukti yang cukup untuk menyatakan
bahwa variabel x memiliki pengaruh signifikan terhadap variabel y.

## ***Uji Asumsi Klasik Regresi Linear Sederhana***

    error = model1$residuals
    error

    ##          1          2          3          4          5          6          7 
    ##  9.0446035  7.3021275  3.8071435 -1.9353325 -0.9253005 -7.1627605 -6.8952045 
    ##          8          9         10         11         12         13         14 
    ## -7.1326645 -2.8751405 -4.8651085 -4.5975525  3.6700035  1.4225115  2.6900675 
    ##         15 
    ##  8.4526075

    shapiro.test(error)

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  error
    ## W = 0.92457, p-value = 0.226

    ad.test(error)

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  error
    ## A = 0.35232, p-value = 0.4178

    lillie.test(error)

    ## 
    ##  Lilliefors (Kolmogorov-Smirnov) normality test
    ## 
    ## data:  error
    ## D = 0.12432, p-value = 0.7701

Berdasarkan hasil uji normalitas dengan uji Shapiro-Wilk (0.226),
Anderson-Darling (0.4178), dan Kolmogorov-Smirnov (0.7701), ditemukan
bahwa nilai p-value pada semua uji lebih besar dari tingkat signifikansi
0.05. Oleh karena itu, dapat disimpulkan bahwa tidak ada cukup bukti
untuk menolak H0 (asumsi bahwa nilai kesalahan tidak memiliki distribusi
normal pada tingkat kepercayaan 95%).

## ***Uji Non Autokorelasi***

    dwtest(model1)

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  model1
    ## DW = 0.48462, p-value = 1.333e-05
    ## alternative hypothesis: true autocorrelation is greater than 0

Dengan p-value sebesar 1.33e-05, yang lebih kecil dari tingkat
signifikansi 0.05, kita dapat menolak H0. Oleh karena itu, dapat
disimpulkan bahwa terdapat cukup bukti untuk menyatakan adanya
autokorelasi pada residual dalam taraf kepercayaan 95%. Hal ini berarti
bahwa asumsi tentang non autokorelasi tidak terpenuhi atau residual
tidak saling bebas.

Selain itu, nilai Durbin-Watson yang seharusnya berada dalam rentang 1
sampai 3, tetapi nilai yang didapatkan adalah 0.48462, menunjukkan
adanya autokorelasi dalam data.

## ***Uji Homokedastisitas***

    bptest(model1, studentize = TRUE, data = dataanreg)

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  model1
    ## BP = 0.52819, df = 1, p-value = 0.4674

Dalam melakukan uji homokedastisitas menggunakan uji Breusch-Pagan,
ditemukan bahwa nilai p-value adalah 0.4674, yang lebih besar daripada
tingkat signifikansi 0.05. Oleh karena itu, kita tidak dapat menolak H0.
Dengan kata lain, tidak terdapat cukup bukti untuk menyatakan bahwa
varians residual dalam model tidak konstan atau adanya
heterokedastisitas. Sehingga, asumsi homokedastisitas pada data
terpenuhi.

## ***Transformasi Data***

    x2 <- sqrt(x)
    y2 <- sqrt(y)
    model2 = lm(y2~x2, data = dataanreg)

## ***Uji Non Autokorelasi Hasil Transformasi***

    dwtest(model2)

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  model2
    ## DW = 2.6803, p-value = 0.8629
    ## alternative hypothesis: true autocorrelation is greater than 0

Dengan melakukan uji autokorelasi pada hasil transformasi menggunakan
uji Durbin-Watson, ditemukan bahwa nilai DW adalah 2.6803, yang
menandakan ketiadaan autokorelasi pada model karena berada dalam rentang
1 hingga 3. Nilai p-value sebesar 0.8629, yang lebih besar dari tingkat
signifikansi 0.05, menunjukkan bahwa kita tidak memiliki cukup bukti
untuk menolak H0. Oleh karena itu, dapat disimpulkan bahwa tidak ada
cukup bukti untuk menyatakan adanya autokorelasi dalam model.

## ***Model Regresi Linear Sederhana Hasil Transformasi***

    plot(x2, y2)

![](G1401221074_Anindya-Septyani_Tugas-Individu-Anreg-Pertemuan-7_files/figure-markdown_strict/unnamed-chunk-14-1.png)

    summary(model2)

    ## 
    ## Call:
    ## lm(formula = y2 ~ x2, data = dataanreg)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.42765 -0.17534 -0.05753  0.21223  0.46960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.71245    0.19101   45.61 9.83e-16 ***
    ## x2          -0.81339    0.03445  -23.61 4.64e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2743 on 13 degrees of freedom
    ## Multiple R-squared:  0.9772, Adjusted R-squared:  0.9755 
    ## F-statistic: 557.3 on 1 and 13 DF,  p-value: 4.643e-12

Berdasarkan hasil analisis rteregresi linear sederhana data yang telah
ditransformasi, Terdapat 2 koefisien dalam model yaitu b0 sebesar
8.71245 dan b1 sebesar -0.81339 sehingga dapat dimodelkan regresi
linearnya sebagai berikut:

*ŷ*<sup>\*</sup> = 8.71245 − 0.81339*x*

Nilai koefisien b1 sebesar -0.8133 mengindikasikan bahwa, secara dugaan,
setiap kenaikan satu satuan pada variabel x berhubungan dengan penurunan
rata-rata sebesar 0.81339 pada variabel y. Sementara itu, koefisien b0
sebesar 8.71245 menunjukkan bahwa dugaan rata-rata variabel y adalah
sekitar 8.71245 ketika nilai variabel x sama dengan nol. Hal ini
mengimplikasikan adanya pengaruh faktor-faktor lain yang tidak
dimasukkan dalam model, atau dengan kata lain, terdapat nilai rata-rata
y yang tidak dapat dijelaskan oleh variabel x.
