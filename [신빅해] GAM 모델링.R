###B167(소비총액)을 y로 두는 GAM 모델
library(mgcv)
library(data.table)
library(dplyr)
#install.packages('dplyr')

####1. 데이터 불러오기####
dat <- read.csv(file ="파일 위치.csv", header = TRUE, fileEncoding = "euc-kr")
dat

## 수식 만들기
a<-list("s(B1)")
a
for(i in 2:166){
  z<-paste0(" + s(B",i,")")
  a<-paste0(a, z)
}
a
####여성, 남성 데이터####
Female <- subset(dat, P1 == "F")
Male <- subset(dat, P1 == "M")

####성별*연령대 데이터####
dat_gender_age <- group_by(dat, P1, P2)
#####변수 100개 했더니 모델 터진다 메모리 확장 필요####
model <- gam(B167 ~ s(B1) + s(B2) + s(B3) + s(B4) + s(B5) + s(B6) + s(B7) + s(B8) + s(B9) + s(B10) + s(B11) + s(B12) + s(B13) + s(B14) + s(B15) + s(B16) + s(B17) + s(B18) + s(B19) + s(B20) + s(B21) + s(B22) + s(B23) + s(B24) + s(B25) + s(B26) + s(B27) + s(B28) + s(B29) + s(B30) + s(B31) + s(B32) + s(B33) + s(B34) + s(B35) + s(B36) + s(B37) + s(B38) + s(B39) + s(B40) + s(B41) + s(B42) + s(B43) + s(B44) + s(B45) + s(B46) + s(B47) + s(B48) + s(B49) + s(B50)
             , data = dat, method = "GCV.Cp", select = TRUE, weights=NULL)
#변수 50개는 버티는구나!
# > print(summary(model))
# 
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   B167 ~ s(B1) + s(B2) + s(B3) + s(B4) + s(B5) + s(B6) + s(B7) + 
#   s(B8) + s(B9) + s(B10) + s(B11) + s(B12) + s(B13) + s(B14) + 
#   s(B15) + s(B16) + s(B17) + s(B18) + s(B19) + s(B20) + s(B21) + 
#   s(B22) + s(B23) + s(B24) + s(B25) + s(B26) + s(B27) + s(B28) + 
#   s(B29) + s(B30) + s(B31) + s(B32) + s(B33) + s(B34) + s(B35) + 
#   s(B36) + s(B37) + s(B38) + s(B39) + s(B40) + s(B41) + s(B42) + 
#   s(B43) + s(B44) + s(B45) + s(B46) + s(B47) + s(B48) + s(B49) + 
#   s(B50)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2218141       7133     311   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df         F  p-value    
# s(B1)  4.613e+00      9    28.643  < 2e-16 ***
#   s(B2)  3.086e+00      9     0.995 0.021800 *  
#   s(B3)  8.400e-01      9     0.557 0.014583 *  
#   s(B4)  7.740e+00      9     9.418  < 2e-16 ***
#   s(B5)  3.474e+00      9     4.179  < 2e-16 ***
#   s(B6)  5.477e+00      9     2.568 0.000232 ***
#   s(B7)  7.391e+00      9    67.789  < 2e-16 ***
#   s(B8)  2.029e+00      9     0.705 0.029858 *  
#   s(B9)  5.332e+00      9     4.716  < 2e-16 ***
#   s(B10) 8.718e+00      9    20.663  < 2e-16 ***
#   s(B11) 7.270e+00      9     9.540  < 2e-16 ***
#   s(B12) 8.999e+00      9 85237.619  < 2e-16 ***
#   s(B13) 8.924e+00      9   463.634  < 2e-16 ***
#   s(B14) 2.258e-01      9     0.033 0.252955    
# s(B15) 6.471e+00      9   113.152  < 2e-16 ***
#   s(B16) 9.961e-01      9    16.136  < 2e-16 ***
#   s(B17) 9.860e-01      9    17.160  < 2e-16 ***
#   s(B18) 1.003e+00      9     0.401 0.042176 *  
#   s(B19) 3.860e+00      9    10.995  < 2e-16 ***
#   s(B20) 8.944e+00      9    67.107  < 2e-16 ***
#   s(B21) 8.853e+00      9    24.424  < 2e-16 ***
#   s(B22) 2.804e+00      9     3.204  < 2e-16 ***
#   s(B23) 8.547e-05      9     0.000 0.793999    
# s(B24) 4.952e+00      9    13.523  < 2e-16 ***
#   s(B25) 4.758e+00      9    13.116  < 2e-16 ***
#   s(B26) 7.630e+00      9     6.813  < 2e-16 ***
#   s(B27) 7.456e+00      9    13.271  < 2e-16 ***
#   s(B28) 9.046e-01      9     1.138 0.000763 ***
#   s(B29) 6.065e+00      9     3.242 2.85e-05 ***
#   s(B30) 8.252e+00      9     7.597  < 2e-16 ***
#   s(B31) 8.992e+00      9  2370.129  < 2e-16 ***
#   s(B32) 7.591e-05      9     0.000 0.851118    
# s(B33) 8.947e+00      9  2351.515  < 2e-16 ***
#   s(B34) 8.411e+00      9  1161.039  < 2e-16 ***
#   s(B35) 8.880e+00      9   258.632  < 2e-16 ***
#   s(B36) 5.618e+00      9    12.130  < 2e-16 ***
#   s(B37) 6.278e+00      9     7.721  < 2e-16 ***
#   s(B38) 1.600e+00      9     3.402  < 2e-16 ***
#   s(B39) 6.974e+00      9     5.276  < 2e-16 ***
#   s(B40) 8.742e+00      9    15.246  < 2e-16 ***
#   s(B41) 7.447e+00      9    28.011  < 2e-16 ***
#   s(B42) 3.846e+00      9     4.518  < 2e-16 ***
#   s(B43) 2.286e+00      9     1.348 0.001874 ** 
#   s(B44) 4.475e+00      9     2.940 1.97e-05 ***
#   s(B45) 5.539e+00      9    53.850  < 2e-16 ***
#   s(B46) 5.715e-01      9     0.152 0.117305    
# s(B47) 1.886e-04      9     0.000 0.410675    
# s(B48) 4.426e+00      9   236.864  < 2e-16 ***
#   s(B49) 4.221e+00      9     2.981 1.49e-05 ***
#   s(B50) 7.650e+00      9     8.588  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.658   Deviance explained = 65.8%
# GCV = 2.4089e+13  Scale est. = 2.4076e+13  n = 473228
print(summary(model))

######1.전체 데이터에 대해 컬럼 1차 그룹화####
dat2<-mutate(dat,
             Bhouse = B1+B2+B3+B4+B5,
             Btraffic = B7+B8+B9+B10+B11,
             Bgoods = B13+B14+B15+B16+B17+B18+B19+B20+B21+B22+B23+B24+B25+B26+B27+B28+B29+B30+B31+B32+B57+B58+B59+B60+B78+B79,
             Bonline = B33+B34,
             Bfood = B35+B36+B37+B38+B39+B40+B41+B42,
             Betc = B6+B43+B44+B45+B46+B47+B48+B49+B50+B51+B124+B125+B126+B127+B128+B136+B152+B153+B154,
             Bfurniture = B52+B53+B134,
             Bfee = B61+B62,
             Bcloth = B63+B64+B65+B66+B67+B68+B69+B70+B71+B72+B73+B74+B75+B76,
             Bcultural = B80+B81+B82+B83+B84+B85+B86+B87+B88+B89+B91+B92+B93+B95+B96+B121+B137+B138,
             Bactivity = B90+B97+B98+B99+B100+B101+B102+B103+B104+B105+B120+B123,
             Binsurance = B106+B107,
             Bedu = B94+B108+B122+B155+B156+B160,
             Bwedding = B109+B110+B111+B114+B115,
             Bfuneral = B112+B116+B117,
             Bcar = B118+B119+B133+B135+B157+B158+B159+B165+B166,
             Bpro = B129+B130+B131+B132,
             Bdoc = B139+B140+B141+B142+B143+B144+B145+B146+B147+B148+B149,
             Bmanufactur = B150+B151,
             Boil = B161+B162+B163+B164,
             Btax = B12
             )
#B54, 55, 56, 77, 113 는 별도로 모델링
#ver1. method = "GCV.Cp"
model <- gam(B167 ~ s(Bhouse) + s(Btraffic) + s(Bgoods) + s(Bonline) + s(Bfood) + s(Betc) + s(Bfurniture) + s(Bfee) + s(Bcloth)
             + s(Bcultural) + s(Bactivity) + s(Binsurance) + s(Bedu) + s(Bwedding) + s(Bfuneral) + s(Bcar) + s(Bpro) + s(Bdoc)
             + s(Bmanufactur) + s(Boil) + s(Btax) + s(B54) + s(B55) + s(B56) + s(B77) + s(B113),
             data = dat2, method = "GCV.Cp", select = TRUE, weights=NULL)
print(summary(model))
gam.check(model)

#ver2. method = "REML" -> ERROR!
model2 <- gam(B167 ~ s(Bhouse) + s(Btraffic) + s(Bgoods) + s(Bonline) + s(Bfood) + s(Betc) + s(Bfurniture) + s(Bfee) + s(Bcloth)
             + s(Bcultural) + s(Bactivity) + s(Binsurance) + s(Bedu) + s(Bwedding) + s(Bfuneral) + s(Bcar) + s(Bpro) + s(Bdoc)
             + s(Bmanufactur) + s(Boil) + s(Btax) + s(B54) + s(B55) + s(B56) + s(B77) + s(B113),
             data = dat2, method = "REML", select = TRUE, weights=NULL)
print(summary(model2))
gam.check(model2)

####성별 데이터 전체에 대해 모델 적용 ####
mut <- function(X){mutate(X,
                          Bhouse = B1+B2+B3+B4+B5,
                          Btraffic = B7+B8+B9+B10+B11,
                          Bgoods = B13+B14+B15+B16+B17+B18+B19+B20+B21+B22+B23+B24+B25+B26+B27+B28+B29+B30+B31+B32+B57+B58+B59+B60+B78+B79,
                          Bonline = B33+B34,
                          Bfood = B35+B36+B37+B38+B39+B40+B41+B42,
                          Betc = B6+B43+B44+B45+B46+B47+B48+B49+B50+B51+B124+B125+B126+B127+B128+B136+B152+B153+B154,
                          Bfurniture = B52+B53+B134,
                          Bfee = B61+B62,
                          Bcloth = B63+B64+B65+B66+B67+B68+B69+B70+B71+B72+B73+B74+B75+B76,
                          Bcultural = B80+B81+B82+B83+B84+B85+B86+B87+B88+B89+B91+B92+B93+B95+B96+B121+B137+B138,
                          Bactivity = B90+B97+B98+B99+B100+B101+B102+B103+B104+B105+B120+B123,
                          Binsurance = B106+B107,
                          Bedu = B94+B108+B122+B155+B156+B160,
                          Bwedding = B109+B110+B111+B114+B115,
                          Bfuneral = B112+B116+B117,
                          Bcar = B118+B119+B133+B135+B157+B158+B159+B165+B166,
                          Bpro = B129+B130+B131+B132,
                          Bdoc = B139+B140+B141+B142+B143+B144+B145+B146+B147+B148+B149,
                          Bmanufactur = B150+B151,
                          Boil = B161+B162+B163+B164,
                          Btax = B12
)}
Female2 <- mut(Female)
Male2 <- mut(Male)
model <- gam(B167 ~ s(Bhouse) + s(Btraffic) + s(Bgoods) + s(Bonline) + s(Bfood) + s(Betc) + s(Bfurniture) + s(Bfee) + s(Bcloth)
             + s(Bcultural) + s(Bactivity) + s(Binsurance) + s(Bedu) + s(Bwedding) + s(Bfuneral) + s(Bcar) + s(Bpro) + s(Bdoc)
             + s(Bmanufactur) + s(Boil) + s(Btax) + s(B54) + s(B55) + s(B56) + s(B77) + s(B113),
             data = Male2, method = "GCV.Cp", select = TRUE, weights=NULL)
print(summary(model))
gam.check(model)

####컬럼 그룹 내 유의도 판단####
model <- gam(B167 ~ s(B80) + s(B81) + s(B82) + s(B83) + s(B84) + s(B85) + s(B86) +   s(B87) + s(B88) + s(B89) + s(B91) + s(B92) + s(B93) + s(B95) + s(B96) + s(B121) + s(B137) + s(B138),
             data = dat, method = "GCV.Cp", select = TRUE, weights=NULL)
print(summary(model))
