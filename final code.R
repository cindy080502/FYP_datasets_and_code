library(readr)
library(readxl)
library(demography)
library(TSA)
library(tseries)
library(rgl)

#Data import
mpopulation<-read_excel("Male_population.xlsx")
fpopulation<-read_excel("Female_population.xlsx")

mmortality<-read_excel("Male_death_rate.xlsx")
fmortality<-read_excel("Female_death_rate.xlsx")

year<-seq(1999,2021,1)
age<-seq(0,90,5)
mpopulation<-matrix(c(mpopulation$`1999`,mpopulation$`2000`,mpopulation$`2001`,mpopulation$`2002`,mpopulation$`2003`,mpopulation$`2004`,mpopulation$`2005`,mpopulation$`2006`,mpopulation$`2007`,mpopulation$`2008`
                      ,mpopulation$`2009`,mpopulation$`2010`,mpopulation$`2011`,mpopulation$`2012`,mpopulation$`2013`,mpopulation$`2014`,mpopulation$`2015`,mpopulation$`2016`,mpopulation$`2017`,mpopulation$`2018`
                      ,mpopulation$`2019`,mpopulation$`2020`,mpopulation$`2021`),nrow = 19,ncol = 23)
#mmortality[is.na(mmortality)]=0
mmortality_rate<-matrix(c(mmortality$`1999`,mmortality$`2000`,mmortality$`2001`,mmortality$`2002`,mmortality$`2003`,mmortality$`2004`,mmortality$`2005`,mmortality$`2006`,mmortality$`2007`,mmortality$`2008`
                          ,mmortality$`2009`,mmortality$`2010`,mmortality$`2011`,mmortality$`2012`,mmortality$`2013`,mmortality$`2014`,mmortality$`2015`
                          ,fmortality$`2016`,mmortality$`2017`,mmortality$`2018`,mmortality$`2019`,mmortality$`2020`,mmortality$`2021`),nrow = 19,ncol = 23)

fpopulation<-matrix(c(fpopulation$`1999`,fpopulation$`2000`,fpopulation$`2001`,fpopulation$`2002`,fpopulation$`2003`,fpopulation$`2004`,fpopulation$`2005`,fpopulation$`2006`,fpopulation$`2007`,fpopulation$`2008`
                      ,fpopulation$`2009`,fpopulation$`2010`,fpopulation$`2011`,fpopulation$`2012`,fpopulation$`2013`,fpopulation$`2014`,fpopulation$`2015`,fpopulation$`2016`,fpopulation$`2017`,fpopulation$`2018`
                      ,fpopulation$`2019`,fpopulation$`2020`,fpopulation$`2021`),nrow = 19,ncol = 23)
#mmortality[is.na(mmortality)]=0
fmortality_rate<-matrix(c(fmortality$`1999`,fmortality$`2000`,fmortality$`2001`,fmortality$`2002`,fmortality$`2003`,fmortality$`2004`,fmortality$`2005`,fmortality$`2006`,fmortality$`2007`,fmortality$`2008`
                          ,fmortality$`2009`,fmortality$`2010`,fmortality$`2011`,fmortality$`2012`,fmortality$`2013`,fmortality$`2014`,fmortality$`2015`,fmortality$`2016`,fmortality$`2017`,fmortality$`2018`
                          ,fmortality$`2019`,fmortality$`2020`,fmortality$`2021`),nrow = 19,ncol = 23)


# Create demogdata object
mdata <- demogdata(mmortality_rate,mpopulation, age,year,type = "mortality",label = "China", name = "male")
fdata<-demogdata(fmortality_rate,fpopulation, age,year,type = "mortality",label = "China", name = "female")

#3d plot
# Set the color vector for the Morandi color palette
mondrian_colors <- c("#FF4040", "#FFBF00", "#9ACD32", "#4169E1", "#C71585", "#FF4500", "#FF69B4",
                     "#FFA500", "#FFD700", "#7CFC00", "#00FFFF", "#00CED1", "#8A2BE2", "#FF00FF",
                     "#FF1493", "#FF8C00", "#FFDAB9", "#1E90FF", "#BDB76B", "#FFA07A", "#DDA0DD",
                     "#20B2AA", "#7B68EE")
#Setting the x,y,z of the 3d plot of male mortality rates
mx<-age_groups
my<-year
mz<-c(mmortality$`1999`,mmortality$`2000`,mmortality$`2001`,mmortality$`2002`,mmortality$`2003`,mmortality$`2004`,mmortality$`2005`,mmortality$`2006`,mmortality$`2007`,mmortality$`2008`
      ,mmortality$`2009`,mmortality$`2010`,mmortality$`2011`,mmortality$`2012`,mmortality$`2013`,mmortality$`2014`,mmortality$`2015`
      ,fmortality$`2016`,mmortality$`2017`,mmortality$`2018`,mmortality$`2019`,mmortality$`2020`,mmortality$`2021`)
#Drawing 3d diagrams
plot3d(mx,my,mz,xlab = "Age", ylab = "Year", zlab = "Mortality Rate",col = mondrian_colors,
       size=8,type = 'p',cex.lab = 3)

#etting the x,y,z of the 3d plot of female mortality rates
fx<-age_groups
fy<-year
fz<-c(fmortality$`1999`,fmortality$`2000`,fmortality$`2001`,fmortality$`2002`,fmortality$`2003`,fmortality$`2004`,fmortality$`2005`,fmortality$`2006`,fmortality$`2007`,fmortality$`2008`
      ,fmortality$`2009`,fmortality$`2010`,fmortality$`2011`,fmortality$`2012`,fmortality$`2013`,fmortality$`2014`,fmortality$`2015`,fmortality$`2016`,fmortality$`2017`,fmortality$`2018`
      ,fmortality$`2019`,fmortality$`2020`,fmortality$`2021`)
#Drawing 3d diagrams
plot3d(fx,fy,fz,xlab = "Age", ylab = "Year", zlab = "Mortality Rate",col = mondrian_colors,
       size=8,type = 'p',cex.lab = 3)

# Transpose dataset
transposed_data <- t(mmortality)
t1<-as.data.frame(transposed_data)
colnames(t1)<-as.character(t1[1,])
t1<-t1[-1,]
t1<-as.data.frame(t1)
plot(year,t1$`0-4`,type = 'l',ylab = "mortality rate",main = "Age 0-4",cex.lab = 1.5)
#plot(year,t1$`20-24`,type = 'l',ylab = "mortality rate",main = "Age 20-24")
plot(year,t1$`40-44`,type = 'l',ylab = "mortality rate",main = "Age 40-44",cex.lab = 1.5)
#plot(year,t1$`60-64`,type = 'l',ylab = "mortality rate",main = "Age 60-64")
plot(year,t1$`80-84`,type = 'l',ylab = "mortality rate",main = "Age 80-84",cex.lab = 1.5)
#plot(year,t1$`90+`,type = 'l',ylab = "mortality rate",main = "Age 90+")

par(mar=c(5,7,5,7))
par(mgp=c(3,1,0))
plot(year,t1$`0-4`,type = 'l',ylab = "mortality rate",main = "Mortality Rate of Partial Age Groups",ylim=c(0,0.3),col=mondrian_colors[1],cex.lab = 1.5 )
#lines(year,t1$`5-9`,col="#FFDAB9")
#lines(year,t1$`10-14`,col="#7B68EE")
lines(year,t1$`55-59`,col=mondrian_colors[2] )
lines(year,t1$`60-64`,col=mondrian_colors[3] )
lines(year,t1$`65-69`,col=mondrian_colors[4] )
lines(year,t1$`70-74`,col=mondrian_colors[5] )
lines(year,t1$`75-79`,col=mondrian_colors[6] )
lines(year,t1$`80-84`,col=mondrian_colors[7] )
lines(year,t1$`85-89`,col=mondrian_colors[8] )
lines(year,t1$`90+`,col=mondrian_colors[9] )
#par(xpd=TRUE)
legend("right", legend=c("0-4", "55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+"), col=mondrian_colors[1:9] ,lty=c(1,1),yjust = 10,xpd=TRUE,inset = -0.3)

# Transpose dataset
transposed_data_2 <- t(fmortality)
t2<-as.data.frame(transposed_data_2)
colnames(t2)<-as.character(t2[1,])
t2<-t2[-1,]
t2<-as.data.frame(t2)
plot(year,t2$`0-4`,type = 'l',ylab = "mortality rate",main = "Age 0-4",cex.lab = 1.5)
#plot(year,t2$`20-24`,type = 'l',ylab = "mortality rate",main = "Age 20-24")
plot(year,t2$`40-44`,type = 'l',ylab = "mortality rate",main = "Age 40-44",cex.lab = 1.5)
#plot(year,t2$`60-64`,type = 'l',ylab = "mortality rate",main = "Age 60-64")
plot(year,t2$`80-84`,type = 'l',ylab = "mortality rate",main = "Age 80-84",cex.lab = 1.5)
#plot(year,t2$`90+`,type = 'l',ylab = "mortality rate",main = "Age 90+")

par(mar=c(5,7,5,7))
par(mgp=c(3,1,0))
plot(year,t2$`0-4`,type = 'l',ylab = "mortality rate",main = "Mortality Rate of Partial Age Groups",ylim=c(0,0.3),col=mondrian_colors[11],cex.lab = 1.5)
lines(year,t2$`55-59`,col=mondrian_colors[12] )
lines(year,t2$`60-64`,col=mondrian_colors[13] )
lines(year,t2$`65-69`,col=mondrian_colors[14] )
lines(year,t2$`70-74`,col=mondrian_colors[15] )
lines(year,t2$`75-79`,col=mondrian_colors[16] )
lines(year,t2$`80-84`,col=mondrian_colors[17] )
lines(year,t2$`85-89`,col=mondrian_colors[18] )
lines(year,t2$`90+`,col=mondrian_colors[19] )
legend("right", legend=c("0-4", "55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+"), col=mondrian_colors[11:19] ,lty=c(1,1),yjust = 10,xpd=TRUE,inset = -0.3)


# Fitting the Lee-Carter model
mlca <- lca(mdata,adjust = "dt")
summary(mlca)
plot(mlca)

flca <- lca(fdata)
summary(flca)
plot(flca)

#Trend of changes in the estimated parameter ax of the male and female mortality models
am<-data.frame(age,mlca$ax)
af<-data.frame(age,flca$ax)
age1<-c(0,20,40,60,80)
plot(am,type="l",lwd=2,col="red",ylim=c(-9,0),ylab="ax",xaxt="n",cex.lab = 2)
axis(1, at = age1, labels = c("0-4", "20-24", "40-44", "60-64", "80-84"))
lines(af,lwd=2,col="blue")
legend("top", legend=c("male", "female"), col=c("red","blue"),lty=c(1,1))

#Trend of changes in the estimated parameter bx of the male and female mortality models
bm<-data.frame(age,mlca$bx)
bf<-data.frame(age,flca$bx)
age1<-c(0,20,40,60,80)
plot(bm,type="l",lwd=2,col="red",ylim=c(0,0.15),ylab="bx",xaxt="n",cex.lab = 1.7)
axis(1, at = age1, labels = c("0-4", "20-24", "40-44", "60-64", "80-84"))
lines(bf,lwd=2,col="blue")
legend("top", legend=c("male", "female"), col=c("red","blue"),lty=c(1,1))

#Trend of changes in the estimated parameter kt of the male and female mortality models
km<-data.frame(year,mlca$kt)
kf<-data.frame(year,flca$kt)
plot(km,type="l",lwd=2,col="red",ylim=c(-20,15),ylab="kt",cex.lab = 2)
lines(kf,lwd=2,col="blue")
legend("top", legend=c("male", "female"), col=c("red","blue"),lty=c(1,1))


par(mfrow=c(2,1))
plot(x=mlca$age, y=log(mlca$male[,1]), type="l", lty=1,
     main="Observed vs fitted Lee Carter Model - 1999 and 2021 Male Mortality",)
lines(mlca$age, y=mlca$fitted$y[,1],
      col=1, lty=2)
legend("top", legend=c("Obs 2", "Pred 2021"), col=c(1,1,3,3), lty=c(1,2,1,2))

plot(x=flca$age, y=log(flca$female[,1]), type="l", lty=1,
     main="Observed vs fitted Lee Carter Model - 1999 and 2021 Female Mortality",)
lines(flca$age, y=flca$fitted$y[,1],
      col=1, lty=2)
legend("top", legend=c("Obs 2", "Pred 2021"), col=c(1,1,3,3), lty=c(1,2,1,2))


#ARIMA
#ADF 检验
f_adf_test <- adf.test(flca$kt)
print(f_adf_test)
m_adf_test <- adf.test(mlca$kt)
print(m_adf_test)

#acf plot and pacf plot
acf <- acf(flca$kt,lag.max=50)
#plot(acf)
pacf <- pacf(flca$kt,lag.max=50) 

acf <- acf(mlca$kt,lag.max=50)
#plot(acf)
pacf <- pacf(mlca$kt,lag.max=50) 

#Fit Arima_female
farima<-auto.arima(flca$kt,ic="aic")
tsdiag(farima)
Box.test(farima$residuals,type="Ljung-Box")
f_forecast.fit <- forecast(farima,h=20)
plot(f_forecast.fit)

#Fit Arima_male
marima<-Arima(mlca$kt,order=c(0,2,1))
tsdiag(marima)
Box.test(marima$residuals,type="Ljung-Box")
m_forecast.fit <- forecast(marima,h=20)
plot(m_forecast.fit)

#count forecasting mortality rate_female
fm_forecast_2022<-exp(af$flca.ax+f_forecast.fit$mean[1]*bf$flca.bx)
fm_forecast_2023<-exp(af$flca.ax+f_forecast.fit$mean[2]*bf$flca.bx)
fm_forecast_2024<-exp(af$flca.ax+f_forecast.fit$mean[3]*bf$flca.bx)
fm_forecast_2025<-exp(af$flca.ax+f_forecast.fit$mean[4]*bf$flca.bx)
fm_forecast_2026<-exp(af$flca.ax+f_forecast.fit$mean[5]*bf$flca.bx)
fm_forecast_2027<-exp(af$flca.ax+f_forecast.fit$mean[6]*bf$flca.bx)
fm_forecast_2028<-exp(af$flca.ax+f_forecast.fit$mean[7]*bf$flca.bx)
fm_forecast_2029<-exp(af$flca.ax+f_forecast.fit$mean[8]*bf$flca.bx)
fm_forecast_2030<-exp(af$flca.ax+f_forecast.fit$mean[9]*bf$flca.bx)
fm_forecast_2031<-exp(af$flca.ax+f_forecast.fit$mean[10]*bf$flca.bx)
prediction<-c(fm_forecast_2022)

plot(fm_forecast_2022,type = 'l',xlab="age",ylab="mortality rate",cex.lab=1.5)
lines(fm_forecast_2026,col="red")
lines(fm_forecast_2031,col="blue")
legend("top", legend=c("Predicted mortality in 2022", "Predicted mortality in 2026",
                       "Predicted mortality in 2031"), col = c("black","red","blue"),lty=c(1,1,1))

print(round(fm_forecast_2022,6))
print(round(fm_forecast_2023,6))

#count forecasting mortality rate_male
mm_forecast_2022<-exp(am$mlca.ax+m_forecast.fit$mean[1]*bm$mlca.bx)
mm_forecast_2023<-exp(am$mlca.ax+m_forecast.fit$mean[2]*bm$mlca.bx)
mm_forecast_2024<-exp(am$mlca.ax+m_forecast.fit$mean[3]*bm$mlca.bx)
mm_forecast_2025<-exp(am$mlca.ax+m_forecast.fit$mean[4]*bm$mlca.bx)
mm_forecast_2026<-exp(am$mlca.ax+m_forecast.fit$mean[5]*bm$mlca.bx)
mm_forecast_2027<-exp(am$mlca.ax+m_forecast.fit$mean[6]*bm$mlca.bx)
mm_forecast_2028<-exp(am$mlca.ax+m_forecast.fit$mean[7]*bm$mlca.bx)
mm_forecast_2029<-exp(am$mlca.ax+m_forecast.fit$mean[8]*bm$mlca.bx)
mm_forecast_2030<-exp(am$mlca.ax+m_forecast.fit$mean[9]*bm$mlca.bx)
mm_forecast_2031<-exp(am$mlca.ax+m_forecast.fit$mean[10]*bm$mlca.bx)

plot(mm_forecast_2022,type = 'l',xlab="age",ylab="mortality rate",cex.lab=1.5)
lines(mm_forecast_2026,col="red")
lines(mm_forecast_2031,col="blue")
legend("top", legend=c("Predicted mortality in 2022", "Predicted mortality in 2026",
                       "Predicted mortality in 2031"), col = c("black","red","blue"),lty=c(1,1,1))

mm_forecast_2022<-exp(am$mlca.ax+m_forecast.fit$mean[1]*bm$mlca.bx)
mm_forecast_2023<-exp(am$mlca.ax+m_forecast.fit$mean[2]*bm$mlca.bx)

print(round(mm_forecast_2022,6))
print(round(mm_forecast_2023,6))

f_forecast.fit$residuals


