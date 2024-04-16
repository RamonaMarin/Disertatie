## 0. Preparation

rm(list = ls(all = TRUE))



library(ggplot2)
library(data.table)
library(igraph)
require(timeDate)
library(stringr)
library(graphics)
library(magick)
library(scales)

library(tidyverse)
library(zoo)
library(plotly)
library(dyn)
library(readxl)
library(caret)
library(fGarch)
library(stochvol)
library(pastecs)
library(vars)
library(tseries)
library(rms)
library(Hmisc)


options(digits=6)
#----------------------------------------START UPDATE----------------------------------------

wdir = "C:\\Users\\ramon\\Desktop\\FACULTATE\\MASTER\\DISERTATIE\\FRM RO\\FRM"
setwd(wdir)

load('BVB_16_04_2024_5%.RData')



p1<-ggplot(data = FRM,aes(x=date,y=frm))+
  labs(x = "Date", y="FRM@RO")+
  scale_x_date(date_breaks = "1 year" , date_labels = "%Y")+
  geom_line(color="blue")+
  
  annotate(geom="text", x=as.Date("2020-04-09"), y=0.035,
           label="2020 Covid-19 crisis", color="red") +
  annotate(geom="text", x=as.Date("2022-02-24"), y=0.025, 
           label="2022 War crisis", color="red") 
#annotate(geom="point", x=as.Date("2020-04-09"), y=0.1,
#size=20, shape=21, fill="transparent")

p1


########### plot FRM  ################
FRM<-FRM_index
FRM$date<-as.Date(FRM$date)


#FRM = FRM %>% na.locf()
FRM$risk_level <- (100 * ecdf(FRM_index$frm)(FRM_index$frm)) %>% round(digits = 2)
FRM$risk <- as.factor(ifelse(FRM$risk_level < 10, '1. Low risk',
                             ifelse(FRM$risk_level < 40, '2. General risk', 
                                    ifelse(FRM$risk_level < 70, '3. Elevated risk', 
                                           ifelse(FRM$risk_level < 90, '4. High risk', 
                                                  ifelse(FRM$risk_level <= 100, '5. Severe risk',0))))))

p<-ggplot(data = FRM,aes(x=date,y=frm,text=risk_level))+
  labs(x = "Date", y="FRM@RO")+
  scale_x_date(date_breaks = "1 year" , date_labels = "%Y")+
  scale_color_manual(values = c("1. Low risk" = "green",
                                "2. General risk"="blue",
                                "3. Elevated risk"="yellow",
                                "4. High risk"="orange",
                                "5. Severe risk"="red")) +
  geom_point(aes(color =risk),size=1)
ggplotly(p)



p1<-ggplot(data = FRM,aes(x=date,y=frm))+
  labs(x = "Date", y="FRM@RO")+
  scale_x_date(date_breaks = "1 year" , date_labels = "%Y")+
  geom_line(color="blue")+
  annotate(geom="text", x=as.Date("2020-04-09"), y=0.04, 
           label="2020 Covid-19 crisis", color="red") +
  
  annotate(geom="text", x=as.Date("2022-02-24"), y=0.025, 
           label="2022 War crisis", color="red")
#annotate(geom="point", x=as.Date("2020-04-09"), y=0.1,
#size=20, shape=21, fill="transparent")

p1


bet<-(as.data.frame(macro_return))[ c("BETI")]


data<-cbind(FRM,tail(bet, n =nrow(FRM)))

p2<-ggplot()+
  geom_line(data = data,aes(x=date,y=frm), color = "red")+
  geom_line(data=data,aes(x=date,y=BETI),  color = "blue")
p2

###GARCH model
install.packages("fGarch")
library(fGarch)

y=data$BETI
fit = garchFit(~garch(1,1),data=y,trace=F,include.mean=FALSE)  
data$vol<-fit@sigma.t



p3<-ggplot()+
  geom_line(data=data,aes(x=date,y= frm,colour="FRM@RO"))+
  geom_line(data=data,aes(x=date,y= vol,colour="GARCH (1,1) Volatility")) +
  scale_color_manual(name = "", values = c("FRM@RO" = "blue",
                                           "GARCH (1,1) Volatility" = "red"))+
  labs(x = "Date",y="")
p3

# Use VAR and Granger causality
install.packages("vars")
library(vars)

gr<-data[, c("frm", "vol")] 
p1ct<-VAR(gr, p=5, type = "both") 
p1ct 
summary(p1ct, equation="frm") 
plot(p1ct, names = "frm")

#Run Granger-causality test
causality(p1ct, cause = "frm")

causality(p1ct, cause = "vol")

model1<-lm(data=data, vol~frm)
summary(model1)

#Google Trends  model
google<-read_xlsx("C:\\Users\\ramon\\Desktop\\FACULTATE\\MASTER\\DISERTATIE\\FRM RO\\FRM\\Input\\RO\\criza.xlsx")

google$date=seq(as.Date("2015/1/1"), as.Date("2024/3/1"), by = "month")
google$month <- paste0(year(google$Date),         # Convert dates to monthly
                       "/",
                       month(google$Date))

FRM$month=paste0(year(FRM$date),         # Convert dates to monthly
                 "/",
                 month(FRM$date))


frm_monthly<- FRM %>%
  group_by(month) %>%
  summarise_at(vars(frm), list("frm" = mean))

data_google<-full_join(google,frm_monthly,by="month")

data_google<-drop_na(data_google)


p<-ggplot()+
  geom_line(data=data_google,aes(x=as.Date(Date),y= scales::rescale(frm, to=c(0,1)),colour="FRM@RO"),size=1)+
  geom_line(data=data_google,aes(x=as.Date(Date),y= scales::rescale(SVI, to=c(0,1)),colour="Google Trends SVI"
  ),size=1) +
  scale_color_manual(name = "", values = c("FRM@RO" = "blue",
                                           "Google Trends SVI" = "red"))+
  labs(x = "Date",y="")
p


gr<-data_google[, c("frm", "SVI")] 
p1ct<-VAR(gr, p=5, type = "both") 
p1ct 
summary(p1ct, equation="frm") 
plot(p1ct, names = "frm")

#Run Granger-causality test
causality(p1ct, cause = "frm")

causality(p1ct, cause = "SVI")

model1<-lm(data=data_google, SVI~frm)
summary(model1)



#Crisis model
gdp<-read_xlsx("C:\\Users\\ramon\\Desktop\\FACULTATE\\MASTER\\DISERTATIE\\FRM RO\\FRM\\Input\\RO\\gdp.xlsx")

gdp$date=seq(as.Date("2015/1/1"), as.Date("2023/12/1"), by = "quarter")

gdp$quarter <- paste0(year(gdp$date),         # Convert dates to quarterly
                      "/0",
                      quarter(gdp$date))

FRM$quarter=paste0(year(FRM$date),         # Convert dates to quarterly
                   "/0",
                   quarter(FRM$date))

frm_quarterly<- FRM %>%
  group_by(quarter) %>%
  summarise_at(vars(frm), list("frm" = mean))

data_crisis<-full_join(gdp,frm_quarterly,by="quarter")

data_crisis<-drop_na(data_crisis)


install.packages("rms")
library(rms)

model1<-lrm(technical_recession ~ frm,data=data_crisis)
print(model1)


model2<-lrm(technical_recession ~ Lag(frm,1),data=data_crisis)
print(model2)


model3<-lrm(technical_recession ~ Lag(frm,2),data=data_crisis)
print(model3)

model4<-lrm(technical_recession ~ Lag(frm,3),data=data_crisis)
print(model4)

##AIC
aic_value1 <- AIC(model1)
aic_value2 <- AIC(model2)
aic_value3 <- AIC(model3)
aic_value4 <- AIC(model4)

print(aic_value1)
print(aic_value2)
print(aic_value3)
print(aic_value4)


model5<-glm(technical_recession ~ Lag(frm,1),data=data_crisis, family=binomial())
print(model5)

model6<-glm(technical_recession ~ frm,data=data_crisis, family=binomial())
print(model6)

install.packages("pROC")
library(pROC)
predicted_probabilities <- predict(model6, type = "response")
roc_curve <- roc(data_crisis$technical_recession, predicted_probabilities)
# Afișează curba ROC
plot(roc_curve)
# Calculează AUC
auc(roc_curve)

##modelul ales in functie de criterii
model<-lrm(technical_recession ~ frm,data=data_crisis)
print(model)

model_glm<-glm(technical_recession ~ frm,data=data_crisis, family=binomial())
# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
prob <- predict(model_glm, type="response") # predicted values for model glm
prob <- predict(model, type="fitted")  # predicted values for model lrm
data_crisis$technical_recession<-as.factor(data_crisis$technical_recession)

table(data_crisis$technical_recession)

predicted<-as.factor(as.numeric(prob>0.5))
table(predicted)
# use caret and compute a confusion matrix
install.packages("caret")
library(caret)

#confusionMatrix(data = predicted, reference = tail(data_crisis,-1)$recession)
confusionMatrix(data = predicted, reference = data_crisis$technical_recession[1:nrow(data_crisis)])

#results<-as.data.frame(cbind(data_crisis$technical_recession[1:nrow(data_crisis)]$quarter,
#                           predicted,prob,data_crisis$technical_recession[1:nrow(data_crisis)]))

results <- as.data.frame(cbind(data_crisis$quarter,
                               predicted,
                               prob,
                               data_crisis$technical_recession))

print(results)
results<-results %>% 
  rename(
    Quarter = V1,
    "Technical Recession" = V4,
    "Predicted"=predicted
  )

results$`Predicted Recession`<-as.numeric(results$Predicted)-1
results$`Technical Recession`<-as.numeric(results$`Technical Recession`)-1
results$`Predicted Probability`<-as.numeric(results$prob)



library(lubridate)
results$Quarter<-parse_date_time(results$Quarter,orders = "%Y%q")
results$Quarter<-as.Date(results$Quarter)

p4<-ggplot()+
  geom_line(data=results,aes(x=Quarter,y= `Predicted Probability`,colour="Predicted Probability"),
            size=1)+
  geom_line(data=results,aes(x=Quarter,y= `Technical Recession`,colour="Technical Recession"),
            size=0.75) +
  geom_line(data=results,aes(x=Quarter,y= `Predicted Recession`,colour="Predicted Recession"),
            linetype="dotted",size=1.5) +
  scale_color_manual(name = "", values = c("Predicted Probability" = "blue",
                                           "Technical Recession" = "red",
                                           "Predicted Recession"="black"))+
  #scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  labs(x = "Date",y="")

p4

p5<-ggplot()+
  
  
  geom_line(data=results,aes(x=Quarter,y= `Technical Recession`,colour="Technical Recession"),
            size=0.75) +
  geom_line(data=results,aes(x=Quarter,y= `Predicted Probability`,colour="Predicted Probability"),
            linetype="dotted",size=2) +
  scale_color_manual(name = "", values = c("Predicted Probability" = "blue",
                                           "Technical Recession" = "red"))+
  #scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  labs(x = "Date",y="")

p5


#####
# 
# data$crisis=ifelse(data$BETI<quantile(data$BETI,0.05),1,0)
# table(data$crisis)
# data$crisis<-as.factor(data$crisis)
# 
# model2<-lrm(crisis ~frm,data=data)
# print(model2)
# 
# model3<-glm(crisis ~frm,data=data, family=binomial())
# print(model3)
# 
# 
# library(caret)
# # Use your model to make predictions, in this example newdata = training set, but replace with your test set    
# prob <- predict(model3, type="response") # predicted values
# 
# 
# 
# # use caret and compute a confusion matrix
# confusionMatrix(data = as.factor(as.numeric(prob>0.25)), reference = data$crisis)

## 6. Network
date_start_fixed = 20081008
date_end_fixed = 20081008
N0_fixed_net = which(gsub("-", "", names(FRM_history)) == date_start_fixed)
N1_fixed_net = which(gsub("-", "", names(FRM_history)) == date_end_fixed)
stock_main = "ROTLV.BX"

scale_net = 500

fig = image_graph(width = 1000, height = 1000, res = 96, bg = "transparent")
options(show.error.messages = FALSE)

for (t in N0_fixed_net:N1_fixed_net) try( {
  adj0 = read.csv(file=paste0(output_path, "/Adj_Matrices/Fixed/adj_matrix_", 
                              gsub("-", "", names(FRM_history)[t]), ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  
  adj0 = as.matrix(adj0)[1:J, 1:J] 
  adj0 = apply(adj0, 2, as.numeric)
  netw1 = graph_from_adjacency_matrix(adj0, mode = "directed", weighted = T)
  V(netw1)$color = ifelse(V(netw1)$name == stock_main, "orange", "lightgrey")
  colors = rep("Gray", alpha.f = .8, length(E(netw1)))
  colors = ifelse(head_of(netw1, E(netw1))$name == stock_main, 'blue', colors) #inflow
  colors = ifelse(tail_of(netw1, E(netw1))$name == stock_main, 'orange', colors) #outflow
  if (channel == "SP500") colnames(adj0) = colnames(adj0) %>% substr(1, nchar(colnames(adj0))-7)
  plot(netw1, layout = layout_in_circle, vertex.label = colnames(adj0), edge.width = 0.8, 
       edge.color = colors, edge.arrow.size = 0.9, edge.arrow.width = 1, 
       vertex.size = scale_net*FRM_individ_fixed[t, -1])
  title(xlab = paste0(FRM_index$date[t], "\n FRM@RO: ", round(FRM_index$frm[t], 5)), 
        cex.lab = 1.15, font.lab = 2, line = -0.5)
})
options(show.error.messages = TRUE)
dev.off()
# 
animation <- image_animate(fig, fps = 5)
image_write(animation, paste0(output_path, "/Network/Network_", date_start_fixed, "_", 
                              date_end_fixed, "_", channel, ".gif"))


## 6. Network macros
date_start_fixed = 20081008
date_end_fixed = 20081008
N0_fixed_net = which(gsub("-", "", names(FRM_history)) == date_start_fixed)
N1_fixed_net = which(gsub("-", "", names(FRM_history)) == date_end_fixed)
stock_main = "VIX"

scale_net = 500

fig = image_graph(width = 1000, height = 1000, res = 96, bg = "transparent")
options(show.error.messages = FALSE)

for (t in N0_fixed_net:N1_fixed_net) try( {
  adj0 = read.csv(file=paste0(output_path, "/Adj_Matrices/Fixed/adj_matrix_", 
                              gsub("-", "", names(FRM_history)[t]), ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  
  adj0 = as.matrix(adj0)[1:length(adj0), 1:length(adj0)] 
  adj0 = apply(adj0, 2, as.numeric)
  netw1 <-graph_from_adjacency_matrix(adj0, mode = "directed", weighted = T)
  
  V(netw1)$color = ifelse(V(netw1)$name == stock_main, "orange", "lightgrey")
  colors = rep("Gray", alpha.f = .8, length(E(netw1)))
  #colors = ifelse(head_of(netw1, E(netw1))$name == stock_main, 'blue', colors) #inflow
  colors = ifelse(tail_of(netw1, E(netw1))$name == stock_main, 'orange', colors) #outflow
  
  plot(netw1, layout = layout_in_circle, vertex.label = colnames(adj0), edge.width = 0.8, 
       edge.color = colors, edge.arrow.size = 0.9, edge.arrow.width = 1)
  title(xlab = paste0(FRM_index$date[t], "\n FRM@RO: ", round(FRM_index$frm[t], 5)), 
        cex.lab = 1.15, font.lab = 2, line = -0.5)
})
options(show.error.messages = TRUE)

# 

