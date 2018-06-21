########### Neural network : LSTM

#### installing keras in R
# please comment the first and third line once the installation is done. 
install.packages("keras")
library(keras)
install_keras()

# importing data into R
##please change the path to the csv file below
data=read.csv("/Users/aditya357/Downloads/EURUSD_M1 dataset_201805.csv",header=T)

## Batch size and lookback
tsteps=100
batch_size=100

# splitting into training and test set
num_train=round(round(nrow(data)*0.75)/batch_size)*batch_size
train=data[1:num_train,]
test=data[(num_train+1):nrow(data),]

 
 
# generating tensors for LSTM
dim_train=floor((dim(train)[1])/batch_size)*batch_size
input_train=array(dim=c(dim_train-tsteps,tsteps,1))
output_train=array(dim=c(dim(train)[1]-tsteps,1))
for (j in seq(1,dim(input_train)[1])){
		input_train[j,,]<-array(train[(j:(j+tsteps-1)),5],dim=c(tsteps,1))
		output_train[j,]<-train[j+1,5]
}

##### making an LSTM model

model<-keras_model_sequential()
model%>%
layer_lstm(units=40,input_shape=c(tsteps,1),batch_size=batch_size,
return_sequences=F,stateful=T)%>%
layer_dense(units=1)

model%>%compile(loss="mae",optimizer="adam")
model

#### fitting the model for 20 epochs
epochs=20


model %>% fit(x=input_train, y=output_train,batch_size=batch_size,epochs=epochs,verbose=1,
shuffle=F)

### predicting on the training data
predict_train <- model %>% 
    predict(input_train, batch_size = batch_size) %>%
    .[,1]
   
### plotting the predicted and 
plot(predict_train,type="l",col="red",ylim=c(1.16,1.21))
lines(output_train, type="l",col="green")


## preparing the test data
test=data[(num_train+1-tsteps):nrow(data),]
if (dim(test)[1]%%tsteps !=0){
	req_pad=tsteps-dim(test)[1]%%tsteps+tsteps
	test=rbind(train[(dim(train)[1]-req_pad+1):dim(train)[1],],test)
}


input_test=array(dim=c(dim(test)[1]-tsteps,tsteps,1))
#input_test[1,,]<-input_train[dim(input_train)[1],,]
for (j in seq(1,dim(test)[1]-tsteps)){
		input_test[j,,]<-array(test[(j:(j+tsteps-1)),5],dim=c(tsteps,1))
}

if (dim(input_test)[1]%%batch_size !=0){
	slack=batch_size-(dim(input_test)[1]%%batch_size)
	
	
	sec=dim(input_test)[2]
	#pad=array(input_test[1:slack,,],dim=c(slack,sec,1))
	pad=array(input_train[(dim(input_train)[1]-slack):dim(input_train)[1],,],dim=c(slack,sec,1))
	input_test=abind(pad,input_test,along=1)
}

### predicting on the test data
predict_test <- model %>% 
    predict(input_test,batch_size=batch_size) %>%
    .[,1]

### plotting the test and 
actual_test=data[(num_train+1-tsteps):nrow(data),5]
predicted_vals=predict_test[(length(predict_test)-length(actual_test)+1):length(predict_test)]

### calculating the RMSE values
test_rmse_lstm=sqrt(mean((predicted_vals-actual_test)^2))
#test RMSE from LSTM
test_rmse_lstm
# train RMSE from LSTM
train_rmse_lstm=sqrt(mean((predict_train-output_train)^2))
train_rmse_lstm


############ ARIMA Forecast
library(tseries)
# please comment the line below once the installation is done.
install.packages("forecast")
library(forecast)
ts_data<-ts(data)

# splitting into training and test set
num_train=round(round(nrow(data)*0.75)/batch_size)*batch_size
train_ts=ts_data[1:num_train,]
test_ts=ts_data[(num_train+1):nrow(ts_data),]

#fitting arima model
mod_arima=auto.arima((train_ts[,5]))
mod_arima
mod1<-arima(train_ts[,5],order=c(0,1,1))
summary(mod1)

#getting the forecasts
forecast_arima<-forecast(mod1,h=length(actual_test))




########## Exponential moving average forecast


exp <- ses(train_ts[,5],length(actual_test), initial="simple")
plot(as.numeric(exp$mean),type="l",col="yellow",ylim=(c(1.15,1.18)))

## plots
lines(predicted_vals,type="l",col="red",ylim=range(data$close))
lines(actual_test, type="l",col="green")
lines(as.numeric(forecast_arima$mean),type="l",col="blue")
legend("topright", legend=c("LSTM", "actual","ARIMA","EWMA"),
       col=c("red", "green","blue","yellow"), lty=1:2, cex=0.8)










