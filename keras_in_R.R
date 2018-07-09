library(keras)
library(tensorflow)
mnist=dataset_mnist()
train_x=mnist$train[[1]]
train_y=mnist$train[[2]]
test_x=mnist$test[[1]]
test_y=mnist$test[[2]]
train_y=to_categorical(train_y)
test_y=to_categorical(test_y)
m1=keras_model_sequential()

system.time(
m1 %>% layer_dense(units=20,activation='relu',input_shape = 784) %>% layer_dense(units=10,activation='softmax')  %>%
compile(optimizer='adam',loss='categorical_crossentropy',metrics='accuracy') %>% fit(train_x_1,train_y,epochs = 10,batch_size=10,validation_split = 0.2)
)

train_x_1=train_x
dim(train_x_1)=c(60000,784)
dim(train_x_1)



train_x[1,,]

train_x_1[1,]
