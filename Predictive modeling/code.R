# Selecting the variables that need to be 'miced' 
namenacol <-c('LotFrontage', 'MasVnrType', 'MasVnrArea', 'MSZoning', 'Utilities' , 'BsmtFullBath', 'BsmtHalfBath'   , 'Functional', 'Exterior1st', 'Exterior2nd' , 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 'Electrical', 'KitchenQual', 'GarageCars', 'GarageArea', 'SaleType')   
full_m <- full[namenacol]  
# Do mice  
require(mice)  
imp.full <- mice(full_m, m=1, method='cart', printFlag=FALSE)  
full_imp <- complete(imp.full)  

arageArea ~ .,
                    data = full[!is.na(full$GarageArea),col.pred],
                    method = "anova",
                    na.action=na.omit)

full$GarageArea[is.na(full$GarageArea)] <- round(predict(area.rpart, full[is.na(full$GarageArea),col.pred]))

# Add up area of 1st floor and 2nd floor to get total floor area.  
full$FloorArea <- full$X1stFlrSF+full$X2ndFlrSF  
# Add up area of 1st floor, 2nd floor, low quality finshed area and above ground living area to get total living area.  
full$AllLivArea <- full$X1stFlrSF+full$X2ndFlrSF+full$LowQualFinSF+full$GrLivArea  
# Add up numbers of full bathrooms and 0.5* half bathrooms to get total bathroom.   
full$totalbath <- full$BsmtFullBath+0.5*full$BsmtHalfBath+full$FullBath+0.5*full$HalfBath  
# Add up all the porch area to get total porch area.  
full$totalPorchArea <- full$OpenPorchSF+full$EnclosedPorch+full$X3SsnPorch+full$ScreenPorch  
boruta.train <- Boruta(SalePrice~., data = train_1, doTrace = 2)
 # get rid of Id, which is nothing but noise.
full_noid <- full[,-1]  
 # get the names of numeric variables (There is an easier way using sapply function)
numnames <-  c('MSSubClass','LotArea','OverallQual','OverallCond','YearBuilt','YearRemodAdd','X1stFlrSF','X2ndFlrSF','LowQualFinSF','GrLivArea','FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr','TotRmsAbvGrd','Fireplaces','WoodDeckSF','OpenPorchSF','EnclosedPorch','X3SsnPorch','ScreenPorch','PoolArea','MiscVal','MoSold','YrSold','FloorArea','AllLivArea','OverallRate','totalPorchArea','LotFrontage','MasVnrArea','BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath','GarageCars','GarageArea','subtractYearBuilt','subtractYearRemodAdd','subtractYrSold','totalbath')  
full_s <- full_noid[numnames]  
# Scale all the numeric data except saleprice  
full_ss <- apply(full_s, 2, function(x) scale(x))  
# Dummized all the character variables  
dummy_full <- model.matrix(~MSSubClass+LotFrontage+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+BsmtFullBath+BsmtHalfBath+GarageCars+GarageArea+subtractYearBuilt+subtractYearRemodAdd+subtractYrSold+totalbath+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+Fireplaces+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold+SalePrice+FloorArea+AllLivArea+OverallRate+totalPorchArea+MSZoning+Street+Alley+LotShape+LandContour+Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+Heating+HeatingQC+CentralAir+Electrical+KitchenQual+Functional+FireplaceQu+GarageType+GarageYrBlt+GarageFinish+GarageQual+GarageCond+PavedDrive+PoolQC+Fence+MiscFeature+SaleType+SaleCondition+SeasonSold-1,full_noid)  
require(randomForest)  
rf <- randomForest(SalePrice~.,train_1,do.trace=TRUE)  
prf <- predict(rf,test_1

# take log for SalePrice to make it more normal
train$SalePrice <- log(label_df$SalePrice)
library(caret)
library(plyr)
library(xgboost)
library(Metrics)
library(glmnet)
# Create custom summary function in proper format for caret
custom_summary <- function(data, lev = NULL, model = NULL){
        out = rmsle(data[, "obs"], data[, "pred"])
        names(out) = c("rmse")
        out
}

# Create control object
control <- trainControl(method = "cv",  # Use cross validation
                       number = 5,     # 5-folds
                       summaryFunction = custom_summary                      
)
set.seed(12)  
  
# Lasso
fit.glmnet <- train(SalePrice~., 
                    data=train, 
                    method="glmnet", 
                    metric="RMSE", 
                    preProc=c("center", "scale"), 
                    #tuneGrid = expand.grid(.alpha=c(0.008,0.01,0.03),.lambda=c(65500,65600,65700)),
                    trControl=control,
                    maximize = FALSE)
print(fit.glmnet)
pl<- predict(fit.glmnet, test)
epl <- exp(pl)

grid = expand.grid(nrounds=c(100, 200, 400, 800), # Test 4 values for boosting rounds
                   max_depth= c(4, 6),           # Test 2 values for tree depth
                   eta=c(0.1, 0.05, 0.025),      # Test 3 values for learning rate
                   gamma= c(0.1), 
                   colsample_bytree = c(1), 
                   min_child_weight = c(1),
                   subsample =0.5 )
set.seed(12)
xgb_tree_model =  train(SalePrice~., data=train, method="xgbTree", trControl=control,  tuneGrid=grid, metric="rmse", maximize = FALSE) 
px<- predict(xgb_tree_model, test)
epx <- exp(px)  
  
# take average of these three models
ept <- (epx+epl+prf)/3
