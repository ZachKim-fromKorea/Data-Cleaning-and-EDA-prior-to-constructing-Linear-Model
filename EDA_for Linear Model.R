##```{r r_script introduction, include=FALSE}
cat("===================== EDA prior to Constructing Linear Model =====================","\n","\n")

categorical.var <- function(data){
	categorical.variable <- c()
	i <- 0
	while(i<ncol(data)){
	    i <- i + 1
	    if(is.factor(data[,i])==TRUE) {
	    categorical.variable <- c(categorical.variable,i)
	    }
	}
return(list("length"=length(categorical.variable),"variables"= colnames(data)[categorical.variable]))
}
numeric.var <- function(data){
	numeric.variable <- c()
	i <- 0
	while(i<ncol(data)){
	    i <- i + 1
	    if(is.numeric(data[,i])==TRUE) {
	    numeric.variable <- c(numeric.variable,i)
	    }
	}
return(list("length"=length(numeric.variable),"variables"= colnames(data)[numeric.variable]))
}
outlier <- function(data, x){
	y <- data[,x]
	IQR <- fivenum(y)[4]-fivenum(y)[2]
	outlier.y <- y>=(fivenum(y)[4] + 1.5*IQR)|y<=(fivenum(y)[2] - 1.5*IQR)
return(sum(outlier.y))
}
## function to calculate skewness
skewness <- function(data, x){
	y <- data[,x]
return(mean(((y-mean(y))/sqrt(var(y)))^3))
}
## function to calculate kurtosis
kurtosis <- function(data, x){
	y <- data[,x]
return(mean(((y-mean(y))/sqrt(var(y)))^4))
}

##```{r function to conduct Exploratory Data Analysis prior to constructing linear model}
lm.eda <- function(data = data){

check <- readline("Did you assign the result to an object? (Y / N) :")
if(check=="N"){stop("Plz assign the result to an object")}

## Setup _ set R repository & install packages
# set R repository
r = getOption("repos")
r["CRAN"] = "http://cran.seoul.go.kr"
options(repos = r)
rm(r)
# bring-in packages
cat("-----------------------------------------------------------------","\n")
if(!require(MASS)){
	cat("Install MASS","\n")
	install.packages("MASS")
	library(MASS)}else{
	cat("MASS loaded","\n")
}
if(!require(imputeMissings)){
	cat("Install imputeMissings","\n")
	install.packages("imputeMissings")
	library(imputeMissings)}else{
	cat("imputeMissings loaded","\n")
}
if(!require(car)){
	cat("Install car","\n")
	install.packages("car")
	library(car)}else{
	cat("car loaded","\n")
}
if(!require(gvlma)){
	cat("Install gvlma","\n")
	install.packages("gvlma")
	library(gvlma)}else{
	cat("gvlma loaded","\n")
}
if(!require(corrplot)){
	cat("Install corrplot","\n")
	install.packages("corrplot")
	library(corrplot)}else{
	cat("corrplot loaded","\n")
}


## Basic EDA_ introduction to data
cat("-----------------------------------------------------------------","\n",
	paste("n =", dim(data)[1], "p =", dim(data)[2]),"\n")

## Designate Response Variable
y_index <- readline("Enter the name or column index of Response Variable : ")
y <- data[,which(colnames(data)==y_index)]

## EDA_ Function to sort out Categorical Variable
categorical.var <- function(data){
	categorical.variable <- c()
	i <- 0
	while(i<ncol(data)){
	    i <- i + 1
	    if(is.factor(data[,i])==TRUE) {
	    categorical.variable <- c(categorical.variable,i)
	    }
	}
return(list("length"=length(categorical.variable),"variables"= colnames(data)[categorical.variable]))
}
print(categorical.var(data))
## EDA_ Function to sort out Numeric Variable
numeric.var <- function(data){
	numeric.variable <- c()
	i <- 0
	while(i<ncol(data)){
	    i <- i + 1
	    if(is.numeric(data[,i])==TRUE) {
	    numeric.variable <- c(numeric.variable,i)
	    }
	}
return(list("length"=length(numeric.variable),"variables"= colnames(data)[numeric.variable]))
}
print(numeric.var(data))
## EDA on predictor variable
cat("\n","============================ Predictors(X) ==========================","\n","\n",
	"* There are total", dim(data)[2]-1, "number of predictors","\n")

### 1) NAs of predictor X
X <- data[,-which(colnames(data)==y_index)]
## Columns with NA value
cat("--------------------------------------------------------------------","\n",
	"** Get rid of predictors with too many NAs **", "\n",
	"--------------------------------------------------------------------","\n",
	"\n","[[List of Predictors with NAs]] :","\n")
if(length(colnames(X)[colSums(is.na(X))!=0])==0){
	cat(" - Proportion of Missing Values for each variables are less than 50%","\n")
}else{
print(colnames(X)[colSums(is.na(X))!=0])
}

#--------------------------------------------------------------------------#
### Missing Values

## 1) Remove columns that has as much NAs as 50%
## function to calculate percentage of missing values
pctNA <- function(data, x){
	if(is.numeric(x)){
	return(sum(is.na(data[,x]))/dim(data)[1])
	}else{return(sum(is.na(data[,which(names(data)==x)]))/dim(data)[1])
	}
}

## Calculate percentge of NAs for each variables
NA.X <- c()
for(i in 1:dim(X)[2]){	
	NA.X[i] <- pctNA(X,i)
}
cat("\n"," [[Predictors that have as much NAs as 50%]] :","\n")

## remove predictors with too many NAs
if(length(names(X)[NA.X>=0.5])!=0){
	print(names(X)[NA.X>=0.5])
	X <- X[,-which(colnames(X)%in%names(X)[NA.X>=0.5])]
}else{
	cat("   - None","\n")
}

cat("   -",length(names(X)[NA.X>=0.5]),"Predictors above are being Removed due to too many Missing values","\n",
	"  -",dim(X)[2]-length(names(X)[NA.X>=0.5]),"Predictors Left","\n")

############################## 2) Imputation ###############################
cat("\n","--------------------------------------------------------------------","\n",
	"** Missing Value Imputation **", "\n",
	"--------------------------------------------------------------------","\n",
	"\n","[[List of Predictors with NAs]] :","\n")
print(colnames(X)[colSums(is.na(X))!=0])
cat("   *",length(colnames(X)[colSums(is.na(X))!=0]),"predictors need imputation","\n","\n",
	"Percentage of NAs :","\n")

if(length(colnames(X)[colSums(is.na(X))!=0])!=0){
	### number of NAs
	which.col <- colnames(X)[colSums(is.na(X))!=0]
	which.col <- which(colnames(X)%in%which.col)
	NA.X <- c()
	for(i in 1:length(which.col)){
		NA.X[i] <- pctNA(X,which.col[i])
		cat(paste(colnames(X)[which.col[i]],":", round(pctNA(X,which.col[i])*100,2),"%"),"\n")
	}

	## Use package "importMissings()"
	for(i in 1:length(which.col)){
		if(NA.X[i]<=0.2){	# 일단 NA 비율이 0.2 넘어가지 않는 변수에 대해 median 값으로 impute
		X[is.na(X[,which.col[i]]), which.col[i]] <- median(X[,which.col[i]], na.rm=TRUE)
		cat(colnames(X)[which.col[i]],"has been imputed by median/mode","\n")
		}
	}
	if(sum(NA.X[i]>0.2)!=0){  # NA 비율이 0.2 넘어가는 변수가 있을 경우 randomForest
		impute(X, method="randomForest")
		cat("Conduct imputation with randomForest","\n")
		}else{
		cat("All NAs have been imputed by median/mode","\n")
	}
cat("\n", "* End of Imputation","/",paste("n =",dim(X)[1], ", p=", dim(X)[2]),"\n")
}else{cat("NA does not exist -> end of imputation","\n")
}

#--------------------------------------------------------------------------#

cat("\n","--------------------------------------------------------------------","\n",
	"** Reducing Outliers **", "\n",
	"--------------------------------------------------------------------","\n")

### Outliers

######################### Combine Data First ############################
data <- cbind(y, X)	# first column is the response variable

## 1) eliminate outlier X with cook's distance

## if categorical variables have too many classes, exclude from conducting linear regresion
if(categorical.var(data)$length==0){
	cat("No categorical variables","\n")
}else{
	categorical.variable <- which(colnames(data)%in%categorical.var(data)$variables)
	length.levels <- c()
	for(i in 1:categorical.var(data)$length){
		length.levels[i] <- length(levels(data[,categorical.variable[i]]))
		cat(names(data)[categorical.variable[i]],"has",length(levels(data[,categorical.variable[i]])),"levels","\n")
	}
	cat("Remove",colnames(data)[categorical.variable[length.levels>10]]," -> Too many classes","\n")
	data <- data[,-(categorical.variable[length.levels>10])]
}
lm.fit1 <- lm(y~., data=data)
cat("--------------------------------------------------------------------","\n",
	"Adj R-squared prior to removing outliers :", round(summary(lm.fit1)$adj.r.squared,4),"\n")

## measure the cook's distance
cooksd <- cooks.distance(lm.fit1)
#X11(width=35, height=15)
#plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
#		abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
#		text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

## Bonferonni outlier Test
outliers <- outlierTest(lm.fit1)

cat("Bonferonni Outlier Test : lm.fit <- lm(y~., data)","\n")
print(outlierTest(lm.fit1)) # from library 'car', studentized residual이 큰값
cat("Number of Outliers : ", length(outliers$rstudent),"-> 'Remove'","\n")

# calculate studentized residuals
rstudent <- studres(lm.fit1) # from library MASS
remove.outlier <- head(order(rstudent, decreasing=TRUE), length(outliers$rstudent))

# remove outliers
data <- data[-remove.outlier,]

lm.fit2 <- lm(y~., data=data)
cat("Adj R-squared after removing outliers :", round(summary(lm.fit2)$adj.r.squared,4),
	"(",(summary(lm.fit2)$adj.r.squared-summary(lm.fit1)$adj.r.squared)/summary(lm.fit1)$adj.r.squared*100,"% increase)","\n")

####### Normalization_ Transformation to obtain symmetric distribution

## function to calculate number of outliers 
outlier <- function(data, x){
	y <- data[,x]
	IQR <- fivenum(y)[4]-fivenum(y)[2]
	outlier.y <- y>=(fivenum(y)[4] + 1.5*IQR)|y<=(fivenum(y)[2] - 1.5*IQR)
return(sum(outlier.y))
}
## function to calculate skewness
skewness <- function(data, x){
	y <- data[,x]
return(mean(((y-mean(y))/sqrt(var(y)))^3))
}
## function to calculate kurtosis
kurtosis <- function(data, x){
	y <- data[,x]
return(mean(((y-mean(y))/sqrt(var(y)))^4))
}

## calculate number of outliers and skewness
numeric.data <- data[,numeric.var(data)$variables]

##
col.outliers <- c()
col.skewness <- c()
col.kurtosis <- c()
for(i in 1:dim(numeric.data)[2]){
	col.outliers[i] <- outlier(numeric.data, i)
	col.skewness[i] <- round(skewness(numeric.data, i),4)
	col.kurtosis[i] <- round(kurtosis(numeric.data, i),4)
}
ref.data <- cbind(colnames(numeric.data), col.outliers, col.skewness, col.kurtosis)
ref.data <- as.data.frame(ref.data)

###### Return the results
return(list(eda.data = data, predictor.reference = ref.data))
}

cat(" 1) lm.eda(data) loaded" ,"\n",
	"2) categorical.var(data) loaded" ,"\n",
	"3) numeric.var(data) loaded" ,"\n",
	"4) outlier(data,x) loaded" ,"\n",
	"5) skewness(data,x) loaded" ,"\n",
	"6) kurtosis(data,x) loaded" ,"\n")