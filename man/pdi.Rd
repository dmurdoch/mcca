\name{pdi}
\alias{pdi}
\title{Calculate PDI Value}
\usage{
pdi(y, d, method="multinom", \dots)
}
\description{
compute the Polytomous Discrimination Index (PDI) value of two or three or four categories classifiers with an option to define the specific model or user-defined model.
}
\arguments{
\item{y}{The multinomial response vector with two, three or four categories. It can be factor or integer-valued.}
  \item{d}{The set of candidate markers, including one or more columns. Can be a data frame or a matrix; if the method is "prob", then d should be the probablity matrix.}
  \item{method}{Specifies what method is used to construct the classifier based on the marker set in d. Available option includes the following methods:"multinom": Multinomial Logistic Regression which is the default method, requiring R package nnet;"tree": Classification Tree method, requiring R package rpart;"svm": Support Vector Machine (C-classification and radial basis as default), requiring R package e1071;"lda": Linear Discriminant Analysis, requiring R package lda;"prob": d is a risk matrix resulted from any external classification algorithm obtained by the user.}
  \item{\dots}{Additional arguments in the chosen method's function.}
}
\details{
The function returns the PDI value for predictive markers based on a user-chosen machine learning method. Currently available methods include logistic regression (default), tree, lda, svm and user-computed risk values. This function is general since we can evaluate the accuracy for marker combinations resulted from complicated classification algorithms.


}
\value{
Returns an object of class "mcca.pdi". The PDI value of the classification using a particular learning method on a set of marker(s).

An object of class "mcca.pdi" is a list containing at least the following components:
  \item{call}{the matched call.}
  \item{measure}{the value of measure.}
  \item{table}{the category-specific value of measure.}
}
\references{
Li, J., Gao, M., D’Agostino, R. (2019). Evaluating Classification Accuracy for Modern Learning Approaches. Statistics in Medicine (Tutorials in Biostatistics). 38(13): 2477-2503.

Van Calster B, Vergouwe Y, Looman CWN, Van Belle
V, Timmerman D and Steyerberg EW. Assessing the
discriminative ability of risk models for more than two outcome
categories. European Journal of Epidemiology 2012; 27: 761 C
770.

Li, J., Feng, Q., Fine, J.P., Pencina, M.J., Van Calster, B. (2018). Nonparametric estimation and inference for polytomous discrimination index. Statistical Methods in Medical Research. 27(10): 3092—3103.
}
\author{
Ming Gao: gaoming@umich.edu

Jialiang Li: stalj@nus.edu.sg
}
\note{
Users are advised to change the operating settings of various classifiers since it is well known that machine learning methods require extensive tuning. Currently only some common and intuitive options are set as default and they are by no means the optimal parameterization for a particular data analysis. Users can put machine learning methods' parameters after tuning. A more flexible evaluation is to consider "method=prob" in which case the input d should be a matrix of membership probabilities with k columns and each row of d should sum to one.
}
\seealso{
\code{\link{hum}}
}
\examples{
str(iris)
data <- iris[, 3]
label <- iris[, 5]
pdi(y = label, d = data,method = "multinom")

## Call:
## pdi(y = label, d = data, method = "multinom")

## Overall Polytomous Discrimination Index:
##  0.9845333

## Category-specific Polytomous Discrimination Index:
##   CATEGORIES VALUES
## 1          1 1.0000
## 2          2 0.9768
## 3          3 0.9768

pdi(y = label, d = data,method = "tree")
pdi(y = label, d = data,method = "tree",control = rpart::rpart.control(minsplit = 200))

data <- data.matrix(iris[, 3])
label <- as.numeric(iris[, 5])
# multinomial
require(nnet)
# model
fit <- multinom(label ~ data, maxit = 1000, MaxNWts = 2000)
predict.probs <- predict(fit, type = "probs")
pp<- data.frame(predict.probs)
# extract the probablity assessment vector
head(pp)
pdi(y = label, d = pp, method = "prob")
}
\keyword{ PDI }
