library(glmnet)
library(MASS)

cross10 <- function(data,method='lasso',times=2) {
  last <- colnames(data)[ncol(data)]
  formula_full <- paste(last,' ~ ',paste(colnames(data)[1:(ncol(data)-1)],collapse=' + '),sep='')
  r <- NULL
  all_pred <- NULL
  for (j in 1:times) {
    #Create 10 groups and randomly shuffle
    groups <- cut(1:nrow(data),breaks=10,labels=F)
    groups <- sample(groups,length(groups),replace=FALSE)
    
    pred <- NULL
    for (i in 1:10) {
      train <- which(groups != i)
      train_set <- data[train,]
      test_set <- data[-train,]
      
      if (method == 'rrBLUP') {
      g <- mixed.solve(train_set$res,Z=train_set[,1:(ncol(train_set)-1)],K=NULL,SE=FALSE,return.Hinv=FALSE)
      p <- (as.matrix(test_set[,1:(ncol(test_set)-1)]) %*% as.matrix(g[["u"]]))[,1] + g[["beta"]]
      }
      if (method == 'ridge') {
      f <- lm.ridge(res ~ ., data = train_set, lambda = seq(0,1000,1))
      g <- lm.ridge(res ~ ., data = train_set, lambda = as.numeric(glance(f)[3]))
      p <- scale(test_set[,1:(ncol(test_set)-1)],center = F, scale = g$scales)%*% g$coef + coef(g)[1]
      p <- p[,1]
      }
      if (method == 'lasso') {
      test_set <- test_set[,-ncol(test_set)]
      fit <- cv.glmnet(as.matrix(train_set[,1:(ncol(train_set)-1)]),train_set[["res"]],family="gaussian",alpha=1)
      p <- predict(fit,newx=as.matrix(test_set), s="lambda.min")
      p <- p[,1]
      }
      if (method == 'gboost') {
      gbm1 <- gbm(as.formula(formula_full),# formula
          data=train_set,                    # dataset
	      distribution="gaussian",     # see the help for other choices
	          n.trees=500,                # number of trees
		      shrinkage=0.05,              # shrinkage or learning rate,
		      				     	       	  # 0.001 to 0.1 usually work
								      bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
								          train.fraction = 0.9,        # fraction of data for training,
									  		   	       	 # first train.fraction*N used for training
													     cv.folds = 5,                # do 3-fold cross-validation
													         keep.data=TRUE,              # keep a copy of the dataset with the object
														     verbose=FALSE,               # don't print out progress
														         n.cores=1)                   # use only a single core (detecting #cores is

															 best.iter <- gbm.perf(gbm1,method="cv")
															 p <- predict(gbm1,test_set,best.iter)
															 names(p) <- rownames(test_set)
      }
      #print(cor(p,data[names(p),]$res))
      pred <- c(pred,p) 
    }
    
    all_pred <- rbind(all_pred,pred[rownames(data)])
    rsq <- 1-(var(pred[rownames(data)]-data$res)/var(data$res))
    r <- c(r,rsq)
  }

  all_pred <- colMeans(all_pred)
  return(list(r,all_pred))
}

