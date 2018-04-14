# coxboost
# survival tree 
# library
library(mlr)
source("/Users/jerome/Projects/survival_prediction/pkb_survival_input.R")



# as.numeric
reformat_df <- function(df) {
    for (i in 2:ncol(df)) {
        tmp_len <- length(unique(df[,i]))
        if (tmp_len > 30) {
            df[,i] <- as.numeric(df[,i])
        } else if (tmp_len < 8) {
            df[,i] <- as.factor(df[,i])
        }
    }
    return(df)
}

x_matrix <- reformat_df(x_matrix)
res_df$sample <- as.character(res_df$sample)
res_df$status <- as.logical(2 - res_df$censor)   

# data split
i <- 1
# xtrain <- x_matrix[-test_idx[[i]], -1]
# xtest <- x_matrix[test_idx[[i]], -1]
# ytrain <- res_df[-test_idx[[i]], -1]
# ytest <- res_df[test_idx[[i]], -1]

x <- x_matrix[, -1]
y <- res_df[, -1]
# learner 
## learner list
svlearners=list(
    #makeLearner("surv.coxph"),
    #makeLearner("surv.cv.CoxBoost"),
    makeLearner("surv.rpart")
    #makeLearner("surv.glmnet"),
    #makeLearner("surv.randomForestSRC")
)
# survival task (status:	censoring status 1=censored, 2=dead)
surv.task = makeSurvTask(id = "Survival",data = y, target=c("survival","status"))

# learner details
#Slot $par.set is an object of class ParamSet containing the type of hyperparameters (e.g., numeric, logical), potential default values and the range of allowed values.
surv.lrn = makeLearner("surv.randomForestSRC")
surv.lrn$par.set
getParamSet(surv.lrn)

# tune the model 
# Create a task
tune_surv_task = makeSurvTask(id = "coxboost", data = y, target=c("survival","status"))
# Define the model
rdesc = makeResampleDesc("RepCV",folds=3L,reps=3L)

# Create the grid params
control.grid = makeTuneControlGrid() 
ps = makeParamSet(
    makeDiscreteParam("penalty", values = c(0.001, 0.01, 0.1, 1, 10, 50)),
    makeDiscreteParam("stepsize.factor", values = c(0.1, 1, 2))
    
)

# Tune the model
tuned = tuneParams(surv.lrn, task = tune_surv_task, 
                   resampling = rdesc, 
                   control = control.grid, 
                   par.set = ps)

# show the results
tuned
param_list = list()
param_list$penalty = 0.1
param_list$stepsize.factor = 1

# apply the new par
surv.lrn = makeLearner("surv.cv.CoxBoost", par.vals = param_list)

# training
cindex_vec <- c()
for (i in 1:k) {
    # training
    cat(paste("Running: test_label", i))
    cat("\n")
    mod = train(surv.lrn, surv.task, subset = train_idx[[i]])
    
    # predicting 
    task.pred = predict(mod, task = surv.task, subset = test_idx[[i]])
    
    # performances 
    cindex_vec <- c(cindex_vec, performance(task.pred))
}

mean(cindex_vec)
write.table(cindex_vec, "coxboost_results.txt", quote = F, row.names = F)

plot_df <- data.frame(test_sample = 0:9, cindex = cindex_vec )
# plot
ggplot(plot_df, aes(x=test_sample, y=cindex, group=1)) + geom_line(linetype="dotted") + labs(title = "Survival Tree")

# sur
coxboost_cindex <- cindex_vec