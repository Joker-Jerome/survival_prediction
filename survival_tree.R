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
y <- y[, -1]
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
surv.lrn = makeLearner("surv.rpart")
surv.lrn$par.set
getParamSet(surv.lrn)

# tune the model 
# Create a task
turn_surv_task = makeSurvTask(id = "tree", data = y, target=c("survival","status"))
# Define the model
resamp = makeResampleDesc("CV", iters = 4L)
rdesc = makeResampleDesc("RepCV",folds=3L,reps=3L)

# Create the grid params
control.grid = makeTuneControlGrid() 
ps = makeParamSet(
    makeDiscreteParam("cp", values = seq(0.001, 0.006, 0.002)),
    makeDiscreteParam("minsplit", values = c(1, 5, 10, 50)),
    makeDiscreteParam("maxdepth", values = c(15, 20, 25, 30))
   
)

# Tune the model
tuned = tuneParams(surv.lrn, task = turn_surv_task, 
                   resampling = rdesc, 
                   control = control.grid, 
                   par.set = ps)

# show the results
tuned
param_list = list()
param_list$maxdepth = 30
param_list$minsplit = 1
param_list$cp = 0.005

# apply the new par
surv.lrn = makeLearner("surv.rpart", par.vals = param_list)

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
write.table(cindex_vec, "survival_tree_results.txt", quote = F, row.names = F)

plot_df <- data.frame(test_sample = 0:9, cindex = cindex_vec )
# plot
ggplot(plot_df, aes(x=test_sample, y=cindex, group=1)) + geom_line(linetype="dotted") + labs(title = "Survival Tree")

# sur
surv_tree_cindex <- cindex_vec
ggplot(cindex)) + geom_boxplot() + labs(title = "Survival Tree")
plotLearnerPrediction(surv.lrn, task = surv.task, subset = test_idx[[i]])

# Initalising the Benchmark study
set.seed(1)
svbnmrk=benchmark(svlearners,surv.task,rdesc)

bmrkdata=getBMRPerformances(svbnmrk, as.df = TRUE)

plotBMRRanksAsBarChart(svbnmrk)
#+scale_fill_manual(values=myfillcolors)



plotBMRBoxplots(svbnmrk)+aes(fill=learner.id)
#+coord_flip()
#+scale_fill_manual(values=myfillcolors,name="Learners")

ggplot(bmrkdata)+geom_path(aes(x=iter,y=cindex,color=learner.id),size=1,alpha=0.8)+facet_wrap(~learner.id,scales="free",ncol=2)
#+scale_color_manual(values=myfillcolors)
