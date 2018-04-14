# packages
library(survival)
library(ggplot2)
library(plyr)

# read in the argument
# args <- commandArgs(trailingOnly = TRUE)
# cat(args, sep = "\n")

# input folder
input_dir <- "/Users/jerome/Projects/PKB2_development/data/simu_toy_survival_denoise/"
input_dir <- "/Users/jerome/Projects/PKB2_development/simulation_survival/Surv1_M50"
setwd(input_dir)

# reading
exp_file <- "./expression.txt"
cli_file <- "./clinical.txt"
res_file <- "./response.txt"
exp_df <- read.table(exp_file, sep = ",", header = T)
cli_df <- read.table(cli_file, sep = ",", header = T)
res_df <- read.table(res_file, sep = ",", header = T)

# cv label 
k <- 10
test_idx <- list()
train_idx <- list()
for (i in 0:(k-1)) {
    tmp_nam <- paste0("test_label", i, sep = "")
    tmp_vec <- as.character(read.table(paste0(tmp_nam, ".txt", sep = ""), header = F)[,1])
    #assign(tmp_nam, tmp_vec)
    test_idx[[i+1]] <- match(tmp_vec, res_df$sample)
    train_idx[[i+1]] <- setdiff(1:nrow(res_df), test_idx[[i+1]])
}

# merge two input matrices
x_matrix <- join(exp_df, cli_df, by = "sample")
for (i in 2:(ncol(exp_df))) {
    x_matrix[, i] <- as.numeric(x_matrix[, i])
}

res_df$survival <- as.numeric(res_df$survival)
res_df$sample <- as.character(res_df$sample)
res_df$status <- as.logical(2 - res_df$censor)   
y <- join(x_matrix, res_df, by = "sample")
y <- y[, -1]
