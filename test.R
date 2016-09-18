df <- mpg
# getSummary(df, savefile=T, filedir='C:/BackUp')
file_dir <-'C:/BackUp'
outlier_rate <- 0.01
remove_outlier <- T
K <- 5

visualizeOnevar(df, file_dir=file_dir, outlier_rate=0.01, remove_outlier=T, K=5)



