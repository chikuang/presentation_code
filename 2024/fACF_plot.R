library(FTSgof)

set.seed(5)
data(Spanish_elec)
rainbow3D(Spanish_elec) #3D rainbow plot 

# Spanish_elec_fd <- funData(argvals = 1:24, X= t(Spanish_elec))
# either Spanish_elec_fd or Spanish_elec should get the same output

fACF(Spanish_elec, H=20, alpha=0.05, wwn_bound=TRUE, M=NULL) #fACF plot

par(yaxt = "n", cex = 1, main = NULL)  # Suppress y-axis tick labels

# par(ylab = " ")

# Figure 3
# The first differenced Spanish daily electricity price profiles
SE_diff <- Spanish_elec[,2:dim(Spanish_elec)[2]]- Spanish_elec[,1:(dim(Spanish_elec)[2]-1)]
rainbow3D(SE_diff) #3D rainbow plot 
fACF(SE_diff, H=20, alpha=0.05, wwn_bound=FALSE, M=NULL) #fACF plot
SE_diff2 <- SE_diff
start <- 267
index <- start:(start+13)
SE_diff2[,index] <- 15*SE_diff2[,index] 
fACF(SE_diff2, H=20, alpha=0.05, wwn_bound=FALSE, M=NULL) #fACF plot

# Figure 4 (right)
#fSACF of the first differenced Spanish daily electricity price profiles
fSACF(SE_diff, H=20, alpha=0.05)
as.numeric(A)
