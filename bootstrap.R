## Obtaining data from Excel ##

t <- read.table("clipboard")	# Copy a matrix from Excel files. 
				# Each raw consists of three numbers, CG, CHG, and CHH-methylated cytosine numbers in each sequenced clone.
				# You can find a test file in the same project folder. 


## Settings Required ##

sn <- 15			# Number of clones sequensed
pn <- c(8,8,37,53)		# total number of CG, CHG, CHH, and total cytosine in the sequenced region
n <- 1000			# number of bootstrap resampling


## Resampling for designated times ##

rvnt <- NULL
rvpt <- NULL
for (i in 1:n){
bv <- sample(1:sn,sn,replace=T)
rvn <- c(colSums(t[bv,]),sum(colSums(t[bv,])))
rvp <- rvn/pn/sn*100
rvnt <- rbind(rvnt,rvn)
rvpt <- rbind(rvpt,rvp)
}
vm <- c(mean(rvpt[,1]),mean(rvpt[,2]),mean(rvpt[,3]),mean(rvpt[,4]))
vv <- c(var(rvpt[,1]),var(rvpt[,2]),var(rvpt[,3]),var(rvpt[,4]))


## Results ##

vm				# calculated mean of methylated cytosine (%) for CG, CHG, CHH, and total cytosine
sqrt(vv)			# standard deviation (%)
