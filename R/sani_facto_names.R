sani.facto.names<-function(MCA=mca.fr, quali.plus.rank=rank.qualisup.fr){
df<-MCA$call$X
#
data.frame(unlist(lapply(df[, -quali.plus.rank], FUN = function(x) length(levels(x)))))->dfb
ldfb<-list()
for(i in 1:(dim(dfb)[1])){rep(x = row.names(dfb)[i], times=dfb[i, 1])->ldfb[[i]]}
unlist(ldfb)->modafb
#
data.frame(unlist(lapply(df[, quali.plus.rank], FUN = function(x) length(levels(x)))))->dfk
ldk<-list()
for(i in 1:(dim(dfk)[1])){rep(x = row.names(dfk)[i], times=dfk[i, 1])->ldk[[i]]}
unlist(ldk)->modak
#
res<-list(modafb, modak)
return(res)
}
