#' Permutation Test
#' @export
#' @param bdata data
#' @param R number of replications
#' @examples perm(bdata=cars, R=1000)

perm=function(bdata, R=1000){
  x=bdata[,1]
  y=bdata[,2]
  z=c(x,y)
  n=length(x)
  totn=length(z)
  dif=mean(x)-mean(y)
  dif_bs=double(R)
  for(i in 1:R){
    smp=sample(z, replace=FALSE)
    xs=smp[1:n]
    ys=smp[(n+1):totn]
    dif_bs[i]=mean(xs)-mean(ys)
  }
  c(bias=mean(dif_bs)-dif, se=sd(dif_bs), pvalue=sum(dif_bs>dif)/R)
}
