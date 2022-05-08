#' Parametric Bootstrap Test
#' @export
#' @param bdata data
#' @param R number of replications
#' @details function rmvnorm requires to install package mvtnorm

pbs=function(bdata, R=1000){
  x=bdata[,1]
  y=bdata[,2]
  r=cor(x,y)
  muhx=mean(x)
  muhy=mean(y)
  mu_v=c(muhx,muhy)
  vx=var(x)
  vy=var(y)
  var_m=matrix(c(vx,r*sqrt(vx)*sqrt(vy), r*sqrt(vx)*sqrt(vy), vy), ncol=2)# cov matrix
  n=length(x)
  cor_bs=double(R)
  for(i in 1:R){
    smp=rmvnorm(n, mu_v, var_m)
    cor_bs[i]=cor(smp[,1], smp[,2])
  }
  c(bias=mean(cor_bs)-r, se=sd(cor_bs), pvalue=sum(cor_bs>r)/R)
}
