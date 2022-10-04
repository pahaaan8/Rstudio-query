#/////////////////////////////////////#
#       UAS SIMULASI STATISTIKA       #
#   MUHAMMAD FARHAN PERWIRA NASUTION  #
#             1708108010021           #
#/////////////////////////////////////#

#power : uji T
#n adalah banyak data
#x adalah rata-rata nilai data
#sigma adalah simpangan baku data 
#alpha adalah 
#nosim adalah banyak perulangan

t=function(a=NULL,n,m,x,alpha,sigma,nosim=1000){  #input yang dimasukkan
  if(is.null(a)){
    n=n
    x = x
    sigma = sigma
  }
  else{
    n = length(a)
    x = mean(a)
    sigma = sd(a)
  }
    z = rnorm(nosim)
  df=n-1
  xsq=rchisq(nosim, df=df)
  t = qt(1-alpha,df)
  
  power = mean(z+sqrt(n)*(x-m)/sigma > t/sqrt(n-1)*sqrt(xsq))

  structure(list(n=n,rata.data=x,nilai.hipotesis=m, alpha=alpha, power=power)) #output yang di dapatkan
}

#ex
t(NULL,2,5,30,0.5,0.9)
