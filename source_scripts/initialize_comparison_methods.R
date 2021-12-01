# --- Description --------------------------------------------------------------
# Functions used by OMGA and DKAT


# --- Kernel U-test ------------------------------------------------------------
# Test single response variable and return P-value
# Implementation of test from:
#  Tao He, Ping-Shou Zhong, Yuehua Cui and Vidyandhar Mandrekar.
#  "Unified Tests for Nonparametric Functions in RKHS with Kernel Selection and
#  Regularization." Statistica Sinica (Accepted). DOI: 10.5705/ss.202020.0339
kernelUTestAsymptotic <- function(residuals, y_variance, kernel_matrix)
{
  n <- length(residuals)
  kernel_matrix_diag0 <- kernel_matrix
  diag(kernel_matrix_diag0) <- rep(0, n)
  centering_matrix <- diag(n) - matrix(1, n, n) / n
  hk0 <- centering_matrix %*% kernel_matrix_diag0
  hk0hk0 <- hk0 %*% hk0
  hk0h <- hk0 %*% centering_matrix
  squared_trace_hk0 <- sum(diag(hk0)) * sum(diag(hk0))
  trace_hk0hk0 <- sum(diag(hk0hk0))
  trace_hk0h_hadamard <- sum(diag(hk0h * hk0h))
  test_stat_variance <-
    ((2 - 12 / (n - 1)) * trace_hk0hk0 - (2 / n) * squared_trace_hk0 +
       (mean((residuals/sqrt(y_variance)) ^ 4) - 3) *
       ((6 / n) * trace_hk0hk0 - (1 / n) * squared_trace_hk0 +
          trace_hk0h_hadamard)) / (n - 1) ^ 2
  kernel_first_moment <- sum(diag(kernel_matrix)) / n
  satterthwaite_scale <- test_stat_variance / (2 * kernel_first_moment)
  satterthwaite_df <- kernel_first_moment / satterthwaite_scale
  test_statistic_normalized <-
    t(residuals) %*% kernel_matrix_diag0 %*% (residuals) / y_variance / (n - 1)
  return(pchisq(
    q = (test_statistic_normalized + kernel_first_moment) / satterthwaite_scale,
    df = satterthwaite_df,
    lower.tail = FALSE))
}

# --- OMGA ---------------------------------------------------------------------
#  Copyright: Yamin Deng, 2020
#  github.com/yamin-19/OMGA
#Inputs: P.values is a vector of single-trait p-values; Pheno is a trait matrix
combinePvaluesFisher <- function(P.values, Pheno,
                           kendall.only=FALSE,miss.pval=FALSE,
                           verbose=FALSE){

  log.P.values <- log(P.values)

  PAR <- cal.gamma.par(Pheno,kendall.only,verbose)
  SHAPE <- PAR$SHAPE
  SCALE <- PAR$SCALE

  T <- -2 * sum(log.P.values)
  res <- pgamma(T, shape=SHAPE, scale=SCALE, lower.tail=FALSE)

  if(miss.pval && is.na(res)){
    idx <- which(is.na(res))
    for (i in idx){
      ##cat("i = ",i,"\n")
      x <- log.P.values[i,]
      y <- x[!is.na(x)]
      if(length(y) == 1){
        res[i] <- exp(y)
      } else {
        T <- -2*sum(y)
        PAR <- cal.gamma.par(Pheno[,!is.na(x)],kendall.only,verbose)
        SHAPE <- PAR$SHAPE
        SCALE <- PAR$SCALE
        res[i] <- pgamma(T, shape=SHAPE, scale=SCALE, lower.tail=FALSE)
      }
    }
  }
  return(res)
}

### utilities
adj.cor <- function(r,n){
  fac <- (1-r^2) / (2*(n-3))
  res <- r * (1 + fac)
  return(res)
}
cal.cor.pearson <- function(x,y){
  r <- cor(x,y) #,use="pairwise.complete.obs")
  return(r)
}
### internal functions
cal.cor.kendall <- function(x,y){
  tau <- cor(x,y, method="kendall")
  ## use="pairwise.complete.obs")
  ## tau <- cor.fk(x)
  r <- sin(pi*tau/2)
  return(r)
}


cal.tetrachoric <- function(x,y){
  obj <- table(x,y)
  if(nrow(obj) != 2 | ncol(obj) != 2){
    res <- NA
  }else{
    if(obj[1,1] ==0 && obj[2,2]==0){
      res <- -1
    }else if(obj[1,2]==0 & obj[2,1]==0){
      res <- 1
    }else{
      idx <- obj == 0
      if(any(idx)){
        obj[idx] <- 0.5
      }
      obj <- obj/sum(obj)
      p11 <- obj[1,1]
      p12 <- obj[1,2]
      p21 <- obj[2,1]
      p22 <- obj[2,2]

      pr <- p11 + p12
      pc <- p11 + p21
      h <- qnorm(1-pr)
      k <- qnorm(1-pc)
      make.score.eq <- function(h,k,p11){
        function(r){
          mvtnorm::pmvnorm(lower=c(h,k),
                           cor=matrix(c(1,r,r,1),2,2)) - p11
        }
      }
      score.eq <- make.score.eq(h,k,p11)
      res <- uniroot(score.eq,c(-1,1))$root
    }
  }
  return(res)
}
cal.r.BR <- function(x.bin, y.cont){
  x.bin <- as.numeric(factor(x.bin)) - 1
  if(length(unique(x.bin)) != 2){
    warning("Neither x nor y is binary variable. Use Kendall tau to calculate correlation")
    res <- cal.cor.kendall(x.bin, y,cont)
  }else{
    x.bar <- mean(x.bin)
    y.bar <- mean(y.cont)
    n <- length(x.bin)
    fac <- n * x.bar * y.bar
    nom <- sum(x.bin * y.cont) - fac
    den <- sum(sort(x.bin) * sort(y.cont)) - fac
    res <- nom/den
  }
  return(res)
}

cal.r.Lord <- function(x.bin, y.cont){
  res <- cal.r.BR(x.bin, y.cont)
  if(res < 0) res <- - cal.r.BR(x.bin, -y.cont)
  return(res)
}

biserial <- function(x,y){
  ## x: binary 0 and 1;
  ## y: continuous
  res <- cal.r.Lord(x.bin=x, y.cont=y)
  return(res)
}

polyserial <- function(x,y,ML=FALSE){
  ## x: ordinal
  ## y: continuous
  x.tab <- table(x)
  N <- sum(x.tab)
  x.cut <- qnorm(cumsum(x.tab)/N)
  x.cut <- x.cut[-length(x.cut)]
  res <- sqrt((N - 1)/N)*sd(x)*cor(x, y)/sum(dnorm(x.cut))
  if(res> 1) res <- 1
  if(res< -1) res <- -1
  return(res)
}
polychoric <- function(x,y,ML=FALSE){
  ## x: oridinal
  ## y: oridinal
  obj <- table(x,y)
  if(nrow(obj) < 2 | ncol(obj) < 2){
    res <- NA
  }else{
    if(FALSE){## Add 0.5 to 0 cell
      idx <- obj == 0
      if(any(idx)){
        obj[idx] <- 0.5
      }
    }
    R <- nrow(obj)
    C <- ncol(obj)
    tab <- obj/sum(obj)
    row.cut <- qnorm(cumsum(rowSums(obj))/sum(obj))[-R]
    col.cut <- qnorm(cumsum(colSums(obj))/sum(obj))[-C]
    make.like.fn <- function(obj,row.cut, col.cut){
      R <- nrow(obj)
      C <- ncol(obj)
      lo <- - Inf
      up <- Inf
      row.cut1 <- c(lo,row.cut,up)
      col.cut1 <- c(lo,col.cut,up)
      function(rho){
        COR <- matrix(c(1,rho,rho,1),2,2)
        PHI <- matrix(0,R,C)
        for (i in 1:R){
          a1 <- row.cut1[i]
          a2 <- row.cut1[i+1]
          for (j in 1:C){
            b1 <- col.cut1[j]
            b2 <- col.cut1[j+1]
            PHI[i,j] <- mvtnorm::pmvnorm(lower=c(a1,b1),
                                         upper=c(a2,b2),
                                         cor=COR)
          }
        }
        res <- - sum(obj* log(PHI))
        return(res)
      }
    }
    like.fn <- make.like.fn(obj,row.cut,col.cut)
    res <- optimize(like.fn,c(-1, 1))$minimum
  }
  return(res)
}


detect.type <- function(x){
  res <- "NULL"
  if(is.null(x)){
    res <- "NULL"
  }else{
    val <- unique(x)[!is.na(unique(x))]
    len <- length(val)
    if(len == 2){
      res <- "dichotomous"
    }else if (len > 2 & len <= 5){
      res <- "ordinal"
    }else{
      res <- "continuous"
    }
  }
  return(res)
}

mixed.cor <- function(x, y,kendall.only=FALSE,verbose=FALSE){
  idx <- complete.cases(x, y)
  x <- x[idx]
  y <- y[idx]

  num.x <- length(unique(x))
  num.y <- length(unique(y))
  if(kendall.only){
    x.type <- "continuous"
    y.type <- "continuous"
    x <- as.numeric(x)
    y <- as.numeric(y)
  }else{
    x.type <- detect.type(x)
    y.type <- detect.type(y)
  }
  if(x.type == "dichotomous" & y.type=="continuous"){ # biserial
    if(verbose) message("biserial:")
    x <- as.numeric(factor(x)) - 1
    res <- biserial(x,y)
  }else if(y.type == "dichotomous" & x.type == "continuous"){ # biserial
    if(verbose) message("biserial:")
    y <- as.numeric(factor(y)) - 1
    res <- biserial(y,x)
  }else if(x.type == "dichotomous" & y.type == "dichotomous"){ # tetrachoric
    if(verbose) message("tetrachoric:")
    x <- as.numeric(factor(x)) - 1
    y <- as.numeric(factor(y)) - 1
    res <- cal.tetrachoric(x,y)
  }else if(x.type == "ordinal" & y.type == "continuous"){ # polyserial
    if(verbose) message("polyserial:")
    x <- as.numeric(factor(x))
    res <- polyserial(x,y)
  }else if(y.type == "ordinal" & x.type == "continuous"){ # polyserial
    if(verbose) message("polyserial:")
    y <- as.numeric(factor(y))
    res <- polyserial(y,x)
  }else if((x.type == "ordinal" & y.type == "ordinal") |
           (x.type == "dichotomous" & y.type == "ordinal") |
           (x.type == "ordinal" & y.type == "dichotomous")){ # polychoric
    if(verbose) message("polychoric:")
    x <- as.numeric(factor(x))
    y <- as.numeric(factor(y))
    res <- polychoric(x,y)
  }else{ # if(x.type == "continuous" & y.type == "continuous"){ # kendall
    if(verbose) message("kendall correlation\n")
    res <- cal.cor.kendall(x,y)
  }
  return(res)
}
cal.cor.v <- function(obj,kendall.only=FALSE,verbose=FALSE){
  m <- ncol(obj)
  res <- rep(NA, m*(m-1)/2)
  k <- 1
  for (i in 1:(m-1)){
    for (j in (i+1):m){
      res[k] <- mixed.cor(obj[,i],obj[,j],kendall.only,
                          verbose=verbose)
      k <- k + 1
    }
  }
  return(res)
}

cal.gamma.par <- function(Pheno,kendall.only=FALSE,verbose=FALSE){
  a1 <- 3.9081
  a2 <- 0.0313
  a3 <- 0.1022
  a4 <- -0.1378
  a5 <- 0.0941

  n <- nrow(Pheno)
  K <- ncol(Pheno)

  rho.v <- cal.cor.v(Pheno,kendall.only,verbose)
  rho.adj <- adj.cor(rho.v, n)


  vT <- 4*K + 2* sum(
    a1 * (rho.adj^2) +
      a2 * (rho.adj^4) +
      a3 * (rho.adj^6) +
      a4 * (rho.adj^8) +
      a5 * (rho.adj^10) -
      a1/n*(1-rho.adj^2)^2
  )
  ET <- 2*K
  v <- 2*ET^2/vT
  r <- vT/(2*ET)
  return(list(SHAPE=v/2,SCALE=2*r))
}



# --- DKAT ---------------------------------------------------------------------
#  Copyright: Xiang Zhan, 2017
#  github.com/xyz5074/DKAT

# L is phenotype matrix, K is genotype matrix
DKAT <-
  function(K,L){
    tr=function(x){return(sum(diag(x)))}
    n=nrow(K)
    I.n=diag(1,n)
    I.1=rep(1,n)
    H=I.n-I.1%*%t(I.1)/n
    K=H%*%K%*%H
    L=H%*%L%*%H
    A=K/tr(K%*%K)  ## standard-version of K
    W=L/tr(L%*%L)

    Fstar=tr(A%*%W)
    mean.krv=tr(A)*tr(W)/(n-1)## mean of DKAT

    T=tr(A);T2=tr(A%*%A);S2=sum(diag(A)^2)
    Ts=tr(W);T2s=tr(W%*%W);S2s=sum(diag(W)^2)
    temp1=2*((n-1)*T2-T^2)*((n-1)*T2s-Ts^2)/(n-1)^2/(n+1)/(n-2)
    temp21=n*(n+1)*S2- (n-1)*(T^2+2*T2)
    temp22=n*(n+1)*S2s- (n-1)*(Ts^2+2*T2s)
    temp23=(n+1)*n*(n-1)*(n-2)*(n-3)
    temp2=temp21*temp22/temp23
    variance.krv=temp1+temp2## variance of DKAT

    T3=tr(A%*%A%*%A);S3=sum(diag(A)^3);U=sum(A^3);R=t(diag(A))%*%diag(A%*%A);B=t(diag(A))%*%A%*%diag(A)
    T3s=tr(W%*%W%*%W);S3s=sum(diag(W)^3);Us=sum(W^3);Rs=t(diag(W))%*%diag(W%*%W);Bs=t(diag(W))%*%W%*%diag(W)
    t1=n^2*(n+1)*(n^2+15*n-4)*S3*S3s
    t2=4*(n^4-8*n^3+19*n^2-4*n-16)*U*Us
    t3=24*(n^2-n-4)*(U*Bs+B*Us)
    t4=6*(n^4-8*n^3+21*n^2-6*n-24)*B*Bs
    t5=12*(n^4-n^3-8*n^2+36*n-48)*R*Rs
    t6=12*(n^3-2*n^2+9*n-12)*(T*S2*Rs+R*Ts*S2s)
    t7=3*(n^4-4*n^3-2*n^2+9*n-12)*T*Ts*S2*S2s
    t81=(n^3-3*n^2-2*n+8)*(R*Us+U*Rs);t82=(n^3-2*n^2-3*n+12)*(R*Bs+B*Rs)
    t8=24*(t81+t82)
    t9=12*(n^2-n+4)*(T*S2*Us+U*Ts*S2s)
    t10=6*(2*n^3-7*n^2-3*n+12)*(T*S2*Bs+B*Ts*S2s)
    t11=-2*n*(n-1)*(n^2-n+4)*((2*U+3*B)*S3s+(2*Us+3*Bs)*S3)
    t12=-3*n*(n-1)^2*(n+4)*((T*S2+4*R)*S3s+(Ts*S2s+4*Rs)*S3)
    t13=2*n*(n-1)*(n-2)*((T^3+6*T*T2+8*T3)*S3s+(Ts^3+6*Ts*T2s+8*T3s)*S3)
    t14=T^3*((n^3-9*n^2+23*n-14)*Ts^3+6*(n-4)*Ts*T2s+8*T3s)
    t15=6*T*T2*((n-4)*Ts^3+(n^3-9*n^2+24*n-14)*Ts*T2s+4*(n-3)*T3s)
    t16=8*T3*(Ts^3+3*(n-3)*Ts*T2s+(n^3-9*n^2+26*n-22)*T3s)
    t17=-16*(T^3*Us+U*Ts^3)-6*(T*T2*Us+U*Ts*T2s)*(2*n^2-10*n+16)
    t18=-8*(T3*Us+U*T3s)*(3*n^2-15*n+16)-(T^3*Bs+B*Ts^3)*(6*n^2-30*n+24)
    t19=-6*(T*T2*Bs+B*Ts*T2s)*(4*n^2-20*n+24)-8*(T3*Bs+B*T3s)*(3*n^2-15*n+24)
    t201=24*(T^3*Rs+R*Ts^3)+6*(T*T2*Rs+R*Ts*T2s)*(2*n^2-10*n+24)
    t202=8*(T3*Rs+R*T3s)*(3*n^2-15*n+24)+(3*n^2-15*n+6)*(T^3*Ts*S2s+T*S2*Ts^3)
    t203=6*(T*T2*Ts*S2s+Ts*T2s*T*S2)*(n^2-5*n+6)+48*(T3*Ts*S2s+T3s*T*S2)
    t20=-(n-2)*(t201+t202+t203)
    temp31=t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+t16+t17+t18+t19+t20
    temp32=n*(n-1)*(n-2)*(n-3)*(n-4)*(n-5)
    mom3=temp31/temp32
    skewness.krv= (mom3-3*mean.krv*variance.krv-mean.krv^3)/variance.krv^1.5 ## skewness of DKAT

    m1=mean.krv
    m2=variance.krv
    m3=skewness.krv
    shape=4/m3^2
    scale=sqrt(m2)*m3/2
    location=m1-2*sqrt(m2)/m3
    PIIIpars=list(shape,location,scale)
    pv=1-ppearsonIII(Fstar, params=PIIIpars)
    if (is.na(pv)) {pv=1}  ## usually happens if var=0 in the denominator
    return(pv)
  }
