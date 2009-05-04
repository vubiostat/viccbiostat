QC<-function(dat, group=NULL, info=NULL, method=NULL, cutoff=NULL,rand=NULL,fixed=NULL,pt=NULL)
  {
        nrow <- dim(dat)[1]
        ncol <- dim(dat)[2]
        dat <- dat[3:ncol]

       if (is.null(dat))
       {
             stop("Please provide a dataset.")
       }

       if (is.null(method))
       {
             stop("Please provide a method.")
       }

       if (is.null(group) || group == "cross")
       {
          if (method == "pearson" || method == "spearman")
          {
               result <- cor(dat, method=method)
               avg = apply(result, 2, CalAvg)
               return (avg)
          }
          else if (method == "kappa")
          {
               library(Design)
               library(Hmisc)
               library(chron)
               library(e1071)

                if (is.null(cutoff))
                    stop("Please input a vaule to dichotomize data .")
                else
                    dat1 <- ifelse((dat > cutoff), 1, 0)

                cnt = ncol-2

                result = matrix(NA, nrow=cnt, ncol=1)
                avg = matrix(NA,nrow=cnt,ncol=1)

                for (i in 1:cnt)
                {
                    for ( j in 1: cnt)
                    {
                      tabs <-table(dat1[,i],dat1[,j])
                      result[j,] <-  as.numeric(classAgreement(tabs)[2])
                    }
                    avg[i,] = (sum(as.numeric(result))-1)/(cnt-1)
                }
                return (avg)
          }
          else if (method == "ICC")
          {
             if (is.null(rand) || is.null(fixed))
            {
              stop("Please provide random and fixed effects")
            }

            library(nlme)
            library(gmodels)

            randEffect = as.factor(rand)
            fixedEffect = as.factor(fixed)

            varcomp=matrix(NA,nrow=nrow,ncol=2)

            for (i in 1:nrow)
            {
              y=dat[i,]
              y=as.numeric(y)
              fm11=try(lme( y ~ fixedEffect, random =  ~1|randEffect),silent=T)
              if (class(fm11)!="try-error")
              {
                va =VarCorr(fm11)
                varcomp[i,]=as.numeric(va[c(1,2),1])
              }
            }
              icc = varcomp[,1]/ (varcomp[,2]+ varcomp[,1])
              return (icc)
          }
            else if (method == "CV")
          {
                 dat = t(dat)
                 avg=matrix(NA,nrow=nrow,ncol=1)
                 std = sd(dat)

                 for (i in 1:nrow)
                 {
                  avg[i,1]=mean(dat[,i])
                 }
                  CV = std / avg
                  colnames(CV) <- "CV"
                  return (CV)
          }
          else
          {
              stop("Please provide correct method.")
          }
       }
       else if (group == "within")
       {
              if (is.null(info))
              {
                 stop("Please provide clinical information.")
              }

              if (length(dat) != length(info))
              {
                 stop("Length of dataset and group info do not match.")
              }

              if (method == "pearson" || method == "spearman")
              {
                li<-names(as.list(summary(as.factor(info))))
                avg = matrix(NA, nrow=max(summary(as.factor(info))), ncol=length(li))

                for (i in 1:length(li))
                {
                  ind = which(info==li[i])
                  subDat = dat[,ind]

                  result <- cor(subDat, method=method)
                  avgtmp = as.numeric(apply(result, 2, CalAvg))
                  if (length(avgtmp) < max(summary(as.factor(info))) )
                  {
                    avgtmp[(length(avgtmp)+1):max(summary(as.factor(info)))] = 0
                  }
                     avg[,i]=as.numeric(avgtmp)

                }

                colnames(avg) = as.character(li)
                return (avg)
              }

              else if (method == "kappa")
              {
                  library(Design)
                  library(Hmisc)
                  library(chron)
                  library(e1071)
                    
                   if (is.null(cutoff))
                    dat1 <- ifelse((dat > 0), 1, 0)
                   else
                    dat1 <- ifelse((dat > cutoff), 1, 0)
                  li<-names(as.list(summary(as.factor(info))))
                  m = max(summary(as.factor(info)))
                  result = matrix(NA, nrow=m, ncol=1)
                  avg = matrix(NA, nrow = m, ncol=length(li))

                  for (l in 1:length(li))
                  {
                      ind = which(info==li[l])
                      subDat = dat1[,ind]

                    for (i in 1:length(ind))
                    {
                      for ( j in 1: length(ind))
                      {
                        tabs <-table(subDat[,i],subDat[,j])
                        result[j,] <-  as.numeric(classAgreement(tabs)[2])
                      }
                      if (length(ind) < m )
                      {
                          result[(length(ind)+1):m] = 0
                      }
                      avg[i,l] = (sum(as.numeric(result))-1)/(length(ind)-1)
                    }
                  }
                    colnames(avg)=as.character(li)
                    return (avg)
              }
               else if (method == "ICC")
               {

                  library(nlme)
                  library(gmodels)

                  li<-names(as.list(summary(as.factor(info))))
                  varcomp=matrix(NA,nrow=nrow,ncol=2)
                  icc = matrix(NA,nrow=nrow,ncol=length(li))

                  for (l in 1:length(li))
                  {
                    ind = which(info==li[l])
                    subDat = dat[,ind]
                    pts = pt[ind]
                    for (i in 1:nrow)
                    {
                      y=subDat[i,]
                      y=as.numeric(y)
                      fm11=try(lme( y ~ 1, random =  ~1|pts),silent=T)
                      if (class(fm11)!="try-error")
                      {
                        va =VarCorr(fm11)
                        varcomp[i,]=as.numeric(va[c(1,2),1])
                      }
                    }
                     icc[,l] = as.numeric(varcomp[,1]/ (varcomp[,2]+ varcomp[,1]))
                  }
                  colnames(icc) = as.character(li)
                  return (icc)
               }
               else if (method == "CV")
               {

                 li<-names(as.list(summary(as.factor(info))))
                 avg = matrix(NA,nrow=nrow,ncol=1)
                 cv=matrix(NA,nrow=nrow,ncol=length(li))

                 for (l in 1:length(li))
                 {
                   ind = which(info==li[l])
                   subDat = dat[,ind]
                   subDat = t(subDat)

                   std = sd(subDat)

                   for (i in 1:nrow)
                   {
                    avg[i,1]=mean(subDat[,i])
                   }
                    cv[,l] = std / avg
                 }
                  colnames(cv) = as.character(li)
                  return (cv)
               }
               else
               {
                   stop("Please provide a correct method.")
               }
       }
       else
       {
           stop("please provide correct group.")
       }
 }

# calculate average of whole data set
CalAvg = function(vec)
{
    tmp = (sum(vec)-1)/(length(vec)-1)
    return (tmp)
}








