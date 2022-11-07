#############################################################################################
##  Normal dağılmış anakütleler üzerinde t ve Wilcox testinin performansının kıyaslanması  ##
#############################################################################################
# t ve Wilcox testlerinin performanslarını kıyaslamak için 
# bir fonksiyon yazalım.
comp2 <- function(n = 10, mu = 0, mu0 = 0){
  
  pval.t <- NULL 
  pval.w <- NULL
  
  for(i in 1:10000){
    samp <- rnorm(n, mu)
    pval.t[i] <- t.test(samp, mu = mu0)$p.value
    pval.w[i] <- wilcox.test(samp, mu = mu0)$p.value
    
  }
  return(list(t_test = mean(pval.t < 0.05),
              w_test = mean(pval.w < 0.05)))
}

# comp2() fonksiyonu istenilen örneklem hacminde iki testin 
# performansını karşılaştırma yarar. "mu" argümanı ile 
# oluşturmak istediğimiz örneklemin ortalamasını,
# "mu0" argümanı ile test etmek istediğimiz anakütle ortalamasını
# belirleyebiliriz.

# mu = mu0 olacak şekilde bir kurgu oluşturduğumuzda 1.tip hata
# olasılığını, mu != mu0 şeklinde bir kurgu oluşturduğumuzda ise
# testin gücünü hesaplıyor olduğumuzu unutmayınız.

# Örneğin, aşağıdaki kurguda 30 örneklem hacmi için 1 ortalama ile 
# normal dağılmış örneklemler oluşturup, bu örneklemlerin ortalamasının
# mu0 = 1 diyerek 1'e eşit olup olmadığını test ediyoruz. Burada comp2()
# fonksiyonunun döndürdüğü sonuç ilgili kurgu için 1.tip hata olasılığır.
comp2(n = 30, mu = 1, mu0 = 1) 

# Örneğin, aşağıdaki kurguda 30 örneklem hacmi için 1 ortalama ile 
# normal dağılmış örneklemler oluşturup, bu örneklemlerin ortalamasının
# mu0 = 1.5 diyerek 1.5'a eşit olup olmadığını test ediyoruz. Burada comp2()
# fonksiyonunun döndürdüğü sonuç ilgili kurgu için testin gücüdür.
comp2(n = 30, mu = 1, mu0 = 1.5)

############################################################################################
##  Üstel dağılmış anakütleler üzerinde t ve Wilcox testinin performansının kıyaslanması  ##
############################################################################################
# comp2() fonksiyonuna bir argüman daha ekleyerek, üretilen örneklemin
# normal dağılım dışında bir dağılımdan üretilmesini sağlayalım ve
# bunun için alternatif olarak üstel dağılımı seçelim.
comp3 <- function(n = 10, mu = 0, mu0 = 0, normal = TRUE){
  
  pval.t <- NULL 
  pval.w <- NULL
  
  for(i in 1:10000){
    ifelse(normal == TRUE, samp <- rnorm(n, mu), samp <- rexp(n, rate = 1))
    pval.t[i] <- t.test(samp, mu = mu0)$p.value
    pval.w[i] <- wilcox.test(samp, mu = mu0)$p.value
    
  }
  return(list(t_test = mean(pval.t < 0.05),
              w_test = mean(pval.w < 0.05)))
}

# Aşağıdaki örnekte görüldüğü gibi normal dağılmadığı bilinen bir anakütleden
# çekilen örneklemler üzerinde Wilcox testi t-testine göre daha düşük 
# performans gösterebilmektedir.
comp3(n = 20, mu0 = 0.7, normal = FALSE)

##############################################################################################
##  Laplace dağılmış anakütleler üzerinde t ve Wilcox testinin performansının kıyaslanması  ##
##############################################################################################
comp4 <- function(n = 10, mu = 0, mu0 = 0, normal = TRUE){
  library(VGAM) # for generating random sample from Laplace dist.
  pval.t <- NULL 
  pval.w <- NULL
  
  for(i in 1:10000){
    ifelse(normal == TRUE, samp <- rnorm(n, mu), samp <- rlaplace(n, location = 1))
    pval.t[i] <- t.test(samp, mu = mu0)$p.value
    pval.w[i] <- wilcox.test(samp, mu = mu0)$p.value
    
  }
  return(list(t_test = mean(pval.t < 0.05),
              w_test = mean(pval.w < 0.05)))
}

# Aşağıdaki örnekte görüldüğü gibi normal dağılmadığı bilinen ancak simetrik dağılan
# bir anakütleden çekilen örneklemler üzerinde Wilcox testi t-testine göre daha iyi 
# performans gösterebilmektedir.
comp4(n = 40, mu0 = 1.4, normal = FALSE)

##################################################################
##     Normallik testlerinin performanslarının kıyaslanması     ##
##################################################################
comp5 <- function(n = 8, mu = 0, normal = TRUE){
  library(VGAM)    # for generating random sample from Laplace dist.
  library(nortest) # for using Anderson-Darling test
  pval.sw <- NULL 
  pval.ad <- NULL
  pval.ks <- NULL
  
  for(i in 1:10000){
    ifelse(normal == TRUE, samp <- rnorm(n, mu), samp <- rlaplace(n, location = 1))
    pval.sw[i] <- shapiro.test(samp)$p.value
    pval.ad[i] <- ad.test(samp)$p.value
    pval.ks[i] <- ks.test(samp, "pnorm")$p.value
  }
  return(list(sw_test = mean(pval.sw < 0.05),
              ad_test = mean(pval.ad < 0.05),
              ks_test = mean(pval.ks < 0.05)))
}

# Burada önceki deneylerden farklı olarak, normal = TRUE olarak comp5() 
# fonksiyonunu çalıştırdığımız durumda normallik testlerinin ilgili 
# örneklem hacmi için 1.tip hata olasılığını hesaplamış oluruz.
comp5(n = 50)

# Eğer normal = FALSE olarak fonksiyonu çalıştırırsak normallik testlerinin
# gücünü, yani örneklemlerin normal dağılmamış (Laplace) bir anakütleden
# geldiğini tespit edebilme becerisini hesaplamış oluruz.
# Burada güç değerinin, örneklem hacmi seçimine ve Laplace dağılımının
# parametre seçimine bağlı olduğunu unutmayınız.
comp5(n = 50, normal = FALSE)

#####################################################################################
##  Normallik testlerinin farklı dağılımlara karşı performanslarının kıyaslanması  ##
#####################################################################################
# Burada bir önceki fonksiyonu birden fazla dağılım için genelleştirelim.
comp6 <- function(n = 8, dist = "laplace"){
  library(VGAM)    # for generating random sample from Laplace dist.
  library(nortest) # for using Anderson-Darling test
  pval.sw <- NULL 
  pval.ad <- NULL
  pval.ks <- NULL
  
  for(i in 1:10000){
    ifelse(dist == "laplace", samp <- rlaplace(n, location = 1), samp <- rexp(n, 1))
    pval.sw[i] <- shapiro.test(samp)$p.value
    pval.ad[i] <- ad.test(samp)$p.value
    pval.ks[i] <- ks.test(samp, "pnorm")$p.value
  }
  return(list(sw_test = mean(pval.sw < 0.05),
              ad_test = mean(pval.ad < 0.05),
              ks_test = mean(pval.ks < 0.05)))
}

comp6(n = 20, dist = "laplace")
comp6(n = 20, dist = "exponential")


