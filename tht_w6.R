#############################################################################################
##  Greenhouse verisi üzerinde bazı uygulamalar                                            ##
#############################################################################################

tomato <- data.frame(house      = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3),
                     fertilizer = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
                     kg         = c(0.51, 0.25, 0.64, 0.22, 1.05, 0.99, 0.40, 0.94,
                                    0.06, 0.42, 1.13, 0.43, 0.22, 0.25, 0.81))

# Seralara göre ortalama domates hasatının kg cinsinden eşitliğinin test edilmesi için, 
# genellikle eğer gerekli varsayımlar (varyans homojenliği ve normalik) sağlanıyorsa ANOVA 
# testi, sağlanmıyorsa Kruskal-Wallis testi tercih edilir. 

# Varyans homojenliği varsayımını Bartlett testi ile sınayalım.
bartlett.test(kg ~ house, data = tomato)

# Normallik varsayımını Shapiro-Wilk testi ile sınayalım.
shapiro.test(tomato$kg)

# Veri setinde gerekli varsayımların sağlandığı görüldüğü için ANOVA testini kullanılabilir.
summary(aov(kg ~ house, data = tomato))

# Eğer varsayımlardan biri ya da her ikisi de sağlanmasaydı Kruskal-Wallis testini kullanmak
# daha düşük 1.tip hata riski almak için daha iyi bir tercih olurdu.
kruskal.test(kg ~ house, data = tomato)

#############################################################################################
##  Normal dağılmış anakütleler in varyans homojenliklerinin test edilmesi                 ##
#############################################################################################
library(car) # leveneTest() fonksiyonunu kullanabilmek için
comp <- function(n1 = 10, mean1 = 1, sigma1 = 1,
                 n2 = 10, mean2 = 1, sigma2 = 1,
                 n3 = 10, mean3 = 1, sigma3 = 1){
  
  pval.b <- NULL 
  pval.l <- NULL
  pval.f <- NULL
  
  for(i in 1:10000){
    samp1 <- rnorm(n1, mean = mean1, sd = sqrt(sigma1))
    samp2 <- rnorm(n2, mean = mean2, sd = sqrt(sigma2))
    samp3 <- rnorm(n3, mean = mean3, sd = sqrt(sigma3))
    dataset <- data.frame(obs = c(samp1, samp2, samp3),
                          pop = c(rep("1", n1), rep("2", n2), rep("3", n3)))
    pval.b[i] <- bartlett.test(obs ~ pop, data = dataset)$p.value
    pval.l[i] <- leveneTest(obs ~ factor(pop), data = dataset)[1, 3]
    pval.f[i] <- fligner.test(obs ~ pop, data = dataset)$p.value
    
    plot1 <- ggplot(dataset, aes(x = obs, fill = pop)) + 
       geom_density(alpha = 0.5) +
       labs(x = "Observations",
            fill = "Sample") +
       theme_bw()
  }
  return(list(bartlett_test = mean(pval.b < 0.05),
              levene_test   = mean(pval.l < 0.05),
              fligner_test  = mean(pval.f < 0.05),
              plot1 = plot1))
}

# Testlerin 1.tip hata olasılıklarını hesaplamak için tüm sigma değerlerinin
# birbirine eşit olması gerekmektedir.


comp(n1 = 50, n2 = 50, n3 = 50,
     mean1 = 1, mean2 = 5, mean3 = 10)

# Testlerin güçlerini hesaplamak için bir ya da birden fazla sigma değerinin
# diğerlerinden farklı olması gerekmektedir.
comp(sigma1 = 1,
     sigma2 = 1,
     sigma3 = 1.9)

# sigma değerleri arasındaki farkı arttırarak etki büyüklüğünü arttırmış oluruz
# ve testlerin güç değerlerini arttırmış oluruz.

#############################################################################################
##  ANOVA ve Kruskal-Wallis testlerinin performanslarının kıyaslanması                     ##
#############################################################################################
comp2 <- function(n1 = 10, mu1 = 1, sigma1 = 1,
                  n2 = 10, mu2 = 1, sigma2 = 1,
                  n3 = 10, mu3 = 1, sigma3 = 1){
  
  pval.a <- NULL 
  pval.k <- NULL

  for(i in 1:10000){
    samp1 <- rnorm(n1, mu1, sqrt(sigma1))
    samp2 <- rnorm(n2, mu2, sqrt(sigma2))
    samp3 <- rnorm(n3, mu3, sqrt(sigma3))
    dataset <- data.frame(obs = c(samp1, samp2, samp3),
                          pop = c(rep("1", n1), rep("2", n2), rep("3", n3)))
    pval.a[i] <- summary(aov(obs ~ pop, data = dataset))[[1]][[1,"Pr(>F)"]]
    pval.k[i] <- kruskal.test(obs ~ pop, data = dataset)$p.value
    
    plot1 <- ggplot(dataset, aes(x = obs, fill = pop)) + 
      geom_density(alpha = 0.5) +
      labs(x = "Observations",
           fill = "Sample") +
      theme_bw()
   
  }
  return(list(anova_test   = mean(pval.a < 0.05),
              kruskal_test = mean(pval.k < 0.05),
              plot1 = plot1))
}

# Testlerin 1.tip hata olasılıklarını hesaplamak için mu değerlerinin birbirine
# eşit olarak seçilmesi gerekiyor.
comp2()

# Testlerin güç değerlerini hesaplamak için ise mu değerlerinden biri ya da 
# birkaçının diğerlerinden farklı olması gerekmektedir.
comp2(mu1 = 1,
      mu2 = 1,
      mu3 = 2)







