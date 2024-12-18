Proyecto_modulo 4.
================
Ing Winston Jarjury
2024-12-18

``` r
library(openxlsx)
library(foreign)
library(forecast)
library(gmodels)
library(lmtest)
library(ResourceSelection)
library(ROCR)
library(memisc)
library(QuantPsyc)
library(Epi)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(plotly)
```

``` r
db=read.csv("C:\\Users\\Winston\\Downloads\\Base-y-presentacion_4681YE (1)\\Base y presentación\\germancredit.csv")

#Eliminamos las columnas de variables cualitativas
db=db[sapply(db, is.numeric)]
attach(db)
names(db)
```

    ## [1] "Default"     "duration"    "amount"      "installment" "residence"  
    ## [6] "age"         "cards"       "liable"

``` r
#Generamos nuestros modelos iniciales

logit=glm(Default~.,
          family = binomial(logit),
          data=db)

probit=glm(Default~.,
          family = binomial(probit),
          data=db)
```

``` r
memisc::mtable(logit,probit,digits = 6,sdigits = 6)
```

    ## 
    ## Calls:
    ## logit: glm(formula = Default ~ ., family = binomial(logit), data = db)
    ## probit: glm(formula = Default ~ ., family = binomial(probit), data = db)
    ## 
    ## ==================================================
    ##                       logit           probit      
    ## --------------------------------------------------
    ##   (Intercept)       -1.569798***    -0.960494***  
    ##                     (0.429977)      (0.255200)    
    ##   duration           0.026212***     0.016066***  
    ##                     (0.007703)      (0.004684)    
    ##   amount             0.000071*       0.000043*    
    ##                     (0.000034)      (0.000021)    
    ##   installment        0.203560**      0.119905**   
    ##                     (0.072517)      (0.042959)    
    ##   residence          0.040909        0.022224     
    ##                     (0.066909)      (0.040110)    
    ##   age               -0.021431**     -0.012525**   
    ##                     (0.007083)      (0.004143)    
    ##   cards             -0.156890       -0.092687     
    ##                     (0.130500)      (0.077070)    
    ##   liable             0.128003        0.075966     
    ##                     (0.201313)      (0.120147)    
    ## --------------------------------------------------
    ##   Log-likelihood  -579.224047     -579.074737     
    ##   N               1000            1000            
    ## ==================================================
    ##   Significance: *** = p < 0.001; ** = p < 0.01;   
    ##                 * = p < 0.05

``` r
exp(coefficients(logit))
```

    ## (Intercept)    duration      amount installment   residence         age 
    ##   0.2080873   1.0265583   1.0000706   1.2257586   1.0417576   0.9787973 
    ##       cards      liable 
    ##   0.8547979   1.1365567

``` r
exp(coefficients(probit))
```

    ## (Intercept)    duration      amount installment   residence         age 
    ##   0.3827037   1.0161960   1.0000429   1.1273899   1.0224727   0.9875533 
    ##       cards      liable 
    ##   0.9114790   1.0789257

``` r
#--------Prueba de bondad de ajuste--------#

realizar_test_hoslem <- function(modelo, datos, g = 10) { 
  resultado <- hoslem.test(datos$Default, fitted(modelo), g = g) 
  cat("Modelo:", deparse(substitute(modelo)), "\n") 
  cat("Hipótesis Nula (H0): El modelo se ajusta bien a los datos observados.\n") 
  cat("Hipótesis Alternativa (H1): El modelo no se ajusta bien a los datos observados.\n") 
  cat("Valor p del test de Hosmer-Lemeshow:", resultado$p.value, "\n") 
  if (resultado$p.value < 0.05) { 
    cat("Conclusión: Rechazamos la hipótesis nula. El modelo no se ajusta bien a los datos.\n") } 
  else { cat("Conclusión: No rechazamos la hipótesis nula. El modelo se ajusta bien a los datos.\n") } 
  cat("\n") }
```

``` r
realizar_test_hoslem(logit,db)
```

    ## Modelo: logit 
    ## Hipótesis Nula (H0): El modelo se ajusta bien a los datos observados.
    ## Hipótesis Alternativa (H1): El modelo no se ajusta bien a los datos observados.
    ## Valor p del test de Hosmer-Lemeshow: 0.831707 
    ## Conclusión: No rechazamos la hipótesis nula. El modelo se ajusta bien a los datos.

``` r
realizar_test_hoslem(probit,db)
```

    ## Modelo: probit 
    ## Hipótesis Nula (H0): El modelo se ajusta bien a los datos observados.
    ## Hipótesis Alternativa (H1): El modelo no se ajusta bien a los datos observados.
    ## Valor p del test de Hosmer-Lemeshow: 0.7540422 
    ## Conclusión: No rechazamos la hipótesis nula. El modelo se ajusta bien a los datos.

``` r
threshold=mean(fitted(logit))
thresholdp=mean(fitted(probit))

threshold
```

    ## [1] 0.3

``` r
thresholdp
```

    ## [1] 0.2998025

``` r
#Tabla de clasificación


ClassLog(logit,db$Default,cut = threshold)
```

    ## $rawtab
    ##        resp
    ##           0   1
    ##   FALSE 437 132
    ##   TRUE  263 168
    ## 
    ## $classtab
    ##        resp
    ##                 0         1
    ##   FALSE 0.6242857 0.4400000
    ##   TRUE  0.3757143 0.5600000
    ## 
    ## $overall
    ## [1] 0.605
    ## 
    ## $mcFadden
    ## [1] 0.05179588

``` r
ClassLog(probit,db$Default,cut = thresholdp)
```

    ## $rawtab
    ##        resp
    ##           0   1
    ##   FALSE 440 129
    ##   TRUE  260 171
    ## 
    ## $classtab
    ##        resp
    ##                 0         1
    ##   FALSE 0.6285714 0.4300000
    ##   TRUE  0.3714286 0.5700000
    ## 
    ## $overall
    ## [1] 0.611
    ## 
    ## $mcFadden
    ## [1] 0.05204031

``` r
#El logit clasifica adecuadamente el 60,50%
#Mientras que el probit clasifica 61,10%
```

``` r
threshold=0.80


ClassLog(logit,db$Default,cut = threshold)
```

    ## $rawtab
    ##        resp
    ##           0   1
    ##   FALSE 700 300
    ## 
    ## $classtab
    ##        resp
    ##         0 1
    ##   FALSE 1 1
    ## 
    ## $overall
    ## [1] 0.7
    ## 
    ## $mcFadden
    ## [1] 0.05179588

``` r
ClassLog(probit,db$Default,cut = threshold)
```

    ## $rawtab
    ##        resp
    ##           0   1
    ##   FALSE 700 300
    ## 
    ## $classtab
    ##        resp
    ##         0 1
    ##   FALSE 1 1
    ## 
    ## $overall
    ## [1] 0.7
    ## 
    ## $mcFadden
    ## [1] 0.05204031

``` r
#------Evaluamos la capacidad de los otros criterios----------------#

predl=prediction(logit$fitted.values,db$Default)

predp=prediction(probit$fitted.values,db$Default)


perfl=performance(
  predl,measure = "tpr",x.measure = "fpr"
)

perfp=performance(
  predp,measure = "tpr",x.measure = "fpr"
)


plot(perfl,colorize=T,lty=3)
abline(0,1,col="black")
```

![](Proc-modulo-4-WINTON-JARJURY_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
plot(perfp,colorize=T,lty=3)
abline(0,1,col="black")
```

![](Proc-modulo-4-WINTON-JARJURY_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
aucl=performance(predl,measure = "auc")

aucl=aucl@y.values[[1]]
aucl
```

    ## [1] 0.6506143

``` r
aucp=performance(predp,measure = "auc")

aucp=aucp@y.values[[1]]
aucp
```

    ## [1] 0.6506952

``` r
#Encontrando el punto de corte optimo logit

ROC(form=Default~duration+amount+installment+residence+age+cards+liable,plot="sp")
```

![](Proc-modulo-4-WINTON-JARJURY_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#----para el probit------#

perf1=performance(predp,"sens","spec")


sen=slot(perf1,"y.values")[[1]]
esp=slot(perf1,"x.values")[[1]]
alf=slot(perf1,"alpha.values")[[1]]

mat=data.frame(alf,sen,esp)
```

``` r
#logit

perf2=performance(predl,"sens","spec")


sen2=slot(perf2,"y.values")[[1]]
esp2=slot(perf2,"x.values")[[1]]


alf2=slot(perf2,"alpha.values")[[1]]

mat2=data.frame(alf2,sen2,esp2)
```

``` r
m=melt(mat,id=c("alf"))

m2=melt(mat2,id=c("alf2"))

p1=ggplot(m,aes(alf,value,group=variable,colour=variable))+
  geom_line(size=1.2)+
  labs(title="Punto de corte para el probit")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
p1
```

![](Proc-modulo-4-WINTON-JARJURY_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
p2=ggplot(m2,aes(alf2,value,group=variable,colour=variable))+
  geom_line(size=1.2)+
  labs(title="Punto de corte para el logit")

p2
```

![](Proc-modulo-4-WINTON-JARJURY_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
#Probit=0.6133
#Logit=0.61000



g1=grid.arrange(p1,p2,ncol=2)
```

![](Proc-modulo-4-WINTON-JARJURY_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
threshold=0.6133

ClassLog(logit,db$Default,cut = threshold)
```

    ## $rawtab
    ##        resp
    ##           0   1
    ##   FALSE 691 291
    ##   TRUE    9   9
    ## 
    ## $classtab
    ##        resp
    ##                  0          1
    ##   FALSE 0.98714286 0.97000000
    ##   TRUE  0.01285714 0.03000000
    ## 
    ## $overall
    ## [1] 0.7
    ## 
    ## $mcFadden
    ## [1] 0.05179588

``` r
ClassLog(probit,db$Default,cut = threshold)
```

    ## $rawtab
    ##        resp
    ##           0   1
    ##   FALSE 691 291
    ##   TRUE    9   9
    ## 
    ## $classtab
    ##        resp
    ##                  0          1
    ##   FALSE 0.98714286 0.97000000
    ##   TRUE  0.01285714 0.03000000
    ## 
    ## $overall
    ## [1] 0.7
    ## 
    ## $mcFadden
    ## [1] 0.05204031

``` r
threshold=0.61


ClassLog(probit,db$Default,cut = threshold)
```

    ## $rawtab
    ##        resp
    ##           0   1
    ##   FALSE 691 291
    ##   TRUE    9   9
    ## 
    ## $classtab
    ##        resp
    ##                  0          1
    ##   FALSE 0.98714286 0.97000000
    ##   TRUE  0.01285714 0.03000000
    ## 
    ## $overall
    ## [1] 0.7
    ## 
    ## $mcFadden
    ## [1] 0.05204031

``` r
newdata=data.frame(
  duration=36,
  amount=978,
  installment=5,
  residence=4,
  age=39,
  cards=2,
  liable=1)



predict(logit,newdata,type = "response")
```

    ##        1 
    ## 0.401969

#### Conclusión: Debido a que los resultados obtenidos tanto por el probit y logit son similares, tanto en terminos de significancia, como overall de la matriz de confunsion, punto de corte optimo para el threshold, lo ideal es quedarnos con el modelo que mejor clasifique los 0-0 y 1-1, en este caso es el modelo del probit con un overall del 61,10%
