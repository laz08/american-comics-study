df = read.csv(file = 'cmc.data')
colnames(df) <- c('Wife age', 'Wife education', 'Husband education', '# children born', 'Wife religion', 'Wife Working?', 'Husband occupation', 'Std of living', 'Media exposure', 'Contraceptive method' )

par()
hist(df)
corr_mtx = cor(df)
corrplot::corrplot(is.corr = F, 
                   title = "Correlations between factors",
                   corr_mtx, 
                   type = "lower", 
                   method = "circle", mar=c(0,0,2,0))

corr_mtx = cor(df)
corrplot::corrplot(is.corr = F, 
                   title = "Correlations between factors",
                   corr_mtx, 
                   type = "lower", 
                   method = "number", mar=c(2,2,2,2))

#########################

df = read.csv(file = 'dc-wikia-data.csv')
df = drug_use_by_age_per2
#d<-df[!(df$ALIGN=="Neutral Characters" ),]
d<-df[!(d$ALIGN=="Reformed Criminals" ),]
d<-d[!(d$ALIGN=="" ),]
d<-d[!(is.null(d$ALIGN)),]
d$ALIGN <- factor(d$ALIGN)


summary(d$ALIGN)



lda_r <- lda(formula = d$ALIGN ~ d$SEX, data = d)

d
lda_r

fitGraph <- ggplotLDAPrep(lda_r)
ggplot(fitGraph, aes(LD1,LD2, color=labels))+geom_point()

summary(d$ALIGN)
#######################
require("ggplot2")
df_not_qt = df
df_not_qt$`Contraceptive method`[df$`Contraceptive method` == 1] <- "No used"
df_not_qt$`Contraceptive method`[df$`Contraceptive method` == 2] <- "Long-term"
df_not_qt$`Contraceptive method`[df$`Contraceptive method` == 3] <- "Short-term"


#hist(df$`Contraceptive method`)

ggplot(data=df_not_qt, aes(`Contraceptive method`)) + 
  geom_histogram(binwidth = 1,
                 stat = "count",
                 col="black", 
                 fill="blue", 
                 alpha=.8)

require('MASS')
lda_r <- lda(formula = df$`Contraceptive method` ~ ., data = df)


lda_r$prior   #Probabilities of class membership
lda_r$counts  #Counts
lda_r$means
lda_r$scaling
lda_r$svd
#As we can see above, a call to lda returnst he prior probability of each class, the counts for each class in the data, the class-specific means for each covariate, the linear combination coefficients (scaling) for each linear discriminant (remember that in this case with 3 classes we have at most two linear discriminants) and the singular values (svd) that gives the ratio of the between- and within-group standard deviations on the linear discriminant variables.

library(MASS)
library(ggplot2)
lda_r <- lda(formula = df$`Contraceptive method` ~ ., data = df, CV = TRUE)

lda_r


ggplotLDAPrep <- function(x){
  if (!is.null(Terms <- x$terms)) {
    data <- model.frame(x)
    X <- model.matrix(delete.response(Terms), data)
    g <- model.response(data)
    xint <- match("(Intercept)", colnames(X), nomatch = 0L)
    if (xint > 0L) 
      X <- X[, -xint, drop = FALSE]
  }
  means <- colMeans(x$means)
  X <- scale(X, center = means, scale = FALSE) %*% x$scaling
  rtrn <- as.data.frame(cbind(X,labels=as.character(g)))
  rtrn <- data.frame(X,labels=as.character(g))
  return(rtrn)
}
#fitGraph <- ggplotLDAPrep(lda_r)
#ggplot(fitGraph, aes(LD1,LD2, color=labels))+geom_point()

#############################
lda_r <- lda(formula = df$`Contraceptive method` ~ ., data = df)

lda_r

fitGraph <- ggplotLDAPrep(lda_r)
ggplot(fitGraph, aes(LD1,LD2, color=labels))+geom_point()


#############################

# THIS ONE
lda_r <- lda(formula = df$`Contraceptive method` ~ df$`Wife age` + df$`Wife education` + df$`# children born` + df$`Std of living`, data = df)

lda_r

fitGraph <- ggplotLDAPrep(lda_r)
ggplot(fitGraph, aes(LD1,LD2, color=labels))+geom_point()


lda_r <- lda(formula = df_not_qt$`Contraceptive method` ~ df_not_qt$`Wife age` + df_not_qt$`Wife education` + df_not_qt$`# children born` + df_not_qt$`Std of living`, data = df)

lda_r

fitGraph <- ggplotLDAPrep(lda_r)
ggplot(fitGraph, aes(LD1,LD2, color=labels))+geom_point()

############################


lda_r <- lda(formula = df$`Contraceptive method` ~ df$`Wife age` + df$`Wife education` + df$`Husband education` , data = df)

lda_r

fitGraph <- ggplotLDAPrep(lda_r)
ggplot(fitGraph, aes(LD1,LD2, color=labels))+geom_point()




######## PCA



require("FactoMineR", "factoextra")
pca_model = PCA(df, scale.unit=TRUE, ncp=5, graph=FALSE)
pca_vars <- get_pca_var(pca_model)

gradient_colors = c("#00AFBB", "#E7B800", "#FC4E07")
# Cos2 represents the quality of representation for variables on the factor map. 
#It’s calculated as the squared coordinates: var.cos2 = var.coord * var.coord.
fviz_pca_ind(pca_model,col.ind = "cos2", 
             gradient.cols = gradient_colors)
#contrib: contains the contributions (in percentage) of the variables to 
#the principal components. The contribution of a variable (var) to a given 
#principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component).
fviz_pca_var(pca_model, col.var = "contrib",
             gradient.cols = gradient_colors,
             alpha.var = "contrib",
             repel = TRUE
)



lm_model = lm(df$`Contraceptive method` ~ ., data = df)
summary(lm_model)



## ANOVA

boxplot(df$`Wife age` ~ df$`Contraceptive method`, data=df, id.method="y",
        col = c("orange","yellow","purple"))


boxplot(df$`Wife education` ~ df$`Contraceptive method`, data=df, id.method="y",
        col = c("orange","yellow","purple"))


boxplot(df$`# children born` ~ df$`Contraceptive method`, data=df, id.method="y",
        col = c("orange","yellow","purple"))


boxplot(df$`Std of living` ~ df$`Contraceptive method`, data=df, id.method="y",
        col = c("orange","yellow","purple"))




a = aov(df$`Contraceptive method` ~ df$`Wife age`, data = df)


summary(a)