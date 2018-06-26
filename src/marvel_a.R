
if(TRUE){
  df_marvel <- read.csv("~/Documents/DAKD/candidates/comics/marvel-wikia-data.csv" )
  df_dc <- read.csv("~/Documents/DAKD/candidates/comics/dc-wikia-data.csv" )
  
}

if(FALSE){
  df_dc <- read.csv("~/git/dakd-project/data/dc-wikia-data.csv")
  df_marvel <- read.csv("~/git/dakd-project/data/marvel-wikia-data.csv")
  
}

####### MERGE ########

# Bind Company Column
df_dc = cbind(df_dc, company=rep(c("DC"), times= nrow(df_dc)))
df_marvel = cbind(df_marvel, company=rep(c("Marvel"), times= nrow(df_marvel)))

# Since the Year field is in CAPS in the marvel dataset and in minus in the DC dataset, we apply the names of one onto the other one join them.
names(df_marvel) <- names(df_dc)
df = rbind(df_dc, df_marvel)

## Removing unused columns
df  <- subset(df, select = -c(page_id, urlslug, FIRST.APPEARANCE ) )

## Fill void data

df$GSM <- as.character(df$GSM)
df$GSM[(is.na(df$GSM) | df$GSM=="")] <- "Unspecified"
df$GSM <- as.factor(df$GSM)

## Gender cleaning

df$SEX <- as.character(df$SEX)
df$SEX <- gsub(" Characters", "", df$SEX)
df$SEX <- as.factor(df$SEX)

# Eyes cleaning

df$EYE <- as.character(df$EYE)
df$EYE <- gsub(" eyes", "", df$EYE)
df$EYE <- as.factor(df$EYE)

#Align
df$ALIGN <- as.character(df$ALIGN)
df$ALIGN <- gsub(" Characters", "", df$ALIGN)
df$ALIGN <- as.factor(df$ALIGN)

# Identity
df$ID <- as.character(df$ID)
df$ID <- gsub(" Identity", "", df$ID)
df$ID <- as.factor(df$ID)


# Alive
df$ALIVE <- as.character(df$ALIVE)
df$ALIVE <- gsub(" Characters", "", df$ALIVE)
df$ALIVE <- as.factor(df$ALIVE)


# Hair
df$HAIR <- as.character(df$HAIR)
df$HAIR <- gsub(" Hair", "", df$HAIR)
df$HAIR <- as.factor(df$HAIR)

df$YEAR <- as.numeric(df$YEAR)

df_clean = df
##### Removing rows with voids

df_clean = df_clean[!(is.na(df_clean$SEX) | df_clean$SEX==""), ]
df_clean = df_clean[!(is.na(df_clean$ALIVE) | df_clean$ALIVE==""), ]
df_clean = df_clean[!(is.na(df_clean$ALIGN) | df_clean$ALIGN==""), ]
df_clean = df_clean[!(is.na(df_clean$APPEARANCES) | df_clean$APPEARANCES==""), ]
df_clean = df_clean[!(is.na(df_clean$EYE) | df_clean$EYE==""), ]
df_clean = df_clean[!(is.na(df_clean$HAIR) | df_clean$HAIR==""), ]
df_clean = df_clean[!(is.na(df_clean$ID) | df_clean$ID==""), ]

df_clean = droplevels(df_clean)


df_clean$APPEARANCES = as.numeric(df_clean$APPEARANCES)
df$APPEARANCES = as.numeric(df$APPEARANCES)

marvel_clean <- subset(df_clean, company == "Marvel")
dc_clean <- subset(df_clean, company == "DC")



par(mfrow=c(1, 1), mar = rep(2, 4))
# Pie Chart with Percentages
slices <- c(nrow(df_marvel), nrow(df_dc))
lbls <- c("Marvel", "DC")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Companies' characters percentage,\n before clean") 

# Pie Chart with Percentages
slices <- c(nrow(marvel_clean), nrow(dc_clean))
lbls <- c("Marvel", "DC")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Companies' characters percentage,\n after clean") 




##################### STUDY #################

par(mar=c(10,5,5,0))

#### GENDER #####

colors = c("#94C160", "#F42A46", "#F42A46", "#F42A46", "#227D97", "#AA5139")
barplot(table(df_clean$SEX), main="Character's gender",ylab="Frequency",las=2, col =colors)


colors = c("#94C160", "#F42A46", "#F42A46", "#F42A46", "#227D97", "#AA5139")
barplot(table(pop_full$SEX), main="Character's gender",ylab="Frequency",las=2, col =colors)


colors = c("#1E4C4C", "#FFBC35")
barplot(table(df_clean$ALIVE), main="Character's alive",ylab="Frequency",las=2, col =colors)



colors = c("#FF3A35", "#81EBEB", "#FFBA35")
barplot(table(df_clean$ALIGN), main="Character's alignment",ylab="Frequency",las=2, col =colors)


### APPEARANCE COMPARISON
library("reshape2")
library("ggplot2")

colorsV = c("Female"="#F42A46", 
            "Male"="#227D97",
            "Agender"="#F39331",
            "Others"="#52DD26")

women = subset(df_clean, df_clean$SEX == "Female")
men = subset(df_clean, df_clean$SEX == "Male")
agender = subset(df_clean, df$SEX == "Agender" | df$SEX == "Genderless")
others = subset(df_clean, df$SEX == "Genderfluid" | df$SEX == "Transgender" )

women_evol = as.data.frame(table(women$YEAR)) 
men_evol = as.data.frame(table(men$YEAR)) 
agender_evol = as.data.frame(table(agender$YEAR)) 
others_evol = as.data.frame(table(others$YEAR))

 ggplot(women_evol, aes(Var1, Freq, group = 1)) + 
    geom_line(data = women_evol, aes(colour="Female")) +
    geom_line(data = men_evol, aes(colour="Male")) +
   geom_line(data = agender_evol, aes(colour="Agender")) +
   geom_line(data = others_evol, aes(colour="Others")) +
   scale_colour_manual("", 
                       values = colorsV) +
   ylab("Characters added") +
   xlab("Year") +
   theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
   ggtitle("Number of additions per year") + 
   theme(plot.title = element_text(lineheight=.8, face="bold"))
 
 
 women_evolCum = women_evol
 men_evolCum = men_evol
 agender_evolCum = agender_evol
 others_evolCum = others_evol
 
 ## TODO> Do total
 women_evolCum$Freq = cumsum(women_evol$Freq)
 men_evolCum$Freq = cumsum(men_evol$Freq) 
 agender_evolCum$Freq = cumsum(agender_evol$Freq)
 others_evolCum$Frew = cumsum(others_evol$Freq)
 
# women_evolCum$Var1 = as.numeric(women_evol$Var1)
# men_evolCum$Var1 = as.numeric(men_evol$Var1)
# agender_evolCum$Var1 = as.numeric(agender_evol$Var1)
# others_evolCum$Var1 = as.numeric(others_evol$Var1)
 
 
 ggplot(women_evol, aes(Var1, Freq, group = 1)) + 
   geom_line(data = women_evolCum, aes(colour="Female")) +
   geom_line(data = men_evolCum, aes(colour="Male")) +
   geom_line(data = agender_evolCum, aes(colour="Agender")) +
   geom_line(data = others_evolCum, aes(colour="Others")) +
   scale_colour_manual("", 
                       values = colorsV) +
   theme(axis.text.x=element_text(angle = 90, hjust = 0)) + 
   ggtitle("Gender characters produced") +
   ylab("Characters until now") +
   xlab("Year") +
   theme(plot.title = element_text(lineheight=.8, face="bold")) 
 #  scale_x_continuous(breaks=seq(1936, 2013, 10))
 
 #### Focus on non female nor male
 
 ggplot(agender_evolCum, aes(Var1, Freq, group = 1)) + 
   geom_line(data = agender_evolCum, aes(colour="Agender")) +
   geom_line(data = others_evolCum, aes(colour="Others")) +
   scale_colour_manual("", 
                       values = colorsV) +
   theme(axis.text.x=element_text(angle = 90, hjust = 0)) + 
   ggtitle("Gender characters produced") +
   ylab("Characters until now") +
   xlab("Year") +
   theme(plot.title = element_text(lineheight=.8, face="bold")) 
 
 
 mean(as.vector(women$APPEARANCES))
 mean(as.vector(men$APPEARANCES))
      
 median(as.vector(women$APPEARANCES))
 median(as.vector(men$APPEARANCES))
 
 
 ggplot(df_clean, aes(x=SEX, y=APPEARANCES, fill=SEX)) + geom_boxplot() +
   ggtitle("Appearances per gender") +
   ylab("Appearances") +
   xlab("Gender") +
   theme(plot.title = element_text(lineheight=.8, face="bold")) 
 
 

  ## AVG
 
 ggplot(df_clean, aes(x=SEX, y=APPEARANCES)) + 
   stat_summary(fun.y="mean", geom="point" , colour = "#BD1C35", size = 5) +
   stat_summary(fun.y = "median", colour = "black", size = 3,  pch=15, geom = "point")
 summary(df_clean$APPEARANCES) + guides(fill="legend")
 
 
 
 
 
 ################### MCA ############################
 require("FactoMineR", "factoextra")
 if(FALSE){
   df_clean2 = df_clean[!(is.na(df_clean$EYE) | df_clean$EYE==""), ]
   df_clean2 = df_clean2[!(is.na(df_clean2$HAIR) | df_clean2$HAIR==""), ]
   df_clean2 = df_clean2[!(is.na(df_clean2$ID) | df_clean2$ID==""), ]
   
   df_clean2  <- subset(df_clean2, select = -c(APPEARANCES, YEAR ) )
   
   mca_res = MCA(df_clean2, graph =  TRUE)
  
  df_clean2 = droplevels(df_clean2)
 } 
 
 
 ### SUBSETTING FURTHER. At least 800 appearances.
 
 minAppearances = 800
 pop = df_clean[!(df_clean$APPEARANCES < minAppearances), ]
 
 pop_full = pop
 
pop <- subset(pop, select = -c(APPEARANCES, YEAR ) )

pop2 = pop
pop <- pop[,-1]
rownames(pop) <- pop2[,1]
rm(pop2)

pop = droplevels(pop)

mca_res = MCA(pop, graph =  FALSE)
nrow(pop)



fviz_screeplot(mca_res, addlabels = TRUE, ylim = c(0, 15))


## BiPlot
fviz_mca_biplot(mca_res, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())


### Correlation
fviz_mca_var(mca_res, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


## Categories relationship
fviz_mca_var(mca_res, col.var="blue", shape.var = 8,
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


#### Boxplot

ggplot(pop_full,  aes(x=SEX, y=APPEARANCES, fill=SEX)) + geom_boxplot() +
  ggtitle("Appearances per gender") +
  ylab("Appearances") +
  xlab("Gender") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) 


### MCA heat map
fviz_mca_var(mca_res, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

### Cos2 - relationship with dim
fviz_cos2(mca_res, choice = "var", axes = 1:2)

### Contribution to data
# Contributions of rows to dimension 1
fviz_contrib(mca_res, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(mca_res, choice = "var", axes = 2, top = 15)

fviz_contrib(mca_res, choice = "var", axes = 1:2, top = 15)

fviz_ellipses(mca_res, 1:8, geom = "point")

res.desc <- dimdesc(mca_res, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]

###### GENERAL PLOTS #############
colorsV = c("Female"="#F42A46", 
            "Male"="#227D97",
            "Agender"="#F39331",
            "Others"="#52DD26")


# Pie Chart with Percentages


createPieChartByCompanies <- function(df){
  
  marvel_pop <- subset(df, company == "Marvel")
  dc_pop <- subset(df, company == "DC")
  
  slices <- c(nrow(marvel_pop), nrow(dc_pop))
  lbls <- c("Marvel", "DC")
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
      main="Companies") 
  
}


createGendersChart <- function(df) {
  colors = c("#94C160", "#F42A46", "#F42A46", "#F42A46", "#227D97", "#AA5139")
  barplot(table(df$SEX), main="Character's gender",ylab="Frequency",las=2, col =colors)
  
}


createAlignmentChart <- function(pop_full) {
  
  alignmentCountDF = data.frame()
  alignmentCountDF = rbind(alignmentCountDF, c(length(which(pop_full$SEX == "Female" & pop_full$ALIGN == "Good")),
                            length(which(pop_full$SEX == "Female" & pop_full$ALIGN == "Bad")),
                            length(which(pop_full$SEX == "Female" & pop_full$ALIGN == "Neutral"))))
  
  
  alignmentCountDF = rbind(alignmentCountDF, c(length(which(pop_full$SEX == "Male" & pop_full$ALIGN == "Good")),
                            length(which(pop_full$SEX == "Male" & pop_full$ALIGN == "Bad")),
                            length(which(pop_full$SEX == "Male" & pop_full$ALIGN == "Neutral"))))
  
  names(alignmentCountDF) <- c("Good", "Bad", "Neutral")
  rownames(alignmentCountDF) <- c("Female", "Male")
  
  write.csv(alignmentCountDF, "alignment2.csv")
}


colors = c("#1E4C4C", "#FFBC35")
barplot(table(pop_full$ALIVE), main="Character's alive",ylab="Frequency",las=2, col =colors)

createAlignmentChart(df_clean)

#createPieChartByCompanies(pop_full)
#createGendersChart(pop_full)

