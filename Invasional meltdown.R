## Invasional meltdown
setwd("C:/Users/isma-/OneDrive/Escritorio/Invasional meltdown")
df <- read_excel("Invasion_Melt.xlsx")
df<- df %>% filter(!Species=="synurella ambulans")
df<- df %>% filter(!Species=="orchesta cavimana")
df<- df[!duplicated(df$site_id),]
unique(df$country)
table(df$country)
unique(df$site_id)

a <- df %>% group_by(site_id, taxon) %>% summarise(Ponto = n())
b <- a %>% mutate(PontoCaspian = 1)
c <- b %>% group_by(site_id) %>% summarise(Ponto_Caspian = sum(PontoCaspian))
d <- c %>% group_by(Ponto_Caspian) %>% summarise(Numberofsite_id= n())
d$Ponto_Caspian <- as.factor(d$Ponto_Caspian)
ggplot(d, aes(Ponto_Caspian ,Numberofsite_id)) + geom_bar(stat="identity", fill="darkgreen")+ ylab("Number of site_id (Location)")+ 
  xlab("Number of Ponto-Caspian species") + theme_bw()

##### Number of entries for each species
b<- b %>% group_by(taxon) %>% summarise(entries=sum(PontoCaspian))


### Regresion -->  Ponto caspian vs Time arrival
df <- read_excel("Invasion_Melt.xlsx", sheet = "Plot")
df<- df %>% filter(!Country=="Hungary")
df <- read_excel("PRUEBA1.xlsx")
#df<- df %>% filter(!Country=="Hungary")
df1 <- df %>% group_by(`Ponto-Caspian`, Country) %>% summarise(Time =mean(Time))

df$Ponto_Caspian <- as.factor(df$Ponto_Caspian)
ggplot2::ggplot(df, aes(Ponto_Caspian, Time)) + geom_jitter() +
  geom_smooth(method = "glm", alpha=1/5)+
  theme_classic2() +  theme_cleveland()
#ggplot(df, aes(`Ponto-Caspian`, Time)) + geom_point() +geom_smooth(method = "lm")

#lm <- lm(`Ponto-Caspian`~ Time, data = df1)

lm <- glmer.nb(Ponto_Caspian ~ Time + (1 | Country), data = df)
summary(lm)

deviance(lm)
deviance_explained = 3423.9 / (783)

hist(residuals(lm))
hist(df$Ponto_Caspian)
r.squaredGLMM(lm)
par(mfrow=c(2,2))  # draw 4 plots in same window
gvlma::gvlma(lm)
plot(lm)

#Time series spanned 
df <- read_excel("ponto-caspian.xlsx")

df1 <- df %>% group_by(site_id,year) %>% summarise(count = n())
df2 <- df1 %>% group_by(site_id,year,count) %>% summarise(count=n())
df3 <- df2 %>% group_by(site_id) %>% summarise(sumar= sum(count))
mean(df3$sumar) #->mean sampling years
sd(df3$sumar)


df1 <-df %>% group_by(site_id) %>% mutate(total = max(year)- min(year))
mean(df1$total) # -> mean o total years
sd(df1$total)

##### Plot time series ----

#Plot of time series:
df <- read_excel("Invasion_Melt.xlsx", sheet = "First invader")
df <- read_excel("ponto-caspian.xlsx")
df1 <- df[,c(1,2,6)]
df1 <- df1 %>% group_by(site_id, country) %>% mutate(End = max(year),
                                                    star=min(year))
df1$site_id <- as.factor(df1$site_id)
df1 <- df1[order(df1$star),]

mycolors <- brewer.pal(11, name = 'Paired')
mycolors2 <- data.frame(cbind(colB=mycolors, country= unique(as.character(df1$country))))
df1 <- merge(df1, mycolors2, by="country")
df1 <- df1[order(-df1$star),]
par(mar=c(4, 4, 1, 1)) 

plot(df1$star,c(1:length(df1$star)), xlim=c(1968, 2020), type="n", 
     las=1, axes=F, xlab="Year", ylab= "Time series")
segments(x0=df1$star, x1=df1$End, y0=c(1:length(df1$star)),
         y1=c(1:length(df1$star)), col=as.character(df1$colB),lwd=1.7)



abline(v=c(1970,1980,1990,2000,2010,2020), col=c("black"), lty=c(2), lwd=c(1))
axis(1)
legend("bottomleft", lty=1,lwd=2, col=unique(as.character(df1$colB)), 
       legend=c("United Kingdom", 
                "Hungary", "Austria", "Spain",
                "Netherlands", "Germany","Belgium","France", "Denmark","Latvia",
                "Switzerland"))


#### Europe map
df <- read_excel("Invasion_Melt.xlsx")
df<- df[!duplicated(df$site_id),]
points(df$Long , df$Lat, 
       col="black", pch=21, cex=1, bg="grey70")

newmap <- getMap(resolution="high")
par(mar=c(0.5,0.5,0.5,0.5)) ## ajustando los márgenes
plot(newmap, col="tan1", border="black",bg="lightblue", 
     xlim=c(10,34), ylim=c(33,63))

riversData <- readOGR("Europe_Hydrography.shp") # load the shapefile
plot(riversData, col= "steelblue2", add=T)



######### Mixed model by Miguel
df <- read_excel("mixed_model.xlsx") 
head(df)
str(df)
df$Richness <- as.factor(df$Richness)
model <- lmer(Dissimilarity3 ~ year + (1 | Richness), data = df,
              control=lmerControl(check.nobs.vs.nlev = "ignore",
                  check.nobs.vs.rankZ = "ignore",
                      check.nobs.vs.nRE="ignore"))

model <- lm(Dissimilarity3 ~ year +  Richness, data = df)

summary(model)
r.squaredGLMM(model)
check_model(model)
plot_model(model, type = "est")
plot_model(model, type = "pred", terms = "year")

###### Jaccard similarity
df <- read_excel("Jaccard_Dissimilarity.xlsx") 
str(df)
df<- df %>% filter(df$Ponto== "Without PC")
df$year<-  as.factor(df$year)
ggplot(df, aes(year, Standardized, group=1)) + geom_point()+geom_line() + ylim(0.004,0.006)+
  theme_classic2() +  theme_cleveland() 

#### Order or arrival
df <- read_excel("Invasion_Melt.xlsx", sheet = "First invader")
df1<- df %>% group_by(Group) %>% summarise(entries = n())
sum(df1$entries) #277
df1$Prop <- (df1$entries/274) * 100
df1

df <- read_excel("Invasion_Melt.xlsx", sheet = "Bivalvia")
df1<- df %>% group_by(Group,sp) %>% summarise(entries = n())
sum(df1$entries) #133
df1$Prop <- (df1$entries/133) * 100
df1

df <- read_excel("Invasion_Melt.xlsx", sheet = "Amphipod")
df1<- df %>% group_by(Group,sp) %>% summarise(entries = n())
sum(df1$entries) #97
df1$Prop <- (df1$entries/97) * 100
df1

df <- read_excel("Invasion_Melt.xlsx", sheet = "First invader")
df1<- df %>% group_by(Group) %>% summarise(entries = n())
sum(df1$entries) #277
df1$Prop <- (df1$entries/274) * 100
df1


########## Cumulative plot
df <- read_excel("PRUEBA.xlsx")
df <- read_excel("PRUEBA1.xlsx", sheet = "Hoja3")
ggplot(df, aes(year)) + stat_ecdf(geom = "step")+theme_classic2()+theme_cleveland()

#####       Piecewise Regression in R ------ Species over time
df <- read_excel("Invasion_Melt.xlsx")
a <- df %>% group_by(year, taxon) %>% summarise(Ponto = n())
b <- a %>% mutate(PontoCaspian = 1)
b<-b %>% group_by(year) %>% summarise(Ponto= sum(PontoCaspian))
b<- b %>% filter(year< 2019)
ggplot(b, aes(year,Ponto)) + geom_line()
fit <- lm(year ~ Ponto, data=b)

segmented.fit <- segmented(fit, seg.Z = ~year, psi=2005)
summary(segmented.fit)


###### Distance to the native region 
df <- read_excel("Distance_river.xlsx")
df<- df %>% filter(!Country=="Hungary")
df <- read_excel("TEST.xlsx", sheet = "Distance")

ggplot(df, aes(Lenght,Ponto_Caspian)) + geom_jitter() +geom_smooth(method = "glm", alpha=1/10)+
  theme_classic2() +  theme_cleveland()

model4 <- glmer.nb(Ponto_Caspian ~  Lenght + First_year+
                  (1 | Country), control = glmerControl(tolPwrss=1e-3), data = df)

summary(model4)
check_model(model4)

library(glmmTMB)

model4 <- glmer(Ponto_Caspian ~ Lenght + First_year + (1 | Country),
                data = df, family = nbinom2)


summary(lm)
summary(model4)

p1<- plot_model(model4, type = "pred", terms = "Lenght")
p2<- plot_model(lm, type = "pred", terms = "Time")
p1+p2
p1
df0<- df %>% group_by(year,taxon) %>% summarise(Number=n())
df0<- df0 %>% mutate(Number2= 1)
df0<- df0%>% group_by(year) %>% summarise(Number=sum(Number2))

ggplot(df0, aes(year,Number)) + geom_line() + theme_cowplot()

db<- df %>% group_by(site_id,year)  %>% summarise(n=n())
db<- db %>% mutate(Number2= 1)
db<- db%>% group_by(year) %>% summarise(Number=sum(Number2))


df0$Richness <- c(12,15,4,2,13,4,12,4,7,2,59,37,45,66,45,51,62,32,57,55,66,28,61,56,46,29,47,70,43,48,43,59,32,67,46,78,
                  78,67,67,87,79,72,83,81,83,76,33,37,1)
df0$Phillip <- df0$Number/df0$Richness
df0

ggplot(df0, aes(year,Phillip)) + geom_line()

model<- lm(Number ~ Richness, data = df0)
model<- glm(Number ~year+ Richness, data = df0)
summary(model)

## plot_model
library(sjPlot)
plot_model( type = "std2",
            sort.est = T,
            vline.color = "black",
            show.values = T,
            value.offset = .3)
tab_model()


df1<- df %>% group_by(year) %>% summarise(Rel= mean(`Relative abundance`))
ggplot(df1, aes(year,Rel)) + geom_line() + geom_point() + theme_classic2()+
  theme_cleveland()


a<- gam(Rel~ s(year), data=df1)
summary(a)
p1<- plot_smooths(
  model = a,
  series = year)
p1





########################################################################
######################         PART 2.0            ######################
########################################################################


#Number of species over time 
df <- read_excel("Invasion_Melt.xlsx")
head(df)

df<- df %>% group_by(site_id) %>% mutate(First_year=min(year))

a<- df %>% group_by(taxon, year) %>% dplyr::summarise(entries =n())
b<- a %>% group_by(year) %>% dplyr::summarise(entries =n())

ggplot(b, aes(year, entries)) + geom_line() + geom_smooth(alpha=1/10) + 
  theme_classic2() +  theme_cleveland()

#Number of time series over time 
c<- df %>% group_by(site_id, year) %>% summarise(total_time_series=n()) 
d<- c %>% group_by(year) %>% dplyr::summarise(total_time_series =n())

data= right_join(b,d, by="year") %>% filter(!year=="2020")
mean(data$entries)
sd(data$entries)
hist(data$entries)
model1<- glm(entries ~ year + total_time_series,  family = poisson(link = "log"),
             data=data)
summary(model1)

p1<- plot_model(model1, type = "eff", terms = "year")
# Get the deviance
deviance(model1) #27.95145


#Cumulative number of time series over time 
#df <- read_excel("PRUEBA1.xlsx", sheet = "Hoja3")
df <- read_excel("Invasion_Melt.xlsx", sheet = "First ocurrence")


ggplot(df, aes(Cumulative)) + stat_ecdf(geom = "step")+ theme_classic2()+theme_cleveland()

hist(df$Cumulative)
mean(df$Cumulative)
sd(df$Cumulative)

model2<- glm(Cumulative ~ year + Number_time_series,  family = poisson(link = "log"),
             data=df)
summary(model2)


p2<- plot_model(model2,type = "eff", terms = "year")
p2

p1+p2


############### Trends of key taxa and overall 

df <- read_excel("Invasion_Melt.xlsx")
head(df)
 
a<-df %>% group_by(site_id, year) %>% summarise(Abundance=mean(abundance)) %>%
  filter(Abundance < 5000) %>% filter(year<2020)
hist(Overall$Abundance)

Overall<- right_join(a,d, by="year")
model3 <- glmer(Abundance ~ year + total_time_series+
        (1 + year | site_id), data = Overall, family=negative.binomial(theta=0.5))




df <- read_excel("Invasion_Melt.xlsx")
df<- df %>% filter(Group=="Prosobranchia")
unique(df$Group)
unique(df$site_id)



xy.list <- split(df$abundance, df$site_id) #132 time series 
xy.list <- xy.list[lengths(xy.list) >= 5]  # 110 time series
length(xy.list)


MK <-as.data.frame(do.call(rbind,lapply(xy.list[1:137],function(x)unlist(My.mmkh(x))))) #96 are time series,
head(MK)
#also note that My.mmkh is Pilottos adapted Mann Kendall trend test
setDT(MK, keep.rownames = TRUE)[]
colnames(MK)[1] = "site_id"
colnames(MK)[6] = "P_value"
colnames(MK)[11] = "S_statistic"
MK$site_id<-as.numeric(MK$site_id)

Over<- right_join(MK,df, by="site_id")


Over<<- Over[c(1:2206),]
Over<<- Over[, c(1,9,11,18,19,20)]
Over$const=1
Over<<- Over[!duplicated(Over$site_id), ]


res <<- rma.mv(S_statistic, old.variance, random = ~ (Long+Lat) | const, struct="SPGAU",
               data= Over)
res
plot_model(res)
forest(res, showweights = T, order="obs", slab= Over$site_id)




devtools::install_github("daniel1noble/orchaRd", force = TRUE)
library(orchaRd)


model_results <- orchaRd::mod_results(res, mod = "1", group = "site_id", data = Over)
print(model_results)

Over<- setDF(Over)
Over <- read_excel("a.xlsx")
model_results <- orchaRd::mod_results(res, mod = "1", group = "site_id", data = Over)

orchard_plot(res, mod = "1", group = "site_id",
          transfm = "none",data = Over)

data(english)
# We need to calculate the effect sizes, in this case d
english <- escalc(measure = "SMD", n1i = NStartControl, sd1i = SD_C, m1i = MeanC, n2i = NStartExpt, sd2i = SD_E, m2i = MeanE, 
                  var.names=c("SMD","vSMD"),
                  data = english)
english_MA <- rma.mv(yi = SMD, V = vSMD, random = list( ~ 1 | StudyNo, ~ 1 | EffectID), data = english)
summary(english_MA)


model_results <- orchaRd::mod_results(english_MA, mod="Int", data=english, group = EffectID)
print(model_results)

orchard_plot(english_MA, group = "StudyNo",
             mod = "ManipType", xlab = "Standardised mean difference")


plot_model(res, type="eff")



ggplot(Over, aes(const, S_statistic)) + 
geom_boxplot(
  width = .25, 
  outlier.shape = NA
) +
  geom_jitter(data = Over,
             aes(x = const, S_statistic,
                 size = S_statistic),
             alpha = .6)+
  scale_size_continuous(name = "Number of estimates\nper year",
                        breaks = c(-400, -200, 0, 200,400)) 




################### Order of arrival ###########

df <- read_excel("Invasion_Melt.xlsx", sheet = "previous year")
head(df)
df <- df[,-c(3,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,35,36)]



########## Ponto caspian in reference to all invasive species
setwd("C:/Users/isma-/OneDrive/Escritorio/full databse/DATA")

df <- read_excel("Global_dataset.xlsx") 
df <- df %>% filter(Alien =="Y")


setwd("C:/Users/isma-/OneDrive/Escritorio/Invasional meltdown")
df1 <- read_excel("Invasion_Melt.xlsx")
Ponto_caspian <- unique(df1$taxon)

df1 <- df %>% filter(taxon %in% Ponto_caspian)

unique(df$taxon)
df <- df %>% filter(!taxon=="corbicula \"fluminalis\"")
unique(df1$taxon)


a<- df %>% group_by(taxon, year) %>% dplyr::summarise(entries =n())
b<- a %>% group_by(year) %>% dplyr::summarise(entries =n())%>% filter(!year ==2020)

c<- df1 %>% group_by(taxon, year) %>% dplyr::summarise(entries =n())
d<- c %>% group_by(year) %>% dplyr::summarise(entries =n()) %>% filter(!year ==2020)


ggplot(b, aes(year, entries)) + geom_line(color= "red") + geom_smooth(data=b,alpha=1/10) + 
  geom_line(data=d, aes(year, entries), color= "blue")+ geom_smooth(data=d,alpha=1/10, color= "blue")+
  theme_classic2() +  theme_cleveland()



############ Meta-regression based on richness ######

setwd("C:/Users/isma-/OneDrive/Escritorio/Invasional meltdown")
df <- read_excel("Invasion_Melt.xlsx")
unique(df$Group)
df<- filter(df, Group=="Bivalvia")

df1<- df %>% group_by(taxon, site_id,year) %>% summarise(Count=n())
df2<- df1 %>%group_by(site_id, year) %>% summarise(Richness=n())


xy.list <- split(df2$Richness, df2$site_id) #132 time series 
xy.list <- xy.list[lengths(xy.list) >= 5]  # 110 time series
length(xy.list) # 168 time series?


{
df2<- df2 %>% filter(!site_id=="100000011")
df2<-df2 %>% filter(!site_id=="108000020")
df2<-df2 %>% filter(!site_id=="108000087")
df2<-df2 %>% filter(!site_id=="109000226")
df2<-df2 %>% filter(!site_id=="109000243")
df2<-df2 %>% filter(!site_id=="109000247")
df2<-df2 %>% filter(!site_id=="109000366")
df2<-df2 %>% filter(!site_id=="114000015")
df2<-df2 %>% filter(!site_id=="114000020")
df2<-df2 %>% filter(!site_id=="114000045")
df2<-df2 %>% filter(!site_id=="114000083")
df2<-df2 %>% filter(!site_id=="114000087")
df2<-df2 %>% filter(!site_id=="117000007")
df2<-df2 %>% filter(!site_id=="117000017")
df2<-df2 %>% filter(!site_id=="117000030")
df2<-df2 %>% filter(!site_id=="114000014")
} # Overall 
{
  df2<- df2 %>% filter(!site_id=="100000001")
  df2<- df2 %>% filter(!site_id=="100000002")
  df2<- df2 %>% filter(!site_id=="100000308")
  df2<- df2 %>% filter(!site_id=="100000309")
  df2<- df2 %>% filter(!site_id=="100000312")
  df2<- df2 %>% filter(!site_id=="100000313")
  df2<- df2 %>% filter(!site_id=="108000164")
  df2<- df2 %>% filter(!site_id=="109000235")
  df2<- df2 %>% filter(!site_id=="109000243")
  df2<- df2 %>% filter(!site_id=="109000247")
  df2<- df2 %>% filter(!site_id=="109000321")
  df2<- df2 %>% filter(!site_id=="114000014")
  df2<- df2 %>% filter(!site_id=="114000020")
  df2<- df2 %>% filter(!site_id=="114000048")
  df2<- df2 %>% filter(!site_id=="114000056")
  df2<- df2 %>% filter(!site_id=="114000062")
  df2<- df2 %>% filter(!site_id=="114000067")
  df2<- df2 %>% filter(!site_id=="114000083")
  df2<- df2 %>% filter(!site_id=="118000004")
} # Amphipods

element = xy.list[[116]]


MK <-as.data.frame(do.call(rbind,lapply(xy.list[1:99],function(x)unlist(My.mmkh(x))))) #96 are time series,
head(MK)

setDT(MK, keep.rownames = TRUE)[]
colnames(MK)[1] = "site_id"
colnames(MK)[6] = "P_value"
colnames(MK)[11] = "S_statistic"
MK$site_id<-as.numeric(MK$site_id)


Over<- right_join(MK,df, by="site_id")

Over<<- Over[c(1:1903),]
Over<<- Over[, c(1,9,11,18,19,20)]
Over$const=1
Over<<- Over[!duplicated(Over$site_id), ]


res <<- rma.mv(S_statistic, old.variance, random = ~ (Long+Lat) | const, struct="SPGAU",
               data= Over)
res


data <- read_excel("Meta_reg.result.xlsx")
unique(data$Model)
data$Model = 
  factor(data$Model, levels=c("Prosobranchia", "Bivalves", "Amphipods", "Overall"))


ggplot(data, aes(Model, Estimate, ymin=CI_lower, ymax=CI_upper))  +
  theme_classic() +
  geom_pointrange(size=0.8,position = position_dodge(0.9)) + 
  geom_hline(aes(yintercept = 0),linetype="dashed")+
  theme(legend.position="bottom") + coord_flip()+ ylim(-10, 20)






