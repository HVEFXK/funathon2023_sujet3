# Lecture du fichier requirements.txt
requirements <- readLines("requirements_R.txt")

# Installation des packages
for (package in requirements) {
  install.packages(package)
}

library(aws.s3)
library(dplyr)
library(readr)
library(tidyverse)

bucket <- "projet-funathon"
path_data <- "2023/sujet3/diffusion"

description_indiv <- s3read_using(read_delim, object = paste(path_data, "description-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
habitudes_indiv <- s3read_using(read_delim, object = paste(path_data, "habitudes-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
actphys_sedent <- s3read_using(read_delim, object = paste(path_data, "actphys-sedent.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
fpq <- s3read_using(read_delim, object = paste(path_data, "fpq.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)

library(ggplot2)
library(ggcorrplot)
library(sf)

# Option d'affichage
options(dplyr.width = Inf)
options(repr.plot.width=20, repr.plot.height=10)


table_ponderation <- description_indiv %>%
  select(
    NOIND,
    pond_indiv_adu_pop2,
    pond_indiv_enf_pop1,
    region_adm_12cl,
    agglo_5cl,
    sex_PS,
    tage_PS,
    RUC_4cl,
    diplome_PR,
    nbpers,
    menopause,
    poids_perception,
    fume,
    nb_cigares_jour
  )

activite<-actphys_sedent %>% 
  left_join(table_ponderation)
activite_pond<-activite %>% 
  mutate_at(c('pond_indiv_adu_pop2','pond_indiv_enf_pop1'),
        ~replace_na(.,0)) %>% 
  mutate(POND=pond_indiv_adu_pop2+pond_indiv_enf_pop1)
 
test<-activite_pond %>% 
  select(pond_indiv_adu_pop2,pond_indiv_enf_pop1,POND)
view(test)
view(activite_pond$POND)

counts_ <- actphys_sedent %>% group_by(profil_activite,sedentarite) %>%
  summarise(n=n(),.groups="drop") %>% 
  spread(sedentarite,n,fill=0)

ggplot(actphys_sedent) +
  geom_histogram(aes(x = activite_jogging_score,
                     color="grey",fill="lightblue"))

# Graphique en barres horizontales
ggplot(data=actphys_sedent,aes(x=profil_activite,y=sedentarite))+
  geom_histogram(stat='identity')+
  coord_flip()+
  labs(title="Profil des activités",
       x="Profil des activités",
       y="Nombre d'individus")

counts_agglo <- table_ponderation %>% group_by(agglo_5cl) %>% summarise(n=n())

# Générer le graphique en barres horizontales
ggplot(data=counts_agglo,aes(x=agglo_5cl,y=n))+
  geom_histogram(stat="identity")+
  coord_flip()+ 
  labs(title="Histogramme des types d'agglomération",
       x="Type d'agglomération",
       y="Nombre d'individus")
table_revenu <- table_ponderation %>% mutate(categorie_ruc=case_when(RUC_4cl==1 ~ "<900 €/mois/UC",
                                                                          RUC_4cl==2 ~ "[900-1 340[ €/mois/UC",
                                                                          RUC_4cl==3 ~ "[1 340-1 850[ €/mois/U",
                                                                          RUC_4cl==4 ~ ">=1 850 €/mois/UC"))
table2<-activite_pond %>% group_by(sedentarite)%>% 
  summarise(revenu_moyen=mean(RUC_4cl,na.rm=TRUE))

ggplot(data=table2,aes(x=sedentarite,y=revenu_moyen))+
  geom_histogram(stat="identity")+
  labs(title="Revenu moyen par activité",
       x="sedentarité",
       y="Revenu moyen") 

#matrice de corrélation entre les variables
df_num <- activite %>% select(where(is.numeric))

df_num <- df_num %>% select(c("RUC_4cl","jvideo_score","activite_jogging_score",
                              "sedentarite","activite_athletisme_score",
                              "fume","menopause","diplome_PR","poids_perception"))

matrice_correlation <- model.matrix(~0+., data=df_num) %>% 
  cor(use="pairwise.complete.obs")

matrice_correlation %>%   ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=15)
