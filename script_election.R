> summary(resultats_pres2022$Part_Macron)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   20.84   25.75   26.23   31.12  100.00 
> summary(resultats_pres2022$Part_abs)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   18.49   22.10   24.12   26.85  100.00
> moyenneMacron <- mean(resultats_pres2022$Part_Macron)
> moyenneMacron  # on appelle la variable créée
[1] 26.22618
> paste("La moyenne des % de vote Macron est de ", moyenneMacron)
[1] "La moyenne des % de vote Macron est de  26.2261807927442"
> paste("La moyenne des parts de vote Macron est de ", round(moyenneMacron, digits=2), "%")
[1] "La moyenne des parts de vote Macron est de  26.23 %"
Requete1 <- resultats_pres2022 [resultats_pres2022$Part_LePen > 10 , ]
View (Requete1) // permet de visualiser la nouvelle extraction
> detach("package:dplR", unload = TRUE) #On instale et on active la librairie
> count(Requete1)
# A tibble: 1 × 1
    n> 
  <int>
1 64347
Requete2 <- resultats_pres2022 [resultats_pres2022$Part_LePen > 30 & resultats_pres2022$Part_LePen < 60 , ]
> View (Requete2) #permet de visualiser la nouvelle extraction
> voteJadot2040 <- resultats_pres2022 [resultats_pres2022$Part_Jadot > 20 & resultats_pres2022$Part_Jadot < 40 , ]
> View(voteJadot2040)
voteRochefort <- resultats_pres2022 [resultats_pres2022$Libelle =='Rochefort' & resultats_pres2022$Code_dep == '17' ,]
View (voteRochefort) # permet de visualiser la nouvelle extraction
> voteSaintes <- resultats_pres2022 [resultats_pres2022$Libelle =='Saintes' & resultats_pres2022$Code_dep == '17' ,]

>  write.csv(voteSaintes,'E:/SIG/COURS/10_R_ET_GITHUB/Data/votesaintes.csv',row.names=FALSE)
> voteCandidats1_4 <- resultats_pres2022 [ , c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")]
> View (voteCandidats1_4)
> typeof(voteCandidats1_4)
[1] "list"
> sapply(voteCandidats1_4, class)
   Part_Macron     Part_LePen Part_Melenchon  Part_Pecresse 
     "numeric"      "numeric"      "numeric"      "numeric" 
voteCandidatsSainte <- resultats_pres2022[resultats_pres2022$Libelle == 'Saintes', c("Part_Macron", "Part_LePen", "Part_Melenchon")]
> View(voteCandidatsSainte)
> ecarttype <- sqrt(mean(voteCandidatsSainte$Part_Macron^2)-mean(voteCandidatsSainte$Part_Macron)^2)
> ecarttype <- sqrt(mean(voteCandidatsSainte$Part_LePen^2)-mean(voteCandidatsSainte$Part_LePen)^2)
> ecarttype <- sqrt(mean(voteCandidatsSainte$Part_Melenchon^2)-mean(voteCandidatsSainte$Part_Melenchon)^2)
>  voteCandidats1_4$colonneTest <- voteCandidats1_4$Part_Macron +voteCandidats1_4$Part_Melenchon
> 
> View(voteCandidats1_4)
> voteCandidatsSainte$MacronSup30 <- ifelse(voteCandidatsSainte$Part_Macron > 30, 'MacronSup', NA)
> voteCandidatsVaucluse <- resultats_pres2022[resultats_pres2022$Libelle_dep == 'Vaucluse', c( "Code_com","Libelle","Part_Arthaud")]
> resultats_communesVaucluse <- voteCandidatsVaucluse %>%
+     summarize(Somme_Part_Arthaud = sum(Part_Arthaud))
# On définit les noms des candidats dans une variable nommée « col1 »
col1 <- c("Macron", "Melenchon", "Le Pen", "Pecresse")
# On reprend les % de vote issus de l’extraction précédente, dans une variable nommée « col2 »
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_Melenchon), 
mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Pecresse)) 
# On crée le tableau de données du graphique en spécifiant que les valeurs du graph correspondent à « col2 »
data <- data.frame(group=col1, value=col2)
# On crée le graphique :
ggplot(data, aes(x="", y=value , fill=group)) +
 geom_bar(stat="identity", width=1) +
 geom_col() +
 coord_polar("y", start=0) +
# On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
scale_fill_manual(values = c("#0a3895", "#b831f3",
 "#f33157", "#6091f6"))

# On définit les noms des candidats dans une variable nommée « col1 »
col1 <- c("Hidalgo", "Jadot", "Melenchon")
# On reprend les % de vote issus de l’extraction précédente, dans une variable nommée « col2 »
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_Melenchon), 
mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Pecresse)) 
# On crée le tableau de données du graphique en spécifiant que les valeurs du graph correspondent à « col2 »
data <- data.frame(group=col1, value=col2)
# On crée le graphique :
ggplot(data= data, aes(x=reorder(group, -value), y=value, fill=group)) +
 geom_bar(stat="identity")+
# On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
 geom_text(aes(label=round(value, digits=2)), vjust=1.6, color="white", size=3.5)+
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))

voteCandidatsLaRochelle <- resultats_pres2022 [ , c("Part_Hidalgo", "Part_Jadot", "Part_Melenchon")]
# On définit les noms des candidats dans une variable nommée « col1 »
col1 <- c("Hidalgo", "Jadot","Melenchon")
# On reprend les % de vote issus de l’extraction précédente, dans une variable nommée « col2 »
col2 <- c(mean(voteCandidatsLaRochelle$Part_Hidalgo), mean(voteCandidatsLaRochelle$Part_Jadot), 
          mean(voteCandidatsLaRochelle$Part_Melenchon))
# On crée le tableau de données du graphique en spécifiant que les valeurs du graph correspondent à « col2 »
data <- data.frame(group=col1, value=col2)
# On crée le graphique :
ggplot(data, aes(x="", y=value , fill=group)) +
    geom_bar(stat="identity", width=1) +
    geom_col() +
    coord_polar("y", start=0) +
# On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
scale_fill_manual(values = c( "#DF0101","#04B404", "#F781F3"))
    

Transpo <- resultats_pres2022 %>% pivot_longer(
cols = c(Part_DupontAignan, Part_LePen, Part_Pecresse, Part_Zemmour), 
names_to="Candidat", values_to="Parts_vote")
View(Transpo)
 
# plot
ggplot(Transpo, aes(x=Candidat, y=Parts_vote, fill=Candidat)) +
    geom_boxplot(varwidth = TRUE, alpha=0.2) +
    theme(legend.position="none") +
    scale_x_discrete(labels=my_xlab)

voteRochefort_Candidats1_4 <- voteRochefort [ , c("Code_BV", "Part_Macron" , "Part_LePen" , "Part_Melenchon", 
"Part_Pecresse")]
# On reformatte le tableau de données (nécessite le package tidyr)
reformat <-
voteRochefort_Candidats1_4 %>% pivot_longer(cols=c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse"),
names_to='candidats', values_to='parts')
# On réalise autant de graphs qu’il existe de Bureaux de vote (« facet_wrap »)
ggplot(data=reformat, aes(x=candidats,y=parts,fill=candidats)) +
 geom_bar(stat="identity") +
 facet_wrap(~Code_BV) +
 ggtitle("Parts de vote dans les BV de Rochefort") +
 theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
 theme(axis.text.x = element_text(angle=90)) +
 scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))


voteRoyan <- resultats_pres2022 [resultats_pres2022$Libelle =='Royan' & resultats_pres2022$Code_dep == '17' ,]
View (voteRoyan) # permet de visualiser la nouvelle extraction

voteRoyan_Candidats1_4 <- voteRochefort [ , c("Code_BV", "Part_Macron" , "Part_LePen" , "Part_Melenchon", 
"Part_Pecresse")]
# On reformatte le tableau de données (nécessite le package tidyr)
reformat <-
voteRoyan_Candidats1_4 %>% pivot_longer(cols=c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse"),
names_to='candidats', values_to='parts')
# On réalise autant de graphs qu’il existe de Bureaux de vote (« facet_wrap »)
ggplot(data=reformat, aes(x=candidats,y=parts,fill=candidats)) +
 geom_bar(stat="identity") +
 coord_polar("y", start=0) +
 facet_wrap(~Code_BV) +
 ggtitle("Parts de vote dans les BV de Royan") +
 theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
 theme(axis.text.x = element_text(angle=90)) +
 scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))

  # Requête de sélection
> requete <- dbGetQuery(conn, 'SELECT * from php.form;')
> View(requete)

# Exécutez la requête de suppression
requete1 <- dbSendQuery(conn, 'DELETE FROM php.form WHERE typedemande = 1;')