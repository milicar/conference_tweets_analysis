### Analiza 
### Mrezne metrike

### 2. Da li svi ucesnici ujednaceno komuniciraju ili se stvaraju grupe sa intenzivnijom komunikacijom? 

### Iako i dosadasnji graficki prikazi dosta govore, probacu da saznam nesto vise o stepenu cvorova.

deg_dist <- lapply(conf_graph_list, function(x) { 
  res <- list()
  res$dd_all <- degree.distribution(x, mode = "all")
  res$dd_in <- degree.distribution(x, mode = "in")
  res$dd_out <- degree.distribution(x, mode = "out")
  return(res)})


# pdf("2_deg_distribution.pdf")
# lapply(1:length(deg_dist), function(x){ 
#   xmax <- max(sapply(deg_dist[[x]], length))
#   ymax <- max(sapply(deg_dist[[x]], max))
#   plot(c(0,xmax), c(0,ymax), type = "n", xlab = "degree", ylab = "frequency", 
#        main = paste("Degree distribution\n", graph_title[x]) )
#   lines(deg_dist[[x]]$dd_all, col = "red", lwd = 5)
#   lines(deg_dist[[x]]$dd_in, col = "green", lwd = 3)
#   lines(deg_dist[[x]]$dd_out, col = "purple", lwd = 2)
#   legend("topright", legend = c("out degree", "in degree", "total degree"), lwd = c(2, 3, 5), 
#          col = c("purple", "green", "red"))
# })
# dev.off()

TODO # vrednosti stepena ne treba da krecu od 1, nego od 0..
pdf("2_deg_distribution_log.pdf")
lapply(1:length(deg_dist), function(x){ 
  xmax <- max(sapply(deg_dist[[x]], length))
  ymax <- max(sapply(deg_dist[[x]], max))
  plot(c(0.9,xmax), c(0.002,ymax), log = "xy", type = "n", xlab = "degree", ylab = "frequency", 
       main = paste("Degree distribution\n", graph_title[x]) )
  lines(deg_dist[[x]]$dd_all, col = "red", lwd = 2, type = "o")
  lines(deg_dist[[x]]$dd_in, col = "green", lwd = 2, type = "o")
  lines(deg_dist[[x]]$dd_out, col = "purple", lwd = 2, type = "o")
  legend("topright", legend = c("out degree", "in degree", "total degree"), lwd = c(2, 2, 2), 
         col = c("purple", "green", "red"))
})
dev.off()


### Distribucije stepena prate "power law" distribuciju, odnosno najveci broj ucesnika ima veoma mali stepen, 
### a sa porastom stepena frekvencija naglo opada i pojavljuje se karakteristican dugacki "rep". Distribucije po
### vremenskim intervalima su dosta slicne, sa izuzetkom perioda u kome se odrzao skup, gde je broj ucesnika koji imaju
### odlazni (a time i ukupni) stepen jednak nuli - relativno blizu nule, odnosno, mala je frekvencija ucesnika koji 
### nisu u tvitu pomenuli nekog drugog ucesnika, ali je velika frekvencija onih koji nisu pomenuti u tvitovima drugih,
### onih ciji je dolazni stepen jednak nuli. Mali broj pominjanih ucesnika, kao i male vrednosti odlaznog stepena za veliku
### vecinu ucesnika ukazuju na to da je komponenta u kojoj ucestvuje 97% svih cvorova zapravo vise proizvod posrednih, 
### a ne direktnih veza medu ucesnicima.

conf_v_degs <- lapply(conf_graph_list, function(x) { 
  res <- list()
  res$d_all <- degree(x, mode = "all")
  res$d_in <- degree(x, mode = "in")
  res$d_out <- degree(x, mode = "out")
  return(res)})

conf_degs_anlys <- lapply(conf_v_degs, function(x) {
  res <- list()
  res$mean <- sapply(x, mean)
  res$sd <- sapply(x, sd)
  return(res)
})

conf_degs_anlys_mat <- matrix(unlist(conf_degs_anlys), ncol = 3, byrow = TRUE, dimnames = list(
  c("time_1 mean", "time_1 sd", "time_2 mean", "time_2 sd", "time_3 mean", "time_3 sd", "time_4 mean", "time_4 sd",
    "time_5 mean", "time_5 sd"), c("degree all", "degree in", "degree out")
))


### Varijacije u stepenu su izrazene u periodu trajanja skupa, i to u odlaznom stepenu 5 jedinica, u dolaznom 
### stepenu 13, sto govori o tome da se ucesnici mnogo vise razlikuju po ugledu nego po samom kvantitetu kontakata;
### osim toga, varijacije u ukupnom stepenu su 18 jedinica, sto znaci da se ne radi o podeli ucesnika na one koji 
### imaju visok dolazni i one koji imaju visok odlazni stepen, vec na one koji imaju ukupan stepen visok i na one 
### druge. 

### U graficki prikaz mreze sada moze da se ubaci i informacija o stepenu cvora. Za neki uopsteni pregled cu da prikazem 
### samo informaciju o ukupnom stepenu, dok bi se poredenjem odlaznog i dolaznog stepena mogao naslutiti uticaj 
### pojedinacnih ucesnika. Kombinovanjem informacija o stepenu i broju tvitova iz originalnog skupa moze da se prati obim
### interakcija ucesnika sa razlicitim brojem tvitova o skupu.

pdf("2_stepen_all.pdf")
lapply(conf_graph_list, function(x) { plot(x, layout = layout_with_fr(x), frame = TRUE, margin = c(0.2, 0, 0, 0),
                                           main = x$title, sub = "users with total degree and number of tweets with #WIELead or @wieilc", vertex.label = NA, 
                                           vertex.size = rescale(degree(x, mode = "all"), to = c(2, 10)), vertex.color = yellowred[round(rescale(log(V(x)$nstarttw), to = c(0,26)))],
                                           edge.arrow.size = 0.15, edge.color = alpha("black", rescale(log(E(x)$weight), to = c(0.3, 1))))
  legend("bottomright", ncol = 2, title = "n tweets and degree", legend = c("high n", "low n", "high degree", "low degree"), cex = 0.8, 
         pt.cex = c(1, 1, 2.5, 1), col = "black", pch = 21, pt.bg = c(yellowred[26], yellowred[1], NA, NA))})
dev.off()

### Jasno je da komunikacija nije ujednacena u celoj mrezi. Na nivou mreze moze da se izracuna koeficijent klasterovanja,
### koji predstavlja odnos trouglova i trijada u mrezi; funkcija transitivity u igraph paketu racuna ovu metriku ne uzimajuci
### u obzir smer i tezinu veza. Izolati takode ne uticu na rezultat. 

conf_transitivity <- sapply(conf_graph_list, transitivity, type = "global", isolates = NaN)
# time_1 -> time_5 : 0.20504475 0.15757879 0.06979682 0.16435882 0.21428571 

### Ovo je vrlo mala vrednost za period odrzavanja skupa. (neke vrednosti za poredenje?) Ali, ako se uzme u obzir razlika
### velicina slabe i jake  maksimalne komponente (odnosno jednostrane i, uslovno receno, uzajamne komunikacije), onda 
### ova brojka izgleda vrlo moguca i upucuje na strukturu bez mnogo klika ili jezgara. Dalje bi se mogle istraziti razlicite
### metrike koje upucuju na strukturu mreze, kao sto su otkrivanje grupa i klastera i centralizovanost mreze. Ukoliko bi
### se ispostavilo da je mreza u velikoj meri centralizovana, onda bi to moglo da objasni brzo nestajanje mreze posle
### skupa, kada oni cvorovi koji su drzali mrezu "na okupu" izgube svoj uticaj.


### Funkcije za nalazenje klika u igraph paketu tretiraju usmerene mreze kao neusmerene, uz poruku upozorenja - zbog toga
### sam koristila neusmerenu verziju mreza, sa vezom za svaki par uzajamnih veza. Ovde mi vise nije od interesa puka 
### povezanost, vec me zanima uzajamna komunikacija, koja ima potencijala da opstane i posle skupa.
### Za izracunavanje klika, kao i za dalje analize i vizuelizacije, zgodno je izbaciti izolate.

conf_no_iso_graph_list <- lapply(conf_graph_list, function(x) {res <- delete_vertices(x, V(x)[degree(x) == 0])} )
conf_no_iso_undirected <- lapply(conf_no_iso_graph_list, as.undirected, mode = "mutual")

conf_largest_clique <- sapply(conf_no_iso_undirected, clique_num) # velicina najvece klike (sa najvecim brojem clanova)
# time_1 -> time_5 :  3      3      4      3      3 

conf_largest_cliques_list <- lapply(conf_no_iso_undirected, largest_cliques) # lista najvecih klika
conf_n_largest_cliques <- sapply(conf_largest_cliques_list, length) # broj najvecih klika
# time_1 -> time_5 :  2      4      3      2      2

conf_max_cliques_list <- lapply(conf_no_iso_undirected, max_cliques, min = 3) # lista maksimalnih klika (koje nisu podgraf vece klike)
conf_n_max_cliques <- sapply(conf_no_iso_undirected, count_max_cliques, min = 3) # broj maksimalnih klika (od barem 3 cvora)
# time_1 -> time_5 :  2      4     49      2      2

### Klika je sama po sebi dosta restriktivna mera, a uz ovako definisane veze u mrezi, velicina najvece klike od cetiri 
### clana mozda i nije tako losa. Isto vazi i za broj maksimalnih klika u periodu odrzavanja skupa.

conf_coreness <- lapply(conf_no_iso_undirected, coreness) # za svaki cvor, k jezgra kome pripada (stepen, ne broj clanova)
conf_coreness_freqs <- lapply(conf_coreness, table) # frekvencije coreness vrednosti cvorova
conf_coreness_max <- sapply(conf_coreness_freqs, function(x) max(as.numeric(names(x)))) # najveca coreness vrednost po mrezi
# time_1 -> time_5 :   2      2      3      2      2 

### Vrednosti ove metrike odgovaraju rezultatima za najvece klike u pogledu stepena cvorova u grupama, odnosno, nesto 
### blazi kriterijumi povezanosti cvorova ne govore o tome da ima vecih grupa koje su cvrsto medusobno povezane, 
### ali ne toliko da su klike. Sto se tice broja jezgara, igraph ne odreduje pripadnost odredenog cvora konkretnim jezgrima.

conf_coreness_freqs
conf_cliques_freqs <- lapply(conf_max_cliques_list, function(x){
  table(sapply(x, length))
})


### Koliko je tacno mreza centralizovana? 

conf_centralizations_matrix <- matrix( data = c(
  sapply(conf_no_iso_graph_list, function(x) centr_betw(x, directed = TRUE)$centralization),
  sapply(conf_no_iso_graph_list, function(x) centr_betw(x, directed = FALSE)$centralization),
  sapply(conf_no_iso_graph_list, function(x) centr_clo(x, mode = "in")$centralization),
  sapply(conf_no_iso_graph_list, function(x) centr_clo(x, mode = "out")$centralization),
  sapply(conf_no_iso_graph_list, function(x) centr_clo(x, mode = "all")$centralization),
  sapply(conf_no_iso_graph_list, function(x) centr_degree(x, mode = "in")$centralization),
  sapply(conf_no_iso_graph_list, function(x) centr_degree(x, mode = "out")$centralization),
  sapply(conf_no_iso_graph_list, function(x) centr_degree(x, mode = "all")$centralization),
  sapply(conf_no_iso_graph_list, function(x) centr_eigen(x, directed = TRUE)$centralization),
  sapply(conf_no_iso_graph_list, function(x) centr_eigen(x, directed = FALSE)$centralization)),
  nrow = 10, ncol = 5, byrow = TRUE, dimnames = list( c("betweenness_centrality_directed","betweenness_centrality_undirected", 
                                                        "closeness_centrality_in", "closeness_centrality_out", "closeness_centrality_all", 
                                                        "degree_centrality_in", "degree_centrality_out", "degree_centrality_all",
                                                        "eigenvector_centrality_directed", "eigenvector_centrality_undirected"),
                                                      c("time_1", "time_2", "time_3", "time_4", "time_5"))
)


#**************************
### U vezi sa eigenvector centralnostima, funkcije koje ovo racunaju u igraph paketu oslanjaju se na ARPACK solver, i tu nesto
### nije kako treba (https://github.com/igraph/igraph/issues/589), funkcije ne vracaju konstantne rezultate, uglavnom u 
### zavisnosti od toga da li ima izolata, da li je  mreza usmerena, a ni tezine nisu dozvoljene. Za centralizovanost mreze, 
### kada se meri uzimajuci u obzir smer veza, moguci su problemi u smislu razlicitih vrednosti, ali izracunate centralnosti 
### za isti period daju razlicite vrednosti i kad se ne uzima u obzir smer veza:

eig_undir <- list()
for(i in 1:10){
  eig_undir[[i]] <- eigen_centrality(conf_no_iso_graph_list[[5]], directed = FALSE)
}
res_undir <- vector()
for (i in 1:10) {
  for (j in i:10){
    if (i == j) next
    else res_undir <- c(res_undir, assertthat::are_equal(eig_undir[i], eig_undir[j]))
  }
}
res_undir

eigen_dir <- list()
for(i in 1:10){
  eig_dir[[i]] <- eigen_centrality(conf_no_iso_graph_list[[5]], directed = TRUE, weights = NA)
}
res_dir <- vector()
for (i in 1:10) {
  for (j in i:10){
    if (i == j) next
    else res_dir <- c(res_dir, assertthat::are_equal(eig_dir[i], eig_dir[j]))
  }
}
res_dir

#*******************

### Centralizacije se racunaju normalizovane prema maksimalnoj vrednosti za mrezu date velicine; za mere
### intermedijarnosti, stepena i bliskosti maksimalnu centralizaciju ima mreza oblika zvezde, dok je za meru
### svojstvenog vektora to mreza sa samo jednom vezom i izolatima. Neke mere centralizacije za mrezu u toku
### odrzavanja skupa su dosta visoke. Centralizacija po intermedijarnosti (0.25) ukazuje na to da mozda postoje
### cvorovi koji "kontrolisu" komunikaciju u mrezi, ali ova vrednost nije previsoka; zanemarivanje smera veza bi
### preuvelicalo ovaj uticaj. Centralizacija po bliskosti je srednje visoka ako posmatramo sve veze bez obzira
### na smer (0.64), sto bi znacilo da postoji jedan broj cvorova koji je blizi ostalim cvorovima u sveukupnoj
### komunikaciji. Centralizacija po dolaznom stepenu je srednje visoka (0.55), sto ukazuje na postojanje jednog
### broja cvorova sa visokim dolaznim stepenom, a to su cvorovi sa velikim uticajem (najverovatnije organizatori
### skupa). Centralizacija po svojstvenom vektoru je izrazito visoka (0.96), i to u svim posmatranim periodima, sa
### minimalnim razlikama za usmerene i neusmerene mreze. Ovo moze da znaci da veoma uticajni cvorovi intenzivno
### komniciraju medusobno i na taj nacin dobijaju visoku centralnost svojstvenog vektora, dok su manje uticajni
### cvorovi iskljuceni iz komunikacije.
### Nesto kasnije cu ispitati i koji su konkretno ucesnici centralni, za sada mogu da zakljucim da je mreza srednje
### centralizovana - uzimajuci prosek ovih vrednosti (0.47 za sve ili 0.6 za po jednu vrednost po kategoriji:
### 0.25, 0.64, 0.55 i 0.96).



conf_clust_eb <- lapply(conf_no_iso_graph_list, cluster_edge_betweenness)
sapply(conf_clust_eb, function(x) max(x$membership))
# time_1 -> time_5 :  75     25     45     62     51 

conf_clust_wt <- lapply(conf_no_iso_graph_list, cluster_walktrap)
sapply(conf_clust_wt, function(x) max(x$membership))
# time_1 -> time_5 :  37     34     32     27     32 

### Klasterizacija daje veliki broj klastera, sto bi moglo da ukazuje na izuzetnu podeljenost mreze.

my_rainbow_wt <- rainbow(sapply(conf_clust_wt, function(x) max(x$membership)), start = 0, end = 1)
length(my_rainbow_wt)

for(i in 1:length(conf_no_iso_graph_list)){
  for(j in 1:length(V(conf_no_iso_graph_list[[i]]))) { 
    V(conf_no_iso_graph_list[[i]])[j]$clustwt <- conf_clust_wt[[i]]$membership[j]
  }
}

pdf("2_clusters_wt.pdf")
lapply(conf_no_iso_graph_list, function(x) { 
  important <- names(sort(degree(x), decreasing = TRUE)[1:5])
  plot(x, layout = layout_with_fr(x), frame = TRUE, 
       main = paste("walktrap clustering\nnumber of clusters: ", max(V(x)$clustwt), "\n", x$title), 
       vertex.label = ifelse(V(x)$name %in% important, V(x)$name, NA),
       vertex.label.cex = 0.8, vertex.label.color = "black", vertex.label.font = 2, 
       vertex.size = rescale(degree(x, mode = "all"), to = c(2, 10)), 
       vertex.color = my_rainbow_wt[V(x)$clustwt], edge.arrow.size = 0.15,
       edge.color = alpha("black", rescale(log(E(x)$weight), to = c(0.3, 1))))
}
)
dev.off()

### Iz grafickog prikaza se vidi da je veliki broj klastera u periodima pre i posle skupa jednim delom uslovljen 
### brojem komponenti, ali u vreme trajanja skupa to nije objasnjenje. Ovakav raspored cvorova ne daje lep pregled
### tako gusto prepletenih klastera i, mada postoje bolja resenja za prikaz klastera, pokusacu ipak nesto drugo:
### ova mreza u vreme trajanja skupa izgleda kao ego-mreza jednog cvora, organizatora skupa, prema cijem nalogu 
### sam i vrsila pocetnu pretragu, pa bi se moglo pokusati sa analizom mreze bez njega.

degree(conf_no_iso_graph_list[[3]], V(conf_no_iso_graph_list[[3]])$name == "wieilc", mode = "all")
# => 343
length(V(conf_no_iso_graph_list[[3]]))
# => 445

conf_no_wieilc <- delete_vertices(conf_no_iso_graph_list[[3]], V(conf_no_iso_graph_list[[3]])$name == "wieilc")

length(V(conf_no_wieilc)[degree(conf_no_wieilc) == 0])
# => 34

conf_clust_wt_no_wieilc <- cluster_walktrap(conf_no_wieilc)
max(conf_clust_wt_no_wieilc$membership)
# => 80


for(i in 1:length(V(conf_no_wieilc))) V(conf_no_wieilc)[i]$clustwt_nowieilc <- conf_clust_wt_no_wieilc$membership[i]

my_rainbow_wt80 <- rainbow(80, start = , end = 1)


important2 <- names(sort(degree(conf_no_wieilc), decreasing = TRUE)[1:5])
pdf("2_no_wieilc.pdf")
plot(conf_no_wieilc, layout = layout_with_fr(conf_no_wieilc), frame = TRUE, 
     main = "network without wieilc node\nnumber of clusters: 80\nfrom 2017-05-19 to 2017-05-26", 
     vertex.label = ifelse(V(conf_no_wieilc)$name %in% important2, V(conf_no_wieilc)$name, NA),
     vertex.label.cex = 0.8, vertex.label.color = "black", vertex.label.font = 2,
     vertex.size = rescale(degree(conf_no_wieilc, mode = "all"), to = c(2, 10)), 
     vertex.color = my_rainbow_wt80[V(conf_no_wieilc)$clustwt_nowieilc], edge.arrow.size = 0.15,
     edge.color = alpha("black", rescale(log(E(conf_no_wieilc)$weight), to = c(0.3, 1))))
dev.off()


### Posle uklanjanja glavnog organizatora iz mreze, ostalo je 34 izolata (u pocetnoj mrezi ih je bilo jos 12) 
### ali se mreza nije raspala na komponente, sto potvrduje neku srednju centralizovanost (ima vise srednje vaznih
### cvorova). Ipak bez ovog cvora mreza je podeljena na 80 klastera, u odnosu na 32 (za isti period i isti algoritam
### za celu mrezu), sto pokazuje znacaj tog cvora za povezivanje grafa. Osim toga, u vizuelizaciji se primecuje drvolika 
### struktura, bez znacajnog prisustva klika.
### Zakljucak je da komunikacija u mrezi nije ujednacena, o cemu govore distribucija stepena i srednja, ali primetna,
### centralizovanost mreze (mali broj uticajnih cvorova), nizak koeficijent klasterovanja i relativno mali broj 
### klika i velicina jezgara (slaba medusobna povezanost "ne-centralnih" cvorova) i veliki broj klastera (velika 
### heterogenost mreze).

