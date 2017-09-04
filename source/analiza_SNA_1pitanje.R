### Analiza podataka
### Mrezne metrike

library(dplyr)
library(igraph)
library(scales)
conf_graph_list <- readRDS("results/list_of_conf_graphs.RData")


### 1. Da li su ucesnici komunicirali i pre skupa ili ostvarena komunikacija predstavlja nove kontakte? 

conf_connected <- sapply(conf_graph_list, is.connected)
conf_connected  # time_1 -> time_5 : FALSE
conf_density <- sapply(conf_graph_list, edge_density)
conf_density   # time_1 -> time_5 : 0.001732515 0.001642544 0.006622135 0.001544788 0.001435390 

### Mreza, ocekivano za tu velicinu, nije povezana, ali je gustina u vreme trajanja skupa povecana cetiri 
### puta. Na zalost, gustina mreze posle skupa je manja cak i od one pre skupa. Buduci da igraph funkcijom 
### edge_density racuna gustinu samo kao kolicnik postojecih i svih mogucih veza, ne uzimajuci u obzir tezinu
### veza, moze se zakljuciti da je povecana gustina mreze rezultat veceg broja pominjanja razlicitih ucesnika,
### odnosno uspostavljanja novih kontakata, a ne intenzivnije komunikacije izmedu manjeg broja ucesnika.

### Mozda bi malo opipljivije bilo istraziti komponente - njihov broj i velicinu maksimalne komponente.
### Slabe komponente ovde podrazumevaju (najmanje) jednostrano pominjanje jednog ucesnika u tvitu drugog.
conf_components_weak <- lapply(conf_graph_list, components, mode = "weak")

conf_comp_no_weak <- sapply(conf_components_weak, "[[", "no") # broj komponenti
conf_comp_no_weak  #  time_1 -> time_5 : 243    262     13    265    275 

conf_comp_max_weak <- sapply(conf_components_weak, function(x) { max(x$csize) }) # najveca komponenta
conf_comp_max_weak  # time_1 -> time_5 : 120    142    445    140    105 

conf_comp_max_weak_ratio <- conf_comp_max_weak / length(V(conf_graph_list[[3]])) # procenat svih ucesnika u komp.
conf_comp_max_weak_ratio # => time_1 -> time_5 : 0.2625821 0.3107221 0.9737418 0.3063457 0.2297593 


### Ocekivano, broj komponenti je najmanji za vreme trajanja skupa, i tada je maksimalna komponenta najveca,
### obuhvata cak 97% ucesnika (u odnosu na 31% pre skupa), sto znaci da su gotovo svi ucesnici pomenuli barem 
### nekog od drugih ucesnika. Ovo potvrduje rezultate o gustini mreze:
cor(conf_density, conf_comp_max_weak) # => [1] 0.9951469
cor(conf_density, conf_comp_no_weak) # => [1] -0.9982268

### Medutim, imajuci na umu retvitove, vredi pogledati sta se desava sa jakim komponentama, koje bi predstavljale
### intenzivniju komunikaciju (ne nuzno dvosmernu - cvorovi se smatraju povezanima ako postoji bar po jedna 
### usmerena putanja u oba smera, ali ne obavezno duzine 1), ili, jos restriktivnije, svesti graf na neusmeren, 
### zadrzavajuci samo reciprocne veze - dvosmernu komunikaciju.
conf_components_strong <- lapply(conf_graph_list, components, mode = "strong")

conf_comp_no_strong <- sapply(conf_components_strong, "[[", "no")
conf_comp_no_strong  # time_1 -> time_5 : 358    388    324    397    388 

conf_comp_max_strong <- sapply(conf_components_strong, function(x) { max(x$csize) })
conf_comp_max_strong  # time_1 -> time_5 :  10     22    132     21      9 

conf_comp_max_strong_ratio <- conf_comp_max_strong / length(V(conf_graph_list[[3]]))
conf_comp_max_strong_ratio # time_1 -> time_5 : 0.02188184 0.04814004 0.28884026 0.04595186 0.01969365 

### Za jake komponente rezultati su donekle drugaciji - broj komponenti u vreme skupa je veliki, a maksimalna 
### komponenta obuhvata 29% ucesnika (naspram 4.8% i 2.1% pre skupa). Rezultati za maksimalnu komponentu i dalje 
### potvrduju rezultate o gustini:
cor(conf_density, conf_comp_max_strong) # => [1] 0.992854
cor(conf_density, conf_comp_no_strong) # => [1] -0.8882035


### Sama maksimalna komponenta ne govori o broju manjih komponenti koje bi mogle biti prostori intenzivnije 
### komunikacije (mada veliki broj komponenti moze da uputi na odgovor). Jasniju sliku moze dati 
### pregled po velicinama komponenti:

conf_comp_size_weak <- lapply(conf_components_weak, function(x) { table(x$csize) })
conf_comp_size_strong <- lapply(conf_components_strong, function(x) { table(x$csize) })


comp_plot_colors <- c("blue", "red", "green", "orange", "purple")
comp_plot_sizes <- c(5, 4, 3, 3, 2)
comp_plot_titles <- c("Weak component sizes", "Strong component sizes")
periods <- c("from 2017-04-21 to 2017-05-04", "from 2017-05-05 to 2017-05-18", "from 2017-05-19 to 2017-05-26", 
             "from 2017-05-27 to 2017-06-09", "from 2017-06-10 to 2017-06-23")

comp_sizes1 <- list(conf_comp_size_weak, conf_comp_size_strong)

pdf("visuals/1_component_sizes.pdf")
lapply(1:length(comp_sizes1), function(x){
  xmax <- max(sapply(comp_sizes1[[x]], function(y) max(as.numeric(names(y)))))
  ymax <- max(sapply(comp_sizes1[[x]], max))
  plot(c(1,xmax), c(1,ymax), type = "n", xlab = "component sizes", ylab = "frequency", log = "xy", 
       main = comp_plot_titles[x])
  lapply(1:length(comp_sizes1[[x]]), function(z){
    lines(comp_sizes1[[x]][[z]], type = "o", col = comp_plot_colors[z], lwd = comp_plot_sizes[z])
  })
  legend("topright", legend = periods, lwd = comp_plot_sizes, col = comp_plot_colors)
})
dev.off()


### Ipak ne postoji neki znacajan broj "srednje velikih" komponenti u vreme skupa, ni jakih ni slabih - dominantna
### je maksimalna komponenta. Ovo bih ocenila kao vrlo pozitivno, skup je uspeo da ujedini ucesnike u komunikaciji
### (mada i sama organizacija skupa - razudenost tema ili ciljne publike - moze da utice na formiranje veceg broja
### manjih komponenti i treba da se uzme u obzir ukoliko se primete takvi rezultati.)
### Sa druge strane, procenat ucesnika koji su se ukljucili u intenzivniju komunikaciju bi mogao da se tumaci na 
### razlicite nacine, u zavisnosti od ocekivanja organizatora, poredenja sa drugim slicnim skupovima, a treba imati 
### u vidu i to da nisu svi oni koji su tvitovali o skupu stvarno i ucestvovali.
TODO ### da li cu nekako razdvojiti ucesnike od posmatraca?


### Za istrazivanje dvosmerne komunikacije, graf cu pretvoriti u neusmereni, zadrzavajuci po jednu vezu za dve uzajamne:
conf_mutual_graph_list <- lapply(conf_graph_list, as.undirected, mode = "mutual")

conf_density_mutual <- sapply(conf_mutual_graph_list, edge_density)
conf_density_mutual  # time_1 -> time_5 : 0.0004858714 0.0004336749 0.0016891243 0.0003518378 0.0003228187 

conf_components_mutual <- lapply(conf_mutual_graph_list, components)

conf_comp_no_mutual <- sapply(conf_components_mutual, "[[", "no")
conf_comp_no_mutual # time_1 -> time_5 : 360    395    340    398    391 

conf_comp_max_mutual <- sapply(conf_components_mutual, function(x) { max(x$csize) })
conf_comp_max_mutual # time_1 -> time_5 :    10     13    114     21      4 
conf_comp_max_mutual_ratio <- conf_comp_max_mutual / length(V(conf_graph_list[[3]]))
conf_comp_max_mutual_ratio  # time_1 -> time_5 : 0.021881838 0.028446389 0.249452954 0.045951860 0.008752735


conf_comp_size_mutual <- lapply(conf_components_mutual, function(x) { table(x$csize) })

comp_sizes2 <- list(conf_comp_size_weak, conf_comp_size_strong, conf_comp_size_mutual)
comp_plot_titles2 <- c("Weak component sizes", "Strong component sizes", "Mutual component sizes")


pdf("visuals/1_component_sizes2.pdf")
lapply(1:length(comp_sizes2), function(x){
  xmax <- max(sapply(comp_sizes2[[x]], function(y) max(as.numeric(names(y)))))
  ymax <- max(sapply(comp_sizes2[[x]], max))
  plot(c(1,xmax), c(1,ymax), type = "n", xlab = "component sizes", ylab = "frequency", log = "xy", 
       main = comp_plot_titles2[x])
  lapply(1:length(comp_sizes2[[x]]), function(z){
    lines(comp_sizes2[[x]][[z]], type = "o", col = comp_plot_colors[z], lwd = comp_plot_sizes[z])
  })
  legend("topright", legend = periods, lwd = comp_plot_sizes, col = comp_plot_colors)
})
dev.off()


### U slucaju dvosmerne komunikacije, gustina mreze je, ocekivano, dosta manja od one kod neusmerenog grafa.
### Broj komponenti i velicina najvece komponente nisu drasticno drugaciji od onih kod jakih komponenti usmerenog 
### grafa, cetvrtina ucesnika je ostvarila uzajamnu komunikaciju sa drugim ucesnicima skupa, nasuprot samo 2.8% i 2.1%
### u periodima pre skupa. 

### Jos bi neke metrike na nivou cele mreze mogle da potvrde ove rezultate, na primer prosecna putanja (prosecni geodezik)
### i dijametar mreze (najduzi geodezik). Tezinu ivica necu uzimati u obzir, jer ovde ispitujem bilo kakav kontakt medu
### ucesnicima. Mere koje uzimaju u obzir smer veze odgovaraju jakim, a one druge slabim komponentama.

conf_avg_path_directed <- sapply(conf_graph_list, mean_distance, directed = TRUE, unconnected = TRUE)
# time_1 -> time_5 : 2.438159 3.399861 2.684841 2.831637 3.247514 
conf_avg_path_undirected <- sapply(conf_graph_list, mean_distance, directed = FALSE, unconnected = TRUE)
# time_1 -> time_5 : 4.946441 5.008145 2.655157 4.594015 4.862088 

conf_diam_directed <- sapply(conf_graph_list, diameter, directed = TRUE, unconnected = TRUE, weights = NA)
# time_1 -> time_5 :  8      9      6      8      9 
conf_diam_undirected <- sapply(conf_graph_list, diameter, directed = FALSE, unconnected = TRUE, weights = NA)
# time_1 -> time_5 :  11     14      6     11     10 


### I prosecna putanja i precnik mreze su najmanji za period trajanja skupa, iako je tada ukljuceno najvise
### ucesnika, ali je i gustina mreze tada najveca i to odgovara ideji o dobro povezanim cvorovima. Sa druge strane,
### zanimljiv je drugi period, za koji je i precnik mreze veci i prosecna putanja je duza, a gustina je manja nego 
### u prvom periodu, sto ukazuje na to da su se komponente povezale malim brojem veza, formirajuci mrezu nalik na 
### liniju. Tako, iako je u odnosu na prvi period veci procenat ucesnika koji formira najvecu komponentu, komunikacija
### ipak nije narocito intenzivna. 
### Razlog za vece vrednosti ovih metrika u slucaju kada se ne uzima u obzir smer veza je taj sto se zbog vrednosti
### parametre unconnected = TRUE posmatraju samo komponente, odnosno uzimaju se u obzir samo postojece putanje,
### i to za parametar directed = FALSE cvorovi povezani na bilo koji nacin, a za directed = FALSE samo jake komponente.
### Buduci da su jako povezane komponente manje, za njih ove metrike imaju manje vrednosti. Ovo je primetno u periodima
### pre i posle skupa, kada je broj komponenti velik, dok je u vreme trajanja skupa mreza ipak bolje povezana, pa su 
### rezultati gotovo isti. S druge strane, da je posmatran graf u celini, sa unconnected = FALSE, za putanje koje nedostaju
### bi se uzela vrednost broja cvorova, odnosno duzina najduze moguce putanje plus 1, sto u ovom slucaju ne bi imalo svrhu.

network_metrics_matrix <- matrix(data = c(conf_density, conf_density_mutual, 
                                          conf_comp_no_weak, conf_comp_no_strong, conf_comp_no_mutual,
                                          conf_comp_max_weak_ratio, conf_comp_max_strong_ratio, conf_comp_max_mutual_ratio,
                                          conf_avg_path_directed, conf_avg_path_undirected,
                                          conf_diam_directed, conf_diam_undirected),
                                 nrow = 12, ncol = 5, byrow = TRUE,
                                 dimnames = list(c("density directed", "density mutual", 
                                                   "n weak components", "n strong components", "n mutual components",
                                                   "max weak component", "max strong component", "max mutual component",
                                                   "avg path directed", "avg path undirected", 
                                                   "diameter directed", "diameter undirected"),
                                                 c("time_1", "time_2", "time_3", "time_4", "time_5")))

options(scipen = 10)
network_metrics_matrix


metrics_plot_colors <- c("green", "brown", "orange", "red", "skyblue", "yellow", "slategray", "deeppink", 
                         "purple", "darkgreen", "black", "darkblue")
metrics_plot_order <- c(5, 4, 3, 12, 11, 10, 9, 6, 7, 8, 1, 2)
pdf("visuals/1_network_metrics.pdf")
plot(c(1, 7), c(0.0001, 500), type = "n", xlab = "time period", ylab = "", log = "y", main = "Network metrics")
for(i in 1:nrow(network_metrics_matrix)){
  lines(network_metrics_matrix[i,], col = metrics_plot_colors[i], lwd = 2)
  
}
legend("topright", cex = 0.7, legend = dimnames(network_metrics_matrix)[[1]][metrics_plot_order], 
       col = metrics_plot_colors[metrics_plot_order], lwd = 2, text.width = 1.5, y.intersp = 2, seg.len = 1.5)
dev.off()

### Ovo vrlo lepo moze da se vidi i preko grafickog prikaza mreza.
### Za identifikaciju grafova, dodacu im atribut "title" kome mogu lako da pristupim preko funkcija iz igraph paketa


for(i in 1:length(conf_graph_list)) { conf_graph_list[[i]] <- set_graph_attr(conf_graph_list[[i]], 
                                                                             "title", periods[i])}

pdf("visuals/1_plain.pdf")
lapply(conf_graph_list, function(x) { plot(x, layout = layout_with_fr(x), frame = TRUE, main = x$title , 
                                           sub = "plain graph", vertex.label = NA, vertex.size = 2, edge.arrow.size = 0.15)})

dev.off()

### Jasno se vide udeo izolata u vremenskim periodima i razvoj maksimalne komponente - na osnovu podataka o
### jednostranom pominjanju ucesnika.

### Nevezano za odgovor na postavljeno pitanje, u prikaz mogu, od do sada dostupnih podataka, da ukljucim jos i 
### tezinu ivica, a mogu da dodam kao atribut cvorova broj tvitova koje svaki ucesnik ima u pocetnom skupu tvitova 
### (obelezenih sa #WIELead ili @wieilc). Na taj nacin moze da se prati gde se nalaze ucesnici koji su najvise
### tvitovali sa ovim kljucnim recima.

startertweets <- readRDS("data/startertweets")

nstarttw <- startertweets %>% distinct(id, .keep_all = TRUE) %>%
  filter(screenName %in% names(V(conf_graph_list[[3]]))) %>% count(screenName)

######################################### videti zasto ovo ne radi u funkciji; zar ne dodeljuje direktno grafu?
lapply(conf_graph_list, function(x) { for (i in 1:length(V(x))) 
  V(x)[i]$nstarttw <- as.numeric(nstarttw[nstarttw$screenName == V(x)[i]$name, "n"])})

dodaj_atr <- function(x) { for (i in 1:length(V(x))) V(x)[i]$nstarttw <- 1}
dodaj_atr(conf_graph_list[[1]])
#########################################

for(i in 1:length(conf_graph_list)){
  for(j in 1:length(V(conf_graph_list[[i]]))) { 
    V(conf_graph_list[[i]])[j]$nstarttw <- as.numeric(nstarttw[nstarttw$screenName == 
                                                                 V(conf_graph_list[[i]])[j]$name, "n"])}
}

### Boje za kodiranje cvorova, od zute do crvene; atribut nstarttw ce, posle preracunavanja, da odredi indeks 
### boje - svetlozuta za najmanji, crvena za najveci broj tvitova
yellowred <- rev(rainbow(26, start = 0, end = 1/6)) 


pdf("visuals/1_num_keywords.pdf")
lapply(conf_graph_list, function(x) { plot(x, layout = layout_with_fr(x), main = x$title, frame = TRUE,
          sub = "users with number of tweets originally collected (with #WIELead or @wieilc)",
          vertex.label = NA, vertex.size = 2, vertex.color = yellowred[round(rescale(log(V(x)$nstarttw), to = c(0,26)))],
          edge.arrow.size = 0.15, edge.color = alpha("black", rescale(log(E(x)$weight), to = c(0.3, 1))))
  legend("bottomright", title = "users with n of tweets", legend = c("high n", "low n"), cex = 0.8, 
         pt.cex = 1, col = "black", pch = 21, pt.bg = c(yellowred[26], yellowred[1]))})
dev.off()


### Iako donekle prati logicki rezon da ce korisnici sa najvise tvitova u vreme skupa biti centralniji, a korisnici 
### sa manje tvitova na periferiji, primetna su i neka odstupanja. Cvorovi crvene boje na periferiji grafa bi mogli 
### da se objasne kao korisnici koji su dosta tvitovali koristeci 'hesteg' skupa, ali nisu pominjali druge ucesnike 
### direktno, dok bi zuti cvorovi blizu centra grafa mogli da budu korinici koji su dosta komunicirali sa drugim 
### korisnicima, ali o nekim drugim temama. Nesto od ovoga ce se mozda razjasniti kasnijom analizom.

saveRDS(conf_graph_list, "results/list_of_conf_graphs_after_1st.RData")
