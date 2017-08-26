library(twitteR)
library(dplyr)

### originalni tvitovi su skidani tokom i posle trajanja skupa, funkcijom searchTwitter iz paketa twitteR, za
### kljucne reci #WIELead i @wieilc; sam skup je odrzan 22.05. i 23.05.2017.
### funkcija searchTwitter kao rezultat vraca listu, koja moze da se prebaci u data frame pomocu 
### funkcije twListToDF iz istog paketa
### startertweets je data frame sa svim tvitovima prikupljenim na ovaj nacin; buduci da su neki tvitovi oznaceni
### sa obe ove kljucne reci, potrebno je izbaciti duplikate

startertweets <- readRDS("startertweets")

if(packageVersion("dplyr") >= "0.5.0") {
uniquesttw <- distinct(startertweets, id, .keep_all = TRUE) 
} else uniquesttw <- distinct(startertweets, id)


### iz ovog skupa sam izabrala ucesnike koji su tvitovali tokom trajanja dogadaja, ali i one koji su tvitovali
### u vremenskom periodu od tri dana pre i posle skupa (ovo je mozda presiroko)

### napomena: prilikom poredenja vremena u POSIXct formatu, u koloni "created", uvek mi je racunao 
### vremensku razliku od 2h i vracao 'pomerene' rezultate; pri prebacivanju u Date format dobije se ispravan 
### datum, tako da sam ovaj format koristila za sve analize.
### pomenuta razlika se moze videti u pretragama redova:
#
# t1 <- time_narrowed[time_narrowed$created > "2017-05-22 00:00:00 UTC" & time_narrowed$created < "2017-05-23 00:00:00 UTC", c("created", "date")]
# t2 <- time_narrowed[time_narrowed$date == "2017-05-22", c("created", "date")]
#
### > min(t1$created) => [1] "2017-05-21 22:14:27 UTC"
### > max(t1$created) => [1] "2017-05-22 21:59:32 UTC"
### > min(t2$created) => [1] "2017-05-22 00:05:39 UTC"
### > max(t2$created) => [1] "2017-05-22 23:59:59 UTC"
### ali: > c(min(t1$created)) => [1] "2017-05-22 00:14:27 CEST"

time_narrowed <- uniquesttw %>% mutate("date" = as.Date(created)) %>% filter(date < "2017-05-27" & date > "2017-05-18")
participants <- unique(time_narrowed$screenName)                                         


### za izabrane korisnike sam skinula dostupne tvitove pomocu funkcija get_tweets i dl_tweets, prvobitno
### zasnovanih na funkciji ekstrakcija Marka Galjaka (https://github.com/gljk/Master-rad/blob/master/3.%20Listinzi%20koda.R),
### ali u konacnoj verziji prilicno izmenjenih

### funkcija get_tweets uzima kao argumente listu korisnika za koje se skidaju tvitovi, broj tvitova po korisniku,
### da li treba da ukljuci retvitove i da li treba da stampa trenutno stanje - ovo zbog kontrole izvrsavanja
### funkcije, zato sto se za veliki broj tvitova funkcija jako dugo izvrsava (na mom racunaru nesto manje od 
### 1 minuta za jednog korisnika, 3200 tvitova), a desava se i da stane u pozivu:
### .Call(R_curl_fetch_memory, url, handle, nonblocking); ovo poslednje je ostalo kao bag
### inace, korisnike za koje API vrati gresku funkcija upisuje u vektor locked_users, pa je moguce ponovo 
### pokusati skidanje tvitova za ove korisnike, za slucaj da nije u pitanju obrisani ili 'protected' nalog, 
### nego trenutni problemi sa pristupom podacima

get_tweets <- function(usrs, ntweets, rts = TRUE, verbose = FALSE) {
  all_tweets <- data.frame()
  if (!exists("locked_users", where = 1)) assign("locked_users", vector(), pos = 1) # pos 1 == .GlobalEnv
  
  for (i in 1 : length(usrs)) {
    tryCatch({
      t1<-Sys.time()
      if (verbose) cat("In tryCatch with", i, usrs[i], "fetching user timeline... ")
      twlist <- userTimeline(user = usrs[i], n = ntweets, includeRts = rts)
      t2 <- Sys.time()
      if (verbose) cat("done in", t2-t1, "\n Converting to df.... ")
      twlist <- twListToDF(twlist)
      t3 <- Sys.time()
      if (verbose) cat("done in", t3-t2, "\n Binding to resulting df... ")
      all_tweets <- rbind(all_tweets, twlist)
      t4 <- Sys.time()
      if (verbose) cat("done in", t4-t3, "\n\n")
  
    }, error = function(err){
      if (verbose) cat("In error with", i, usrs[i], "\n\n")
      assign("locked_users", c(locked_users, usrs[i]), pos = 1)
    }) # end of tryCatch
  }
  return(all_tweets)
}


### funkcija dl_tweets vodi racuna o twitterAPI limitu i poziva funkciju get_tweets; vraca data frame sa svim 
### prikupljenim tvitovima

TODO # dodatno testiranje, uspela sam da probijem limit!!! (prethodno testiranje sa preko 900 korisnika je proslo,
### skidanje (serijski) sa ~400 korisnika nije ispostovan sleep!)

dl_tweets <-function(user_list, num_tweets, rts = TRUE, verbose = FALSE) { 
  res <- data.frame()
  limit <- as.numeric(getCurRateLimitInfo()[41, "remaining"])
  reset <- getCurRateLimitInfo()[41, "reset"]
  if (limit <= 1)  {
    if (verbose) cat("Waiting for twitter API limit to reset, due:", as.character(reset), "UTC\n\n")	
    Sys.sleep(60 * as.numeric((reset) - Sys.time()) + 10)
  }
  if (length(user_list) <= limit) res <- rbind(res, get_tweets(user_list, num_tweets, rts, verbose))
  else {
    res <- rbind(res, get_tweets(user_list[1:(limit-1)], num_tweets, rts, verbose))
    dl_tweets(user_list[limit:length(user_list)], num_tweets, rts, verbose)
  }
  return(res)
}

### prvo sam skinula sve dostupne tvitove za izabrane ucesnike, zatim sam izdvojila samo one ucesnike 
### za koje su dostupni tvitovi za ceo period koji cu da posmatram, i na kraju sam izdvojila tvitove
### za te ucesnike i taj vremenski period 

# all_tweets <- dl_tweets(participants, 3200)
all_tweets <- rbind(readRDS("all_tweets1"), readRDS("all_tweets2"), readRDS("all_tweets3"), readRDS("all_tweets4"))

my_subset <- function(tweets, from_incl, to_incl){
  tweets <- mutate(tweets, "date" = as.Date(tweets$created))
  selected_participants <- tweets[, c("screenName", "date")] %>% group_by(screenName) %>% 
      summarize("minDate" = min(date)) %>% filter(minDate <= from_incl)
  selected_participants <- as.vector(selected_participants$screenName)
  selected_tweets <- tweets %>% filter(screenName %in% selected_participants &
                                             date >= from_incl & date <= to_incl)
  
}

selected_tweets <- my_subset(all_tweets, "2017-04-21", "2017-06-23")

### kasnije u toku analize sam dosla do toga da je bilo gresaka pri skidanju tvitova - da li zbog tviter API-ja
### ili zbog same funkcije za skidanje - uglavnom, bilo je duplikata, ali i neki tvitovi iz originalnog skupa
### tvitova prikupljanih tokom trajanja dogadaja, nisu bili u skupu tvitova prikupljenih kasnije, putem 
### userTimeline funkcije. Odlucila sam da ih ukljucim u izabrane tvitove za analizu, radi sto kompletnijeg 
### skupa tvitova (vodeci racuna da ukljucim samo tvitove korisnika koji zadovoljavaju uslov dostupnosti tvitova),
### a duplikate da izbacim.

tws_to_insert_ids <- setdiff(time_narrowed$id, selected_tweets$id)
selected_tweets <- rbind(selected_tweets, time_narrowed[time_narrowed$id %in% tws_to_insert_ids &
                                                          time_narrowed$screenName %in% selected_tweets$screenName,])

# table(duplicated(selected_tweets$id)) # true: 28

selected_tweets <- selected_tweets[!duplicated(selected_tweets$id),]

### sve posmatrane tvitove sam podelila na pet grupa, po vremenu tvitovanja: osnovnu grupu ("time_3") cine tvitovi
### iz perioda trajanja skupa uz tri dana pre i posle skupa, odnosno od 19.05.2017. do 26.05.2017. (ukljucujuci 
### i te datume), kao period najintenzivnije komunikacije u vezi sa skupom; ostali periodi su simetricno rasporedeni
### pre i posle skupa, svaki u trajanju od po dve nedelje

selected_tweets$timeframe <- 
  ifelse(selected_tweets$date >= "2017-04-21" & selected_tweets$date <= "2017-05-04", "time_1", 
                                    (ifelse(selected_tweets$date >= "2017-05-05" & selected_tweets$date <= "2017-05-18", "time_2",
                                            (ifelse(selected_tweets$date >= "2017-05-19" & selected_tweets$date <= "2017-05-26", "time_3",
                                                    (ifelse(selected_tweets$date >= "2017-05-27" & selected_tweets$date <= "2017-06-09", "time_4", "time_5")))))))


splitted <- split(selected_tweets, selected_tweets$timeframe)

### pregledanjem dobijene liste i pripremanjem za pravljenje matrice, primetila sam da je broj ucesnika razlicit
### u ovim vremenskim odeljcima, sto je u redu, jer sam trazila da najraniji dostupni tvit po korisniku bude
### 21.4. ili ranije, pa je tako moguce da je korisnik tvitovao 20.04. (sto nije uslo u posmatrane tvitove), ali
### nije tvitovao posle toga do konferencije; (dva korisnika koja se nisu pojavljivala u trecem vremenskom odeljku,
### sto je vreme samog dogadaja, na osnovu kojeg su tvitovi i prikupljani, ubacila sam prethodnim postupkom dopune
### konacnog skupa tvitova)


### funkcija za pravljenje matrice povezanosti, takode zasnovana na funkciji matrica_povezanosti Marka Galjaka
### (https://github.com/gljk/Master-rad/blob/master/3.%20Listinzi%20koda.R), delimicno izmenjena

make_adjacency_matrix <- function(twts) {
  partcp <-unique(twts$screenName)
  lpart <-length(partcp)
  mat <-matrix(nrow = lpart, ncol = lpart)
  colnames(mat) <- partcp
  rownames(mat) <- partcp
  for (i in 1:lpart) {
    l <-twts[twts$screenName == partcp[i], "text"]
    for (j in 1:lpart) {
      if (j == i) next
      k <- partcp[j]
      mat[i, j] <-table(grepl(k, l))["TRUE"]
    }
  }
  mat[is.na(mat)] <-0
  return(mat)
}

### pravljenje liste matrica povezanosti; izvrsenje je trajalo oko 13 minuta za 457 ucesnika, 269163 tvita 
conf_adj_mat_list <- lapply(splitted, make_adjacency_matrix)

### pravljenje liste grafova 

library(igraph)
conf_graph_list <- lapply(conf_adj_mat_list, graph_from_adjacency_matrix, mode = "directed", weighted = TRUE)

### Mreze imaju razlicit broj cvorova, u zavisnosti od toga koliko je ucesnika tvitovalo u svakom vremenskom
### periodu, odnosno, period kada je skup odrzan sadrzi sve ucesnike/cvorove, jer se za njih posmatra mreza,
### a periodi pre i posle ukljucuju samo one ucesnike koji su tada tvitovali. Izolati u mrezama su ucesnici
### koji su tvitovali, ali nisu pominjali druge ucesnike.

conf_vnum <- sapply(conf_graph_list, function(x) length(V(x))) 
# time_1 -> time_5 : 396    430    457    427    417 
conf_iso_num <- sapply(conf_graph_list, function(x) length(V(x)[degree(x) == 0]))
# time_1 -> time_5 : 223    246     12    254    257


### Analiza podataka
### Mrezne metrike
  
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

conf_comp_max_weak_ratio <- conf_comp_max_weak / length(unique(selected_tweets$screenName)) # procenat svih ucesnika u komp.
conf_comp_max_weak_ratio # => time_1 -> time_5 : 0.2625821 0.3107221 0.9737418 0.3063457 0.2297593 

### Ocekivano, broj komponenti je najmanji za vreme trajanja skupa, i tada je maksimalna komponenta najveca,
### obuhvata cak 97% ucesnika (u odnosu na 31% pre skupa), sto znaci da su gotovo svi ucesnici pomenuli barem 
### nekog od drugih ucesnika. Ovo potvrduje rezultate o gustini mreze:
cor(conf_density, conf_comp_max_weak) # => [1] 0.9951469

### Medutim, imajuci na umu retvitove, vredi pogledati sta se desava sa jakim komponentama, koje bi predstavljale
### intenzivniju komunikaciju (ne nuzno dvosmernu - cvorovi se smatraju povezanima ako postoji bar po jedna 
### usmerena putanja u oba smera, ali ne obavezno duzine 1), ili, jos restriktivnije, svesti graf na neusmeren, 
### zadrzavajuci samo reciprocne veze - dvosmernu komunikaciju.
conf_components_strong <- lapply(conf_graph_list, components, mode = "strong")

conf_comp_no_strong <- sapply(conf_components_strong, "[[", "no")
conf_comp_no_strong  # time_1 -> time_5 : 358    388    324    397    388 

conf_comp_max_strong <- sapply(conf_components_strong, function(x) { max(x$csize) })
conf_comp_max_strong  # time_1 -> time_5 :  10     22    132     21      9 

conf_comp_max_strong_ratio <- conf_comp_max_strong / length(unique(selected_tweets$screenName))
conf_comp_max_strong_ratio # time_1 -> time_5 : 0.02188184 0.04814004 0.28884026 0.04595186 0.01969365 

### Za jake komponente rezultati su donekle drugaciji - broj komponenti u vreme skupa je veliki, a maksimalna 
### komponenta obuhvata 29% ucesnika (naspram 4.8% i 2.1% pre skupa). Rezultati za maksimalnu komponentu i dalje 
### potvrduju rezultate o gustini, dok je kod broja komponenti situacija nesto manje jasna:
cor(conf_density, conf_comp_max_strong) # => [1] 0.992854
cor(conf_density, conf_comp_no_strong) # => [1] -0.8882035, nasuprot
cor(conf_density, conf_comp_no_weak) # => [1] -0.9982268

### Sama maksimalna komponenta ne govori o broju manjih komponenti koje bi mogle biti prostori intenzivnije 
### komunikacije (mada veliki broj komponenti moze da uputi na odgovor). Jasniju sliku moze dati 
### pregled po velicinama komponenti:

conf_comp_size_weak <- lapply(conf_components_weak, function(x) { table(x$csize) })
conf_comp_size_strong <- lapply(conf_components_strong, function(x) { table(x$csize) })


TODO ### plotovanje ovog iznad

### Ipak ne postoji neki znacajan broj "srednje velikih" komponenti u vreme skupa, ni jakih ni slabih - dominantna
### je maksimalna komponenta. Ovo bih ocenila kao vrlo pozitivno, skup je uspeo da ujedini ucesnike u komunikaciji
### (mada i sama organizacija skupa - razudenost tema ili ciljne publike - moze da utice na formiranje veceg broja
### manjih komponenti i treba da se uzme u obzir ukoliko se primete takvi rezultati.)
### Sa druge strane, procenat ucesnika koji su se ukljucili u intenzivniju komunikaciju bi mogao da se tumaci na 
### razlicite nacine, u zavisnosti od ocekivanja organizatora, poredenja sa drugim slicnim skupovima, a treba imati 
### u vidu i to da nisu svi oni koji su tvitovali o skupu stvarno i ucestvovali.
TODO ### da li cu nekako razdvojiti ucesnike od posmatraca?


### Za istrazivanje dvosmerne komunikacije, graf cu pretvoriti u neusmereni, zadrzavajuci po jednu vezu za dve uzajamne:
conf_undirected_graph_list <- lapply(conf_graph_list, as.undirected, mode = "mutual")

conf_density_undirected <- sapply(conf_undirected_graph_list, edge_density)
conf_density_undirected  # time_1 -> time_5 : 0.0004858714 0.0004336749 0.0016891243 0.0003518378 0.0003228187 

conf_components_undirected <- lapply(conf_undirected_graph_list, components)

conf_comp_no_undirected <- sapply(conf_components_undirected, "[[", "no")
conf_comp_no_undirected # time_1 -> time_5 : 360    395    340    398    391 

conf_comp_max_undirected <- sapply(conf_components_undirected, function(x) { max(x$csize) })
conf_comp_max_undirected # time_1 -> time_5 :    10     13    114     21      4 
conf_comp_max_undireted_ratio <- conf_comp_max_undirected / (length(unique(selected_tweets$screenName)))
conf_comp_max_undireted_ratio  # time_1 -> time_5 : 0.021881838 0.028446389 0.249452954 0.045951860 0.008752735


conf_comp_size_undirected <- lapply(conf_components_undirected, function(x) { table(x$csize) })
conf_comp_size_undirected
TODO ### ovo dodati na plot sa prethodna dva comp_size rezultata!


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

network_metrics_matrix <- matrix(data = c(conf_density_undirected, conf_density, 
                                                 conf_comp_no_weak, conf_comp_no_strong, conf_comp_no_undirected,
                                                 conf_comp_max_weak_ratio, conf_comp_max_strong_ratio, conf_comp_max_undireted_ratio,
                                                 conf_avg_path_undirected, conf_avg_path_directed,
                                                 conf_diam_undirected, conf_diam_directed),
                                        nrow = 12, ncol = 5, byrow = TRUE,
                                        dimnames = list(c("density undirected", "density directed", 
                                                          "n weak components", "n strong components", "n undirected components",
                                                          "max weak component", "max strong component", "max undirected component",
                                                          "avg path undirected", "avg path directed", "diameter undirected",
                                                          "diameter directed"),
                                                        c("time_1", "time_2", "time_3", "time_4", "time_5")))

options(scipen = 10)
network_metrics_matrix
TODO ### dodati plot za matricu, ne bi trebalo da bude pretesko.. treba skalirati, inverz n komponenti


### Ovo vrlo lepo moze da se vidi i preko grafickog prikaza mreza.
### Za identifikaciju grafova, dodacu im atribut "title" kome mogu lako da pristupim preko funkcija iz igraph paketa

graph_title <- c("from 2017-04-21 to 2017-05-04", "from 2017-05-05 to 2017-05-18", "from 2017-05-19 to 2017-05-26", 
                 "from 2017-05-27 to 2017-06-09", "from 2017-06-10 to 2017-06-23")
for(i in 1:length(conf_graph_list)) { conf_graph_list[[i]] <- set_graph_attr(conf_graph_list[[i]], 
                                                                         "title", graph_title[i])}

pdf("1_osnovni.pdf")
lapply(conf_graph_list, function(x) { plot(x, layout = layout_with_fr(x), frame = TRUE, main = x$title , 
                                  sub = "plain graph", vertex.label = NA, vertex.size = 2, edge.arrow.size = 0.15)})
dev.off()

### Jasno se vide udeo izolata u vremenskim periodima i razvoj maksimalne komponente - na osnovu podataka o
### jednostranom pominjanju ucesnika.

### Nevezano za odgovor na postavljeno pitanje, u prikaz mogu, od do sada dostupnih podataka, da ukljucim jos i 
### tezinu ivica, a mogu da dodam kao atribut cvorova broj tvitova koje svaki ucesnik ima u pocetnom skupu tvitova 
### (obelezenih sa #WIELead ili @wieilc). Na taj nacin moze da se prati gde se nalaze ucesnici koji su najvise
### tvitovali sa ovim kljucnim recima.

nstarttw <- time_narrowed[time_narrowed$screenName %in% unique(selected_tweets$screenName),] %>% 
  count(screenName)

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

### boje za kodiranje cvorova, od zute do crvene; atribut nstarttw ce, posle preracunavanja, da odredi indeks 
### boje - svetlozuta za najmanji, crvena za najveci broj tvitova
yellowred <- rev(rainbow(26, start = 0, end = 1/6)) 
library(scales)

pdf("1_kljucne_reci.pdf")
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
### direktno, dok bi zuti cvorovi blizu centra grafa mogli da budu korinici koji su dosta komunicirali sa drugim k
### orisnicima, ali o nekim drugim temama. Nesto od ovoga ce se mozda razjasniti kasnijom analizom.


### 2. Da li svi ucesnici ujednaceno komuniciraju ili se stvaraju grupe sa intenzivnijom komunikacijom? 

### Iako i dosadasnji graficki prikazi dosta govore, probacu da saznam nesto vise o stepenu cvorova.

deg_dist <- lapply(conf_graph_list, function(x) { 
  res <- list()
  res$dd_all <- degree.distribution(x, mode = "all")
  res$dd_in <- degree.distribution(x, mode = "in")
  res$dd_out <- degree.distribution(x, mode = "out")
  return(res)})


pdf("2_deg_distribution.pdf")
lapply(deg_dist, function(x){ 
  xmax <- max(sapply(x, length))
  ymax <- max(sapply(x, max))
  plot(c(0,xmax), c(0,ymax), type = "n", xlab = "degree", ylab = "frequency", main = "degree distribution" )
  lines(x$dd_all, col = "red", lwd = 5)
  lines(x$dd_in, col = "green", lwd = 3)
  lines(x$dd_out, col = "purple", lwd = 2)
  legend("topright", legend = c("out degree", "in degree", "total degree"), lwd = c(2, 3, 5), col = c("purple", "green", "red"))
})
dev.off()


### Distribucije stepena prate "power law" distribuciju, odnosno najveci broj ucesnika ima veoma mali stepen, 
### a sa porastom stepena frekvencija naglo opada i pojavljuje se karakteristican dugacki "rep". Distribucije po
### vremenskim intervalima su dosta slicne, sa izuzetkom perioda u kome se odrzao skup, gde je broj ucesnika koji imaju
### odlazeci (a time i ukupni) stepen jednak nuli - relativno blizu nule, odnosno, mala je frekvencija ucesnika koji 
### nisu u tvitu pomenuli nekog drugog ucesnika, ali je velika frekvencija onih koji nisu pomenuti u tvitovima drugih,
### onih ciji je dolazni stepen jednak nuli. Ovo je ocekivana pojava za drustvene mreze. 

conf_v_degs <- lapply(conf_graph_list, function(x) { 
  res <- list()
  res$d_all <- degree(x, mode = "all")
  res$d_in <- degree(x, mode = "in")
  res$d_out <- degree(x, mode = "out")
  return(res)})

conf_degs_anlys <- lapply(conf_v_degs, function(x) {
  res$mean <- sapply(x, mean)
  res$sd <- sapply(x, sd)
  return(c(res$mean, res$sd))
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

TODO # prikaz frekvencija?
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

important2 <- sort(degree(conf_no_wieilc), decreasing = TRUE)[1:5]
pdf("2_no_wieilc.pdf")
plot(conf_no_wieilc, layout = layout_with_fr(conf_no_wieilc), frame = TRUE, 
     main = "network without wieilc node\n from 2017-05-19 to 2017-05-26", 
     vertex.label = ifelse(V(conf_no_wieilc)$name %in% important2, V(conf_no_wieilc)$name, NA),
     vertex.size = rescale(degree(conf_no_wieilc, mode = "all"), to = c(2, 10)), 
     vertex.color = my_rainbow_wt[V(conf_wieilc_ego[[1]])$clustwt], edge.arrow.size = 0.15,
     edge.color = alpha("black", rescale(log(E(conf_no_wieilc)$weight), to = c(0.3, 1))))
dev.off()
# zasto mi sad ovde ne ispisuje imena? i zasto izmesa klastere? i tamo i ovde racuna layout nevezano za klastere.. ?

length(V(conf_no_wieilc)[degree(conf_no_wieilc) == 0])
# => 34


### Posle uklanjanja glavnog organizatora iz mreze, ostalo je 34 izolata (u pocetnoj mrezi ih je bilo jos 12) 
### ali se mreza nije raspala na komponente, sto potvrduje neku srednju centralizovanost (ima vise srednje vaznih
### cvorova). Ipak, u vizuelizaciji se primecuje drvolika struktura, bez znacajnog prisustva klika.   
### Zakljucak je da komunikacija u mrezi nije ujednacena, o cemu govore distribucija stepena i srednja, ali primetna,
### centralizovanost mreze (mali broj uticajnih cvorova), nizak koeficijent klasterovanja i relativno mali broj 
### klika i velicina jezgara (slaba medusobna povezanost "ne-centralnih" cvorova) i veliki broj klastera (velika 
### heterogenost mreze).


### 3. Kolika je komunikacija posle skupa i da li je povezana sa grupama? 

### Sva izracunavanja pokazuju da je komunikacija drasticno opala posle skupa: pocev od broja cvorova koji su 
### tvitovali, preko broja izolata, procenta cvorova koji cine najvecu komponentu, broja komponenti, do prosecne
### putanje, precnika i gustine mreze. Cetvrti period za neke metrike pokazuje blaga odstupanja od ocekivanih
### vrednosti, ali ne u tolikoj meri kao drugi period. 
### Varijacije u stepenu nisu mnogo izrazene u periodima posle skupa, centralizovanost mreza je uglavnom manja 
### (osim za centralizovanost prema svojstvenom vektoru), sto moze da govori o ujednacenijoj komunikaciji od one 
### za vreme trajanja skupa. Sto se tice povezanosti, tranzitivnost jeste nesto veca posle skupa, ali broj i 
### velicina klika i velicina jezgara ne ukazuju na neku "siru" komunikaciju. Oba algoritma za klasterovanje su 
### dala veliki broj klastera za sve periode, sto potvrduje veliku heterogenost mreza. Na kraju, i na grafickom 
### prikazu mreza moze da se prati raspadanje glavne komponente na veci broj manjih.



### 4. Ko su centralni ucesnici i kako uticu na teme komunikacije? 

### Ovde mi je cilj da identifikujem centralne cvorove mreze, da bih u kasnijoj analizi teksta mogla da pratim 
### teme u njihovim tvitovima kroz vreme. Centralni cvorovi mogu da imaju veliki uticaj na ostale cvorove, pa bi
### moglo biti zanimljivo proveriti taj uticaj u konkretnom ponasanju - tvitovanju o odredenoj temi. Cvorovi 
### centralni po svom stepenu ovde su oni koji imaju najvise kontakata, uz razliku izmedu centralnosti dolaznog
### stepena (veliki prestiz, kao npr. organizatori skupa) i odlaznog stepena (uticaj putem kvantiteta komunikacije). Cvorovi centralni po 
### bliskosti su najblizi svim cvorovima i mogu najbrze da prosire informacije, na primer, mogu da odvuku komunikaciju
### na novu temu. Centralnost intermedijarnosti je karakteristican za cvorove koji povezuju delove mreze koji bi
### inace bili nepovezani, sto bi mogle biti grupe ucesnika iz razlicitih sfera, odnosno tvitovi o razlicitim 
### temama. Na kraju, centralnost po svojstvenom vektoru karakterise ucesnike koji su povezani sa uticajnim ucesnicima;
### njihov uticaj bi mogao medusobno da se dopunjuje ili blokira. ??????????????????????????????


conf_centralities <- function(n){
  cent <- list()
  lapply(conf_graph_list, function(x) {
    cent$b_dir <- centr_betw(x, directed =  TRUE)$res
    cent$b_undir <- centr_betw(x, directed = FALSE)$res
    cent$c_all <- centr_clo(x, mode = "all")$res
    cent$c_in <- centr_clo(x, mode = "in")$res
    cent$c_out <- centr_clo(x, mode = "out")$res
    cent$d_all <- centr_degree(x, mode = "all")$res
    cent$d_in <- centr_degree(x, mode = "in")$res
    cent$d_out <- centr_degree(x, mode = "out")$res
    cent$e_dir <- centr_eigen(x, directed = TRUE)$vector
    cent$e_undir <- centr_eigen(x, directed = FALSE)$vector
    cent <- lapply(cent, function(y) {
      names(y) <- V(x)$name
      return(sort(y, decreasing = TRUE)[1:n])
    })
  })
}
cc <- conf_centralities(2)

unlist(cc)
# ovo mora nekako da se stampa... ubaci u matricu? df?


# Analiza teksta:
#   
# * Koji je opsti utisak o skupu? (analiza sentimenta)
# * Koliko su informacije nove za ucesnike? (zastupljenost tema skupa u tvitovima - 
#        u toku trajanja skupa prema periodu pre skupa)
# * Koliko teme skupa uticu na ucesnike posle skupa? 
#        (periodi posle skupa prema periodu trajanja skupa)
# * Koja je ucestanost komunikacije u toku skupa? (broj tvitova o temama skupa prema 
#         ukupnom broju tvitova, za razliku od mreznih metrika)
# * Da li centralni ucesnici i ucesnici koji pripadaju grupama ('angazovaniji ucesnici') 
#         imaju vecu korelaciju tvitova sa temama skupa?
# 
# 
