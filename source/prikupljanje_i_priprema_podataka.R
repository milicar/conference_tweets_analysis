library(twitteR)
library(dplyr)

### Originalni tvitovi su skidani u periodu 22.05-27.05.2017. funkcijom searchTwitter iz paketa twitteR, za
### kljucne reci #WIELead i @wieilc; sam skup je odrzan 22.05. i 23.05.2017. godine.
### Funkcija searchTwitter kao rezultat vraca listu, koja moze da se prebaci u data frame pomocu funkcije twListToDF 
### iz istog paketa.
### startertweets je data frame sa svim tvitovima prikupljenim na ovaj nacin; buduci da su neki tvitovi oznaceni
### sa obe ove kljucne reci, potrebno je izbaciti duplikate:

startertweets <- readRDS("startertweets")

if(packageVersion("dplyr") >= "0.5.0") {
  uniquesttw <- distinct(startertweets, id, .keep_all = TRUE) 
} else uniquesttw <- distinct(startertweets, id)


### Iz ovog skupa sam izabrala ucesnike koji su tvitovali sa oznakom skupa bilo kada u vremenskom periodu od tri dana 
### pre, do tri dana posle skupa. Razlog za prosirenje je ukljucivanje ucesnika koji nozda nisu tvitovali u toku trajanja 
### skupa, ali su, na primer, posle skupa tvitovali o utiscima. Ovaj period se poklapa sa prosirenim periodom trajanja 
### skupa koji je koriscen prilikom analize, uz pretpostavku da period najintenzivnije komunikacije pocinje nesto pre, i 
### zavrsava nesto posle samog skupa, ali bi konkretno trajanje tog perioda moglo preciznije da se odredi.

### Napomena: prilikom poredenja vremena u POSIXct formatu, u koloni "created", uvek mi je racunao 
### vremensku razliku od 2h i vracao 'pomerene' rezultate; pri prebacivanju u Date format dobije se ispravan 
### datum, tako da sam ovaj format koristila za sve analize.
### Pomenuta razlika se moze videti u pretragama redova:
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


### Za izabrane korisnike sam skinula dostupne tvitove pomocu funkcija get_tweets i dl_tweets, prvobitno
### zasnovanih na funkciji ekstrakcija Marka Galjaka (https://github.com/gljk/Master-rad/blob/master/3.%20Listinzi%20koda.R),
### ali u konacnoj verziji prilicno izmenjenih.

### Funkcija get_tweets uzima kao argumente listu korisnika za koje se skidaju tvitovi, broj tvitova po korisniku,
### da li treba da ukljuci retvitove i da li treba da stampa trenutno stanje - ovo zbog uvida u izvrsavanje
### funkcije, jer se za veliki broj tvitova funkcija jako dugo izvrsava (na mom racunaru nesto manje od 1 minuta za 
### jednog korisnika, 3200 tvitova), a desava se i da stane u pozivu: .Call(R_curl_fetch_memory, url, handle, nonblocking); 
### Inace, korisnike za koje API vrati gresku funkcija upisuje u vektor locked_users, pa je moguce ponovo pokusati 
### skidanje tvitova za ove korisnike, za slucaj da nije u pitanju obrisani ili 'protected' nalog, nego trenutni 
### problem sa pristupom podacima

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

### Prvo sam skinula sve dostupne tvitove za izabrane ucesnike, a zatim sam izdvojila samo one ucesnike za koje 
### su mi dostupni tvitovi najmanje od 21.04. iz razloga sto neki korisnici tvituju po 3200 tvitova za nekoliko dana, tako
### da nije bilo moguće pristupiti njihovim tvitovima toliko unazad, pa njihovo ponašanje ne bi bilo verodostojno prikazano 
### za ceo posmatrani period. S druge strane, za ucesnike za koje su mi dostupni svi tvitovi barem od 21.04. nisam 
### postavljala uslov da moraju da imaju tvitove tokom celog perioda, jer njihova frekvencija tvitovanja predstavlja
### njihovo ponasanje koje mogu da analiziram, a dostupno mi je u celosti za posmatrani period.
### Za tako odabrane ucesnike, ukupno 457, izdvojila sam tvitove samo za posmatrani period. 

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

### Kasnije u toku analize sam dosla do toga da je bilo gresaka pri skidanju tvitova - da li zbog tviter API-ja
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

### Sve posmatrane tvitove sam podelila na pet grupa, po vremenu tvitovanja: osnovnu grupu ("time_3") cine tvitovi
### iz perioda trajanja skupa uz tri dana pre i posle skupa, odnosno od 19.05.2017. do 26.05.2017. (ukljucujuci 
### i te datume), kao period najintenzivnije komunikacije u vezi sa skupom; ostali periodi su simetricno rasporedeni
### pre i posle skupa, svaki u trajanju od po dve nedelje.

selected_tweets$timeframe <- 
  ifelse(selected_tweets$date >= "2017-04-21" & selected_tweets$date <= "2017-05-04", "time_1", 
         (ifelse(selected_tweets$date >= "2017-05-05" & selected_tweets$date <= "2017-05-18", "time_2",
                 (ifelse(selected_tweets$date >= "2017-05-19" & selected_tweets$date <= "2017-05-26", "time_3",
                         (ifelse(selected_tweets$date >= "2017-05-27" & selected_tweets$date <= "2017-06-09", "time_4", "time_5")))))))


splitted <- split(selected_tweets, selected_tweets$timeframe)

### Pregledanjem dobijene liste i pripremanjem za pravljenje matrice, primetila sam da je broj ucesnika razlicit
### u ovim vremenskim odeljcima, sto je u redu, jer sam trazila da najraniji dostupni tvit po korisniku bude
### 21.4. ili ranije, pa je tako moguce da je korisnik tvitovao 20.04. (sto nije uslo u posmatrane tvitove), ali
### nije tvitovao posle toga do konferencije; (dva korisnika koja se nisu pojavljivala u trecem vremenskom odeljku,
### sto je vreme samog dogadaja, na osnovu kojeg su tvitovi i prikupljani, ubacila sam prethodnim postupkom dopune
### konacnog skupa tvitova).


### Funkcija za pravljenje matrice povezanosti, takode zasnovana na funkciji matrica_povezanosti Marka Galjaka
### (https://github.com/gljk/Master-rad/blob/master/3.%20Listinzi%20koda.R), delimicno izmenjena:

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

