### Analiza teksta

library(dplyr)
library(sentimentr)

### 1. Koji je opsti utisak o skupu?

### Na ovo pitanje probacu da odgovorim analizom sentimenta tvitova koji govore o skupu.

conf_tweets <- readRDS("results/conf_tweets.RData")

### Najjednostavniji nacin za analizu sentimenta je preko recnika sentimenta. Odredivanje sentimenta tvita kao 
### sume sentimenta pojedinacnih reci je dosta jednostavno, ali ne uzima u obzir kontekst, odnosno negacije ili
### priloge ("not good", "quite good", "somewhat good"). 
### Paket sentimentr odreduje sentiment tako sto za svaku rec koja ima odredeni sentiment odredi kontekst
### (broj reci pre i posle konkretne reci moze da se prosledi kao argument, podrazumevano je 5 i 2) u okviru koga
### negacije i modifikatori sentimenta uticu na ukupnu tezinu reci za koju se racuna sentiment. Kod negacija
### se vodi racuna o broju, sa pretpostavkom da se paran broj negacija ponistava; modifikatori pojacavaju ili
### smanjuju intenzitet sentimenta; suprotni veznici ("but", "however"..) takode uticu na intenzitet sentimenta,
### dajuci vecu tezinu delu konteksta posle veznika; zarezi, tacka-zarez i dve tacke sluze kao dodatne granice
### u kontekstu, jer oznacavaju promenu u toku misli. Na kraju se suma tezina za sve reci podeli kvadratnim
### korenom broja reci, dajuci neograniceni skor za celu recenicu.

### Postavlja se pitanje sta raditi sa pominjanjima i hashtag-ovima, koji bi isto tako mogli da nose sentiment:

sentiment("#happy") # => sentiment: 0.75

### Ukoliko korisnici na ovaj nacin ponovo iskazuju isti sentiment kao i u osnovnom tekstu, onda bi se na ovaj 
### nacin duplirala tezina tog sentimenta. Ukoliko se hashtag-ovi shvate kao dodatno pojasnjenje teksta iz kog
### se inace ne moze zakljuciti sentiment, onda bi njihovo ukljucivanje u analizu imalo smisla. U svakom slucaju,
### hashtag-ovi i pominjanja se ne mogu u potpunosti izbaciti, jer ulaze u ukupan broj reci, ali bi se mogli
### zameniti nekom neutralnom zamenskom reci. U ovoj analizi cu "neutralisati" pominjanja i hashtag-ove, po uzoru
### na rad "Twitter Sentiment Classification using Distant Supervision":
### (http://s3.eddieoz.com/docs/sentiment_analysis/Twitter_Sentiment_Classification_using_Distant_Supervision.pdf)

conf_tweets_sentiments <- conf_tweets %>% 
  mutate(text = str_replace_all(text, "(#|@)[A-Za-z\\d-_]+", "HLDR"))

### Ovaj paket racuna sentiment za svaku recenicu, a jedan tvit moze da se sastoji iz vise recenica. Funkcija
### sentiment primenjena na ceo tekst (tvit) vraca po jednu vrednost sentimenta za svaku recenicu (povratna vrednost je
### tipa data frame sa cetiri kolone, od kojih je "sentiment" cetvrta), koje bi zatim trebalo sumirati.

conf_tweets_sentiments <- conf_tweets_sentiments %>% mutate(sentiment = 0)

for(i in 1:nrow(conf_tweets_sentiments)) {
  sents <- sentiment(conf_tweets_sentiments[i, "text"])
  conf_tweets_sentiments[i, "sentiment"] <- apply(sents, 2, sum)[4]
}


conf_tweets_sentiments %>% arrange(desc(sentiment)) %>% top_n(10) %>% select(sentiment, text)

### Najpozitivniji tvit bih ocenila kao donekle pozitivan:
# HLDR Whats a good team? All have a skill, but all have to understand, pitch,  product manage. A good team owns all roles! HLDR
### Medutim, tek na devetom mestu je jedan koji sadrzi reci "amazing", "thanks", "exciting" i "fun":
# HLDR: Another amazing year!! Thanks to all the HLDR  HLDR: your hard work to create an exciting, fun, deep learning
### Iznad po rangu su tvitovi kao:
# .Links, videos, recommended to learn more about VR, AI, machine learning, and IBM Watson Health. HLDR
# Interested in aerospace? See a real HLDR and chat with the women engineering leaders of Planet! Learn more:
### (Treba imati na umu da nisu izbaceni retvitovi, pa su zbog toga mnogi tvitovi isti, sto se vidi i u prvih 10.)

sentiment("learn") # => 0.8
sentiment("learn more") # => 1.01
sentiment("amazing") # => 0.5
sentiment("amazing year") # => 0.35

### Osim sto se rec "learn" pozitivnije ocenjuje nego "amazing", izraz "learn more" sadrzi i modifikator koji 
### daje vecu tezinu, dok "amazing year" ima samo jos jednu neutralnu rec koja smanjuje ukupan rezultat deleci ga
### na dve reci. Iako je "learn" pozitivna rec, "learn more" ne bi trebalo da ima tako visok rezultat, cak ni u
### nekom opstem kontekstu, a pogotovo u kontekstu ucenja i konferencija.


conf_tweets_sentiments %>% arrange(sentiment) %>% top_n(-10) %>% select(sentiment, text)

### Najnegativniji tvit, sa dvostruko vecim skorom od sledece plasiranog, je:
# #HLDR: Huge Potential for Disaster Robotics HLDR HLDR:  via HLDR
### Jasno je da je ovde u pitanju termin "disaster robotics" koji ne bi trebalo da ima negativnu konotaciju.
### Interesantno je i uporediti ovaj sa slicnim tvitom koji ima nesto nizi skor:
# Huge Potential for Disaster Robotics HLDR HLDR:  via HLDR

sentiment("Huge Potential for Disaster Robotics HLDR HLDR:  via HLDR") # => -0.6
sentiment("HLDR: Huge Potential for Disaster Robotics HLDR HLDR:  via HLDR") # => -1.64

### Koliko god da su pravila dobra i uzimaju u obzir veliki broj uticaja na tezinu sentimenta, izgleda da
### tviter poruke, gde pravopis i interpunkcija ne prate normu i gde jedan odgovor (@korisnik : tvit)
### promeni strukturu "recenice", zahtevaju neku manje striktnu analizu. 

### Za pocetak cu ovakve rezultate da odvojim po vremenskim periodima, sumiram skorove i normalizujem prema
### broju tvitova.

conf_tweets_sentiments %>% group_by(timeframe) %>% 
  summarise(sum_sentiment = sum(sentiment), num = length(timeframe), norm_sentiment = sum_sentiment / num,
            max_sentiment = max(sentiment), min_sentiment = min(sentiment))


### Prvo sto se moze primetiti je da su sve sume sentimenta pozitivne, sto znaci da ima mnogo vise tvitova 
### ocenjenih pozitivno, ali buduci da ukupan rezutat po tvitu nije ogranicen, (najvisi skor je 1.9, a najnizi
### -1.6), ne moze se mnogo detaljnije reci. Po periodima, period trajanja konferencije ima nizi normalizovani
### sentiment od perioda neposredno pre (moguce rezultat najava skupa i "learn more") i oba perioda posle skupa. 
### S druge strane, u ovom periodu su tezine sentimenta najvece, i za pozitivne i za negativne tvitove (mada 
### se kod negativnih tvitova radi o pogresno ocenjenom tvitu, sto cu kasnije ispraviti).
### Buduci da ova analiza dosta govori o samim temama skupa, a u toku konferencije je potencijalno bilo vise 
### rasprave nego u periodima posle skupa, mozda bi bilo zanimljivo pogledati koji su tvitovi najpozitivniji i
### najnegativniji za svaki period.

timefr <- sort(unique(conf_tweets_sentiments$timeframe), decreasing = FALSE)

time_sent <- lapply(1:length(timefr), function(x){
  res <- list()
  sent <- conf_tweets_sentiments %>% filter(timeframe == timefr[x]) %>% arrange(sentiment)
  res$pos <- sent %>% top_n(3) %>% arrange(desc(sentiment)) %>% select("sentiment", "text")
  res$neg <- sent %>% top_n(-3) %>% select("sentiment", "text")
  return(res)
})

### Kod pozitivnih tvitova, u periodima pre konferencije se uocava "learn" ili "learn more", uz neosporne i druge 
### pozitivne reci, dok je u periodima posle skupa cesce "thanks". U periodu skupa, pozitivni tvitovi se bave temama 
### skupa. Sto se tice negativnih tvitova, mogli bi se podeliti na one koji su pogresno ocenjeni (verovatno zaslugom:
### "disruptive technology": -0.28, "sneak peek": -0.35, "disaster robotics": -0.7, "crazy morning": -0.53, "homeless": -1)
### i na one koji se bave polozajem zena, sto je blisko povezano sa temama skupa.

### Ono sto svakako treba uraditi jeste normalizacija prema temama skupa, odnosno neutralisanje termina 
### koji predstavljaju teme o kojima se govori na samom skupu, a koji mogu da imaju veliku tezinu, kao
### vec pomenuti "disaster robotics" ili "learn". Na ovaj nacin ce dobijena analiza nesto vise govoriti o 
### sentimentu vezanom za sam skup, a manje o onom vezanom za teme, mada bi bilo vrlo tesko postici potpuno 
### razgranicenje. U slucaju ovog skupa, teme uglavnom nisu neutralne i vezane su, izmedu ostalog, za liderstvo,
### predrasude, uspeh... 
### Pojedinacne reci se mogu neutralisati modifikovanjem recnika koji se koristi za analizu, a prema
### identifikovanim kljucnim recima za teme skupa. Medutim, jedan od najnegativnijih tvitova je i: 
### "You can choose your thoughts," Michelle Molitor, CEO of Nectar. "If you say I'm sick and tired, you will become 
### sick and tired." HLDR
### koji ocigledno ne govori lose o samom skupu, ali neutralisanje reci "sick" i "tired" bi vec bilo preterana 
### intervencija. S druge strane, nece biti moguce neutralisati ni sve pozitivne reci, kao sto je "honor" u 
### "honoring women in ...".


### U slucaju "disaster robotics" nece biti moguce preko recnika neutralisati sintagmu, a neutralisanje samo 
### reci "disaster" bi promenilo sentiment i u onim slucajevima kada se ne pojavljuje u ovoj sintagmi. Ovde bi 
### mogla da se pronadu pojavljivanja cele sintagme, pa da se zamene neutralnim zamenskim recima.



TODO # ceka se odredivanje kljucnih reci, odnosno tema skupa..
TODO # koje su najfrekventnije pozitivne i negativne reci po periodima?
# 




