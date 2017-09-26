### Analiza teksta

library(dplyr)
library(sentimentr)
library(stringr)
library(tidytext)
library(ggplot2)


### 1. Koji je opsti utisak o skupu?

### Na ovo pitanje probacu da odgovorim analizom sentimenta tvitova koji govore o skupu.

conf_tweets <- readRDS("results/tweets.RData") %>% filter(conf == TRUE)

### Najjednostavniji nacin za analizu sentimenta je preko recnika sentimenta. Odredivanje sentimenta tvita kao 
### sume sentimenta pojedinacnih reci je dosta jednostavno, ali ne uzima u obzir kontekst, odnosno negacije ili
### priloge ("not good", "quite good", "somewhat good"). 
### Paket sentimentr odreduje sentiment tako sto za svaku rec koja ima odredeni sentiment odredi kontekst
### (broj reci pre i posle konkretne reci moze da se prosledi kao argument, podrazumevano je 5 i 2) u okviru koga
### negacije i modifikatori sentimenta uticu na ukupnu tezinu reci za koju se racuna sentiment. Kod negacija
### se vodi racuna o broju, sa pretpostavkom da se paran broj negacija ponistava; modifikatori pojacavaju ili
### smanjuju intenzitet sentimenta; suprotni veznici ("but", "however"..) takode uticu na intenzitet sentimenta,
### dajuci vecu tezinu delu konteksta posle veznika; zarez, tacka-zarez i dve tacke sluze kao dodatne granice
### u kontekstu, jer oznacavaju promenu u toku misli. Na kraju se suma tezina za sve reci podeli kvadratnim
### korenom broja reci, dajuci neograniceni skor za celu recenicu.

### Dve konstrukcije karakteristicne za Tviter poruke su pominjanja i hestegovi.
### Buduci da predstavljaju izraz licnih stavova i misljenja, hestegovi ce biti ukljuceni u analizu. Oni mogu 
### da budu nosioci sentimenta:

sentiment("#happy") # => sentiment: 0.75

### Pominjanja sama po sebi nisu problem, ali je problem u tome sto su vrlo cesto pracena sa dve tacke, sto paket
### sentimentr tumaci kao promenu teme. Preciznije, dve tacke menjaju granice konteksta za rec za koju se racuna 
### sentiment, i na taj nacin dovode do velikih razlika u ukupnom rezultatu za sentiment celog tvita:

sentiment("@ConvergeDigest: Huge Potential for Disaster Robotics #WIElead #rescuerobotics:  via @YouTube") 
# => sentiment: -1.64
sentiment("Huge Potential for Disaster Robotics #WIElead #rescuerobotics:  via @YouTube") 
# => sentiment: -0.6

### Ovde ovakva konstrukcija ima ulogu odgovora nekom korisniku (za razliku od pominjanja koje je bez dve tacke, 
### kao @YouTube u primerima iznad), a ne menjanja teme, sto znaci da preveliko davanje vaznosti znaku ':' ovde nema 
### mnogo smisla.

### Broj tvitova sa znakom ':' i sa odgovorom (@user:)

nrow(filter(conf_tweets, str_detect(conf_tweets$text, pattern = ":")))
# => 1425
nrow(filter(conf_tweets, str_detect(conf_tweets$text, pattern = "@[A-Za-z\\d-_]+:")))
# => 1215

### Od svih koriscenja znaka ':', 85% je u funkciji odgovora, odnosno odmah posle pominjanja korisnika, sto bi sigurno
### dosta uticalo na rezultate analize. Buduci da ni sama pominjanja ne bi trebalo da igraju znacajnu ulogu u racunanju
### sentimenta (jedino doprinose broju neutralnih reci), izbacicu obe ove varijante pominjanja.


conf_tweets_sentiments <- conf_tweets %>% 
  mutate(text = str_replace_all(text, "@[A-Za-z\\d-_]+:?", ""))


### Ovaj paket racuna sentiment za svaku recenicu, a jedan tvit moze da se sastoji iz vise recenica. Funkcija
### sentiment primenjena na ceo tekst (tvit) vraca po jednu vrednost sentimenta za svaku recenicu (povratna vrednost 
### je tipa data frame sa cetiri kolone, od kojih je "sentiment" cetvrta), koje bi zatim trebalo sumirati.

get_conf_sentiments <- function(tweetset){
  tweetset <- tweetset %>% mutate(sentiment = 0)
  
  for(i in 1:nrow(tweetset)) {
    sents <- sentiment(tweetset[i, "text"])
    tweetset[i, "sentiment"] <- apply(sents, 2, sum)[4]
  }
  return(tweetset)
}

# izvrsava se oko 2min!
conf_tweets_sentiments <- get_conf_sentiments(conf_tweets_sentiments)


### Prvih 10 najpozitivnijih tvitova:

conf_tweets_sentiments %>% arrange(desc(sentiment)) %>% top_n(10) %>% select(sentiment, text)

### Najpozitivniji tvit bih ocenila kao donekle pozitivan:
# #wielead Whats a good team? All have a skill, but all have to understand, pitch,  product manage. 
#       A good team owns all roles! #womenintech
### Drugi po redu dosta pominje ucenje:
# .Links, videos, recommended to learn more about VR, AI, machine learning, and IBM Watson Health. #WIELead 
### Medu prvih deset ima i onih koje bih ocenila pozitivnije i vise u vezi sa samom konferencijom:
# Congratulations  and  team for another great #WIELead. Learning from best #WIT and making lot of new frien…
# Another amazing year!! Thanks to all the #WIElead  #volunteers: your hard work to create an exciting, fun, deep learning …
### (Treba imati na umu da nisu izbaceni retvitovi, pa su zbog toga mnogi tvitovi isti, sto se vidi i u prvih 10.)

### Malo je cudno da je tvit sa recima "amazing", "exciting", "fun" tek na devetoj poziciji, dosta ispod onog koji 
### pominje "learn":

sentiment("learn") # => 0.8
sentiment("learn more") # => 1.01
sentiment("amazing") # => 0.5
sentiment("amazing year") # => 0.35

### Osim sto se rec "learn" pozitivnije ocenjuje nego "amazing", izraz "learn more" sadrzi i modifikator koji 
### daje vecu tezinu, dok "amazing year" ima samo jos jednu neutralnu rec koja smanjuje ukupan rezultat deleci ga
### na dve reci. "Amazing" u odnosu na neke slicne prideve:

sentiment("astounding") # => 0.75
sentiment("exciting") # => 0.75
sentiment("fantastic") # => 0.75, ali:
sentiment("astonishing") # => 0.5

### Koliko su ovi pridevi zastupljeni u tvitovima?

nrow(filter(conf_tweets_sentiments, str_detect(conf_tweets_sentiments$text, pattern = "\\bamazing"))) # => 55
nrow(filter(conf_tweets_sentiments, str_detect(conf_tweets_sentiments$text, pattern = "\\bexciting"))) # => 13
nrow(filter(conf_tweets_sentiments, str_detect(conf_tweets_sentiments$text, pattern = "\\bfantastic"))) # => 9
nrow(filter(conf_tweets_sentiments, str_detect(conf_tweets_sentiments$text, pattern = "\\bastonishing"))) # => 0

### Mozda se rec "amazing" i inace cesce koristi pa se zato smatra "obicnijom" od, na primer, "exciting". Ali cak i ovi
### pridevi imaju nizu ocenu sentimenta od "learn". Osim sto ima veci intenzitet sentimenta, "learn" je i cesca rec:

nrow(filter(conf_tweets_sentiments, str_detect(conf_tweets_sentiments$text, pattern = "\\blearn"))) # => 88
### (Ovde ce regularni izraz "learn" upariti i oblike "learning", "learned", "learner"...)

### Moje sumnje u to da ce izraz "learn more:" (upotrebljeno kao neka vrsta putokaza na link, a ne u stvarnom 
### znacenju ucenja) dosta doprineti pozitivnom sentimentu nisu opravdane:

nrow(filter(conf_tweets_sentiments, str_detect(conf_tweets_sentiments$text, pattern = "\\blearn more"))) # => 7
nrow(filter(conf_tweets_sentiments, str_detect(conf_tweets_sentiments$text, pattern = "\\bmore:"))) # => 8


### Evo i najnegativnijih tvitova:

conf_tweets_sentiments %>% arrange(sentiment) %>% top_n(-10) %>% select(sentiment, text)

### Najnegativniji tvit jeste vrlo negativan, iako nisam sigurna iz teksta koliko se bas odnosi na sam skup:
# it's the same BS we always got around the mascot issue at FSU. It's depressing and upsetting #wielead
### Medu negativnim tvitovima ima nekoliko pogresno klasifikovanih:
# Huge Potential for Disaster Robotics #WIElead #rescuerobotics:  via 
# That was crazy morning at #wieilc ! Taking picture in the middle of the street ! #traffic #crazy #wielead… 
# #WIELead Leadership: First shatter your inner glass ceiling! Lose your fear  
#    flaws; clear out self-doubt: say hi but Goodbye! #womenintech
### U prvom primeru se radi o pogresno protumacenom terminu i ovo je moguce ispraviti modifikacijom recnika.
### U drugom primeru se radi o neformalnoj upotrebi reci "crazy" sa prenesenim znacenjem, dok je u trecem 
### vise reci upotrebljeno u pravom znacenju, ali u jednoj poeticno-alegoricnoj konstrukciji koja u celini 
### uzev nema negativno znacenje. Ovako prenesena znacenja, kao i ironiju i sarkazam, pristup analizi sentimenta
### preko recnika ne moze da otkrije i ispravno oceni.
### Ipak, medu ovima se naslo i nekoliko tvitova koji se odnose bas na govornike i predavanja na skupu, na primer:
# keep writing and deleting tweets abt that last presenter. that presentation, esp once I looked up the 
#     controversy, really upset me #WIELead


### Koje su pojedinacne reci koje najvise doprinose pozitivnom i negativnom sentimentu?

conf_words_sentiment <- conf_tweets_sentiments %>% 
  unnest_tokens(word, text, token = "regex", pattern = "[^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@])") %>%
  count(timeframe, word) %>% mutate(word_sent = sentiment(word)[,4][[1]], total_word_sent = n * word_sent) 

pdf("visuals/text1_word_sentiment_contribution.pdf")
conf_words_sentiment %>% group_by(total_word_sent < 0) %>% top_n(10, abs(total_word_sent)) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, total_word_sent)) %>%
  ggplot(aes(word, total_word_sent, fill = total_word_sent < 0)) +
  geom_col() + coord_flip() + 
  labs(title = "word sentiment contribution", x = NULL, y = "sentiment * number of occurences") + 
  scale_fill_discrete(name = "", labels = c("positive", "negative")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
dev.off()

### Medu deset najpozitivnijih su se nasle reci "great", "learn", congratulations", "excited", "amazing", a
### medu najnegativnijima su "stop", "disaster", "bias", "competition"... Po periodima:

splitted_words <- split(conf_words_sentiment, conf_words_sentiment$timeframe) # zbog lakse vizualizacije

periods <- c("from 2017-04-21 to 2017-05-04", "from 2017-05-05 to 2017-05-18", "from 2017-05-19 to 2017-05-26", 
             "from 2017-05-27 to 2017-06-09", "from 2017-06-10 to 2017-06-23")

pdf("visuals/text1_word_sentiment_contribution_by_period.pdf")
lapply(1:length(splitted_words), function(x) {
  splitted_words[[x]] %>% group_by(total_word_sent < 0) %>% top_n(10, abs(total_word_sent)) %>%
    ungroup() %>% mutate(word = reorder(word, total_word_sent)) %>% 
    ggplot(aes(word, total_word_sent, fill = total_word_sent < 0)) +
    geom_col() + coord_flip() + 
    labs(title = paste0("word sentiment contribution\n", periods[x]), x = NULL, y = "sentiment * number of occurences") + 
    scale_fill_discrete(name = "", labels = c("positive", "negative")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
})
dev.off()

### Od pozitivnih reci, u periodima pre skupa dominira "learn", nesto ispod su "join" i "sponsor", u periodu trajanja
### skupa, najpozitivnije su reci "great", "honored", "learn", "congratulations", dok su u prvom periodu posle skupa
### to reci "thanks", "great", "learning", "inspiration", i upravo ovo bi moglo da se shvati kao pozitivna ocena
### nakon odrzanog skupa.
### Medu negativnim recima ima nekih koje izgledaju pogresno ocenjene, kao "interviewed", a i nekih koje u ovom
### domenu imaju drugacije znacenje, kao "disruptive". Mozda ce bigrami dati malo vise konteksta:

conf_bigrams_sentiment <- conf_tweets_sentiments %>% 
  mutate(text = str_replace_all(text, pattern = "\\.", replacement = " ")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% count(timeframe, bigram) %>% 
  mutate(bigram_sent = sentiment(bigram)[,4][[1]], total_bigram_sent = n * bigram_sent)
  

splitted_bigrams <- split(conf_bigrams_sentiment, conf_bigrams_sentiment$timeframe)

pdf("visuals/text1_bigram_sentiment_contribution_by_period.pdf")
lapply(1:length(splitted_bigrams), function(x) {
  splitted_bigrams[[x]] %>% group_by(total_bigram_sent < 0) %>% top_n(10, abs(total_bigram_sent)) %>%
    ungroup() %>% mutate(bigram = reorder(bigram, total_bigram_sent)) %>% 
    ggplot(aes(bigram, total_bigram_sent, fill = total_bigram_sent < 0)) +
    geom_col() + coord_flip() + 
    labs(title = paste0("bigram sentiment contribution\n", periods[x]), x = NULL, y = "sentiment * number of occurences") + 
    scale_fill_discrete(name = "", labels = c("positive", "negative")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
})
dev.off()


### Pojavljivanje reci u kontekstu je donekle promenilo rang reci (na primer, u prvom vremenskom odeljku, "learn" 
### se pojavljuje cesce nego "sponsor", ali u varijantama "learn more", "learn about", "learn from", pa su ovi 
### bigrami nesto redi, dok se "sponsor" uvek pojavljuje kao "our sponsor", odnosno "(our) sponsor Ericsson"). 
### Ipak, ovako se dobije malo jasnija slika ocenjenih reci. Na primer, u drugom periodu, "sneak" se odnosi na 
### "(a) sneak peek", sto nije negativno, "blocks" na "blocks 9am" (nejasno), "building blocks" (pozitivno)
### i "roadblocks" (negativno), a "cad" (kao skracenica) na "cad infrastructure" i "spotlight cad", koji nemaju 
### veze sa znacenjem reci "cad" - "nitkov, podlac". U trecem periodu, "stop" se odnosi na "to stop" (nejasno), 
### "stop by" (ne negativno) i "stop doubting" (pozitivno), a "disaster" na "(for) disaster robotics", sto nije 
### negativan pojam, ali se sa vrlo negativnom ocenom nasao i u najnegativnijem tvitu. Pozitivni bigrami su uglavnom 
### ispravno ocenjeni, osim mozda "machine learning" i "our sponsor" koje bih licno ocenila kao neutralne, ali 
### svakako nemaju tako izrazeno suprotno znacenje kao neki od navedenih negativno ocenjenih bigrama.

### Cak i sa ovakvim rezultatima, gde je negativni sentiment mozda preuvelican, intenzitet negativnog sentimenta je 
### mnogo manji nego intenzitet pozitivnog. 

### Sentiment po vremenskim periodima: sumiran, normalizovan brojem tvitova, maksimalan i minimalan rezultat za
### pojedinacni tvit:

conf_tweets_sentiments %>% group_by(timeframe) %>% 
  summarise(sum_sentiment = sum(sentiment), num = length(timeframe), norm_sentiment = sum_sentiment / num,
            max_sentiment = max(sentiment), min_sentiment = min(sentiment))


### Ovde se moze primetiti da su sve sume sentimenta pozitivne, sto znaci da ima mnogo vise tvitova ocenjenih 
### pozitivno, ali buduci da ukupan rezutat po tvitu nije ogranicen, ne moze se mnogo detaljnije reci. 
### I po apsolutnoj vrednosti za pojedinacni tvit, pozitivni sentiment je dominantniji: najvisi skor po tvitu 
### je 1.9, a najnizi -0.8. Gledano po periodima, period trajanja konferencije ima nizi normalizovani sentiment 
### od perioda neposredno pre i oba perioda posle skupa, ali u isto vreme, u tom periodu su i najvece vrednosti
### sentimenta po tvitu, odnosno, najvece su varijacije.

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


### Kod pozitivnih tvitova, u periodima pre konferencije ima nekoliko najava skupa ili predavanja, kao i nekoliko
### pominjanja "learn" i "learn more" koji imaju visoku ocenu sentimenta, a osim toga su i dosta frekventni u 
### tvitovima. U periodu skupa je i najpozitivniji tvit, koji govori o tome sta cini dobar tim, sto bih protumacila 
### kao temu skupa, pre nego kao neko pozitivno misljenje o skupu. I druga dva tvita govore o temama skupa, o ucenju 
### i zenama u tehnickim naukama. U poslednja dva perioda ima vise tvitova koji se odnose upravo na organizaciju 
### skupa, izrazavajuci cestitke i zahvaljivanja.
### Sto se tice negativnih tvitova, mogli bi se podeliti na one koji su pogresno ocenjeni (verovatno zaslugom:
### "disruptive technology": -0.28, "sneak peek": -0.35, "disaster robotics": -0.7, "crazy morning": -0.53, 
### "homeless": -1) i na one koji se bave polozajem zena inzenjera, sto je blisko povezano sa temama skupa, ali ne 
### govori negativno o samom skupu.

### Iz ovog sazetog sadrzaja tvitova moze se primetiti da je ovakva analiza donekle problematicna, buduci da ne moze
### da razdvoji tvitove koji govore o samom skupu, koliko je dobro ili lose organizovan, koliko je zanimljiv ili 
### kakav je kvalitet predavaca, i one koji govore o temama skupa, a koje u ovom slucaju nisu neutralne i pominju 
### liderstvo, inovacije, osnazivanje, ali i predrasude i borbu za veca prava. 


### Kakav je sentiment kljucnih reci izdvojenih iz programa skupa, i koliko one ucestvuju u ukupnom sentimentu? 
### (Okvirno, jer u celom tvitu na njih utice i kontekst.) Da bih do ovoga dosla, tabelu sentimenta po recima i 
### tabelu kljucnih reci sam spojila po zajednickoj osnovi reci, buduci da se, na primer, "disaster" u kljucnim 
### recima javlja samo u mnozini, a u tvitovima samo u jednini. Zatim sam za svaku rec sumirala broj pojavljivanja 
### i ukupan sentiment (u tabeli conf_word_sentiment su ovi podaci dati po periodima). 

program_keywords <- readRDS("results/final_keywords_df.RData") %>% distinct(word)

keywords_sentiment <- conf_words_sentiment %>% mutate(word_stem = tm::stemDocument(word)) %>% 
  inner_join(program_keywords %>% mutate(word = tm::stemDocument(word)) %>% distinct(word), by = c("word_stem" = "word")) %>%
  group_by(word) %>% 
  summarise(occurences = sum(n), word_sentiment = first(word_sent), total_sentiment = sum(total_word_sent)) %>%
  arrange(total_sentiment)

pdf("visuals/text1_keywords_sentiment_contribution.pdf")
keywords_sentiment %>% group_by(total_sentiment < 0) %>% top_n(5, abs(total_sentiment)) %>%
  ungroup() %>% mutate(word = reorder(word, total_sentiment)) %>%
  ggplot(aes(word, total_sentiment, fill = total_sentiment < 0)) +
  geom_col() + coord_flip() + 
  labs(title = "top 5 positive and negative\nkeywords sentiment contributions", x = NULL, 
       y = "sentiment * number of occurences") + 
  scale_fill_discrete(name = "", labels = c("positive", "negative")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
dev.off()


### Teme skupa su u velikoj meri uticale na to da ukupan sentiment bude izrazito pozitivan. Termin "disaster robotics",
### iako je pogresno ocenjen kao negativan, nije imao veceg udela u ukupnom sentimentu. 
### Buduci da je ocekivano da ce se o temama skupa dosta govoriti u ovim tvitovima, mogla bih da probam da na neki 
### nacin neutralisem njihov uticaj i pogledam kakva je ocena sentimenta ako se ne posmatraju teme skupa. Ipak, teme
### su u ovom slucaju predstavljene vrlo sirokim i opstim terminima, kao sto su ucenje, podsticanje, ohrabrenje, pa
### je moguce da ce se na ovaj nacin neutralisati i vise od samih tema skupa.


### Pokusacu da neutralisem najuticajnije kljucne reci izmenom njihove ocene sentimenta u recniku koji se koristi
### za analizu.

top_words <- keywords_sentiment %>% group_by(total_sentiment < 0) %>% 
  top_n(5, abs(total_sentiment)) %>% ungroup() %>% select(word)

### Recnik i reci koje treba izmeniti se spajaju preko osnove, tako ce se izmeniti i "bias" i "biased", ali nece 
### "wellinformed", iako "well" hoce.

words_to_update <- lexicon::hash_sentiment_jockers %>%    
  mutate(stemmed = tm::stemDocument(lexicon::hash_sentiment_jockers$x)) %>%
  inner_join(top_words %>% mutate(word = tm::stemDocument(top_words$word)) %>% 
               distinct(word), by = c("stemmed" = "word")) %>% select(x) %>% mutate(y = 0)


updated_polarity_table <- update_polarity_table(lexicon::hash_sentiment_jockers, 
                                                drop = words_to_update$x,  # moraju prvo da se izbace postojece vrednosti
                                                x = data.frame(x = words_to_update$x, y = words_to_update$y))


### Nove ocene sentimenta:

conf_tweets_sentiments2 <- conf_tweets %>% 
  mutate(text = str_replace_all(text, "@[A-Za-z\\d-_]+:?", ""))

# izvrsavase oko 2min!
conf_tweets_sentiments2 <- get_conf_sentiments(conf_tweets_sentiments2)


### Po pet najpozitivnijih i najnegativnijih tvitova: (opadajuce do najnegativnijeg)
conf_tweets_sentiments2 %>% group_by(sentiment < 0) %>% top_n(5, abs(sentiment)) %>% ungroup() %>%
  arrange(desc(sentiment)) %>% select(text)

### Sa novim ocenama sentimenta, medu najpozitivnijim i najnegativnijim tvitovima nema neke velike promene,
### svaki od ovih tvitova je i u pocetnoj analizi bio medu prvih deset, a i najpozitivniji i najnegativniji
### tvit je isti kao i sa prvim ocenama. 

conf_tweets_sentiments2 %>% group_by(timeframe) %>% 
  summarise(sum_sentiment = sum(sentiment), num = length(timeframe), norm_sentiment = sum_sentiment / num,
            max_sentiment = max(sentiment), min_sentiment = min(sentiment))


### Maksimalna i minimalna ocena sentimenta su ostale iste, a i ostali zakljucci i dalje vaze, jedina je 
### razlika u tome sto su tvitovi treceg perioda pozitivniji i od tvitova drugog i cetvrtog perioda. Kod 
### ucestvovanja pojedinacnih reci u sentimentu, razlika je samo u rangu reci (zbog izbacenih reci).


### Mislim da moze da se zakljuci da je stav ucesnika o skupu vrlo pozitivan, cak i sa ovako problematicnim
### podacima i metodom koja ne razlikuje tvitove koji govore o skupu i one koji govore o temama skupa (sto 
### bi mozda moglo da se postigne masinskim ucenjem, odnosno treniranjem klasifikatora). 
### Ako bi se i moglo reci da su teme skupa takve da su vise doprinele pozitivnom nego negativnom sentimentu
### ("innovation", "learning" nasuprot "bias" ili "fight"), u cetvrtom periodu, dakle nakon skupa,
### najzastupljenije su reci "thanks", "great", "learning" i "inspiration", a najpozitivniji tvit je:
# Congratulations  and  team for another great #WIELead. Learning from best #WIT and making lot of new frien…
### Medu najnegativnijim tvitovima je bilo onih koji su kritikovali predavace ili predavanja, ali nisu
### ocenjeni najnizim ocenama. I ovde vazi primedba da metoda nije dovoljno precizna i da bi rezultati mogli
### biti i nesto drugaciji primenom drugih metoda. Bez obelezenog trening seta tesko je proceniti gresku, 
### jedino sto mogu jeste da na osnovu licnog utiska potvrdim da je mnogo teze naici na negativan nego na
### pozitivan tvit.

### U vezi sa metodom analize koriscenjem paketa sentimentr, primetila sam dva problema. Prvo, iako funkcije
### paketa uzimaju u obzir dosta parametara i pretpostavljam da su odlicne za klasicnu recenicu, nisu 
### najbolje resenje za strukturu tvitova. Drugi i mnogo veci problem se odnosi na sadrzaj tvitova, odnosno 
### nemogucnost razlikovanja tvitova koji govore o skupu (organizaciji, predavanjima, predavacima) i onih 
### koji govore o temama skupa (liderstvo, inovacije, diskriminacija). U ovom slucaju, teme skupa su dosta
### siroke, opste, a pojedinacne kljucne reci nose dosta sentimenta. Zbog toga je tesko neutralisati teme i
### posmatrati sentiment mimo njih, a sa druge strane, analizom preko recnika ove kljucne reci nuzno budu
### ocenjene i to uglavnom visokim ocenama (pozitivnim i negativnim). U slucaju ovakvih tema, verovatno bi
### se do mnogo boljih rezultata doslo primenom masinskog ucenja, odnosno treniranjem klasifikatora nad
### prethodno obelezenim setom tvitova.