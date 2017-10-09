### Analiza teksta - 3. i 4. pitanje

library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)

# Koliko su informacije nove za ucesnike?
# Koliko teme skupa ostaju predmet interesovanja ucesnika posle skupa?


### Na ova dva pitanja sam mislila da pokusam da odgovorim merenjem zastupljenosti tema skupa u tvitovima 
### ucesnika. Koliko su informacije nove za ucesnike merila bih preko zastupljenosti tema u tvitovima pre 
### skupa u poredenju sa zastupljenoscu tokom skupa. Koliko se ucesnici bave tim temama posle skupa
### merila zastupljenoscu u periodima posle skupa, u poredenju sa periodom skupa. Analize i podaci bi
### bili uglavnom isti, jedino bi se razlikovao period posmatranja.

### Moguci su razliciti problemi u vezi sa ovom analizom. 

### Sto se tice interpretacije mogucih rezultata, na prvom mestu je problem zakljucivanja o znanju na 
### osnovu upotrebe. U nekom opstem slucaju, upotreba kljucnih reci moze da ukaze na neko poznavanje
### teme, ali neupotreba ne ukazuje na nepoznavanje. U konkretnom slucaju, moguce je da ucesnici skupa
### u vremenskom periodu od mesec dana pre skupa (koliko se posmatra u ovoj analizi) jednostavno nisu 
### tvitovali o temama koje su im inace poznate, sto bi dovelo do pogresnog zakljucka da su im teme nove.
### Dalje, treba imati na umu da je vrlo moguce da ucesnici tvituju o predstojecem skupu unapred, na 
### primer retvitovanjem najava predavanja, sto bi navelo na zakljucak da su im teme od ranije poznate. 
### Osim toga, velika je verovatnoca da su ucesnici dobro upoznati sa sirom temom skupa, a neke nove 
### informacije bi mogle biti samo nijanse koje je tesko uociti i izmeriti.

### Drugi problemi su u vezi sa "tehnickim" detaljima analize. Predstavljanje tema skupa pojedinacnim 
### recima bi moglo da da pogresnu sliku, jer reci kao "women" ili "deep" i "learning" nisu specificne za 
### ovaj skup, ali njihova kombinacija je vec dosta dobar pokazatelj da se radi upravo o ovoj konferenciji. 
### Medutim, samo odredivanjem nekog praga broja uparenih reci koji bi ukazao na to da tvit govori o ovim 
### temama opet ne bi dalo dovoljno dobre rezultate: 
### - buduci da su tvitovi dosta kratki, taj prag ne moze da bude visok; 
### - kljucne reci skupa imaju veci broj opstih reci, kao "women", "scale", "mindset" ili "power", "tools",
### "skills", "create", koje bi dovele do toga da prag reci koje se poklapaju sa kljucnim recima bude 
### zadovoljen, a da tvit ipak ne govori o temama skupa; ovaj problem cu pokusati da umanjim 
### predstavljanjem tema bigramima;
### - teme skupa su reprezentovane samo recima koje su se nasle u naslovima predavanja, sto moze da dovede 
### do toga da tvit govori o temama skupa, ali to na osnovu kljucnih reci ne moze da se prepozna; ovaj 
### problem bi mozda mogao da se umanji prosirivanjem skupa kljucnih reci preko recnika sinonima ili uz 
### pomoc baza znanja, ali to cu da ostavim kao mogucnost za kasnija poboljsanja.

tweets <- readRDS("results/tweets.RData")
conf_bigrams <- readRDS("results/conf_bigrams_df.RData")


tweets_bigrams <- tweets %>% unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

conf_matched_bigrams <- inner_join(tweets_bigrams, conf_bigrams) %>% unite(bigram, word1, word2, sep = " ")

### Ako se uparuju bigrami, odnosno identicna konstrukcija, takvih poklapanja je relativno malo (666).

### Koriscenje razlicitih bigrama po periodima:
conf_matched_bigrams %>% distinct(timeframe, bigram) %>% count(timeframe)
# time_1 : time_5 =>  21 29 72 24 27
### U periodu skupa je korisceno najvise razlicitih bigrama.

### Broj razlicitih kombinacija ucesnik-bigram, po periodima:
conf_matched_bigrams %>% distinct(timeframe, screenName, bigram) %>% count(timeframe) 
# time_1 : time_5 =>  60  75 279  50  54

### Broj razlicitih ucesnika koji su koristili bigrame, po periodima:
conf_matched_bigrams %>% distinct(timeframe, screenName) %>% count(timeframe)
# time_1 : time_5 =>  53  55 139  44  39

### Ukupan broj razlicitih ucesnika:
length(unique(conf_matched_bigrams$screenName))
# 211

### Takode, i po broju ucesnika koji su koristili razlicite bigrame, period skupa se izdvaja od ostalih.
### Ipak, relativno mali broj ucesnika koji je koristio ovakve bigrame (211 od ukupno 457 posmatranih,
### manje od 50%) govori o tome da je kriterijum suvise strog.

### Sta se desava ako pogledamo ucesnike kroz vreme, da li su isti ucesnici, oni koji su koristili neki 
### bigram za vreme skupa, nastavili da ga koriste i posle skupa, ili se radi o razlicitim ucesnicima koji 
### "nasumicno" koriste razlicite bigrame?

### Broj koriscenja razlicitih bigrama prema korisniku i vremenu:
participant_bigram_by_timeframe <- conf_matched_bigrams %>% 
  group_by(screenName, timeframe, bigram) %>% count() %>% 
  group_by(screenName, bigram) %>% spread(timeframe, n)

### Ako bi se tabela posmatrala kao matrica bigram-vremenski period, moglo bi da se kaze da je retka.
### Izgleda da je najveci broj koriscenja bigrama bio u periodu skupa, pa cu da ispitam koliko bigrama
### je korisceno samo u ovom periodu i koliko ih je upotrebljeno samo jednom. Ovo je, osim za uvid u 
### koriscenje, vazno i zato sto se "ucenje" i "zaboravljanje" mere upravo prema periodu skupa, pa su 
### bigrami koji su upotrebljeni samo tada ujedno i "nauceni" i "zaboravljeni", odnosno novi su (prvi 
### put se javljaju u periodu skupa) i nisu ostali u upotrebi posle skupa.


time3_uses <- participant_bigram_by_timeframe %>% 
  filter_at(vars(time_1, time_2, time_4, time_5), all_vars(is.na(.)))
### U 253 slucaja je jedan ucesnik koristio odredeni bigram samo u periodu skupa; to je vise od pola
### ucesnik-bigram kombinacija (462), drugim recima, vise od pola upotreba razlicitih bigrama od strane 
### razlicitih ucesnika se odnosi na upotrebu bigrama samo u periodu skupa. Ovde vrednost kolone time_3 
### predstavlja broj koriscenja bigrama u tom periodu.

time3_uses %>% filter(time_3 == 1)
### U 228 slucajeva je jedan ucesnik samo jednom upotrebio odredeni bigram i to u periodu skupa; to je
### 90% slucajeva iz prethodnog primera; (ne znaci da nije upotrebljavao druge bigrame drugacijom dinamikom)

time3_uses %>% group_by(screenName) %>% count(sort = TRUE)
### Broj bigrama koje je odredeni ucesnik upotrebljavao samo u periodu skupa; ne znaci da neke druge bigrame
### nije koristio i u drugim periodima

### Koliko je razlicitih bigrama svaki korisnik upotrebio u svakom vremenskom periodu? (bez obzira
### na to koliko puta)

participant_bigram_occurence <- participant_bigram_by_timeframe %>% 
  mutate_all(funs(ifelse(is.na(.), 0, 1))) %>%  # mutate_all i summarise_all primenjuju funs na sve kolone
  group_by(screenName) %>% select(-bigram) %>%  #        po kojima se ne grupise
  summarise_all(sum)

### Koliko ucesnika je koristilo samo jedan bigram?
participant_bigram_occurence %>% gather(key = timeframe, value = bigrams_count, -screenName ) %>%
  group_by(screenName) %>% summarise(bigrams_count = sum(bigrams_count)) %>% arrange(desc(bigrams_count)) %>%
  filter(bigrams_count == 1) %>% nrow()

### Nesto vise od pola ucesnika koji su upotrebili ovakve fraze je upotrebilo samo jedan bigram ukupno, 
### bez obzira na to u koje vreme i koliko puta.

### Ovakav uslov se pokazao suvise restriktivnim, jer zahteva identicnu formulaciju fraze. Ovo se vidi
### iz toga sto je za manje od pola ucesnika skupa pronadeno poklapanje izmedu tvita i ovako definisane
### fraze, a vise od pola ucesnika koji jesu koristili ovakve fraze, koristili su samo jednu frazu.

### Pokusala sam na nekoliko nacina da dobijem veci broj poklapanja: prvo prosirenjem pretrage na 
### nesusedne reci u tvitu (opcija "skip_ngrams" za tokenizaciju n-grama, sa maksimalno k reci izmedu
### n reci koje cine n-gram), a zatim i pretrazivanjem samo po osnovama reci. Rezultati su bili nesto
### bolji, u smislu da je doslo do veceg broja poklapanja, ali mi se ucinilo da je upotreba i dalje
### dosta retka. Na kraju sam prosirila pretragu na ceo tvit, u kombinaciji sa pretragom samo po 
### osnovama reci.

### Pretrazivanjem celog tvita, odnosno pojedinacnih reci, bez obzira na poziciju reci u tvitu,
### moguce je upariti, na primer, bigram "microsoft hololens" u tvitu:
#  @IEEESA: Deanna Hearns demonstrates @Microsoft's vision of the future of #MixedReality with the @HoloLens 
#             at #WIELead #IEEEWIE @WIEILC h…
### Ovde su reci koje cine bigram/temu skupa suvise udaljene jedna od druge u tekstu tvita, pa bi opciji
### skip_ngrams morao da se prosledi veliko k (broj reci koji moze da se nade izmedu reci koje cine n-gram).
### Drugi primer je uparivanje "robots disasters" u tvitu:
# Thanks for sharing your thoughts on the potential for disaster #robotics, @robinrmurphy!
### gde se ove reci nalaze u obrnutom redosledu, tako da pomocu "skip_ngrams" opcije ne bi uopste bilo
### moguce upariti ovaj bigram. Osim toga, u ovom slucaju je neophodno i pretrazivanje po osnovama reci.

### Za pretrazivanje po pojedinacnim recima, prvo cu da podelim i tvitove i bigrame na reci.

### bigrami su podeljeni na dve reci, potrebno ih je staviti u jednu kolonu po kojoj ce se vrsiti spajanje
### sa recima tvitova; bigid ce biti id bigrama, a which ce sadrzati naziv originalne kolone reci (word1/word2)
gathered_conf_bigrams <- conf_bigrams %>% select(-track) %>% distinct() %>% #neki bigr. su isti za vise trekova
  mutate(bigid = 1:nrow(.)) %>%                                              #id bigrama 
  gather(which, word, word1, word2) %>%                                      #kombinuje word1 i word2 u 1 kolonu
  mutate(word = tm::stemDocument(word))


### izvrsava se oko 7 minuta! deljenje tvita na reci i svodenje na osnovni oblik
tweets_words <- tweets %>% unnest_tokens(word, text, token = "words") %>%
  mutate(word = tm::stemDocument(word))

### spajanje tvitova i tema skupa, sa inner_join ce rezultat biti samo uparene reci; izbacivanje 
### tvitova/bigrama u kojima je uparena samo jedna rec tog bigrama
matched_by_word <- inner_join(tweets_words, gathered_conf_bigrams) %>% 
  group_by(id, bigid) %>% filter(n() >= 2)          


### U matched_by_word su za svaki tvit ostale samo reci koje su se poklopile sa recima iz tema skupa.
### Proveravanje po grupama (id, bigid) da li su upareni celi bigrami nisam uspela da izvedem nimalo 
### elegantno (na primer, moguce je za cetiri pojavljivanja iste id-bigid kombinacije da sva cetiri budu
### ista rec, ili tri iste i jedna razlicita, sto cini jedan kompletan bigram, ili po dve razlicite - dva
### kompletna bigrama). Zbog toga sam oducila da spojim sve reci koje cine jedan tvit, s tim da ih spojim
### i u pravom i u obrnutom redosledu, da bi bilo moguce otkriti i bigrame koji imaju obrnut red reci.
### Tako dobijeni "tekst" sam podelila na bigrame i trazila poklapanje sa bigramima/temama.

### Za ovo je neophodno ponovo spojiti reci tema u bigrame
united_conf_bigrams <- gathered_conf_bigrams %>% group_by(bigid) %>% spread(which, word) %>% 
  unite(bigram, word1, word2, sep = " ") %>% ungroup() %>% select(-bigid)


matched_by_bigram <- matched_by_word %>% group_by(id, bigid) %>%
  summarise(text = str_c(word, rev(word), sep = " ", collapse = " "), 
            timeframe = first(timeframe), screenName = first(screenName)) %>%  # da se ne izgube kolone sumarizacijom
  unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE) %>%
  inner_join(united_conf_bigrams) %>% ungroup()


### broj razlicitih bigrama po periodu je mnogo veci:
matched_by_bigram %>% distinct(timeframe, bigram) %>% count(timeframe) %>% .$n
# 58  77 104  66  67 naspram  21 29 72 24 27 ili za skip_ngrams (+stem / -stem):37 46 87 40 46 / 29 36 79 29 32


### broj razlicitih kombinacija ucesnik-bigram:
matched_by_bigram %>% distinct(timeframe, screenName, bigram) %>% count(timeframe) %>% .$n
# 260 353 559 240 320  naspram  60 75 279 50 54; skip_ngrams:  96 150 336 128 166 / 73  91 317  63  68


### broj razlicitih ucesnika koji su koristili bigrame
matched_by_bigram %>% distinct(timeframe, screenName) %>% count(timeframe) %>% .$n
# 152 179 212 138 170 naspram 53  55 139  44  39; skip_ngrams: 74 102 165  93 125 / 61  62 151  56  50 

length(unique(matched_by_bigram$screenName)) # 369, od ukupno 457, tj. 80.7%

### Prepoznavanje tema je ovako mnogo bolje, za 80% ucesnika je pronadena upotreba tema skupa, sto vise
### odgovara pretpostavci da su ucesnici bar jednom upotrebili neku od kljucnih fraza koje se nalaze
### i u naslovima predavanja, iako bi detaljniji sadrzaj predavanja sigurno dao mnogo vise poklapanja
### i preciznije rezultate.


### Pregled upotrebe po periodima za kombinacije ucesnik-bigram, odnosno koliko puta je odredeni
### ucesnik upotrebio odredeni bigram u svakom periodu:
better_participant_bigram_by_timeframe <- matched_by_bigram %>% 
  group_by(screenName, timeframe, bigram) %>% count() %>% 
  group_by(screenName, bigram) %>% spread(timeframe, n)


### Ponovo cu da ispitam koliko je bigrama korisceno samo u trecem periodu. Da podsetim, ovo je zanimljivo
### zato sto se prema tom periodu meri "ucenje" i "zaboravljanje", pa su bigrami koji su korisceni samo 
### tada ujedno i "nauceni" i "zaboravljeni" posle skupa.

better_time3_uses <- better_participant_bigram_by_timeframe  %>% 
  filter_at(vars(time_1, time_2, time_4, time_5), all_vars(is.na(.))) 
### 448 od 1439, odnosno 31% ucesnik-bigram kombinacija je samo u vreme skupa

### Upotreba samo jednog bigrama, samo u toku skupa:
better_time3_uses %>% filter(time_3 == 1)
### 394, odnosno u 88% slucajeva upotrebe bigrama samo u vreme skupa, radi se o upotrebi samo jednog bigrama


### Koliko je razlicitih bigrama svaki korisnik upotrebio u svakom vremenskom periodu (bez obzira na to
### koliko puta)?

better_part_bigr_occurence <- better_participant_bigram_by_timeframe %>% 
  mutate_all(funs(ifelse(is.na(.), 0, 1))) %>%  
  group_by(screenName) %>% select(-bigram) %>%  
  summarise_all(sum)

### Koliko ucesnika je koristilo samo jedan bigram?
better_part_bigr_occurence %>% gather(key = timeframe, value = bigrams_count, -screenName ) %>%
  group_by(screenName) %>% summarise(bigrams_count = sum(bigrams_count)) %>% 
  arrange(desc(bigrams_count)) %>%
  filter(bigrams_count == 1) %>% nrow()   #90

### 90 ucesnika od 369 koliko ih je koristilo ovakve bigrame je upotrebilo samo jedan bigram ukupno, 
### sto moze da govori o njihovoj slaboj ukljucenosti u komunikaciju o skupu, a posebno u vezi sa ovim
### pitanjem, govori o malom dijapazonu tema za dobar deo ucesnika.


### Ako se posmatraju teme skupa zajedno, odnosno koliko su sve teme skupa zastupljene u tvitovima nekog
### ucesnika, onda moze da se posmatra broj razlicitih bigrama (broj tema) ili suma pojavljivanja bigrama 
### (zastupljenost tema skupa u tvitovima). Ako bi se tragalo za nekom frazom koju je neki ucesnik 
### "naucio", odnosno prvi put upotrebio u vreme skupa ili koju je koristio i nakon skupa, onda bi se
### posmatrale sve kombinacije ucesnik-bigram posebno.

### Vec izracunata tabela pojavljivanja n-grama daje podatke o broju razlicitih n-grama, odnosno tema
### skupa koje je neki ucesnik koristio. 

better_part_bigr_occurence %>% arrange(desc(time_3))

### Radi lakseg racunanja, kombinovacu kolone time_1 i time_2 u jednu, uzimajuci celi deo srednje vrednosti;
### isto i za kolone time_4 i time_5. Rezutate "ucenja" cu da dobijem razlikom broja n-grama upotrebljenih 
### u periodu trajanja skupa i pre skupa. Pozitivne vrednosti ce predstavljati broj "naucenih" n-grama,
### negativne broj "zaboravljenih", dok ce 0 predstavljati nepromenjenu upotrebu, bilo da se radi o jednakom
### broju koriscenih n-grama, bilo da se radi o nuli u oba perioda. Kod "zadrzanog znanja", zeljeni 
### rezultat je jednako koriscenje u periodu skupa kao i posle njega, pa je tu vazno razlikovati jednaku
### upotrebu i jednaku neupotrebu, tj. 0-0 i 1-1. Zbog toga cu "zadrzano znanje" da merim kolicnikom,
### gde ce jednaka neupotreba (0/0) da da NaN, jednaka upotreba 1, "naknadno ucenje" interval (1, Inf)
### npr (posle:1/tokom:0) ili (posle:3/tokom:1),  a "zaboravljanje" interval (0,1), npr. (posle:0/tokom:1)
### ili (posle:3/tokom:6), pri cemu je "zaboravljanje" manje sto je kolicnik blize 1.


learning_different_ngrams <- better_part_bigr_occurence %>% group_by(screenName) %>% 
  transmute(before = trunc(mean(c(time_1, time_2))), time_conf = time_3, 
            after = trunc(mean(c(time_4, time_5)))) %>%
  transmute(learned = time_conf - before, retained = after / time_conf) %>% ungroup()


### Koliko puta je neki ucesni koristio odredeni bigram?

better_participant_bigram_by_timeframe %>% arrange(desc(time_3))

better_participant_bigram_by_timeframe <- better_participant_bigram_by_timeframe %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))

learning_specific_ngrams <- better_participant_bigram_by_timeframe %>% 
  group_by(screenName, bigram) %>% 
  transmute(before = trunc(mean(c(time_1, time_2))), time_conf = time_3, 
            after = trunc(mean(c(time_4, time_5)))) %>%
  transmute(learned = time_conf - before, retained = after / time_conf) %>% ungroup()

### U ovom slucaju, svaki red se odnosi na upotrebu odredenog bigrama; pozitivne vrednosti u koloni 
### "learned" znace da je bigram vise koriscen u toku skupa nego pre (i ovde bi mozda trebalo ubaciti 
### distinkciju izmedu 6-3 i 3-0, da se razlikuje "naucen" od "vise koriscen" ??), negativne da je 
### koriscen manje, nula da je jednako koriscen/nekoriscen. Za kolonu "retained" vazi isto kao i u 
### prethodnoj tabeli.


### Koliko su informacije nove za ucesnike?


### Prema broju bigrama (po ucesniku) koriscenih samo u periodu skupa (448 od ukupno 1439 razlicitih
### korisnik-bigram kombinacija), moglo bi se zakljuciti da je barem 31% koriscenja razlicitih bigrama 
### rezultat "ucenja" (a isti je procenat i "zaboravljanja"). Oko 88% od toga je koriscenje n-grama 
### samo jednom.

better_time3_uses %>% nrow(.)
better_time3_uses %>% filter(time_3 == 1) %>% nrow(.)


### Raspodela pokazatelja "naucenog"
table(learning_different_ngrams$learned)

pdf("visuals/text3_bigrams_learned_index.pdf")
ggplot(learning_different_ngrams, aes(learned)) +
  geom_histogram(binwidth = 1, colour =  "black", fill = "red", alpha = 0.3)+
  labs(title = "\"learned\" index distribution:\nnumber of new bigrams used during conference", 
       x ="\"learned\" index", y = "participant count") +
  scale_x_continuous(breaks = c(-5:10, 20, 30)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) 
dev.off()

### Ako se pogleda raspodela pokazatelja "naucenih" bigrama po ucesnicima, 36% (135) ucesnika nije 
### "naucilo" nijedan bigram, a 38% (143) je u vreme skupa upotrebilo jedan bigram vise ili manje u 
### odnosu na period pre skupa. Moze se reci da po ovom pitanju nema neke znacajne promene u koriscenju 
### bigrama za vreme skupa u odnosu na period pre skupa (mereno preko kljucnih reci iz naslova predavanja).
### Ako bi se uzeli u obzir svi ucesnici, ukljucujuci i one za koje nije otkrivena upotreba tema skupa,
### procenat ucesnika koji su upotrebili jednak broj tema (ili nijednu) pre i tokom skupa je ~49%, dok
### je procenat onih koji su upotrebili jednu temu manje ili vise ~31%.


### Broj pojavljivanja pojedinacnih bigrama: 

table(learning_specific_ngrams$learned)

pdf("visuals/text3_bigram_frequency.pdf")
ggplot(learning_specific_ngrams, aes(learned)) +
  geom_histogram(binwidth = 1, colour =  "black", fill = "red", alpha = 0.3)+
  labs(title = "\"learned\" index distribution:\nuse frequency of a specific bigram", 
       x = "lesser/greater use frequency", y = "bigram use by participants") +
  scale_x_continuous(breaks = c(-5:8)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) 
dev.off()


### Ako se pogledaju rezultati za "ucenje" pojedinacnih n-grama, prosecan bigram nije upotrebljavan vise 
### u toku skupa u odnosu na period pre skupa, a znacajan je i broj bigrama koji su upotrebljeni jednom
### vise u toku skupa.



### Koliko teme skupa ostaju predmet interesovanja ucesnika posle skupa?

summary(learning_different_ngrams$retained)
table(round(learning_different_ngrams$retained, 1))

### Za vizuelizaciju cu da prilagodim Inf i NaN vrednosti: posto su sve druge vrednosti u intervalu [0, 3.5],
### Inf cu da definisem kao maksimum+1, NaN kao minimum-1

myInf <- learning_different_ngrams %>% filter(!is.na(retained) & retained != Inf) %>% .$retained %>% max()+1 
myNaN <- learning_different_ngrams %>% filter(!is.na(retained) & retained != Inf) %>% .$retained %>% min()-1

pdf("visuals/text3_retained_use.pdf")
learning_different_ngrams %>% mutate(retained = ifelse(is.nan(retained), myNaN, 
                                                       ifelse(retained == Inf, myInf, retained))) %>%
  ggplot(aes(retained)) +
  geom_histogram(binwidth = 0.25, colour =  "black", fill = "red", alpha = 0.3)+
  labs(title = "\"retained\" index distribution:\nnumber of bigrams used after conference", 
       x = "\"retained\" index", y = "participant count") +
  scale_x_continuous(breaks = c(-1, 0, 0.5, 1, 2, 3, 4.5),
                     labels = c("never used any\nduring or after", "forgot all\nbigrams",
                                "forgot some\nbigrams", "retained all\nbigrams",
                                "used more\nbigrams after",
                                "used even more\nbigrams after", "only used\nbigrams after" )) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45))
dev.off()


### Medu ovim vrednostima ima 119 NA, - "jednake neupotrebe" (0/0), odnosno toliko ucesnika nije upotrebilo
### nijedan bigram ni tokom ni posle skupa (ovo bi teoretski trebalo da se poklopi sa brojem ucesnika koji su  
### upotrebili bigram pre skupa, a tokom i posle skupa nisu, odnosno sa onima koji na grafikonu za "learned"
### index imaju negativan skor; medutim, posto sam uzimala celobrojni deo proseka upotrebe u periodima 1 i 2,
### ucesnici koji su samo u jednom periodu upotrebili jedan bigram, imaju i za tu vrednost nulu, isto kao i 
### za period posle skupa); 140 ucesnika je "zaboravilo" sve bigrame, odnosno ne koristi ih posle skupa,
### dok 26 njih pokazuje razlicite stepene smanjene upotrebe; 28 njih koristi isti broj bigrama i posle i
### tokom skupa; 38 ucesnika ima vrednost Inf, jos 18 ima skor > 1, odnosno oni koriste vise bigrama u 
### periodu posle skupa.
### Ako bi se ovim ucesnicima dodali i oni za koje nije otkrivena upotreba tema skupa (oni bi pripadali grupi
### "jednake neupotrebe"), moglo bi se reci da ~76% svih ucesnika nije upotrebilo nijedu temu posle skupa (bez
### obzira na to da li su ih koristili u toku skupa ili ne).



### Broj pojavljivanja bigrama:

summary(learning_specific_ngrams$retained)
table(round(learning_specific_ngrams$retained, 1))


### Ponovo cu Inf da prilagodim za vizuelizaciju, NaN ostaje isto
myInf2 <- learning_specific_ngrams %>% filter(!is.na(retained) & retained != Inf) %>% .$retained %>% max()+1 

pdf("visuals/text3_retained_frequency.pdf")
learning_specific_ngrams %>% mutate(retained = ifelse(is.nan(retained), myNaN, 
                                                       ifelse(retained == Inf, myInf2, retained))) %>%
  ggplot(aes(retained)) +
  geom_histogram(binwidth = 0.25, colour =  "black", fill = "red", alpha = 0.3) +
  labs(title = "\"retained\" index distribution:\nuse frequency of a specific bigram", 
       x = "\"retained\" index", y = "bigram use by participants") + 
  scale_x_continuous(breaks = c(-1, 0, 0.5, 1, 2, 5, 7),
                     labels = c("never used\nduring or after", "not used\nafter",
                                "some use\nafter", "used equally\nafter",
                                "used more\nafter",
                                "used even\nmore after", "only used\nafter" )) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45))
dev.off()

### Sto se tice upotrebe pojedinacnih bigrama, u 822 slucaja, ucesnici nisu upotrebili bigram ni tokom 
### ni posle skupa; u 527 slucajeva su ucesnici "zaboravili" bigrame koje su koristili u vreme skupa,
### a u 10 slucajeva su ih manje koristili; u 13 slucajeva su korisnici u jednakoj meri koristili neki
### bigram tokom i posle skupa; u 67 slucajeva, ucesnici su posle skupa poceli da koriste odredene bigrame.

### Moglo bi se reci da preko 90% bigrama, odnosno tema skupa, nije upotrebljeno od strane pojedinacnih
### ucesnika posle skupa (bilo da je upotrebljeno u toku skupa ili ne).


### Na kraju, do nekakvih rezultata sam dosla, i oni ne daju narocito pozitivnu sliku o koriscenju novih 
### bigrama ili o upotrebi istih i posle skupa. Trecina ucesnika je u toku skupa upotrebila isti broj
### tema kao i pre skupa, a druga trecina je upotrebila jednu temu vise ili manje. Posle skupa, 70% 
### ucesnika nije upotrebilo nijednu temu. 
### Smatram da vece prepoznavanje tema u tvitovima nije dalje moguce sa ovako odredenim temama, 
### izdvojenim iz naslova predavanja. Za precizniju i kvalitetniju analizu bilo bi neophodno na neki 
### drugi nacin utvrditi teme skupa.


saveRDS(learning_different_ngrams, "results/learning_different_bigrams.RData")
