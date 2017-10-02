### Analiza teksta - 3. i 4. pitanje

library(dplyr)
library(tidyr)
library(tidytext)

# Koliko su informacije nove za ucesnike?
# Koliko teme skupa ostaju predmet interesovanja ucesnika posle skupa?


### Na ova dva pitanja sam mislila da pokusam da odgovorim merenjem zastupljenosti tema skupa u tvitovima 
### ucesnika. Koliko su informacije nove za ucesnike merila bih preko zastupljenosti tema u tvitovima pre 
### skupa u poredenju sa zastupljenoscu tokom skupa. Koliko se ucesnici bave tim temama posle skupa
### merila zastupljenoscu u periodima posle skupa, u poredenju sa periodom skupa. Analize i podaci bi
### bili uglavnom isti, jedino bi se razlikovao period posmatranja.

### Moguci su razliciti problemi u vezi sa ovom analizom. 

### Sto se tice interpretacije mogucih rezultata, na prvom mestu je problem zakljucivanje o znanju na 
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
### predstavaljanjem tema bigramima;
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
### Takode, visestruka upotreba istog bigrama od strane istog ucesnika ne treba da se uzme u obzir,
### jer me interesuje samo da li ucesnik u nekom trenutku "zna" (koristi) odredeni bigram, a ne njegova 
### frekvencija.

conf_matched_bigrams %>% distinct(timeframe, bigram) %>% count(timeframe)
# time_1 : time_5 =>  21 29 72 24 27
### U periodu skupa je korisceno najvise razlicitih bigrama.

conf_matched_bigrams %>% distinct(timeframe, screenName, bigram) %>% count(timeframe) 
# time_1 : time_5 =>  60  75 279  50  54
# broj razlicitih kombinacija ucesnik-bigram

conf_matched_bigrams %>% distinct(timeframe, screenName) %>% count(timeframe)
# time_1 : time_5 =>  53  55 139  44  39
# broj razlicitih ucesnika koji su koristili bigrame

### Takode, i po broju ucesnika koji su koristili razlicite bigrame, period skupa se izdvaja od ostalih.
### Ipak, mali broj ucesnika koji je koristio ovakve bigrame (139 od ukupno 457 posmatranih - 30%) 
### govori o tome da je kriterijum suvise strog.

### Sta se desava ako pogledamo ucesnike kroz vreme, da li su isti ucesnici, oni koji su koristili neki 
### bigram za vreme skupa, nastavili da ga koriste i posle skupa, ili se radi o razlicitim ucesnicima koji 
### "nasumicno" koriste razlicite bigrame?

participant_bigram_by_timeframe <- conf_matched_bigrams %>% 
  group_by(screenName, timeframe, bigram) %>% count() %>% 
  group_by(screenName, bigram) %>% spread(timeframe, n)
### Broj koriscenja razlicitih bigrama prema korisniku i vremenu; tabela izgleda prilicno "prazno"


only_time3_uses <- participant_bigram_by_timeframe %>% 
  filter_at(vars(time_1, time_2, time_4, time_5), all_vars(is.na(.)))
### U 253 slucaja je jedan ucesnik koristio odredeni bigram samo u periodu skupa; to je vise od pola
### ucesnik-bigram kombinacija, odnosno vise od pola upotreba razlicitih bigrama od strane razlicitih
### ucesnika se odnosi na upotrebu bigrama samo u periodu skupa. Ovde vrednost kolone time_3 predstavlja
### broj koriscenja bigrama u tom periodu.

only_time3_uses %>% filter(time_3 == 1)
### U 228 slucajeva je jedan ucesnik samo jednom upotrebio odredeni bigram i to u periodu skupa; to je
### 90% slucajeva iz prethodnog primera; (ne znaci da nije upotrebljavao druge bigrame drugacijom dinamikom)

only_time3_uses %>% group_by(screenName) %>% count(sort = TRUE)
### Broj bigrama koje je jedan ucesnik upotrebljavao samo u periodu skupa; ne znaci da neke druge bigrame
### nije koristio i u drugim periodima

### Koliko je razlicitih bigrama svaki korisnik upotrebio u svakom vremenskom periodimu? (bez obzira
### na to koliko puta)

participant_bigram_occurence <- participant_bigram_by_timeframe %>% 
  mutate_all(funs(ifelse(is.na(.), 0, 1))) %>%  # mutate_all i summarise_all primenjuju fun. na sve kolone
  group_by(screenName) %>% select(-bigram) %>%  #        po kojima se ne grupise
  summarise_all(sum)

### Koliko ucesnika je koristilo samo jedan bigram?
participant_bigram_occurence %>% gather(key = timeframe, value = bigrams_count, -screenName ) %>%
  group_by(screenName) %>% summarise(bigrams_count = sum(bigrams_count)) %>% arrange(desc(bigrams_count)) %>%
  filter(bigrams_count == 1) %>% nrow()

### Nesto vise od pola ucesnika koji su upotrebili ovakve fraze je upotrebilo samo jedan bigram ukupno, 
### bez obzira na to u koje vreme i koliko puta.

### Ovakav uslov se pokazao suvise restriktivnim, jer zahteva identicnu formulaciju fraze. Nesto
### slobodnije poklapanje bi moglo da se dobije ako se ne zahteva strogi sled dve reci u tvitu. Ovo
### moze da se postigne pomocu opcije "skip_ngrams" za tokenizaciju ngrama sa maksimalno k reci
### izmedu n reci koje cine n-gram. Na ovaj nacin se dobije mnogo vise kombinacija reci, tj. n-grama.

# izvrsava se oko 2.5min, ali zauzima dosta memorije: object.size(tweets_skipgrams) => 563685608 bytes
# ne koristi se dalje za analizu!
tweets_skipgrams <- tweets %>% unnest_tokens(skipgram, text, token = "skip_ngrams", n = 2, k = 3) %>%
  separate(skipgram, c("word1", "word2"), sep = " ")


conf_matched_skipgrams <- inner_join(tweets_skipgrams, conf_bigrams) %>% 
  unite(skipgram, word1, word2, sep = " ")


conf_matched_skipgrams %>% distinct(timeframe, skipgram) %>% count(timeframe) 
# time_1 : time_5 => 29 36 79 29 32  naspram  21 29 72 24 27 iz prethodne analize
# broj razlicitih bigrama po periodu

conf_matched_skipgrams %>% distinct(timeframe, screenName, skipgram) %>% count(timeframe) 
# time_1 : time_5 => 73  91 317  63  68  naspram  60  75 279  50  54
# broj razlicitih kombinacija ucesnik-bigram
 
conf_matched_skipgrams %>% distinct(timeframe, screenName) %>% count(timeframe) 
# time_1 : time_5 => 61  62 151  56  50  naspram  53  55 139  44  39
# broj razlicitih ucesnika koji su koristili bigrame


### Ovakav vid prosirenja nije doveo do znacajne promene u rezultatima. Neke dalje mogucnosti su 
### prosirenje pretrage na ceo tvit ili pretrazivanje samo po osnovama reci. Probacu ovu drugu opciju.

### "skip_ngrams" podrazumevaju veci broj kombinacija istih reci, pa je bolje prvo stemovati tvitove,
### pa tek onda naci ovakve n-grame.
tweets_stemmed <- tweets %>% mutate(text = tm::stemDocument(text))

# izvrsava se oko 1.5 minuta! object.size(stemmed_skipgrams) => 563033008 bytes
stemmed_skipgrams <- tweets_stemmed %>% 
  unnest_tokens(skipgram, text, token = "skip_ngrams", n = 2, k = 3) %>%
  separate(skipgram, c("word1", "word2"), sep = " ")


matched_stemmed_skipgrams <- inner_join(stemmed_skipgrams, 
                                    conf_bigrams %>% 
                                      mutate_at(vars(word1, word2), funs(tm::stemDocument(.)))) %>% 
  unite(skipgram, word1, word2, sep = " ")
### Ukupan broj uparenih bigrama je skoro za trecinu veci nego u prvoj varijanti.

matched_stemmed_skipgrams %>% distinct(timeframe, skipgram) %>% count(timeframe) 
# time_1 : time_5 => 32 43 76 38 43, naspram 29 36 79 29 32 / 21 29 72 24 27 iz prethodnih analiza
### broj razlicitih bigrama po periodu; malo je cudno da nisu prepoznati svi bigrami iz varijante bez
### stemovanja u trecem periodu, verovatno se radi o tome da svodenjem na osnovu ima manje razlicitih 
### reci i bigrama 
#****** ne radi se o tome, npr Abir_Chermiti, fali "business development", potpuno razlicit bigram od 
#****** ostalih koje je koristio ovaj ucesnik; ostaje da se ispita
# setdiff(conf_matched_bigrams, matched_stemmed_skipgrams %>% rename(bigram = skipgram)) %>% nrow(.)
# => 486!

matched_stemmed_skipgrams %>% distinct(timeframe, screenName, skipgram) %>% count(timeframe)
# time_1 : time_5 => 79 140 281 116 147, naspram 73  91 317  63  68 / 60  75 279  50  54
### broj razlicitih kombinacija ucesnik-bigram; isti slucaj u trecem periodu

matched_stemmed_skipgrams %>% distinct(timeframe, screenName) %>% count(timeframe) %>% .$n
# time_1 : time_5 =>  65  98 155  88 118, naspram 61  62 151  56  50  /  53  55 139  44  39
### broj razlicitih ucesnika koji su koristili bigrame je definitivno nesto veci nego u drugim varijantama


participant_stemmed_skipgram_by_timeframe <- matched_stemmed_skipgrams %>% 
  group_by(screenName, timeframe, skipgram) %>% count() %>% 
  group_by(screenName, skipgram) %>% spread(timeframe, n)


stemmed_only_time3_uses <- participant_stemmed_skipgram_by_timeframe %>% 
  filter_at(vars(time_1, time_2, time_4, time_5), all_vars(is.na(.)))
### U vezi sa koriscenjem odredenog bigrama samo u trecem periodu, situacija je nesto bolja nego u 
### prethodnoj varijanti: 249 od 677 slucajeva, za razliku od prethodnih 253 od 462 slucaja. Vrednost 
### kolone time_3 predstavlja broj koriscenja bigrama u tom periodu.

stemmed_only_time3_uses %>% filter(time_3 == 1)
### U 222 slucaja je jedan ucesnik samo jednom upotrebio odredeni bigram i to u periodu skupa, za razliku
### od 228 u prethodnoj varijanti; ovo je i dalje 89.1% slucajeva koriscenja jednog bigrama samo u trecem
### periodu (ucesnik je, moguce, koristio i druge bigrame)


stemmed_only_time3_uses %>% group_by(screenName) %>% count(sort = TRUE)
### Broj bigrama koje je jedan ucesnik upotrebljavao samo u periodu skupa; ne znaci da neke druge bigrame
### nije koristio i u drugim periodima

### Koliko je razlicitih bigrama svaki korisnik upotrebio u svakom vremenskom periodimu? (bez obzira
### na to koliko puta)

participant_stemmed_skipgram_occurence <- participant_stemmed_skipgram_by_timeframe %>% 
  mutate_all(funs(ifelse(is.na(.), 0, 1))) %>%  
  group_by(screenName) %>% select(-skipgram) %>%  
  summarise_all(sum)

### Koliko ucesnika je koristilo samo jedan bigram?
participant_stemmed_skipgram_occurence %>% gather(key = timeframe, value = skipgrams_count, -screenName ) %>%
  group_by(screenName) %>% summarise(skipgrams_count = sum(skipgrams_count)) %>% 
  arrange(desc(skipgrams_count)) %>%
  filter(skipgrams_count == 1) %>% nrow()

### 142 ucesnika od 297 koliko ih je koristilo ovakve bigrame je upotrebilo samo jedan bigram ukupno, 
### bez obzira na to u koje vreme i koliko puta.

### Za sada cu da se zadrzim na ovoj varijanti, mada sigurno ima mesta za poboljsanje rezultata.

### Ako se posmatraju teme skupa zajedno, odnosno koliko su sve teme skupa zastupljene u tvitovima nekog
### ucesnika, onda moze da se posmatra samo broj razlicitih ili ukupnih bigrama (broj razlicitih tema / 
### ukupna zastupljenost tema skupa u tvitovima). Ako bi se tragalo za nekom frazom koju je neki ucesnik
### "naucio", odnosno prvi put upotrebio u vreme skupa ili koju je koristio i nakon skupa, onda bi se
### posmatrale sve kombinacije ucesnik-bigram posebno.

### Vec izracunata tabela pojavljivanja n-grama daje podatke o broju razlicitih n-grama, odnosno tema
### skupa koje je neki ucesnik koristio. 

participant_stemmed_skipgram_occurence %>% arrange(desc(time_3))

### Radi lakseg racunanja, a kada su vrednosti vec tako retke, kombinovacu kolone time_1 i time_2 u jednu,
### uzimajuci vecu vrednost; isto i za kolone time_4 i time_5. Rezutate "ucenja" cu da dobijem razlikom
### broja n-grama upotrebljenih u periodu trajanja skupa i pre skupa. Pozitivne vrednosti ce predstavljati
### broj "naucenih" n-grama, negativne broj "zaboravljenih", dok ce 0 predstavljati nepromenjenu upotrebu,
### bilo da se radi o jednakom broju koriscenih n-grama, bilo da se radi o nuli u oba perioda. Kod 
### "zadrzanog znanja", zeljeni rezultat je jednako koriscenje u periodu skupa kao i posle njega, pa je 
### tu vazno razlikovati jednaku upotrebu i jednaku neupotrebu, tj. 0-0 i 1-1. Zbog toga cu "zadrzano
### znanje" da merim kolicnikom, gde ce jednaka upotreba (1/1) da da 1, jednaka neupotreba (0/0) NaN,
### "naknadno ucenje" (posle:1/tokom:0) ili (posle:3/tokom:1) - (1, Inf),  a "zaboravljanje" 
### (posle:0/tokom:1) - 0, ili (posle:3/tokom:6) - (0,1), odnosno, "zaboravljanje" je manje sto je 
### kolicnik blize 1.


learning_different_ngrams <- participant_stemmed_skipgram_occurence %>% group_by(screenName) %>% 
  transmute(before = max(time_1, time_2), time_conf = time_3, after = max(time_4, time_5)) %>%
  transmute(learned = time_conf - before, retained = after / time_conf) %>% ungroup()


### Koliko puta je neki ucesni koristio odredeni bigram?

participant_stemmed_skipgram_by_timeframe %>% arrange(desc(time_3))

participant_stemmed_skipgram_by_timeframe <- participant_stemmed_skipgram_by_timeframe %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))

learning_specific_ngrams <- participant_stemmed_skipgram_by_timeframe %>% 
  group_by(screenName, skipgram) %>% 
  transmute(before = max(time_1, time_2), time_conf = time_3, after = max(time_4, time_5)) %>%
  transmute(learned = time_conf - before, retained = after / time_conf) %>% ungroup()

### U ovom slucaju, svaki red se odnosi na upotrebu odredenog bigrama; pozitivne vrednosti u koloni 
### "learned" znace da je bigram vise koriscen u toku skupa nego pre (i ovde bi mozda trebalo ubaciti 
### distinkciju izmedu 6-3 i 3-0, da se razlikuje "naucen" od "vise koriscen" ??), negativne da je 
### koriscen manje, nula da je jednako koriscen/nekoriscen. Za kolonu "retained" vazi isto kao i u 
### prethodnoj tabeli.


### Koliko su informacije nove za ucesnike?


### Prema broju bigrama (po ucesniku) koriscenih samo u periodu skupa (249 od ukupno 677 razlicitih
### korisnik-bigram kombinacija), moglo bi se zakljuciti da je barem 37% ukupnih koriscenja bigrama 
### rezultat "ucenja" (a isti je procenat i "zaboravljanja"). Oko 90% od toga je koriscenje n-grama 
### samo jednom.

stemmed_only_time3_uses %>% nrow(.)
stemmed_only_time3_uses %>% filter(time_3 == 1) %>% nrow(.)


### Medutim, ako se posmatra broj naucenih bigrama, srednja vrednost je 0, aritmeticka sredina je 0.34,
### odnosno prosecno nije "naucen" nijedan bigram. U 25% slucajeva "nauceno" je izmedu 1 i 23 bigrama.

summary(learning_different_ngrams$learned)


### Ako se pogledaju rezultati za "ucenje" pojedinacnih n-grama, srednja vrednost je 0, aritmeticka 
### sredina je 0.1, odnosno prosecni bigram nije vise upotrebljavan u toku skupa u odnosu na period pre
### skupa. U 25% slucajeva je broj pojavljivanja bigrama izmedu 1 i 8. Takode, u 25% slucajeva je broj
### koriscenja bigrama manji tokom skupa za 1-6 pojavljivanja.

summary(learning_specific_ngrams$learned)



### Koliko teme skupa ostaju predmet interesovanja ucesnika posle skupa?

summary(learning_different_ngrams$retained)
table(learning_different_ngrams$retained)

### Medu ovim vrednostima ima 42 NA, - "jednake neupotrebe" (0/0), odnosno toliko ucesnika nije upotrebilo
### nijedan bigram ni tokom ni posle skupa (oni bi morali da se poklope sa ucesnicima koji u koloni 
### "learned" imaju negativan skor); 100 ucesnika ima vrednost Inf, jos 13 ima skor > 1, odnosno oni 
### koriste bigrame u periodu posle skupa; 91 ucesnik je "zaboravio" sve bigrame, odnosno ne koristi ih 
### posle skupa, dok 13 njih pokazuje razlicite stepene smanjene upotrebe; 38 njih koristi isti broj 
### bigrama i posle i tokom skupa.

summary(learning_specific_ngrams$retained)
table(learning_specific_ngrams$retained)

### Sto se tice upotrebe pojedinacnih bigrama, 169 bigrama koje su ucesnici upotrebili pre skupa, nisu 
### upotrebljeni ni tokom ni posle skupa; u 264 slucaja su ucesnici "zaboravili" bigrame koje su koristili
### u vreme skupa, a u 3 slucaja su ih manje koristili; u 11 slucajeva su korisnici u jednakoj meri 
### koristili neki bigram tokom i posle skupa; u 230 slucajeva, ucesnici su posle skupa poceli da koriste
### odredene bigrame.


### Na kraju, do nekakvih rezultata sam dosla, medutim, ostaje jos nekoliko stvari da se proveri pre 
### zakljucaka, na prvom mestu - koji je razlog manjeg poklapanja sa kljucnim recima kada se radi sa 
### osnovama reci. Ovakvi rezultati kakvi su sada ne daju narocito pozitivnu sliku o koriscenju novih 
### bigrama ili o upotrebi istih i posle skupa, ali mi se cini da bi stvarno stanje moglo biti dosta 
### drugacije.