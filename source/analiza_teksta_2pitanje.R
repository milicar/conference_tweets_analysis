library(dplyr)
library(ggplot2)

### 2. Koliko je skup interesantan za ucesnike?

### Za razliku od analize sentimenta, ovde cu interesantnost meriti putem broja tvitova koji se odnose na konferenciju.
### Zanima me koliko su ucesnici u toku trajanja skupa tvitovali o samom skupu, da li je konferencija bila dovoljno
### zanimljiva da okupira paznju ucesnika, ili su im neke druge teme ili svakodnevni problemi bili inspirativniji.
### Ovde pod konferencijom ne podrazumevam samo teme predavanja, vec i dodatne sadrzaje, druzenje sa drugim ucesnicima 
### ili samu organizaciju skupa, na primer:
# @EricssonCareers: Say cheese! Commemorate your @wieilc experience at the #Ericsson booth! �� #WIELead 
# Join us in yoga activity and feel better about yourself. ���� #WIELead 

### Iz ovog razloga, pod tvitovima o konferenciji bih podrazumevala sve tvitove koji su oznaceni hestegom skupa ili 
### koji pominju organizatora skupa. Medutim, zbog vec navedenih problema sa nepotpunim tekstom tvitova, ovaj uslov 
### je u pripremi tvitova za analizu prosiren ("source/priprema_za_analizu_teksta.R"), a kolona conf tabele tweets 
### predstavlja klasifikaciju tvitova prema tome da li se odnose na konferenciju ili ne.

### Sto se vremenskog obuhvata tice, cini mi se da ovde moraju da se izdvoje dani trajanja skupa, kada je razumno
### "zahtevati" posvecenost ucesnika konferenciji koja je u toku, od dana neposredno pre i posle skupa, kada su
### ucesnici zauzeti drugim aktivnostima, iako, moguce, relativno intenzivno tvituju i o konferenciji. Da podsetim,
### u podeli na vremenske periode koju koristim u ostalim analizama, period koji obuhvata skup je produzeni period
### trajanja skupa i obuhvata i tri dana pre i posle skupa.

### U pripremi tvitova sam iz teksta tvitova izbacila linkove, oznake retvitova i neke specijalne znakove (kao &amp;).
### Takode sam zadrzala samo kolone "timeframe", "screenName", "id" i "text". Ovo je sadrzaj tabele tweets.

tweets <- readRDS("results/tweets.RData")

### Da bih preciznije izdvojila tvitove koji su objavljeni u vreme (dane) trajanja konferencije, dodacu kolonu
### "date" iz tabele selected_tweets, koju cine svi tvitovi koji su predmet analize.

tweets <- tweets %>% left_join(readRDS("results/selected_tweets.RData") %>% select(id, date))

tweets_ratios <- tweets %>%
  mutate(timeframe = ifelse((date == "2017-05-22" | date == "2017-05-23"), "conf", timeframe)) %>%
  group_by(timeframe) %>% mutate(total_tweets = n()) %>% ungroup() %>%
  filter(conf == TRUE) %>%
  group_by(timeframe) %>% mutate(conf_tweets = n()) %>%
  summarise(conf_tweets = first(conf_tweets), total_tweets = first(total_tweets), 
            tweets_ratio = conf_tweets / total_tweets)


### U proseku, ucesnici su u toku dva dana trajanja konferencije 8,9 od 10 tvitova posvetili drugim temama, sto bih
### ocenila kao prilicno slab rezultat. Ipak, treba imati u vidu da se ovde ucesnicima smatraju svi koji su 
### tvitovali o konferenciji, sto moze da ukljuci, na primer, i medije koji su samo najavljivali skup.
### U ostalim periodima procenat tvitova o skupu je ocekivano mali, s tim da je nesto veci u trecem periodu, koji
### sada predstavlja samo po tri dana pre i posle skupa, jer su dani skupa izdvojeni u posebnu grupu.

### Da li su ova dva dana bila podjednako interesantna?

tweets_by_days_ratio <-  tweets %>%
  mutate(timeframe = ifelse(date == "2017-05-22", "day1", ifelse(date == "2017-05-23", "day2", timeframe))) %>%
  group_by(timeframe) %>% mutate(total_tweets = n()) %>% ungroup() %>%
  filter(conf == TRUE) %>%
  group_by(timeframe) %>% mutate(conf_tweets = n()) %>%
  summarise(conf_tweets = first(conf_tweets), total_tweets = first(total_tweets), 
            tweets_ratio = conf_tweets / total_tweets)


### Drugi dan skupa ima nesto bolji rezultat: udeo tvitova o skupu je veci za 3%, ukupan broj tvitova je nesto manji,
### a broj tvitova o skupu veci nego prvog dana, ali cini mi se da razlika nije znacajna, pogotovo imajuci u vidu da
### klasifikacija tvitova nije potpuno precizna i mogla je da utice na rezultat.

### Angazovanost po ucesniku:

tweets_by_participant <- tweets %>%
  filter(date == "2017-05-22" | date == "2017-05-23") %>%
  group_by(screenName) %>% mutate(total_tweets = n()) %>% ungroup() %>%
  filter(conf == TRUE) %>%
  group_by(screenName) %>% mutate(conf_tweets = n()) %>%
  summarise(conf_tweets = first(conf_tweets), total_tweets = first(total_tweets), tweets_ratio = conf_tweets / total_tweets) %>%
  arrange(desc(tweets_ratio))


pdf("visuals/text2_participants_activity.pdf")
ggplot(tweets_by_participant, aes(tweets_ratio)) +
  geom_histogram(binwidth = 0.05, col =  "gray40", fill = "#A3A500")+
  labs(title = "participants' activity:\nconference vs. total tweets", x = "conference/total tweets ratio",
       y = "number of participants") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
dev.off()


### Ako posmatramo samo dane trajanja skupa, broj ucesnika koji su tvitovali je 343 (za razliku od ukupno 
### posmatranih 457). Ipak, veliki je broj onih koji su jako malo tvitovali o skupu, ali mnogo o drugim temama.
### Za ovaj deo "ucesnika", sa procentom tvitova o skupu svega nesto vecim od nule, moglo bi da se pretpostavi da
### je samo preneo vest o skupu ili retvitovao zanimljiv tvit nekog od ucesnika. Osim dela ucesnika sa niskim 
### kolicnicima, dva veca podskupa su na 50% i na 100% - oni koji su tvitovali samo o skupu i oni koji su jednako
### tvitovali i o skupu i o drugim stvarima. Malo je cudno da izmedu 0.5 i 1 nema veci broj ucesnika, ali moze biti
### da se radi samo o suvise finoj podeli nad podacima gde je malo onih koji imaju dovoljno visok kolicnik.

tweets_by_participant %>% group_by(rounded_ratio = round(tweets_ratio, digits = 1)) %>% summarise(freq = n())

### Vise od trecine posmatranih ucesnika ima manje od 10% tvitova o konferenciji u toku ova dva dana, isto tako,
### trecina ima izmedu 50% i 100% tvitova o konferenciji. Ove brojke daju malo jasniju sliku o prvom rezultatu o
### 11% tvitova o konferenciji.

### Ko su ucesnici sa najvecim procentom?

tweets_by_participant %>% arrange(desc(tweets_ratio)) %>% print(n = 48)

### To su uglavnom ucesnici sa malim ukupnim brojem tvitova koji su pritom i ispravno klasifikovani. Dve 
### posledice neprecizne klasifikacije tvitova o konferenciji su to sto se ne moze reci koliko je jos ucesnika
### sa po jednim ili dva tvita koji su pogresno klasifikovani (a ucesnici bi mozda imali stoprocentnu "angazovanost"), 
### kao i to da organizatori skupa i drugi ucesnici sa vecim brojem tvitova o skupu imaju nesto nizi ovaj kolicnik.

tweets_by_participant %>% arrange(desc(conf_tweets))

tweets %>% filter((date == "2017-05-22" | date == "2017-05-23") & screenName == "wieilc" & conf == FALSE) %>% 
  select(text)

### 61 tvit organizatora nije klasifikovan kao tvit o konferenciji, i to su oni koji najavljuju govornike, 
### zahvaljuju im ili prenose njihove poruke, npr:
# "We engineer for good and social impact" said @VMware's @LilyLChang as she announced #PitchCompetition winner… 
# Up next: Barbara Whye, Chief Diversity and Inclusion Officer at @intel. 
# Thanks for the inspiring words for #WomeninTech, @JJDiGeronimo! Watch JJ's video with @ConvergeDigest:… 

### Ipak, ne moze se na osnovu greske za tvitove organizatora suditi o ukupnoj gresci, jer organizator, jasno, 
### nije pominjao svoj nalog u tvitovima; pretpostavljam da su tvitovi drugih ucesnika ipak bolje klasifikovani.
### Na primer, korisnik "gminks" ima mnogo manje "lazno negativnih" tvitova, ali je ovo sve na osnovu licne 
### procene, bez obelezavanja tvitova je nemoguce preciznije govoriti o gresci.

tweets %>% filter((date == "2017-05-22" | date == "2017-05-23") & screenName == "gminks" & conf == FALSE) %>% 
  select(text)

### U zakljucku, posmatrani ucesnici skupa su vrlo malo (11%) tvitovali o skupu. Na ovakav rezultat su uticali
### losa klasifikacija tvitova o skupu i nedostatak selekcije ucesnika. Oba problema imaju prostora za bolja 
### resenja i mogu da ostanu otvorena za kasnija poboljsavanja.

