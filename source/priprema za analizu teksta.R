### Priprema podataka za analizu teksta

library(dplyr)
library(stringr)
library(tidytext)


### U analizi teksta koristicu tvitove iz selected_tweets data frame-a, tacnije, koristicu podskup ovog
### data frame-a sa samo onim kolonama koje ce mi biti potrebne u analizi, buduci da se analize vrse
### direktno nad tim objektom (za razliku od mreznih analiza). Podskup ce svakako sadrzati id kolonu
### pa ce biti moguce po potrebi doci i do vrednosti svih kolona za tvit.

selected_tweets <- readRDS("results/selected_tweets.RData")

tweets <- selected_tweets[, c("timeframe", "screenName", "id", "text")]

### U podacima za analizu cu zadrzati i retvitove, s obzirom na to da ukazuju na misljenje onoga ko je 
### retvitovao, iako se ne radi o originalnom misljenju. Ipak, samu oznaku za retvit (RT) cu izbaciti, 
### buduci da bi bila medu najfrekventnijim recima, a smatram da nije deo necijeg misljenja/iskaza, kao 
### sto su to stop reci. Isto tako, izbacicu i linkove i neke kodove za specijalne znakove (&amp; &lt; i &gt;).

tweets <- tweets %>% mutate(text = str_replace_all(text, "http[s]?[A-Za-z\\d/:.-_]+|RT|&amp;|&lt;|&gt;", ""))

### Kada sam zapocela analizu, naisla sam na tvitove na stranim jezicima (spanski predlog "de" se nasao medu 
### najfrekventnijim recima posle izbacivanja (engleskih) stop reci). 
### Ovaj problem sam pokusala da resim pomocu broja stop reci u tvitu, tako sto bi tvitovi koji sadrze stop
### reci bili oznaceni kao engleski, medutim, tvitovi su izuzetno kratki i ne moraju da sadrze nijednu stop 
### rec, kao npr. ovaj:
### "Tough questions! Bold Answers ! Flattered audience! #pitch Competition  #WIELead"
### Paket koRpus bi trebalo funkcijom guess.lang da "pogodi" jezik teksta, medutim, sledeca dva tvita:
### "Being inspired by women leaders at the IEEE Women in Engineering International Leadership Conference"                      
### "What if you figure out what you've been advocating for isn't a good idea? Pandkhou: learn to let go even 
### if you don't want…" 
### koRpus je klasifikovao kao "Catalan-Valencian-Balear".
### Dalje sam pokusala za svaki tvit da proverim koliko reci se nalazi u recniku engleskog jezika, odnosno
### u recniku paketa hunspell koji proverava pravilno spelovanje, a instalira se kao dependency za paket tidytext.
### Ovaj recnik je jednostavan popis reci, gde svaki red sadrzi jednu rec, koja moze biti pracena kosom 
### crtom i dodatnim oznakama (koje u ovom slucaju nisu od znacaja). Buduci da recnik nije narocito velik,
### a posebno sam primetila da ne sadrzi oblike mnozine, odlucila sam da stemujem i reci u tvitovima i u recniku.
### Iz tvitova sam iskljucila pominjanja i hashtag-ove, jer tu moze biti skracenica, licnih imena i slicnog.

library(tm)

dict_path <- hunspell::dicpath()[grep(hunspell::dicpath(), pattern = "hunspell")][1] # dicpath() vrati vise lokacija recnika
eng_dict <- data.frame( word = readLines(paste0(dict_path, "/en_US.dic")) %>% 
                          str_replace_all(pattern = "/[A-Za-z]+", "")) %>%  # izbacivanje dodatnih oznaka
  mutate(word = stemDocument(as.character(word), language = "english")) %>%  # stemovanje svodi vise oblika reci na 1
  distinct(word) %>%
  mutate(lang = "english")

# izvrsava se oko 5 minuta!
stemmed_tweets <- tweets %>% 
  mutate(text = str_replace_all(text, pattern = "@[A-Za-z\\d_-]+|#[A-Za-z\\d_-]+", replace = "")) %>% 
  unnest_tokens(word, text, token = "words") %>% # tokenizacijom se ukloni interpunkcija i rec se prebaci u lower_case
  mutate(word = stemDocument(word, language = "english"))

# za probu, ovde ima engleskih i spanskih reci:
stemmed_tweets[236600:236630,] %>% left_join(eng_dict) %>% add_count(id) %>% add_count(id, lang == "english") 

### Ideja je bila da se tvit cijih se bar 50% reci nalazi u ovom recniku klasifikuje kao tvit na engleskom jeziku 
### (50% zbog problematicnog spelovanja i nekompletnosti recnika). Nesto kao ovo, ali ne moze ovako..
# ... group_by(id) %>% mutate(lang = ifelse(n * 0.5 >= nn, "other", "english" ))  # ovo racuna za svaki red, a ne grupu
# ... group_by(id) %>% summarise(eng = length(lang == "english")) .... # 
# varijante sa count unutar mutate mi izbacuju greske

### Na kraju sam shvatila da mi verovatno nece praviti probleme tvitovi na stranim jezicima - prilikom spajanja
### sa kljucnim recima, oni jednostavno nece upariti nijednu rec. Moglo bi biti problema kod ispitivanja najfrekventnijih
### reci iz tvitova (kao sto je bilo sa "de"). Inace, u tvitovima skinutim po kljucnim recima za konferenciju nema
### tvitova na stranim jezicima, ali skidanjem tvitova za svakog ucesnika, skinuti su i takvi tvitovi. 


### Do drugog problema sam dosla kada sam htela da izdvojim tvitove sa konferencije, odnosno one sa oznakama
### #WIELead i @wieilc. Izdvajanjem tvitova samo sa ovim oznakama iz skupa tvitova, ucinilo mi se da je izdvojen 
### vrlo mali broj tvitova. 

conf_tweets <- tweets %>% filter(str_detect(text, "(?i)#wielead|@wieilc"))
nrow(conf_tweets) # 2047
table(conf_tweets$timeframe) # time_1 : time_5 =>  121    158   1625     95     48

### Istrazivanjem sam dosla do toga da i u pocetnom skupu tvitova nemaju svi neku od ove dve oznake.

startertweets <- readRDS("data/startertweets") # 2704

time_narrowed <- startertweets %>% distinct(id, .keep_all = TRUE) %>% 
  mutate("date" = as.Date(created)) %>% filter(date < "2017-05-27" & date > "2017-05-18")
# 1896 tvitova, ali nisu svi ovi ucesnici imali dostupne tvitove, pa nisu svi u konacnom skupu

startertweets %>% filter(!str_detect(text, "(?i)#wielead|@wieilc")) %>% select(text)

### Radi se o tome da tviter skracuje tvitove koji su duzi od 140 karaktera. Mada postoji format "extended tweets"
### koji neke elemente (pominjanja, linkove, slike...) dodaje kao meta-podatke, tako da vise karaktera ostane 
### slobodno za tekst, paket twitteR pomocu kog sam skidala tvitove ne podrzava ovu opciju, niti sam u vreme skidanja
### tvitova znala za ovaj problem.

# https://dev.twitter.com/overview/api/upcoming-changes-to-tweets
# https://twittercommunity.com/t/retrieve-full-tweet-when-truncated-non-retweet/75542/7

### Da bih ukljucila tvitove kao sto su ovi:
# Next up - Stefanie Tompkins (Acting Deputy Director) DARPA speaking at the IEEE WIE International 
#       Leadership Confer… https://t.co/iK7lRJJorn
# We innovate step by step and support the advancement of #womeninengineering #proudtobeengineer 
#       proud to be #wier9… https://t.co/kvpOgfSpaY
### odlucila sam da malo prosirim uslov za ukljucivanje tako da obuhvati i samo pocetak ovih oznaka, kombinaciju
### "wie" i "ieee", kao i kombinaciju women...engineer... 
### "wie" treba da bude ili u kombinaciji sa "ieee" ili da pocinje sa #|@, da bi se izbeglo nemacko "wie"

conf_tweets2 <- tweets %>% 
  filter(str_detect(text, pattern = "(?i)wielead|wieilc|[#|@]wie|women.+engineer|wie.+ieee|ieee.+wie"))

table(conf_tweets2$timeframe) # time_1 : time_5 => 169    241   1721    131    104 


### Sigurno je da bi treniranjem klasifikatora ovi tvitovi bili preciznije izdvojeni, ali to zahteva oznacavanje
### trening seta tvitova, pa ostaje kao opcija za kasnije. Isto tako, i problem klasifikacije tvitova prema jeziku
### bi mozda mogao da se resi na ovaj nacin.

saveRDS(tweets, "results/tweets.RData")
saveRDS(conf_tweets2, "results/conf_tweets.RData")


TODO # parsiranje programa i izdvajanje kljucnih reci
