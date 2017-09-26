### Priprema podataka za analizu teksta

library(dplyr)
library(stringr)
library(tidytext)
library(widyr)

### U analizi teksta koristicu tvitove iz tabele selected_tweets, koja sadrzi tvitove prikupljene 
### za sve ucesnike koji ispunili sledece uslove: tvitovali su o skupu sa oznakama #WIELead ili @wieilc
### bilo kada u periodu 19-26.05.2017. godine; u vreme preuzimanja tvitova imali su aktivan nalog, sa
### dozvolom za preuzimanje tvitova (ne 'protected'); u vreme preuzimanja tvitova su imali dostupne
### tvitove barem od 21.04.2017, sto je pocetak perioda koji se analizira. 
### Od svih prikupljenih tvitova, analiziraju se tvitovi za period od 21.04.2017-23.06.217. godine.

### U analizi teksta cu koristicu podskup ove tabele sa samo onim kolonama koje ce mi biti potrebne 
### u analizi, buduci da se analize vrse direktno nad tim objektom (za razliku od mreznih analiza). 
### Podskup ce svakako sadrzati id kolonu, pa ce biti moguce po potrebi doci i do vrednosti svih kolona 
### za tvit.

selected_tweets <- readRDS("results/selected_tweets.RData")

tweets <- selected_tweets[, c("timeframe", "screenName", "id", "text")]

### U podacima za analizu cu zadrzati i retvitove, s obzirom na to da ukazuju na misljenje onoga ko je 
### retvitovao, iako se ne radi o originalnom misljenju. Ipak, samu oznaku za retvit (RT) cu izbaciti, 
### buduci da bi bila medu najfrekventnijim recima, a smatram da nije deo necijeg misljenja/iskaza, kao 
### sto su to stop reci. Isto tako, izbacicu i linkove i neke kodove za specijalne znakove (&amp; &lt; i &gt;).

tweets <- tweets %>% 
  mutate(text = str_replace_all(text, "http[s]?[A-Za-z\\d/:.-_]+|RT|&amp;|&lt;|&gt;", ""))

### Kada sam zapocela analizu, naisla sam na tvitove na stranim jezicima (spanski predlog "de" se nasao medu 
### najfrekventnijim recima posle izbacivanja (engleskih) stop reci). 
### Ovaj problem sam pokusala da resim na razlicite nacine, medutim, cini mi se da ovi tvitovi nece praviti
### probleme, jednostavno prilikom spajanja sa kljucnim recima, nece upariti nijednu rec. Moglo bi biti problema
### kod ispitivanja najfrekventnijih reci iz tvitova (kao sto je bilo sa "de"). Inace, u tvitovima skinutim po 
### kljucnim recima za konferenciju nema tvitova na stranim jezicima, ali skidanjem tvitova za svakog ucesnika, 
### skinuti su i takvi tvitovi. Skidanjem samo tvitova na engleskom jeziku ne bi se dobili svi tvitovi za ucesnike
### za odredeni period, pa se ne bi moglo reci, na primer, koji procenat njihovih tvitova govori o konferenciji.

### Do drugog problema sam dosla kada sam htela da izdvojim tvitove sa konferencije, odnosno one sa oznakama
### #WIELead i @wieilc. Istrazivanjem sam dosla do toga da cak i u pocetnom skupu tvitova, koji su skidani upravo
### po ovim kljucnim recima, nemaju svi tvitovi neku od ove dve oznake.

### Radi se o tome da tviter skracuje tvitove koji su duzi od 140 karaktera. Mada postoji format "extended tweets"
### koji neke elemente (pominjanja, linkove, slike...) dodaje kao meta-podatke, tako da vise karaktera ostane 
### slobodno za tekst, paket twitteR pomocu kog sam skidala tvitove ne podrzava ovu opciju, niti sam u vreme skidanja
### tvitova znala za ovaj problem. 

### Za ovakve podatke kakve imam, da bih ukljucila tvitove kao sto su, na primer:

# Next up - Stefanie Tompkins (Acting Deputy Director) DARPA speaking at the IEEE WIE International 
#       Leadership Confer… https://t.co/iK7lRJJorn
# We innovate step by step and support the advancement of #womeninengineering #proudtobeengineer 
#       proud to be #wier9… https://t.co/kvpOgfSpaY

### odlucila sam da malo prosirim uslov za ukljucivanje tako da obuhvati i samo pocetak ovih oznaka, kombinaciju
### "wie" i "ieee", kao i kombinaciju women...engineer... 
### "wie" treba da bude ili u kombinaciji sa "ieee" ili da pocinje sa #|@, da bi se izbeglo nemacko "wie"

conf_regex <- "(?i)wielead|wieilc|[#|@]wie|women.+engineer|wie.+ieee|ieee.+wie"

table(tweets %>% filter(str_detect(text, pattern = conf_regex)) %>% select(timeframe)) 
# time_1 : time_5 => 169    241   1721    131    104 


tweets <- tweets %>% mutate(conf = ifelse(str_detect(text, pattern = conf_regex), TRUE, FALSE))


saveRDS(tweets, "results/tweets.RData") 


### Analiza programa skupa

### Za dalje analize ce mi biti potrebne teme skupa. Od dodatnih informacija o skupu, dostupan je bio program
### skupa u html formatu (na sajtu organizatora vise ne postoji ova verzija). Program se nalazi u direktorijumu
### "data" ("data/Agenda | IEEE WIE International Leadership Conference.html")

### S obzirom na sadrzaj programa, koji se sastoji samo iz naslova predavanja i imena predavaca, nije bilo moguce 
### izdvajanje termina preko TF-IDF mere ili izdvajanje tema preko LDA postupka za topic modelling. Do kljucnih reci 
### i tema sam dosla parsiranjem dokumenta, izdvajanjem termina iz naslova predavanja, tako da su teme predstavljene 
### terminima onoliko precizno koliko su sami naslovi uspeli da ih prezentuju.

### Program skupa je u dokumentu podeljen na pet* tematskih celina, uz radionice i uvodna i zavrsna predavanja. 
### Svaka od ovih pet celina ima svoju klasu; program je u nekoliko tabela, a predavanja su u <td> elementu sa 
### atributom npr. class="TTrack1 ..."; naslovi predavanja su u okviru elementa <strong>, na primer:

### <td class="TTrack1 TStripe" width="14%"><strong>Robots, Disasters, and Refugees: How Unmanned Systems are<br>
### Helping</strong><br>  Robin Murphy, Professor, Texas A&amp;M</td>

### Radionice imaju klasu "TDinner" u okviru programa, a i dodatno su izdvojene u posebnu tabelu na kraju stranice; 
### uvodna i zavrsna predavanja nemaju ovu oznaku klase.

### *Tematske celine su oznacene brojevima 1-5, ali ne jednoznacno, odnosno, track 5 je nekada "Women in...", a 
### nekada "Entrepreneurship, track 1 - "Innovation" je u poslednjem bloku "Pitch Competition"; na kraju, ovih celina
### ima zapravo sedam: "Disruptive Technology", "Empowerment", "Entrepreneurship", "Executive Leadership", "Innovation",
### "Pitch Competition", "Women in...". Dodatne celine su "Workshops" i "Other", koja se odnosi na uvodna i zavrsna
### predavanja.

### HTML dokument sam prvo podelila na <td> elemente, u okviru kojih sam izdvajala <strong> elemente, odnosno naslove
### predavanja, vodeci racuna da ih dodelim odgovarajucoj celini. Na kraju sam iz ovih naslova izdvojila pojedinacne
### reci izbacivanjem stop-reci, reci kracih od tri slova, imena i glagola.



### Funkcija headings_matches uzima jedan <td> element, pretrazuje da li ima <strong> elemenata; ako ih ima,
### preciscava tekst i vraca ih (kao vektor) pozivajucoj funkciji

headings_matches <- function(td_string){
  res <- vector()
  matches <- str_match_all(td_string, "<strong>(.+?)</strong>")[[1]]
  if (nrow(matches) > 0){
    for(i in 1:nrow(matches)){                 # sve sto je upareno: precistiti i ubaciti u vektor kljucnih reci
      if (str_detect(matches[i,2], pattern = "^SJCC")) next      # SJCC su lokacije predavanja
      match <- str_replace_all(matches[i, 2], pattern = "^(<[^>]+>)+", replacement = "") %>%   # tagovi na pocetku
        str_replace_all(pattern = "&amp;", replacement = "and") %>%
        str_replace_all(pattern = "<[^>]+>|&nbsp;", replacement = " ")
      res <- c(res, match)
    }
  }
  return(res)
}


### Funkcija make_headings_df uzima putanju programa skupa i pravi data frame naslova predavanja i tematskih celina
### oznacenih u programu; na osnovu klasa <td> elemenata dodeljuje odgovarajuci naziv celine naslovima predavanja

make_headings_df <- function(prog_path){
  
  agenda <- str_c(readLines(prog_path, warn = FALSE), collapse = "")
  
  beg <- str_locate(agenda, "<table")[1,1]
  end <- str_locate(agenda, "(?i)<h2>workshops")[1,1] # u ovom delu programa su ponovljene radionice, sa drugim klasama
  ag <- str_sub(agenda, beg, end-1)   # ag sadrzi deo html-a sa programom
  
  ag_tds <- unlist(strsplit(ag, "</td>"))   # ag_tds je vektor <td> elemenata
  
  program <- data.frame()           # df kljucnih reci po naslovima trekova
  table <- vector()                 # tabela (broj treka) - (naslov), posto se asocijacije menjaju kroz dokument
  
  for(i in 1:length(ag_tds)){
    if (!str_detect(ag_tds[i], "<td")) next 
    if (str_detect(ag_tds[i], "TTrack")){     # ako ima broj treka, treba ga izvuci; da li je dat naslov treka?
      
      num <- str_match(ag_tds[i], "TTrack(\\d)")[1,2]
      if (str_detect(ag_tds[i], "<strong>Track ?\\d:<br>(.+)</strong>")) {      # naslov treka? povezati sa brojem
        table[num] <- str_match(ag_tds[i], "<strong>Track ?\\d:<br>(.+)</strong>")[1,2]
      } else {        # ako naslov nije dat, koristi se stari naslov iz tabele, sve iz <strong> ide u rezultat
        if(length(headings_matches(ag_tds[i])) > 0)
          program <- rbind(program, cbind(track = table[num], text = headings_matches(ag_tds[i])), 
                           stringsAsFactors = FALSE)
      }
      
    } else if(str_detect(ag_tds[i], "TDinner")) {       # TDinner su radionice
      if(length(headings_matches(ag_tds[i])) > 0)
        program <- rbind(program, cbind(track = "Workshops", text = headings_matches(ag_tds[i])), 
                         stringsAsFactors = FALSE)
      
    } else {      # Uvodna i zavrsna predavanja
      if(length(headings_matches(ag_tds[i])) > 0)
        program <- rbind(program, cbind(track = "Other", text = headings_matches(ag_tds[i])), 
                         stringsAsFactors = FALSE)
    }     
  }
  return(program)
}



prog_path <- "data/Agenda | IEEE WIE International Leadership Conference.html"

prog_headings <- make_headings_df(prog_path)


### Izdvajanje reci iz naslova:

keywords <- prog_headings %>% group_by(track) %>% filter(!duplicated(text)) %>% 
  ungroup() %>% unnest_tokens(word, text, token = "words") %>% 
  anti_join(stop_words) %>%                                                  # izbacivanje stop-reci
  filter(!str_detect(word, pattern = "^.{1,2}$|\\d")) %>%                    # izbacivanje reci kracih od 3 slova
  filter(!word %in% (lexicon::common_names ))                                # izbacivanje licnih imena


### Filtriranje prezimena prema listi iz paketa lexicon uparuje 27 reci, od kojih su neke: leader, power, winner,
### bias, risk... tako da sam smatrala da je bolje da ostane nekoliko prezimena, nego da sve ove reci izbacim.

filter(keywords, word %in% str_to_lower(lexicon::freq_last_names$Surname)) %>% distinct(word)


### Izbacivanje glagola:

final_keywords <- keywords %>% distinct(track, word) %>% 
  left_join(lexicon::hash_grady_pos) %>% filter(!str_detect(pos, "Verb") | is.na(pos)) %>% 
  distinct(track, word)                                                       

### Izbacene su reci koje su samo glagoli, odnosno ne mogu imati neko drugo znacenje. Na primer, za "cancelled" 
### postoji samo jedna kombinacija word - POS, ali "cloud" moze biti i imenica i glagol, a posto su sa left_join 
### napravljene sve kombinacije, kada su izbacene kombinacije rec - glagol, ostale su kombinacije rec - imenica, 
### odnosno rec "cloud" je ostala u tabeli kljucnih reci.

### Teme imaju dosta zajednickih kljucnih reci, sto i odgovara cinjenici da se teme preklapaju, pogotovo kada su 
### ovako bliske.

final_keywords %>% group_by(word) %>% mutate(n = n()) %>% pairwise_cor(track, word, n) %>% arrange(desc(correlation))


saveRDS(final_keywords, "results/final_keywords_df.RData")
saveRDS(prog_headings, "results/program_headings_df.RData")

