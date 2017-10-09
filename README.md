# Analiza tvitova sa konferencije 
***

[Cilj](#cilj)  
[Pitanja](#pitanja)  
[Podaci](#podaci)  
[Analiza ukratko](#analiza-ukratko)  
  *[Analiza mreže - novi kontakti](#novi-kontakti-skript-fajl)  
  *[Analiza mreže - homogenost](#homogenost-mreže-skript-fajl)  
  *[Analiza mreže - mreža posle skupa](#mreža-posle-skupa-skript-fajl)  
  *[Analiza teksta - sentiment](#analiza-sentimenta-skript-fajl)  
  *[Analiza teksta - angažovanost](#angažovanost-učesnika-skript-fajl)  
  *[Analiza teksta - upotreba tema skupa](#upotreba-tema-skupa-skript-fajl)  
[Zaključci analize](#zaključci-analize)  


### Cilj 

Cilj ovog projekta je pokušaj merenja uspešnosti naučne konferencije/skupa na osnovu tvitova učesnika. Osim analize sentimenta tvitova, koji bi dali ocenu opšteg utiska učesnika, rad će pokušati da izmeri i stvarnu "dobit" učesnika, koja bi se ogledala u ostvarenim kontaktima i novim informacijama. Novi kontakti će se pratiti kroz ostvarenu komunikaciju primenom metoda analize društvenih mreža, a uticaj izlaganja i predavanja putem analize teksta tvitova. Obe vrste analiza će biti sprovedene nad podacima podeljenim na vremenske segmente pre, tokom i posle održavanja skupa, tako da bi uočene promene govorile o dobiti učesnika.  



### Pitanja

Analizom mreže koju čine učesnici skupa, rad će pokušati da odgovori na sledeća pitanja:

* Da li su učesnici komunicirali i pre skupa ili ostvarena komunikacija predstavlja nove kontakte?
* Da li svi učesnici ujednačeno komuniciraju ili se stvaraju grupe sa intenzivnijom komunikacijom?
* Kolika je komunikacija posle skupa i da li je povezana sa grupama?

Analizom teksta tvitova, rad će pokušati da otkrije sledeće:

* Koji je opšti utisak o skupu?
* Koliko je skup interesantan za učesnike ili koliko ih angažuje?
* Koliko su informacije nove za učesnike?
* Koliko teme skupa ostaju predmet interesovanja učesnika posle skupa?  



### Podaci

U radu su korišćeni podaci o tvitovima i učesnicima IEEE Women in Engineering International Leadership Conference, konferencije održane 22-23.05.2017. godine. Prvo su u periodu 22.05-27.05.2017. prikupljeni tvitovi sa ključnim rečima #WIELEad i @wieilc (oznaka skupa i organizator), a zatim je određen okvir od tri dana pre do tri dana posle skupa, kao prošireni period trajanja skupa, koji dozvoljava i nekim učesnicima koji su tvitovali sa heštegom konferencije, ali ne u toku samog skupa, da budu uključeni u analizu. Za sve učesnike koji su tvitovali unutar tog perioda preuzeti su svi tvitovi, odnosno maksimalan broj tvitova koji twitterAPI dozvoljava (3200). Od ukupnog broja učesnika, u analizu nisu ušli učesnici koji su obrisali nalog, podesili nalog kao 'protected', ili nisu imali dostupne tvitove počev od 21.04.2017. godine. Konačan broj učesnika koji je obuhvaćen analizom je 457. Na kraju, tako prikupljeni tvitovi su podeljeni na pet vremenskih odeljaka, dva odeljka od po dve nedelje pre skupa, odeljak trajanja skupa, uključujući i tri dana pre i tri dana posle, ukupno osam dana, i dva odeljka od po dve nedelje posle skupa. Ovakva vremenska podela je posledica pretpostavke da će uticaj skupa postepeno da se smanjuje i da period posle skupa ne može da se posmatra kao jedna celina. Ukupan period koji je ušao u analizu je 21.04-23.06.2017.  

Za preuzimanje tvitova su korišćene funkcije iz paketa twitteR (v 1.1.9).
Svi tvitovi se nalaze u direktorijumu 'data', kao serijalizovani R objekti:
* startertweets sadrži tvitove prikupljene za vreme trajanja skupa; ~130kB
* all_tweets 1:4 sadrže tvitove prikupljane po učesniku (fajl je podeljen na 4 dela samo zbog ograničenja postavljanja velikih fajlova na GitHub); ~82MB  

Za kreiranje mreže i mrežne analize su korišćene funkcije iz paketa igraph (v 1.1.2). Matrica povezanosti je kreirana tako da se svako pominjanje jednog učesnika u tvitu drugog posmatra kao usmerena veza, a težina veze predstavlja broj pominjanja. Korisceni su i paketi dplyr (v 0.7.3) i scales (v 0.4.1).  

U analizi teksta korisceni su paketi stringr (v 1.2.0), tidyr (v 0.6.3), tidytext (v 0.1.3), tm (v 0.7.1), sentimentr (v 1.0.0) i dplyr (v 0.7.3).  


### Analiza ukratko  

#### Novi kontakti ([skript fajl](source/analiza_SNA_1pitanje.R))

Na pitanje o novoostvarenim kontaktima odgovoreno je pomoću sledećih metrika:  
* gustina mreže, merena ne uzimajući u obzir težine veza, tako da ne odražava intenzitet komunikacije, nego broj kontakata
* broj komponenti i veličina najveće komponente, gde slabe komponente predstavljaju povezanost učesnika na bilo koji način, jake komponente uzimaju u obzir smer veza, a određene su i komponente za neusmeren graf, pri čemu je konverzija vršena tako da veza između učesnika predstavlja dvosmernu komunikaciju u usmerenom grafu
* prosečna putanja i prečnik mreže, koji takođe govore o povezanosti mreže 

Mreza se sastoji od 457 učesnika i, očekivano za tu veličinu, nije povezana, ali je gustina u vreme trajanja skupa povećana četiri puta. `time_1 -> time_5 : 0.001732515 0.001642544 0.006622135 0.001544788 0.001435390` Na žalost, gustina mreže posle skupa je manja čak i od one pre skupa. Budući da igraph funkcijom edge_density računa gustinu samo kao količnik postojećih i svih mogućih veza, ne uzimajući u obzir težinu veza, može se zaključiti da je povećana gustina mreže rezultat većeg broja pominjanja različitih učesnika, odnosno uspostavljanja novih kontakata, a ne intenzivnije komunikacije između manjeg broja učesnika.  

Detaljniji prikaz se može dobiti ispitivanjem komponenti.  
Slabe komponente ovde podrazumevaju barem jednostrano pominjanje jednog učesnika u tvitu drugog. Očekivano, broj komponenti je najmanji za vreme trajanja skupa, `time_1 -> time_5 : 243    262     13    265    275`, i tada je maksimalna komponenta najveća, `time_1 -> time_5 : 120    142    445    140    105`, obuhvata čak 97% učesnika (u odnosu na 31% pre skupa), što znači da su gotovo svi učesnici pomenuli barem nekog od drugih učesnika. Imajući na umu retvitove, vredi ispitati i jake komponente, koje bi predstavljale intenzivniju komunikaciju (ne nužno dvosmernu - čvorovi se smatraju povezanima ako postoji bar po jedna usmerena putanja u oba smera, ali ne obavezno dužine 1), ili, još restriktivnije, svesti graf na neusmeren, zadržavajući samo recipročne veze - dvosmernu komunikaciju. U oba ova slučaja, rezultati su srazmerni jačini uslova za uspostavljanje veze, odnosno najveća komponenta obuhvata manji procenat učesnika, ali rezultati i dalje potvrđuju sliku dobijenu na osnovu gustine mreže.  

[Frekvencija komponenti po veličinama](visuals/1_component_sizes2.pdf): ne postoji neki značajan broj "srednje velikih" komponenti u vreme skupa, ni jakih ni slabih - dominantna je maksimalna komponenta. Ovo bih ocenila kao vrlo pozitivno, skup je uspeo da ujedini učesnike u komunikaciji (mada i sama organizacija skupa - razuđenost tema ili ciljne publike - može da utiče na formiranje većeg broja manjih komponenti i treba da se uzme u obzir ukoliko se primete takvi rezultati.) Sa druge strane, procenat učesnika koji su se uključili u intenzivniju komunikaciju bi mogao da se tumači na različite nacine, u zavisnosti od očekivanja organizatora, poređenja sa drugim sličnim skupovima, a treba imati u vidu i to da nisu svi oni koji su tvitovali o skupu stvarno i učestvovali.  

Još neke metrike na nivou cele mreže mogu da provere ove rezultate, na primer prosečna putanja (prosečni geodezik) i dijametar mreže (najduži geodezik). Metrike su izračunate ne uzimajući u obzir težinu ivica, jer se ispituje bilo kakav kontakt među učesnicima. I prosečna putanja, izračunata za jake komponente: `time_1 -> time_5 : 2.438159 3.399861 2.684841 2.831637 3.247514`, i dijametar usmerene mreže: `time_1 -> time_5 :  8      9      6      8      9 ` su najmanji za period trajanja skupa, iako je tada uključeno najviše učesnika, ali je i gustina mreže tada najveća i to odgovara ideji o dobro povezanim čvorovima. Zanimljiv je i drugi period, za koji je i prečnik mreže veći i prosečna putanja je duža, a gustina je manja nego u prvom periodu, što ukazuje na to da su se komponente povezale malim brojem veza, formirajući mrežu nalik na liniju. Tako, iako je u odnosu na prvi period veći procenat učesnika koji formira najveću komponentu, komunikacija ipak nije naročito intenzivna.  

U [grafičkom prikazu](visuals/1_num_keywords.pdf) se lepo vidi i povezanost mreže i razvoj maksimalne komponente kroz vreme. Na ovom prikazu su čvorovi okarakterisani brojem tvitova koje su objavili sa heštegom skupa (računato prema skupu prvobitno preuzetih tvitova) - od žute sa najmanjim brojem, do crvene sa najvećim brojem. Osim toga, težina ivica, odnosno broj pominjanja drugih čvorova u tvitovima, označena je debljinom veze između dva čvora.  


#### Homogenost mreže ([skript fajl](source/analiza_SNA_2pitanje.R))

Za merenje homogenosti mreže u odnosu na komunikaciju korišćene su sledeće metrike:  
* distribucija stepena i varijantnost stepena, odnosno razlika u broju kontakata između pojedinačnih učesnika
* koeficijent klasterovanja, odnosno tranzitivnost mreže, koja govori o lokalnoj povezanosti učesnika
* broj i veličina najvećih i maksimalnih klika i veličina jezgara, takođe u vezi sa lokalnom povezanošću učesnika
* centralizovanost mreže, koja, zajedno sa prethodnim metrikama, ukazuje na strukturu mreže
* određivanje klastera, koji govore o podeljenosti mreže
* analiza mreže bez glavnog organizatora (čvor sa najvećim stepenom)  

Iako je [grafikon distribucije stepena](visuals/2_deg_distribution_log.pdf) problematičan (skala zapravo počinje nulom, oznake su pomerene za 1), lepo se vidi da distribucije stepena prate "power law" distribuciju, odnosno najveći broj učesnika ima veoma mali stepen, a sa porastom stepena frekvencija naglo opada i pojavljuje se karakterističan dugački "rep" (zbog logaritamske skale nula-vrednosti nisu deo prikaza, pa se tu javljaju prekidi u liniji). Distribucije po vremenskim intervalima su dosta slične, sa izuzetkom perioda u kome se održao skup, gde je broj učesnika koji imaju odlazni (a time i ukupni) stepen jednak nuli - relativno blizu nule, odnosno, mala je frekvencija učesnika koji nisu u tvitu pomenuli nekog drugog učesnika, ali je velika frekvencija onih koji nisu pomenuti u tvitovima drugih, onih čiji je dolazni stepen jednak nuli. Mali broj pominjanih učesnika, kao i male vrednosti odlaznog stepena za veliku većinu učesnika ukazuju na to da je komponenta u kojoj učestvuje 97% svih čvorova zapravo više proizvod posrednih, a ne direktnih veza među učesnicima.  

Varijacije u stepenu su izražene u periodu trajanja skupa, i to u odlaznom stepenu 5 jedinica, u dolaznom stepenu 13, što govori o tome da se učesnici mnogo više razlikuju po ugledu nego po samom kvantitetu kontakata; osim toga, varijacije u ukupnom stepenu su 18 jedinica, što znači da se ne radi o podeli učesnika na one koji imaju visok dolazni i one koji imaju visok odlazni stepen, već na one koji imaju ukupan stepen visok i na one druge.  

Jasno je da komunikacija nije ujednačena u celoj mreži. Na nivou mreže može da se izračuna koeficijent klasterovanja, koji predstavlja odnos trouglova i trijada u mreži. `time_1 -> time_5 : 0.20504475 0.15757879 0.06979682 0.16435882 0.21428571` Ovo je vrlo mala vrednost za period održavanja skupa. Ali, ako se uzme u obzir razlika veličina slabe i jake maksimalne komponente (odnosno jednostrane i, uslovno rečeno, uzajamne komunikacije), onda ova brojka izgleda vrlo moguća i upuuje na strukturu bez mnogo klika ili jezgara.  

Za analizu klika je korišćena neusmerena verzija mreža, sa vezom za svaki par uzajamnih veza, iz razloga što više nije od interesa puka povezanost, već uzajamna komunikacija koja ima potencijala da opstane i posle skupa. Veličina najveće klike: `time_1 -> time_5 :  3      3      4      3      3 `, broj maksimalnih klika (onih koje nisu podgraf neke veće klike): `time_1 -> time_5 :  2      4     49      2      2` . Klika je sama po sebi dosta restriktivna mera, a uz ovako definisane veze u mreži, veličina najveće klike od četiri člana možda i nije tako loša. Isto važi i za broj maksimalnih klika u periodu održavanja skupa. Ako se pogledaju jezgra, najveća coreness vrednost po periodu: `time_1 -> time_5 :   2      2      3      2      2` (coreness ne označava broj učesnika u jezgru, nego stepen svakog učesnika!) Ni nešto blaži kriterijumi povezanosti čvorova ne govore o tome da ima većih grupa koje su čvrsto međusobno povezane, ali ne toliko da su klike.  

Neke mere centralizacije za mrežu u toku održavanja skupa su dosta visoke. Centralizacija po intermedijarnosti (0.25) ukazuje na to da možda postoje čvorovi koji "kontrolišu" komunikaciju u mreži, ali ova vrednost nije previsoka; zanemarivanje smera veza bi preuveličalo ovaj uticaj. Centralizacija po bliskosti je srednje visoka ako posmatramo sve veze bez obzira na smer (0.64), što bi značilo da postoji jedan broj čvorova koji je bliži ostalim čvorovima u sveukupnoj komunikaciji. Centralizacija po dolaznom stepenu je srednje visoka (0.55), što ukazuje na postojanje jednog broja čvorova sa visokim dolaznim stepenom, a to su čvorovi sa velikim uticajem (najverovatnije organizatori skupa). Centralizacija po svojstvenom vektoru je izrazito visoka (0.96), i to u svim posmatranim periodima, sa minimalnim razlikama za usmerene i neusmerene mreže. Ovo može da znači da veoma uticajni čvorovi intenzivno komuniciraju meusobno i na taj način dobijaju visoku centralnost svojstvenog vektora, dok su manje uticajni čvorovi isključeni iz komunikacije. Na osnovu ovoga može da se zaključi da je mreza srednje centralizovana - uzimajući prosek ovih vrednosti.  

Klasterizacija daje veliki broj klastera, sto bi moglo da ukazuje na izuzetnu podeljenost mreze. (edge betweenness: `time_1 -> time_5 :  75     25     45     62     51`, walktrap algoritam: `37     34     32     27     32` ). Iz [grafičkog prikaza](visuals/2_clusters_wt.pdf) se vidi da je veliki broj klastera u periodima pre i posle skupa jednim delom uslovljen brojem komponenti, ali u vreme trajanja skupa to nije objašnjenje. Ovakav raspored čvorova ne daje jasnu sliku, a mreza u vreme trajanja skupa izgleda kao ego-mreža jednog čvora, organizatora skupa (prema čijem nalogu je i vršena početna pretraga), pa je pokušana analiza mreže bez njega.  

Posle uklanjanja glavnog organizatora iz mreže, ostalo je 34 izolata (u početnoj mrezi ih je bilo još 12), ali se mreža nije raspala na komponente, što potvrđuje neku srednju centralizovanost (ima više srednje važnih čvorova). Ipak bez ovog čvora mreža je podeljena na 80 klastera, u odnosu na 32 (za isti period i isti algoritam za celu mrežu), što pokazuje značaj tog čvora za povezivanje grafa. Osim toga, u [vizuelizaciji](visuals/2_no_wieilc.pdf) se primećuje drvolika struktura, bez značajnog prisustva klika.  

Zaključak je da komunikacija u mreži nije ujednačena, o čemu govore distribucija stepena i srednja, ali primetna, centralizovanost mreže (mali broj uticajnih čvorova), nizak koeficijent klasterovanja i relativno mali broj klika i veličina jezgara (slaba međusobna povezanost "ne-centralnih" čvorova) i veliki broj klastera (velika heterogenost mreže).  


#### Mreža posle skupa ([skript fajl](source/analiza_SNA_3pitanje.R))

Na pitanje o komunikaciji posle skupa odgovoreno je tumačenjem rezultata iz prethodnih analiza.  

Sva izračunavanja pokazuju da je komunikacija drastično smanjena posle skupa: počev od broja čvorova koji su tvitovali, preko broja izolata, procenta čvorova koji čine najveću komponentu, broja komponenti, do prosečne putanje, prečnika i gustine mreže. Četvrti period za neke metrike pokazuje blaga odstupanja od očekivanih vrednosti, ali ne u tolikoj meri kao drugi period.  
Varijacije u stepenu nisu mnogo izražene u periodima posle skupa, centralizovanost mreža je uglavnom manja (osim za centralizovanost prema svojstvenom vektoru), što može da govori o ujednačenijoj komunikaciji od one za vreme trajanja skupa. Što se tiče povezanosti, tranzitivnost jeste nešto veća posle skupa, ali broj i veličina klika i veličina jezgara ne ukazuju na neku "širu" komunikaciju. Oba algoritma za klasterovanje su dala veliki broj klastera za sve periode, što potvrđuje veliku heterogenost mreža. Na kraju, i na grafičkim prikazima mreža može da se prati raspadanje glavne komponente na veći broj manjih.  


#### Analiza sentimenta ([skript fajl](source/analiza_teksta_1pitanje.R))

Opšti utisak o skupu je određen putem analize sentimenta tvitova preko rečnika sentimenta. Paket sentimentr, korišćen za analizu, računa sentimet za celu rečenicu, uzimajući u obzir različite činioce, kao što su negacije i njihov broj, modifikatori značenja, suprotni veznici i granice fraza označene interpunkcijom. U pripremi teksta je bilo neophodno izbaciti pominjanja drugih učesnika, jer je ova konstrukcija pogrešno tumačena kao promena teme od strane funkcije za izračunavanje sentimenta.  

Ovako dobijen najpozitivniji tvit bih ocenila kao donekle pozitivan: `#wielead Whats a good team? All have a skill, but all have to understand, pitch,  product manage. A good team owns all roles! #womenintech` Drugi po redu dosta pominje učenje: `.Links, videos, recommended to learn more about VR, AI, machine learning, and IBM Watson Health. #WIELead` Među prvih deset ima i onih koje bih ocenila pozitivnije i više u vezi sa samom konferencijom: `Congratulations  and  team for another great #WIELead. Learning from best #WIT and making lot of new frien…` ili `Another amazing year!! Thanks to all the #WIElead  #volunteers: your hard work to create an exciting, fun, deep learning …`

Najnegativniji tvit jeste vrlo negativan, iako nisam sigurna iz teksta koliko se baš odnosi na sam skup: `it's the same BS we always got around the mascot issue at FSU. It's depressing and upsetting #wielead`  
Među negativnim tvitovima ima nekoliko pogrešno klasifikovanih: `Huge Potential for Disaster Robotics #WIElead #rescuerobotics:  via`, `That was crazy morning at #wieilc ! Taking picture in the middle of the street ! #traffic #crazy #wielead… `, `#WIELead Leadership: First shatter your inner glass ceiling! Lose your fear  flaws; clear out self-doubt: say hi but Goodbye! #womenintech`
U prvom primeru se radi o pogrešno protumačenom terminu i ovo je moguće ispraviti modifikacijom rečnika. U drugom primeru se radi o neformalnoj upotrebi reči "crazy" sa prenesenim značenjem, dok je u trećem više reči upotrebljeno u pravom značenju, ali u jednoj poetično-alegoričnoj konstrukciji koja u celini uzev nema negativno značenje. Ovako prenesena značenja, kao i ironiju i sarkazam, pristup analizi sentimenta preko rečnika ne može da otkrije i ispravno oceni.
Ipak, među ovima se našlo i nekoliko tvitova koji se odnose baš na govornike i predavanja na skupu, na primer: `keep writing and deleting tweets abt that last presenter. that presentation, esp once I looked up the controversy, really upset me #WIELead`  

Pojedinačne reči koje su najviše doprinele pozitivnom i negativnom sentimentu - [sveukupno](visuals/text1_word_sentiment_contribution.pdf) ili [po periodima](visuals/text1_word_sentiment_contribution_by_period.pdf). Od pozitivnih reči, u periodima pre skupa dominira "learn", nešto ispod su "join" i "sponsor"; u periodu trajanja skupa, najpozitivnije su reči "great", "honored", "learn", "congratulations", dok su u prvom periodu posle skupa to reči "thanks", "great", "learning", "inspiration", i upravo ovo bi moglo da se shvati kao pozitivna ocena nakon održanog skupa.
Među negativnim rečima ima nekih koje izgledaju pogrešno ocenjene, kao "interviewed", a i nekih koje u ovom domenu imaju drugačije znaenje, kao "disruptive".  
Nešto više konteksta dali su [bigrami](visuals/text1_bigram_sentiment_contribution_by_period.pdf): u drugom periodu, "sneak" se odnosi na "(a) sneak peek", što nije negativno, "blocks" na "blocks 9am" (nejasno), "building blocks" (pozitivno) i "roadblocks" (negativno), a "cad" (kao skraćenica) na "cad infrastructure" i "spotlight cad", koji nemaju veze sa značenjem reci "cad" - "nitkov, podlac". U trećem periodu, "stop" se odnosi na "to stop" (nejasno), "stop by" (ne negativno) i "stop doubting" (pozitivno), a "disaster" na "(for) disaster robotics", što nije negativan pojam, ali se sa vrlo negativnom ocenom našao i u najnegativnijem tvitu. Pozitivni bigrami su uglavnom ispravno ocenjeni, osim možda "machine learning" i "our sponsor" koje bih lično ocenila kao neutralne, ali svakako nemaju tako izraženo suprotno značenje kao neki od navedenih negativno ocenjenih bigrama.  

Čak i sa ovakvim rezultatima, gde je negativni sentiment možda preuveličan, intenzitet negativnog sentimenta je mnogo manji nego intenzitet pozitivnog.  

Kada se pogleda sentiment po vremenskim periodima: sumiran, normalizovan brojem tvitova, maksimalan i minimalan rezultat za pojedinačni tvit, moze se primetiti da su sve sume sentimenta pozitivne, što znači da ima mnogo više tvitova ocenjenih pozitivno, ali budući da ukupan rezutat po tvitu nije ograničen, ne može se mnogo detaljnije reći. I po apsolutnoj vrednosti za pojedinačni tvit, pozitivni sentiment je dominantniji: najviši skor po tvitu je 1.9, a najniži -0.8. Gledano po periodima, period trajanja konferencije ima niži normalizovani sentiment od perioda neposredno pre i oba perioda posle skupa, ali u isto vreme, u tom periodu su i najveće vrednosti sentimenta po tvitu, odnosno, najveće su varijacije.  

[Sentiment ključnih reči](visuals/text1_keywords_sentiment_contribution.pdf), izdvojenih iz programa skupa pokazuje da su teme skupa u velikoj meri uticale na to da ukupan sentiment bude izrazito pozitivan. Termin "disaster robotics", iako je pogrešno ocenjen kao negativan, nije imao većeg udela u ukupnom sentimentu. Pokušaj neutralizacije po pet ključnih reči skupa koje su imale najviše udela u pozitivnom i negativnom sentimentu nije doveo do značajne promene u ocenama. Najpozitivniji i najnegativniji tvit su ostali isti kao i pre promene, a i prvih pet po rangu se i ranije nalazilo u prvih deset rangiranih.  

Mislim da može da se zaključi da je stav učesnika o skupu vrlo pozitivan, čak i sa ovako problematičnim podacima i metodom koja ne razlikuje tvitove koji govore o skupu, koliko je dobro ili loše organizovan, koliko je zanimljiv ili kakav je kvalitet predavača, i one koji govore o temama skupa, a koje u ovom slučaju nisu neutralne i pominju liderstvo, inovacije, osnaživanje, ali i predrasude i borbu za veća prava. Ako bi se i moglo reći da su teme skupa takve da su više doprinele pozitivnom nego negativnom sentimentu ("innovation", "learning" nasuprot "bias" ili "fight"), u četvrtom periodu, dakle nakon skupa, najzastupljenije su reči "thanks", "great", "learning" i "inspiration", a najpozitivniji tvit je: `Congratulations  and  team for another great #WIELead. Learning from best #WIT and making lot of new frien…` Među najnegativnijim tvitovima je bilo onih koji su kritikovali predavače ili predavanja, ali nisu ocenjeni najnižim ocenama. I ovde važi primedba da metoda nije dovoljno precizna i da bi rezultati mogli biti i nešto drugačiji primenom drugih metoda. Bez obeleženog trening seta teško je proceniti grešku, jedino što mogu jeste da na osnovu ličnog utiska potvrdim da je mnogo teže naići na negativan nego na pozitivan tvit.  


#### Angažovanost učesnika ([skript fajl](source/analiza_teksta_2pitanje.R))

Interesantnost skupa je procenjena preko učestanosti komuniciranja o skupu, odnosno zastupljenosti tvitova o skupu u odnosu na ukupan broj tvitova u istom periodu.  

Ovim je pokušano da se dođe do odgovora na pitanje da li je konferencija bila dovoljno zanimljiva da okupira pažnju učesnika, ili su im neki drugi problemi bili inspirativniji za tvitovanje. U ovoj analizi je posmatran samo period od dva dana trajanja skupa, jer se samo za taj period može "zahtevati" veća posvećenost učesnika. Što se tiče tematskog obuhvata tvitova, ovde nije reč samo o temama skupa, nego o svim stranama skupa, uključujući organizaciju, druženje, prateće sadržaje.. Iz ovog razloga, posmatrali bi se svi tvitovi kojoi su označeni heštegom skupa ili pominju organizatora skupa. Međutim, zbog problema sa formatom prilikom preuzimanja tvitova, tekst tvitova je ostao podsečen, pa nije moguće osloniti se samo na ove oznake za klasifikaciju, nego je ovaj uslov proširen još nekim ključnim rečima, ali ostaje mogućnost da bi bolja klasifikacija dovela do drugačijih rezultata.  

U proseku, učesnici su u toku dva dana trajanja konferencije 8,9 od 10 tvitova posvetili drugim temama, što bih ocenila kao prilično slab rezultat. Ipak, treba imati u vidu problem sa klasifikacijom, ali i činjenicu da se ovde učesnicima smatraju svi koji su tvitovali o konferenciji, što može da uključi, na primer, i medije koji su samo najavljivali skup.  

[Posmatrano po učesniku](visuals/text2_participants_activity.pdf), veliki je broj onih koji su jako malo tvitovali o skupu, ali mnogo o drugim temama. Upravo za ovaj deo "učesnika", sa procentom tvitova o skupu svega nešto većim od nule, moglo bi da se pretpostavi da je samo preneo vest o skupu ili retvitovao zanimljiv tvit nekog od učesnika. Više od trećine posmatranih učesnika ima manje od 10% tvitova o konferenciji u toku ova dva dana, isto tako, trećina ima između 50% i 100% tvitova o konferenciji. Ove brojke daju malo jasniju sliku o prvom rezultatu o 11% tvitova o konferenciji. Učesnici sa najvećim procentom su uglavnom učesnici sa malim ukupnim brojem tvitova koji su pritom i ispravno klasifikovani. Dve posledice neprecizne klasifikacije tvitova o konferenciji su to što se ne može reći koliko je jos učesnika sa po jednim ili dva tvita koji su pogrešno klasifikovani (a učesnici bi možda imali stoprocentnu "angažovanost"), kao i to da organizatori skupa i drugi učesnici sa većim brojem tvitova o skupu imaju nešto niži ovaj količnik.  


#### Upotreba tema skupa ([skript fajl](source/analiza_teksta_3i4pitanje.R))

Koliko su teme skupa nove za učesnike mereno je preko zastupljenosti tema skupa u tvitovima učesnika pre i tokom skupa. Koliko one ostaju predmet interesovanja učesnika mereno je preko zastupljenosti tema u tvitovima tokom i posle skupa.  

Teme skupa su identifikovane u programu skupa, a zastupljenost tema u tvitovima se merila poklapanjem reči i bigrama iz tvitova sa ključnim rečima i bigramima određenih tema.  
U vezi sa interpretacijom rezultata, moguć je problem zakljucivanja o znanju na osnovu upotrebe. U nekom opštem slučaju, upotreba ključnih reči može da ukaže na neko poznavanje teme, ali neupotreba ne ukazuje na nepoznavanje. U konkretnom slučaju, moguće je da učesnici skupa u vremenskom periodu od mesec dana pre skupa (koliko se posmatra u ovoj analizi) jednostavno nisu tvitovali o temama koje su im inače poznate, što bi dovelo do pogrešnog zaključka da su im teme nove. Dalje, treba imati na umu da je vrlo moguće da učesnici tvituju o predstojećem skupu unapred, na primer retvitovanjem najava predavanja, što bi navelo na zaključak da su im teme od ranije poznate. Osim toga, velika je verovatnoća da su učesnici dobro upoznati sa širom temom skupa, a neke nove informacije bi mogle biti samo nijanse koje je teško uočiti i izmeriti.  
Drugi problem je u vezi sa predstavljanjem tema skupa preko reči i bigrama iz naslova predavanja, što može da dovede do toga da tvit govori o temama skupa, ali to na osnovu ključnih reči ne može da se prepozna.  

Otkrivanje tema skupa u tvitovima je, posle nekoliko pokušaja, izvršeno tako da se reči bigrama teme traže u celom tekstu tvita, odnosno ne uslovljavajći poziciju reči u tvitu, i ne uslovljavajući redosled reči; osim toga, poklapanje je vršeno po osnovama reči (tako je, na primer, "disaster robotics" moglo da bude prepoznato u tvitu koji pominje "disaster robotics" ili "robots for disasters").  

Na ovaj način je prepoznata upotreba tema skupa kod 369 učesnika (od ukupno 457); od svih učesnik-bigram kombinacija, 31% se odnosi na upotrebu bigrama u toku skupa, a čak 88% od toga čini upotreba tog bigrama samo jednom. Ovo je važno jer se i broj novih upotreba tema, kao i procenat tema koje se koriste i posle skupa, mere upravo prema trećem periodu, što bi značilo da su teme koje su upotrebljene samo u trećem periodu i "naučene" i "zaboravljene" posle skupa.  

Ako se posmatraju teme skupa zajedno, odnosno koliko su sve teme skupa zastupljene u tvitovima nekog učesnika, onda može da se posmatra broj različitih bigrama (broj tema) ili suma pojavljivanja bigrama (zastupljenost tema skupa u tvitovima). Ako bi se tragalo za nekom frazom koju je neki učesnik "naučio", odnosno prvi put upotrebio u vreme skupa ili koju je koristio i nakon skupa, onda bi se posmatrale sve kombinacije učesnik-bigram posebno.  

Radi lakšeg računanja, periodi pre i posle skupa su posmatrani kao po jedna kolona, uzimajuci za vrednost celi deo srednje vrednosti. Rezutati "učenja" su dobijeni razlikom broja n-grama upotrebljenih u periodu trajanja skupa i pre skupa. Pozitivne vrednosti predstavljaju broj "naučenih" n-grama, negativne broj "zaboravljenih", dok 0 predstavlja nepromenjenu upotrebu, bilo da se radi o jednakom broju korišćenih n-grama, bilo da se radi o nuli u oba perioda. Kod "zadržanog znanja", željeni rezultat je jednako korišćenje u periodu skupa kao i posle njega, pa je tu važno razlikovati jednaku upotrebu i jednaku neupotrebu, tj. 0-0 i 1-1. Zbog toga je "zadržano znanje" mereno količnikom, gde je jednaka neupotreba (0/0) dala NaN, jednaka upotreba 1, "naknadno učenje" interval (1, Inf), npr (posle:1/tokom:0) ili (posle:3/tokom:1),  a "zaboravljanje" interval (0,1), npr. (posle:0/tokom:1) ili (posle:3/tokom:6), pri čemu je "zaboravljanje" manje što je količnik bliže 1.  

Ako se pogleda [raspodela pokazatelja "naucenih" bigrama](visuals/text3_bigrams_learned_index.pdf) po učesnicima, 36% (135) učesnika nije "naučilo" nijedan bigram, a 38% (143) je u vreme skupa upotrebilo jedan bigram više ili manje u odnosu na period pre skupa. Može se reći da po ovom pitanju nema neke značajne promene u korišćenju bigrama za vreme skupa u odnosu na period pre skupa (mereno preko kljucčih reci iz naslova predavanja). Ako bi se uzeli u obzir svi učesnici, uključujući i one za koje nije otkrivena upotreba tema skupa, procenat učesnika koji su upotrebili jednak broj tema (ili nijednu) pre i tokom skupa je ~49%, dok je procenat onih koji su upotrebili jednu temu manje ili više ~31%.  

Ako se pogledaju rezultati za ["učenje" pojedinačnih n-grama](visuals/text3_bigram_frequency.pdf), prosečan bigram nije upotrebljavan više u toku skupa u odnosu na period pre skupa, a značajan je i broj bigrama koji su upotrebljeni jednom više u toku skupa.  

Za vizuelizaciju upotrebe tema posle skupa, vrednosti su prilagođene tako da se NaN zameni sa min-1, a Inf sa max+1.  

Kod [upotrebe različitih tema posle skupa](visuals/text3_retained_use.pdf) ima 119 NA, - "jednake neupotrebe" (0/0), odnosno toliko učesnika nije upotrebilo nijedan bigram ni tokom ni posle skupa (ovo bi teoretski trebalo da se poklopi sa brojem učesnika koji su upotrebili bigram pre skupa, a tokom i posle skupa nisu, odnosno sa onima koji na grafikonu za "learned" index imaju negativan skor; međutim, pošto sam uzimala celobrojni deo proseka upotrebe u periodima 1 i 2, učesnici koji su samo u jednom periodu upotrebili jedan bigram, imaju i za tu vrednost nulu, isto kao i za period posle skupa); 140 učesnika je "zaboravilo" sve bigrame, odnosno ne koristi ih posle skupa, dok 26 njih pokazuje različite stepene smanjene upotrebe; 28 njih koristi isti broj bigrama i posle i tokom skupa; 38 učesnika ima vrednost Inf, još 18 ima skor > 1, odnosno oni koriste više bigrama u periodu posle skupa. Ako bi se ovim učesnicima dodali i oni za koje nije otkrivena upotreba tema skupa (oni bi pripadali grupi "jednake neupotrebe"), moglo bi se reći da ~76% svih učesnika nije upotrebilo nijedu temu posle skupa (bez obzira na to da li su ih koristili u toku skupa ili ne).  

Što se tiče [upotrebe pojedinačnih bigrama](visuals/text3_retained_frequency.pdf), u 822 slučaja učesnici nisu upotrebili bigram ni tokom ni posle skupa; u 527 slučajeva su učesnici "zaboravili" bigrame koje su koristili u vreme skupa, a u 10 slučajeva su ih manje koristili; u 13 slučajeva su korisnici u jednakoj meri koristili neki bigram tokom i posle skupa; u 67 slučajeva, učesnici su posle skupa počeli da koriste određene bigrame. Moglo bi se reći da preko 90% bigrama, odnosno tema skupa, nije upotrebljeno od strane pojedinačnih učesnika posle skupa (bilo da je upotrebljeno u toku skupa ili ne).  



### Zaključci analize

Analizom mreže je ustanovljeno da veliki broj učesnika nije komunicirao pre skupa i da je tokom skupa ostvario nove kontakte, međutim, po završetku skupa komunikacija je bila na nivou od pre skupa, dakle kontakti nisu ostali trajni. Komunikacija u toku skupa nije bila ujednačena, mreža je u dobroj meri centralizovana, odnosno ima manji broj centralnih učesnika, a u kombinaciji sa malom tranzitivnošću i rezultatima o klikama i jezgrima, može se reći da se učesnici nisu u većoj meri povezali međusobno, i da struktura mreže može da odgovara drvolikoj. Ovakva struktura bi mogla da objasni brzo raspadanje mreže posle skupa. Ipak, izdvajanje centralnog učesnika, tj. organizatora, iz mreže (u periodu trajanja skupa), nije dovelo do razbijanja mreže na komponente, iako je jedan manji broj učesnika ostao nepovezan. Ovo bi odgovaralo strukturi sa nekoliko centralnih čvorova i potvrdilo metriku centralizovanosti (0.47-0.6). Takođe, veliki broj identifikovanih klastera govori o velikoj podeljenosti mreže i heterogenosti komunikacije, što takođe objašnjava izostajanje komunikacije posle skupa.  

Analiza teksta je otkrila više "tehničkih" problema. Analizom sentimenta se došlo do vrlo pozitivne ocene skupa, mada je prilično teško bilo razdvojiti sentiment o temama skupa i sentiment o samom skupu, a u ovom slučaju, teme skupa su predstavljene rečima koje imaju visoke ocene sentimenta. I pored ove pozitivne ocene, analiza aktivnosti učesnika skupa je otkrila da su u toku dva dana trajanja skupa učesnici u proseku tek 11% tvitova posvetili konferenciji. Ipak, i ovde ima nekoliko detalja: ovaj rad je sve one koji su ispunili uslove o tvitovanju o konferenciji u određenom periodu smatrao učesnicima, iako je verovatno bilo onih koji su samo preneli po jednu vest o održavanju skupa i za koje se i ne može očekivati bilo kakva angažovanost (merena procentom tvitova o skupu). Drugi detalj se odnosi na klasifikaciju tvitova na one koji govore / ne govore o skupu i na problem sa podsečenim tekstom preuzetih tvitova: zbog ovog problema sa formatom tvitova, nije moguće osloniti se samo na hešteg skupa za klasifikaciju. Klasifikacija je zato proširena još nekim ključnim rečima, ali je ostalo još mesta za precizniju klasifikaciju, što bi možda dovelo do drugačijih rezultata nekih analiza. Na kraju, putem zastupljenosti tema skupa u periodima pre, tokom i posle skupa, došlo se do toga da teme skupa nisu u nekoj većoj meri uticale na učesnike: 49% učesnika je upotrebilo jednak broj tema i pre i tokom skupa, dok je 31% upotrebio prosečno jednu temu manje ili više u toku skupa. Posle skupa, 76% učesnika nije više upotrebilo nijednu temu, bez obzira na to da li su ih upotrebljavali tokom skupa ili ne. Ovde ostaje primedba da je moguće da bi bolje identifikovanje tema skupa dovelo do drugačijih rezultata.  

I pored određenih problema prilikom analize teksta tvitova, ovi rezultati se slažu sa rezultatima analize mreže skupa: komunikacija je ostvarena, učesnici su tvitovali i o temama skupa, ali kao što je povezanost čvorova uglavnom posredna i preko malog broja veza, tako je i angažovanost učesnika relativno mala, i nema nekog većeg korišćenja tema skupa. I pored pozitivne ocene skupa iz analize sentimenta, nakon skupa učesnici uglavnom nisu zadržali ostvarene kontakte, niti su upotrebljavali teme skupa, što bi moglo da ukaže na to da konferencija nije imala većeg uticaja na učesnike, odnosno nema vidljivih i jasnih znakova o ovom uticaju.  



***
Ovaj projekat predstavlja ispitni rad za ispite Softverska analiza društvenih mreža i Primene veštačke inteligencije na grupi Softversko inženjerstvo i računarske nauke master studija Fakulteta organizacionih nauka u Beogradu.
