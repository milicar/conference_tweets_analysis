# Analiza tvitova sa konferencije 



### Cilj 

Cilj ovog projekta je pokušaj merenja uspešnosti naučne konferencije/skupa na osnovu tvitova učesnika. Osim analize sentimenta tvitova, koji bi dali ocenu opšteg utiska učesnika, rad će pokušati da izmeri i stvarnu "dobit" učesnika, koja bi se ogledala u ostvarenim kontaktima i novim informacijama. Novi kontakti će se pratiti kroz ostvarenu komunikaciju primenom metoda analize društvenih mreža, a uticaj izlaganja i predavanja putem analize teksta tvitova. Obe vrste analiza će biti sprovedene nad podacima podeljenim na vremenske segmente pre, tokom i posle održavanja skupa, tako da bi uočene promene govorile o dobiti učesnika.  



### Pitanja

Analizom mreže koju čine učesnici skupa, rad će pokušati da odgovori na sledeća pitanja:

* Da li su učesnici komunicirali i pre skupa ili ostvarena komunikacija predstavlja nove kontakte?
* Da li svi učesnici ujednačeno komuniciraju ili se stvaraju grupe sa intenzivnijom komunikacijom?
* Kolika je komunikacija posle skupa i da li je povezana sa grupama?

Analizom teksta tvitova, rad će pokušati da otkrije sledeće:

* Koji je opšti utisak o skupu?
* Koliko je skup interesantan za učesnike?
* Koliko su informacije nove za učesnike?
* Koliko teme skupa ostaju predmet interesovanja učesnika posle skupa?
* Koje su teme posebno interesantne za učesnike?

Kombinovanjem ovih metoda, rad će pokušati da utvrdi:

* Ko su centralni učesnici i kako su povezani sa temama komunikacije?
* Da li "angažovaniji učesnici" imaju veću "informacionu dobit"? 



### Podaci

U radu su korišćeni podaci o tvitovima i učesnicima IEEE Women in Engineering International Leadership Conference, konferencije održane 22-23.05.2017. godine. Prvo su u periodu 22.05-27.05.2017. prikupljeni tvitovi sa ključnim rečima #WIELEad i @wieilc (oznaka skupa i organizator), a zatim je određen okvir od tri dana pre do tri dana posle skupa, kao prošireni period trajanja skupa, koji dozvoljava i nekim učesnicima koji su tvitovali sa heštegom konferencije, ali ne u toku samog skupa, da budu uključeni u analizu. Za sve učesnike koji su tvitovali unutar tog perioda preuzeti su svi tvitovi, odnosno maksimalan broj tvitova koji twitterAPI dozvoljava (3200). Od ukupnog broja učesnika, u analizu nisu ušli učesnici koji su obrisali nalog, podesili nalog kao 'protected', ili nisu imali dostupne tvitove počev od 21.04.2017. godine. Konačan broj učesnika koji je obuhvaćen analizom je 457. Na kraju, tako prikupljeni tvitovi su podeljeni na pet vremenskih odeljaka, dva odeljka od po dve nedelje pre skupa, odeljak trajanja skupa, uključujući i tri dana pre i tri dana posle, ukupno osam dana, i dva odeljka od po dve nedelje posle skupa. Ovakva vremenska podela je posledica pretpostavke da će uticaj skupa postepeno da se smanjuje i da period posle skupa ne može da se posmatra kao jedna celina. Ukupan period koji je ušao u analizu je 21.04-23.06.2017.

Za preuzimanje tvitova su korišćene funkcije iz paketa twitteR (v 1.1.9).
Svi tvitovi se nalaze u direktorijumu 'data', kao serijalizovani R objekti:
* startertweets sadrži tvitove prikupljene za vreme trajanja skupa; ~130kB
* all_tweets 1:4 sadrže tvitove prikupljane po učesniku (fajl je podeljen na 4 dela samo zbog ograničenja postavljanja velikih fajlova na GitHub); ~82MB

Za kreiranje mreže i mrežne analize su korišćene funkcije iz paketa igraph (v 1.1.2). Matrica povezanosti je kreirana tako da se svako pominjanje jednog učesnika u tvitu drugog posmatra kao usmerena veza, a težina veze predstavlja broj pominjanja.  



### Analiza

Na pitanje o novoostvarenim kontaktima odgovoreno je pomoću sledećih metrika:  
* gustina mreže, merena ne uzimajući u obzir težine veza, tako da ne odražava intenzitet komunikacije, nego broj kontakata
* broj komponenti i veličina najveće komponente, gde slabe komponente predstavljaju povezanost učesnika na bilo koji način, jake komponente uzimaju u obzir smer veza, a određene su i komponente za neusmeren graf, pri čemu je konverzija vršena tako da veza između učesnika predstavlja dvosmernu komunikaciju u usmerenom grafu
* prosečna putanja i prečnik mreže, koji takođe govore o povezanosti mreže  

Za merenje homogenosti mreže u odnosu na komunikaciju korišćene su sledeće metrike:  
* distribucija stepena i varijantnost stepena, odnosno razlika u broju kontakata između pojedinačnih učesnika
* koeficijent klasterovanja, odnosno tranzitivnost mreže, koja govori o lokalnoj povezanosti učesnika
* broj i veličina najvećih i maksimalnih klika i veličina jezgara, takođe u vezi sa lokalnom povezanošću učesnika
* centralizovanost mreže, koja, zajedno sa prethodnim metrikama, ukazuje na strukturu mreže
* određivanje klastera, koji govore o podeljenosti mreže
* analiza mreže bez glavnog organizatora (čvor sa najvećim stepenom)  

Na pitanje o komunikaciji posle skupa odgovoreno je tumačenjem rezultata iz prethodnih analiza.  
* Koji je opšti utisak o skupu?
* Koliko je skup interesantan za učesnike ili koliko ih angažuje?
* Koliko su informacije nove za učesnike?
* Koliko teme skupa ostaju predmet interesovanja učesnika posle skupa?
* Koje su teme posebno interesantne za učesnike?  

U analizi teksta odrediće se opšti utisak o skupu putem analize sentimenta tvitova.  
Interesantnost skupa će se meriti preko učestanosti komuniciranja o skupu, odnosno zastupljenosti tvitova o skupu u odnosu na ukupan broj tvitova u istom periodu.  
Koliko su teme skupa nove za učesnike meriće se preko zastupljenosti tema skupa u tvitovima učesnika pre i tokom skupa. Ovo možda neće biti u potpunosti moguće, budući da korisnici Tvitera pre skupa često tvituju o temama koje će biti zastupljene na skupu, bez obzira na to da li im je tema poznata ili ne, ili retvituju najave predavanja, pa se neke ključne reči vezane za nove informacije koje će tek dobiti na skupu, mogu naći i u tvitovima pre skupa. Osim toga, velika je verovatnoća da su učesnici skupa već dobro upoznati sa širom temom skupa, a neke nove informacije bi predstavljale tek nijanse koje je teže uočiti.  
Koliko teme skupa ostaju predmet interesovanja učesnika posle skupa meriće se putem zastupljenosti tema skupa u tvitovima učesnika tokom i posle skupa.  
Teme posebno interesantne za učesnike identifikovaće se preko ključnih reči i preko heštegova koji se pojavljuju zajedno sa konferencijskim heštegom - sa najvećom učestanosti i kod najvećeg broja učesnika.  


U objedinjenoj analizi odrediće se centralni učesnici prema četiri mere centralnosti, a kasnije će se videti njihova povezanost sa temama komunikacije.  
Na kraju, analiziraće se korelacija između centralnosti učesnika i rezultata o "informacionoj dobiti". Informaciona dobit bi predstavljala bilo koju informaciju (ključnu reč) koja bi bila nova za učesnika skupa, a koja bi se detektovala i u periodu posle skupa kao nešto što je ostavilo dovoljno jak utisak da učesnik skupa tvituje o tome i nakon skupa.  


Teme skupa će biti identifikovane u programu skupa, a zastupljenost tema u tvitovima će se meriti poklapanjem reči iz tvitova sa ključnim rečima određenih tema.  



### Zaključci analize

Analizom mreže je ustanovljeno da veliki broj učesnika nije komunicirao pre skupa i da je tokom skupa ostvario nove kontakte, međutim, po završetku skupa komunikacija je bila na nivou od pre skupa, dakle kontakti nisu ostali trajni. Komunikacija u toku skupa nije bila ujednačena, mreža je u dobroj meri centralizovana, odnosno ima manji broj centralnih učesnika, a u kombinaciji sa malom tranzitivnošću i rezultatima o klikama i jezgrima, može se reći da se učesnici nisu u većoj meri povezali međusobno, i da struktura mreže može da odgovara drvolikoj. Ovakva struktura bi mogla da objasni brzo raspadanje mreže posle skupa. Ipak, izdvajanje centralnog učesnika, tj. organizatora, iz mreže (u periodu trajanja skupa), nije dovelo do razbijanja mreže na komponente, iako je jedan manji broj učesnika ostao nepovezan. Ovo bi odgovaralo strukturi sa nekoliko centralnih čvorova i potvrdilo metriku centralizovanosti (0.47-0.6). Takođe, veliki broj identifikovanih klastera govori o velikoj podeljenosti mreže i heterogenosti komunikacije, što takođe objašnjava izostajanje komunikacije posle skupa.  



***
Ovaj projekat predstavlja ispitni rad za ispite Softverska analiza društvenih mreža i Primene veštačke inteligencije na grupi Softversko inženjerstvo i računarske nauke master studija Fakulteta organizacionih nauka u Beogradu.
