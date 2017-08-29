### Analiza
### Kombinovane metode analize drustvenih mreza i analize teksta


### 4. Ko su centralni ucesnici i kako uticu na teme komunikacije? 

### Ovde mi je cilj da identifikujem centralne cvorove mreze, da bih u kasnijoj analizi teksta mogla da pratim 
### teme u njihovim tvitovima kroz vreme. Centralni cvorovi mogu da imaju veliki uticaj na ostale cvorove, pa bi
### moglo biti zanimljivo proveriti taj uticaj u konkretnom ponasanju - tvitovanju o odredenoj temi. Cvorovi 
### centralni po svom stepenu ovde su oni koji imaju najvise kontakata, uz razliku izmedu centralnosti dolaznog
### stepena (veliki prestiz, kao npr. organizatori skupa) i odlaznog stepena (uticaj putem kvantiteta komunikacije). Cvorovi centralni po 
### bliskosti su najblizi svim cvorovima i mogu najbrze da prosire informacije, na primer, mogu da odvuku komunikaciju
### na novu temu. Centralnost intermedijarnosti je karakteristican za cvorove koji povezuju delove mreze koji bi
### inace bili nepovezani, sto bi mogle biti grupe ucesnika iz razlicitih sfera, odnosno tvitovi o razlicitim 
### temama. Na kraju, centralnost po svojstvenom vektoru karakterise ucesnike koji su povezani sa uticajnim ucesnicima;
### njihov uticaj bi mogao medusobno da se dopunjuje ili blokira. 


conf_centralities <- function(n){
  cent <- list()
  lapply(conf_graph_list, function(x) {
    cent$b_dir <- centr_betw(x, directed =  TRUE)$res
    cent$b_undir <- centr_betw(x, directed = FALSE)$res
    cent$c_all <- centr_clo(x, mode = "all")$res
    cent$c_in <- centr_clo(x, mode = "in")$res
    cent$c_out <- centr_clo(x, mode = "out")$res
    cent$d_all <- centr_degree(x, mode = "all")$res
    cent$d_in <- centr_degree(x, mode = "in")$res
    cent$d_out <- centr_degree(x, mode = "out")$res
    cent$e_dir <- centr_eigen(x, directed = TRUE)$vector
    cent$e_undir <- centr_eigen(x, directed = FALSE)$vector
    cent <- lapply(cent, function(y) {
      names(y) <- V(x)$name
      return(sort(y, decreasing = TRUE)[1:n])
    })
  })
}
cc <- conf_centralities(2)

unlist(cc)
# ovo mora nekako da se stampa... ubaci u matricu? df?

