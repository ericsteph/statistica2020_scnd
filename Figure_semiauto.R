rm(list=ls())

library(tidyverse)
library(data.table)
library(zoo)
source(file = "03.FUN.graph.R")

### 00. Carico i dataset

# Dati della SPO
url01 <- "https://gibonet.gitlab.io/cubi_esterni/SPO_trimestrale/spo.rds"
download.file(url01, destfile = basename(url01), mode = "wb")
spo <- readRDS(basename(url01))
spo

str(spo)

spo$sesso <- parse_factor(spo$sesso)
levels(spo$sesso)

# Dati della STAF
url02 <- "https://gibonet.gitlab.io/cubi_esterni/STAF/px-x-0302010000_105.rds"
download.file(url02, destfile = basename(url02), mode = "wb")
staf <- readRDS(basename(url02))
staf

str(staf)

# Dati della STATIMP
url03 <- "https://gibonet.gitlab.io/cubi_esterni/STATIMP/px-x-0602000000_102.rds"
download.file(url03, destfile = basename(url03), mode = "wb")
statimp <- readRDS(basename(url03))
statimp

str(statimp)


# Creo Dataset con le stesse dimensioni
max_anno <- max(max(spo$anno), max(staf$anno), max(statimp$anno))
               
spo <- spo %>% filter(anno >= max_anno - 13)
staf <- staf %>% filter(anno >= max_anno - 13)
statimp <- statimp %>% filter(anno >= max_anno - 13)

# Più variabili
spo0 <- spo
spo <- spo0 %>%
  pivot_wider(names_from = sesso, values_from = valore_1000) %>%
  mutate(Qu_donne_perc = ( Donne / Totale ) * 100) %>%
  pivot_longer(cols = c(Totale:Qu_donne_perc), names_to = "sesso", values_to = "values")
           
spo <- spo %>%
  pivot_wider(names_from = grande_regione, values_from = values) %>%
  mutate(Qu_ticino_perc = ( Ticino / Svizzera ) * 100) %>%
  pivot_longer(cols = c(Svizzera:Qu_ticino_perc), names_to = "geo", values_to = "values")

spo <- spo %>%
  group_by(geo, sesso) %>%
  mutate(delta_year = values - lag(values, n = 4L), delta_trim = values - lag(values, n = 1L),
         delta_year_perc = (delta_year/values) * 100, delta_trim_perc = (delta_trim / values) * 100) %>%
  pivot_longer(cols = c(values:delta_trim_perc), names_to = "variabili", values_to = "values") %>%
  ungroup()

spo <- spo %>%
  group_by(geo, sesso, variabili) %>%
    mutate(max.value = fifelse(values != 0, max(values, na.rm = TRUE), values)) %>%
    mutate(min.value = fifelse(values != 0, min(values, na.rm = TRUE), values)) %>%
ungroup()

spo <- data.table(spo)


staf0 <- staf
colnames(staf0) <- c("Trimestre", "anno", "trimestre", "Divisione", "Canton.de.travail", "Sexe", "value")

staf <- staf0 %>%
  filter(Divisione == "Division économique - Total") %>%
  select(-Divisione) %>%
  pivot_wider(names_from = Sexe, values_from = value) %>%
  mutate(Qu_donne_perc = ( `Femme` / `Sexe - Total` ) * 100) %>%
  pivot_longer(cols = c(`Sexe - Total`:Qu_donne_perc), names_to = "sesso", values_to = "values")

staf <- staf %>%
  pivot_wider(names_from = Canton.de.travail, values_from = values) %>%
  mutate(Qu_ticino_perc = ( Ticino / Suisse ) * 100) %>%
  pivot_longer(cols = c(Suisse:Qu_ticino_perc), names_to = "geo", values_to = "values")

staf <- staf %>%
  group_by(geo, sesso) %>%
  mutate(delta_year = values - lag(values, n = 4L), delta_trim = values - lag(values, n = 1L),
         delta_year_perc = (delta_year/values) * 100, delta_trim_perc = (delta_trim / values) * 100) %>%
  pivot_longer(cols = c(values:delta_trim_perc), names_to = "variabili", values_to = "values")

staf <- staf %>%
  group_by(geo, sesso, variabili) %>%
  mutate(max.value = fifelse(values != 0, max(values, na.rm = TRUE), values)) %>%
  mutate(min.value = fifelse(values != 0, min(values, na.rm = TRUE), values)) %>%
  ungroup()



staf <- data.table(staf)

statimp0 <- statimp
statimp <- statimp0 %>%
  pivot_wider(names_from = Sesso, values_from = value) %>%
  mutate(Qu_donne_perc = ( Donna / `Sesso - Totale` ) * 100) %>%
  pivot_longer(cols = c(`Sesso - Totale`:Qu_donne_perc), names_to = "sesso", values_to = "values") %>%
  
  pivot_wider(names_from =  Tasso.di.occupazione, values_from = values) %>%
  mutate(Qu_tmp_prz = ( `Tempo partziale` / `Tasso di occupazione - Totale` ) * 100) %>%
  pivot_longer(cols = c(`Tasso di occupazione - Totale`:Qu_tmp_prz), names_to = "tasso.occ", values_to = "values") %>%
  
  pivot_wider(names_from =  Settore.economico, values_from = values) %>%
  mutate(Qu_stt_trz = ( `Settore 3` / `Settori 2-3` ) * 100) %>%
  pivot_longer(cols = c(`Settori 2-3`:Qu_stt_trz), names_to = "settore", values_to = "values")

statimp <- statimp %>%
  pivot_wider(names_from = Grande.regione, values_from = values) %>%
  mutate(Qu_ticino_perc = ( Ticino / Svizzera ) * 100) %>%
  pivot_longer(cols = c(Svizzera:Qu_ticino_perc), names_to = "geo", values_to = "values")

statimp <- statimp %>%
  group_by(geo, sesso, tasso.occ, settore) %>%
  mutate(delta_year = values - lag(values, n = 4L), delta_trim = values - lag(values, n = 1L),
         delta_year_perc = (delta_year/values) * 100, delta_trim_perc = (delta_trim / values) * 100) %>%
  pivot_longer(cols = c(values:delta_trim_perc), names_to = "variabili", values_to = "values")

statimp <- statimp %>%
  group_by(geo, sesso, tasso.occ, settore, variabili) %>%
  mutate(max.value = fifelse(values != 0, max(values, na.rm = TRUE), values)) %>%
  mutate(min.value = fifelse(values != 0, min(values, na.rm = TRUE), values)) %>%
  ungroup()

statimp <- data.table(statimp)


### Ora possiamo fare i grafici

# Classici
### SPO
# P.1: SPO, occupati in Svizzera e in Ticino
primo_anno <- max_anno - 8
titolo <- "Persone occupate (concetto interno), in Svizzera e in Ticino,\nper trimestre, dal"
sottotitolo <- "Dati in migliaia"
fonte <- "Fonte: Statistica delle persone occupate (SPO), UST, Neuchâtel"
cadenza <- 2

tmp <- spo %>%
  filter(variabili == "values", sesso == "Totale", geo %in% c("Svizzera", "Ticino"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo))


p1 <- plot_classic_abs(tmp, "geo",
                   NULL,
                   titolo,
                   sottotitolo,
                   fonte,
                   cadenza,
                   scales = "free_y"
                   )

p1

# P.2: SPO, occupati secondo il sesso, in Ticino
titolo <- "Persone occupate (concetto interno), secondo il genere,\nin Ticino, per trimestre, dal"

tmp <- spo %>%
  filter(variabili == "values", sesso %in% c("Uomini", "Donne"), geo == "Ticino", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso))

p2 <- plot_classic_abs(tmp, "sesso",
                       80,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza)
p2

# P.3: SPO, occupati secondo il sesso, in Svizzera
titolo <- "Persone occupate (concetto interno), secondo il genere,\nin Svizzera, per trimestre, dal"

tmp <- spo %>%
  filter(variabili == "values", sesso %in% c("Uomini", "Donne"), geo == "Svizzera", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso))

p3 <- plot_classic_abs(tmp, "sesso",
                       2000,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza)
p3

# P.4: Quota di persone occupate donna, in Svizzera e in Ticino
titolo <- "Donne occupate (concetto interno), in Svizzera e in Ticino, per trimestre, dal"
sottotitolo <- "Quota: dati in percentuale"

tmp <- spo %>%
  filter(variabili == "values", sesso == "Qu_donne_perc", geo %in% c("Svizzera", "Ticino"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo))


p4 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

p4

# P.5: Tasso di crescita delle persone occupate di genere femminile, in Svizzera e in Ticino
titolo <- "Evoluzione del numero di persone occupate (concetto interno), donne,\nin Svizzera e in Ticino, per trimestre, dal"
sottotitolo <- "Tasso di crescita: dati in percentuale"

tmp <- spo %>%
  filter(variabili == "delta_year_perc", sesso == "Donne", geo %in% c("Svizzera", "Ticino"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo))


p5 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

p5


# P.6: Tasso di crescita delle persone occupate, secondo il genere, in Ticino
titolo <- "Evoluzione del numero di persone occupate (concetto interno),\nsecondo il genere, in Ticino, per trimestre, dal"

tmp <- spo %>%
  filter(variabili %in% "delta_year_perc", sesso %in% c("Donne", "Uomini"), geo %in% "Ticino", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso))


p6 <- plot_classic_abs(tmp, "sesso",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

p6


# P.7: Tasso di crescita delle persone occupate, secondo il genere, in Svizzera
titolo <- "Evoluzione del numero di persone occupate (concetto interno),\nsecondo il genere, in Svizzera, per trimestre, dal"

tmp <- spo %>%
  filter(variabili %in% "delta_year_perc", sesso %in% c("Donne", "Uomini"), geo %in% "Svizzera", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso))


p7 <- plot_classic_abs(tmp, "sesso",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

p7


# P.8: Quota di persone occupate in Ticino rispetto al totale (concetto interno), secondo il genere
titolo <- "Quota di persone occupate in Ticino (concetto interno),\nsecondo il genere, per trimestre, dal"
sottotitolo <- "Quota: dati in percentuale"

tmp <- spo %>%
  filter(variabili %in% "values", sesso != "Qu_donne_perc", geo %in% "Qu_ticino_perc", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso))


p8 <- plot_classic_abs(tmp, "sesso",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

p8


### STAF
# F.1: Frontalieri in Svizzera e in Ticino
primo_anno <- max_anno - 13
titolo <- "Frontalieri, in Svizzera e in Ticino, per trimestre, dal"
sottotitolo <- "Dati in migliaia"
fonte <- "Fonte: Statistica dei frontalieri (STAF), UST, Neuchâtel"
cadenza <- 2

tmp <- staf %>%
  filter(variabili %in% "values", sesso %in% "Sexe - Total", geo %in% c("Suisse", "Ticino"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()

f1 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza,
                       scales = "free_y"
)

f1

# F.2: STAF, frontalieri secondo il sesso, in Ticino
titolo <- "Frontalieri, secondo il genere, in Ticino, per trimestre, dal"

tmp <- staf %>%
  filter(variabili %in% "values", sesso %in% c("Homme", "Femme"), geo == "Ticino", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso)) %>%
  as.data.table()

f2 <- plot_classic_abs(tmp, "sesso",
                       15000,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza)
f2

# F.3: STAF, frontalieri secondo il sesso, in Svizzera
titolo <- "Frontalieri, secondo il genere, in Svizzera, per trimestre, dal"

tmp <- staf %>%
  filter(variabili %in%  "values", sesso %in% c("Homme", "Femme"), geo == "Suisse", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso)) %>%
  as.data.table()

f3 <- plot_classic_abs(tmp, "sesso",
                       80000,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza)
f3

# F.4: Quota di persone occupate donna, in Svizzera e in Ticino
titolo <- "Quota di frontalieri donna, in Svizzera e in Ticino,\nper trimestre, dal"
sottotitolo <- "Quota: dati in percentuale"

tmp <- staf %>%
  filter(variabili %in% "values", sesso %in% "Qu_donne_perc", geo %in% c("Suisse", "Ticino"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  data.table()


f4 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

f4

# F.5: Tasso di crescita dei frontalieri di genere femminile, in Svizzera e in Ticino
titolo <- "Evoluzione del numero di frontalieri, donna,\nin Svizzera e in Ticino, per trimestre, dal"
sottotitolo <- "Tasso di crescita: dati in percentuale"

tmp <- staf %>%
  filter(variabili %in% "delta_year_perc", sesso %in% "Femme", geo %in% c("Suisse", "Ticino"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()


f5 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

f5


# F.6: Tasso di crescita dei frontalieri, secondo il genere, in Ticino
titolo <- "Evoluzione del numero di frontalieri,\nsecondo il genere, in Ticino, per trimestre, dal"

tmp <- staf %>%
  filter(variabili %in% "delta_year_perc", sesso %in% c("Femme", "Homme"), geo %in% "Ticino", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso)) %>%
  as.data.table()


f6 <- plot_classic_abs(tmp, "sesso",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

f6


# F.7: Tasso di crescita dei frontalieri, secondo il genere, in Svizzera
titolo <- "Evoluzione del numero di frontalieri,\nsecondo il genere, in Svizzera, per trimestre, dal"

tmp <- staf %>%
  filter(variabili %in% "delta_year_perc", sesso %in% c("Femme", "Homme"), geo %in% "Suisse", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso))


f7 <- plot_classic_abs(tmp, "sesso",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

f7


# F.8: Quota di frontalieri occupati in Ticino rispetto al totale, secondo il genere
titolo <- "Quota di frontalieri occupati in Ticino,\nsecondo il genere, per trimestre, dal"
sottotitolo <- "Quota: dati in percentuale"
cadenza <- 5

tmp <- staf %>%
  filter(variabili %in% "values",
         sesso != "Qu_donne_perc",
         geo %in% "Qu_ticino_perc", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso)) %>%
  as.data.table()


f8 <- plot_classic_abs(tmp, "sesso",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

f8

### Statimp
# I.1: Impieghi in Svizzera e in Ticino
primo_anno <- max_anno - 13
titolo <- "Impieghi, in Svizzera e in Ticino, per trimestre, dal"
sottotitolo <- "Dati in migliaia"
fonte <- "Fonte: Statistica degli impieghi (STATIMP), UST, Neuchâtel"
cadenza <- 2

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% "Sesso - Totale",
         tasso.occ %in% "Tasso di occupazione - Totale",
         settore %in% "Settori 2-3",
         geo %in% c("Svizzera", "Ticino"), anno >= primo_anno) %>%
  mutate(values = values/1000) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()

i1 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza,
                       scales = "free_y"
)

i1

# I.2: Impieghi a tempo pieno, in Svizzera e in Ticino
titolo <- "Impieghi a tempo pieno, in Svizzera e in Ticino,\nper trimestre, dal"

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% "Sesso - Totale",
         tasso.occ %in% "Tempo pieno",
         settore %in% "Settori 2-3",
         geo %in% c("Svizzera", "Ticino"), anno >= primo_anno) %>%
  mutate(values = values/1000) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()

i2 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza,
                       scales = "free_y"
)

i2


# I.3: Impieghi a tempo parziale, in Svizzera e in Ticino
titolo <- "Impieghi a tempo parziale, in Svizzera e in Ticino,\nper trimestre, dal"

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% "Sesso - Totale",
         tasso.occ %in% "Tempo partziale",
         settore %in% "Settori 2-3",
         geo %in% c("Svizzera", "Ticino"), anno >= primo_anno) %>%
  mutate(values = values/1000) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()

i3 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza,
                       scales = "free_y"
)

i3

# I.4: Quota degli impieghi a tempo parziale, in Svizzera e in Ticino
titolo <- "Quota degli impieghi a tempo parziale, in Svizzera e in Ticino,\nper trimestre, dal"
sottotitolo <- "Quota: valori in percentuale"

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% "Sesso - Totale",
         tasso.occ %in% "Qu_tmp_prz",
         settore %in% "Settori 2-3",
         geo %in% c("Svizzera", "Ticino"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()

i4 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

i4

# I.5: Quota degli impieghi a tempo parziale occupati da donne, in Svizzera e in Ticino
titolo <- "Quota degli impieghi a tempo parziale, tra le donne,\nin Svizzera e in Ticino, per trimestre, dal"

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% "Donna",
         tasso.occ %in% "Qu_tmp_prz",
         settore %in% "Settori 2-3",
         geo %in% c("Svizzera", "Ticino"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()

i5 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

i5

# I.6: Quota degli impieghi a tempo parziale, in Svizzera e in Ticino
titolo <- "Quota degli impieghi a tempo parziale, tra gli uomini,\nin Svizzera e in Ticino, per trimestre, dal"

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% "Uomo",
         tasso.occ %in% "Qu_tmp_prz",
         settore %in% "Settori 2-3",
         geo %in% c("Svizzera", "Ticino"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()

i6 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

i6

# I.7: Nel secondario, quota degli impieghi a tempo parziale, secondo il genere, in Ticino
titolo <- "Quota degli impieghi a tempo parziale, nel settore secondario,\nin Svizzera e in Ticino, per trimestre, dal"

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% c("Uomo", "Donna"),
         tasso.occ %in% "Qu_tmp_prz",
         settore %in% "Settore 2",
         geo %in% "Ticino", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso)) %>%
  as.data.table()

i7 <- plot_classic_abs(tmp, "sesso",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

i7

# I.8: Nel terziario, quota degli impieghi a tempo parziale, secondo il genere, in Ticino
titolo <- "Quota degli impieghi a tempo parziale, nel settore terziario,\nin Svizzera e in Ticino, per trimestre, dal"

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% c("Uomo", "Donna"),
         tasso.occ %in% "Qu_tmp_prz",
         settore %in% "Settore 3",
         geo %in% "Ticino", anno >= primo_anno) %>%
  select(Trimestre, sesso, values, max.value, min.value) %>%
  arrange(Trimestre, desc(sesso)) %>%
  as.data.table()

i8 <- plot_classic_abs(tmp, "sesso",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

i8

# I.9: Nel secondario, quota degli impieghi a tempo parziale, secondo il genere, in Ticino
titolo <- "Quota degli impieghi a tempo parziale, nel settore terziario,\ntra le donne, in Svizzera e in Ticino, per trimestre, dal"

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% "Donna",
         tasso.occ %in% "Qu_tmp_prz",
         settore %in% "Settore 3",
         geo %in% c("Ticino", "Svizzera"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()

i9 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

i9

# I.10: Nel secondario, quota degli impieghi a tempo parziale, secondo il genere, in Ticino
titolo <- "Quota degli impieghi a tempo parziale, nel settore terziario,\ntra gli uomini, in Svizzera e in Ticino, per trimestre, dal"

tmp <- statimp %>%
  filter(variabili %in% "values",
         sesso %in% "Uomo",
         tasso.occ %in% "Qu_tmp_prz",
         settore %in% "Settore 3",
         geo %in% c("Ticino", "Svizzera"), anno >= primo_anno) %>%
  select(Trimestre, geo, values, max.value, min.value) %>%
  arrange(Trimestre, desc(geo)) %>%
  as.data.table()

i10 <- plot_classic_abs(tmp, "geo",
                       NULL,
                       titolo,
                       sottotitolo,
                       fonte,
                       cadenza
)

i10

pdf(file = "grafici.pdf", width = 11.692 , height = 8.267 , paper = "a4r")
p1
p2
p3
p4
p5
p6
p7
p8
f1
f2
f3
f4
f5
f6
f7
f8
i1
i2
i3
i4
i5
i6
i7
i8
i9
i10

dev.off()
