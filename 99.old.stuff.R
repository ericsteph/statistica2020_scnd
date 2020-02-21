### Armonizzo colonna Trimestre e anno e fisso la dimensione
# Creo due funzioni
# crea_anno_trimestre <- function(d, testo = " trim."){
#   d <- d %>%
#     unite(anno, trimestre, col = "anno_trimestre", sep = ", ", remove = FALSE)
#   
#   d$anno_trimestre <- paste0(d$anno_trimestre, testo)
#   d
# }
# 
# separa_anno_trimestre <- function(d, sep = "Q", levels = c("I", "II", "III", "IV") ) {
#   d <- d %>%
#     separate(Trimestre, sep, into = c("anno", "trimestre"))
#   
#   d$anno <- parse_integer(d$anno)
#   d$trimestre <- parse_factor(d$trimestre)
#   levels(d$trimestre) <- levels
#   d
# }

# Applico ai 3 dataset
# spo <- crea_anno_trimestre(spo)
# levels(spo$trimestre) <- as.character(1:4)

# staf <- separa_anno_trimestre(staf)
# staf <- crea_anno_trimestre(staf)
# staf[ , anno := as.integer(format(Trimestre, "%Y"))]
# staf[ , trimestre := as.integer(format(Trimestre, "%q"))]


# statimp <- separa_anno_trimestre(statimp)
# statimp <- crea_anno_trimestre(statimp)
# statimp[ , anno := as.integer(format(Trimestre, "%Y"))]
# statimp[ , trimestre := as.integer(format(Trimestre, "%q"))]

# Etichette
# etichette0 <- as.character(seq(primo_anno, max_anno, by = 2))
# 
# breaks_fig <- paste0(etichette0, ", II trim.")
# 
# etichette <- paste0(etichette0, "\nII trim.")

# breaks_fig <- unique(spo$Trimestre)
# k <- seq(length(breaks_fig), 1, by = -8)
# k <- rev(k)
# breaks_fig <- breaks_fig[k]
# breaks_fig <- c(breaks_fig[1] - 2, breaks_fig)