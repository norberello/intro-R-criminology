works_with_R(
  "3.3.3",
  microbenchmark="1.4.2",
  "tdhock/namedCapture@1da425bb24a2ff1edc89d38654b5c9465aa9fa20")
specimens <- fread("specimens.csv")
nrow(specimens)
length(unique(specimens$SPECIMEN))
specimens[, table(SERVPOL, useNA="always")]
specimens[, table(Type)]
specimens[, table(STUP)]
specimens[, table(STUP, Type)]

ggplot()+
  geom_point(aes(SERVPOL, masse_coca), data=specimens)

ggplot()+
  geom_point(aes(SERVPOL, masse_coca), data=specimens)+
  scale_y_log10()

## Exercise: plot some other masse_ variables using geom_point.

ggplot()+
  geom_point(aes(masse_hero, masse_coca), data=specimens)+
  scale_y_log10()+
  scale_x_log10()+
  coord_equal()

ggplot()+
  geom_abline(slope=1,intercept=0,color="grey")+
  geom_point(aes(masse_specimen, masse_coca, color=SERVPOL), data=specimens)+
  scale_y_log10()+
  scale_x_log10()+
  coord_equal()

ggplot()+
  geom_point(aes(SERVPOL, masse_hero), data=specimens)+
  scale_y_log10()

mass.col.vec <- grep("masse_", names(specimens), value=TRUE)
mass.tall <- melt(
  specimens,
  measure.vars=mass.col.vec,
  id.vars=c("SPECIMEN", "SERVPOL"),
  variable.name="masse_type",
  value.name="mass")
mass.tall[, type := sub("masse_", "", masse_type)]

ggplot()+
  geom_point(aes(SERVPOL, mass, color=type), data=mass.tall)+
  scale_y_log10()

ggplot()+
  facet_grid(. ~ type)+
  geom_point(aes(SERVPOL, mass), data=mass.tall)+
  scale_y_log10()

ggplot()+
  facet_grid(type ~ .)+
  geom_point(aes(SERVPOL, mass), data=mass.tall)+
  scale_y_log10()

specimens.name.vec <- names(specimens)
class.col <- which(specimens.name.vec=="CLASSE")
is.after.class <- class.col < 1:length(specimens.name.vec)
coupage.name.vec <- specimens.name.vec[is.after.class]
coupage.tall <- melt(
  specimens,
  measure.vars=coupage.name.vec,
  id.vars=c("SPECIMEN", "SERVPOL"),
  variable.name="coupage",
  value.name="presence")
coupage.present <- coupage.tall[presence==1,]
coupage.counts <- specimens.present[, list(
  specimens=.N
), by=list(SERVPOL, coupage)]
ggplot()+
  geom_tile(aes(SERVPOL, coupage, fill=log10(specimens)), data=coupage.counts)+
  scale_fill_gradient(low="grey95", high="red")

servpol.counts <- coupage.counts[, list(servpols=.N), by=coupage]
coupage.levels <- servpol.counts[order(servpols), coupage]
coupage.counts[, coupage.fac := factor(coupage, coupage.levels)]
ggplot()+
  geom_tile(aes(SERVPOL, coupage.fac, fill=log10(specimens)),
            data=coupage.counts)+
  scale_fill_gradient(low="grey95", high="red")+
  geom_text(aes(SERVPOL, coupage.fac, label=specimens),
            data=coupage.counts)

## Exercise: how could you sort the rows by 

## SPECIMEN = NNN_MM.AA_M_ où NNN_MM.AA = SAISIE (mise sous séquestre
## globale par un service de police), M = Spécimen M extrait de cette
## saisie ; MM = Mois ; AA = Année
saisie.pattern <- paste0(
  "(?<id>[0-9]+)",
  "_",
  "(?<month>[0-9]+)",
  "[.]",
  "(?<year>[0-9]+)")
specimen.types <- list(
  id=as.integer,
  month=as.integer,
  year=as.integer,
  specimen=as.integer)
saisie.df <- str_match_named(specimens$SAISIE, saisie.pattern, specimen.types)
stopifnot(sum(is.na(saisie.df))==0)

saisie.dt <- data.table(saisie.df)
saisie.dt[, table(year, month)]

specimen.pattern <- paste0(
  "(?<saisie>",
  saisie.pattern,
  ")",
  "_",
  "(?<specimen>[0-9]+)",
  "_")
specimen.df <- str_match_named(
  specimens$SPECIMEN, specimen.pattern, specimen.types)
stopifnot(sum(is.na(specimen.df))==0)
specimen.dt <- data.table(specimen.df)
stopifnot(specimen.dt$saisie == specimens$SAISIE)
specimen.dt
