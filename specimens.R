works_with_R(
  "3.3.3",
  microbenchmark="1.4.2",
  "tdhock/namedCapture@1da425bb24a2ff1edc89d38654b5c9465aa9fa20")
specimens <- fread("SFC1018-HIV2017-Datastup_R.CSV")
nrow(specimens)
length(unique(specimens$SPECIMEN))
specimens[, table(CANTON)]
specimens[, table(Type)]
specimens.name.vec <- names(specimens)
class.col <- which(specimens.name.vec=="CLASSE")
is.after.class <- class.col < 1:length(specimens.name.vec)
coupage.name.vec <- specimens.name.vec[is.after.class]
microbenchmark(melt={
  specimens.tall <- melt(
    specimens,
    measure.vars=coupage.name.vec,
    id.vars=c("SPECIMEN", "CANTON"),
    variable.name="coupage",
    value.name="presence")
  specimens.one <- specimens.tall[presence==1,]
}, by={
  specimens.by <- specimens[, list(
    coupage=strsplit(COUPAGE, " ", fixed=TRUE)[[1]]
  ), by=list(SPECIMEN, CANTON)]
}, strsplit={
  specimens.strsplit <- specimens[, {
    list.of.coupage.vectors <- strsplit(COUPAGE, " ", fixed=TRUE)
    n.coupage.vec <- sapply(list.of.coupage.vectors, length)
    data.table(
      SPECIMEN=rep(SPECIMEN, n.coupage.vec),
      CANTON=rep(CANTON, n.coupage.vec),
      coupage=unlist(list.of.coupage.vectors))
  }]
}, times=10)
specimens.by[specimens.one, on=list(SPECIMEN, CANTON, coupage)]
specimens.one[specimens.by, on=list(SPECIMEN, CANTON, coupage)][is.na(presence),]

coupage.counts <- specimens.one[, list(specimens=.N), by=list(CANTON, coupage)]
ggplot()+
  geom_tile(aes(CANTON, coupage, fill=log10(specimens)), data=coupage.counts)+
  scale_fill_gradient(low="grey95", high="red")

canton.counts <- coupage.counts[, list(cantons=.N), by=coupage]
coupage.levels <- canton.counts[order(cantons), coupage]
coupage.counts[, coupage.fac := factor(coupage, coupage.levels)]
ggplot()+
  geom_tile(aes(CANTON, coupage.fac, fill=log10(specimens)),
            data=coupage.counts)+
  scale_fill_gradient(low="grey95", high="red")+
  geom_text(aes(CANTON, coupage.fac, label=specimens),
            data=coupage.counts)

coupage.counts[, proportion := specimens/sum(specimens), by=CANTON]
ggplot()+
  geom_tile(aes(CANTON, coupage, fill=proportion), data=coupage.counts)+
  scale_fill_gradient(low="grey95", high="red")

ggplot()+
  geom_point(aes(PURETE, masse_ech, color=Type), data=specimens)
ggplot()+
  geom_point(aes(log10(masse_specimen), log10(masse_ech), color=EMBALLAGE), data=specimens)
specimens[, table(DESCRIPTION)]
specimens.desc <- specimens[, list(rows=.N), by=DESCRIPTION]
specimens.desc[order(rows),][rows>1,]
specimens.emb <- specimens[, list(rows=.N), by=EMBALLAGE]
specimens.emb[order(rows),][rows>1,]
saisie.pattern <- paste0(
  "(?<id>[0-9]+)",
  "_",
  "(?<mois>[0-9]+)",
  "[.]",
  "(?<jour>[0-9]+)")
specimen.types <- list(
  id=as.integer,
  mois=as.integer,
  jour=as.integer,
  specimen=as.integer)
saisie.df <- str_match_named(specimens$SAISIE, saisie.pattern, specimen.types)
stopifnot(sum(is.na(saisie.df))==0)
saisie.dt <- data.table(saisie.df)
saisie.dt[, table(jour, mois)]
specimen.pattern <- paste0(
  "(?<saisie>",
  saisie.pattern,
  ")",
  "_",
  "(?<specimen>[0-9]+)",
  "_")
specimen.df <- str_match_named(specimens$SPECIMEN, specimen.pattern, specimen.types)
stopifnot(sum(is.na(specimen.df))==0)
specimen.dt <- data.table(specimen.df)
stopifnot(specimen.dt$saisie == specimens$SAISIE)
