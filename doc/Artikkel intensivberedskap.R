#Kjørefil for resultater til artikkel.
# Kan du også sjekke
#   Kan du også sjekke kor mange av desse vi kan oppgje 30-dagarsmortalitet på den 1. juni?
#   (Liste over når inneliggende ble innlagt?)
library(intensiv)
library(intensivberedskap)


# query <- paste0('Select * FROM MainFormDataContract
#             WHERE cast(DateAdmittedIntensive as date) BETWEEN \'',
#                 datoFra='2020-03-01', '\' AND \'', datoTil=Sys.Date(), '\'')
# DataNIR <- rapbase::LoadRegData(registryName= "nir", query=query, dbType="mysql")  #
DataNIRraa <- NIRRegDataSQL(datoFra = '2020-03-01') #Kun ferdigstilte intensivopphold
DataBeredskapRaa <- NIRberedskDataSQL(datoTil = '2020-05-10')

#kor mange av dei ferdigstilte «beredskapsopphalda» i perioden 10.mars-10.mai som også har ferdigstilt ordinær NIR-registrering?
#Sende meg ei liste over dei avdelingane som enno har ikkje-ferdigstilte NIR-skjema for desse pasientane?


KoroDataRaa <- merge(DataBeredskapRaa, DataNIRraa, suffixes = c('','NIR'),
                     by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)


Elles forslår eg at vi presenterer:
  Alderfordeling
Kjønnsfordeling
SAPS II-skåre
Primærårsak til innlegging på intensiv
NEMS-skåre (truleg mykje høgare enn for andre grupper intensivpasientar – her har vi kontrollar i eige materiale)
Liggjetider
Respiratortider (invasiv/non-invasiv)
Bruk av trakeostomi
Bruk av nyreerstattande behandling
Bruk av vasoaktiv medikasjon
Bruk av ecmo
Tal på covid-pasientar som er overflytta under pågåande intensivbehandling
Intensivmortalitet, 30-dagars mortalitet og SMR
Tal på pasientar som har fått ARDS-diagnosen (obs. dette er ikkje obligatorisk, men noko alle NIR-avsnitta er oppmoda om å registrere)
