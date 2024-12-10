#Kjørefil for resultater til artikkel.
# Kan du også sjekke
#   Kan du også sjekke kor mange av desse vi kan oppgje 30-dagarsmortalitet på den 1. juni?
#   (Liste over når inneliggende ble innlagt?)
library(intensiv)
library(intensivberedskap)


# query <- paste0('Select * FROM mainformdatacontract
#             WHERE cast(DateAdmittedIntensive as date) BETWEEN \'',
#                 datoFra='2020-03-01', '\' AND \'', datoTil=Sys.Date(), '\'')
# DataNIR <- rapbase::LoadRegData(registryName= "nir", query=query, dbType="mysql")  #
DataNIRraa <- NIRRegDataSQL(datoFra = '2020-03-01') #Kun ferdigstilte intensivopphold sendes til Rapporteket
#DataNIR <- NIRPreprosess(DataNIRraa) NB: Vi slår ikke sammen til per pasient for NIR.
#Kobling av Intensiv og Beredskapsdata må gjøres FØR preprosessering
DataBeredskapRaa <- NIRberedskDataSQL(datoTil = '2020-05-10')
DataBeredskapRaa <- DataBeredskapRaa[which(DataBeredskapRaa$FormStatus == 2), ]

#-----------------iNNLEDENDE OVERSIKT, PER 10.JUNI------------
# Samla liggjetid og samla respiratortid
#5. Liggjetid på intensiv for heile forløpet (for overflytta pas. frå inntid fyrste avdeling til utskriving siste avdeling)
#6.       Respirarortid for heile covid-opphaldet (all non-invasiv, all invasiv + samla all non-invasiv + invasiv)


#2. Om pasienten er overført eller ikkje under intensivforløpet med covid-19-sjukdom
#Antregpas >0
#3. Alder i år, kjønn

#4. Helseregion (Sør-Øst, Vest, Midt Nord)

#7.       SAPS II-skåre (fyrste skåre (på fyrste avdeling) for overflytta pasientar)

#8.       Samla NEMS-skåre for forløpet

#9.       Intensivmortalitet og 30-dagars mortalitet

#10.   Trakeostomi +/- under intensivopphaldet

#11.   ECMO-tid

#12.   Nyreerstattande behandling (tal på dagar med kontinuerleg, intermitterande)

#13.   Kontinuerleg vasoaktiv medikasjon +/- under opphaldet

#14.   Risikofaktorar registrerte

#15.   Hovudårsak til intensivinnlegging

#16.   Om det er registrert diagnosen ARDS på opphaldet

#-----------INNLEDENDE SPØRSMÅL----------------------
#kor mange av dei ferdigstilte «beredskapsopphalda» i perioden 10.mars-10.mai som også har ferdigstilt ordinær NIR-registrering?
  length(which(DataBeredskapRaa$HovedskjemaGUID %in% DataNIRraa$SkjemaGUID)) #  186
  #Dobbeltregistrering av beredskapsskjema (har samme HovedskjemaGUID):
  tab <- table(DataBeredskapRaa$HovedskjemaGUID)
  dbl <- names(tab[tab>1])
  tabdbl <- DataBeredskapRaa[which(DataBeredskapRaa$HovedskjemaGUID %in% dbl), c("ShNavn", "DateAdmittedIntensive", "SkjemaGUID", "HovedskjemaGUID")]
  tabdbl[order(tabdbl$HovedskjemaGUID, tabdbl$ShNavn), ]

  #Sende meg ei liste over dei avdelingane som enno har ikkje-ferdigstilte NIR-skjema for desse pasientane?
  DataNIRraa <- NIRRegDataSQL(datoFra = '2020-03-01') #Kun ferdigstilte intensivopphold sendes til Rapporteket
  DataBeredskapRaa <- NIRberedskDataSQL()
  DataBeredskapRaa <- DataBeredskapRaa[which(DataBeredskapRaa$FormStatus == 2), ]
ManglerIntOpph <- DataBeredskapRaa[-which(DataBeredskapRaa$HovedskjemaGUID %in% DataNIRraa$SkjemaGUID),
                                   c("ShNavn", "DateAdmittedIntensive", "SkjemaGUID", "HovedskjemaGUID", 'PatientInRegistryGuid')]
ManglerIntOpph[order(ManglerIntOpph$ShNavn, ManglerIntOpph$DateAdmittedIntensive), ]

#Kan du også sjekke kor mange av desse vi kan oppgje 30-dagarsmortalitet på den 1. juni?
#Dvs. utskrevet innen 1.mai
#ENDRE TIL innleggelsestidspunkt
length(which(as.Date(DataBeredskapRaa$DateDischargedIntensive)<='2020-05-01')) #224
sum(as.Date(DataBeredskapRaa$DateDischargedIntensive)>'2020-05-01' & DataBeredskapRaa$DischargedIntensivStatus==1) #3
table(DataBeredskapRaa$DischargedIntensivStatus)


IntCoviddata <- merge(DataBeredskapRaa, DataNIRraa, suffixes = c('','NIR'),
                     by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)


Elles forslår eg at vi presenterer:
  Alderfordeling
Kjønnsfordeling
SAPS II-skåre
summary()
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
