library(shiny)
library(gdata)
library(ggplot2)
library(ggrepel)
library(markdown)
library(dplyr)

ui <- fluidPage(
  
  tiedekunnat <- c('Koko yliopisto' = 12, "Bio- ja ympäristötieteellinen" = 1, "Eläinlääketieteellinen" = 2, 
                   "Farmasian" = 3, "Humanistinen" = 4, "Kasvatustieteellinen" = 5,
                   "Lääketieteellinen" = 6, "Matemaattis-luonnontieteellinen" = 7,
                   "Maatalous-metsätieteellinen" = 8, "Oikeustieteellinen" = 9,
                   "Teologinen" = 10, "Valtiotieteellinen" = 11),
  
  oppialaryhmat <- c('Koko yliopisto' = 12,
                    'Suomen kieli' = 1011, 'Germaaninen filologia' = 1012, 'Englantilainen filologia' = 1013, 'Muut kieliaineet' = 1014,
                    'Kääntäminen ja tulkkaus' = 1015, 'Kulttuurintutkimus' = 1016, 'Taiteiden tutkimus' = 1017, 'Historia' = 1018,
                    'Luokanopettajat ja erit. pedagogiikka' = 1021, 'Kotitaloustiede' = 1022,
                    'Käsityötiede' = 1023, 'Logopedia' = 1024, 'Lastentarhanopettaja' = 1025, 'Kasvatustiede' = 1026,
                    'Matematiikka ja tilastotiede' = 1041, 'Tietojenkäsittelytiede' = 1042, 'Fysikaaliset tieteet' = 1043,
                    'Maantiede ja geologia' = 1044, 'Kemia' = 1045, 'Bio- ja ympäristötieteet' = 1046, 'Lääketieteellinen' = 1051,
                    'Oikeustieteellinen' = 1061, 'Psykologia' = 1071, 'Teologia' = 1091, 'Valtio-oppi' = 1111,
                    'Taloustiede' = 1112, 'Sosiaalityö' = 1113, 'Sosiaalipsykologia' = 1114, 'Viestintä' = 1115,
                    'Sosiologia ja sosiaalipolitiikka' = 1116, 'Yhteiskunnallinen muutos' = 1117, 'Farmaseutti' = 1121, 'Proviisori' = 1122, 'Maataloustieteet' = 1131,
                    'Elintarviketieteet' = 1132, 'Metsätieteet' = 1133,
                    'Soveltavat taloustieteet ja liiketaloustiede' = 1134, 'Eläinlääketieteellinen' = 1141,
                    'Hammaslääketieteellinen' = 1151, 'Filosofia' = 10110),
  
  taidot <- c('Teoreettinen osaaminen' = 1, 'Analyyttinen systemaattinen ajattelu' = 2, 'Tiedonhaku' = 3,
              'Ongelmanratkaisu' = 4, 'Ryhmätyöskentely' = 5, 'Neuvottelutaidot' = 6, 'Organisointi- ja koordinointitaidot' = 7,
              'Esimiestaidot' = 8, 'Projektinhallinta' = 9, 'Lainsäädäntö' = 10, 'Taloussuunnittelu ja budjetointi' = 11,
              'Yritystoiminnan perusteet' = 12, 'Tieto- ja viestintätekniikka' = 13, 'Suomen kielinen viestintä' = 14,
              'Ruotsin kielinen viestintä' = 15, 'Englannin kielinen viestintä' = 16, 'Viestintä muilla kielillä' = 17,
              'Esiintymistaidot' = 18, 'Opetus-. koulutus- ja ohjaustaidot' = 19, 'Toimiminen monikulttuurisessa ympäristössä' = 20,
              'Opinnoista saatu käytännön osaaminen' = 21,'Yhteistyötaidot' = 22, 'Liiketoiminnan/taloushallinnon perusteiden tuntemus' = 23,
              'Kyky oppia ja omaksua uutta' = 24, 'Luovuus' = 25, 'Tieteidenvälisyys/moniammatillisissa ryhmissä toimiminen' = 26,
              'Stressinsietokyky' = 27, 'Verkostoitumistaidot' = 28, 'Itseohjautuvuus/oma-aloitteisuus' = 29),
  
  navbarPage("Valikko",
    tabPanel(title = "Hajontakuva, tiedekunta", id="Hajontakuva_tdk",
      
      fluidRow(
        column(3,
          selectInput(inputId = "tdk",
                      label = "Valitse tiedekunta",
                      choices=tiedekunnat, selected=12)
        ),
        column(2,
          radioButtons(inputId = "vuosi_tdk", label="Valmistumisvuosi", choices=list(2011, 2009, 2007, 2005, 2003), selected = 2011)
        ),
        column(3,
          textInput('filenameTdk', "Tiedostonimi"),
          downloadButton('savePlotTdk', 'Lataa kuvaaja')
        ),
        column(3,
          fluidRow(
          sliderInput('yokehrangetdk', "Yliopisto-opiskelu kehitti", min = 1, max = 6, value= c(1,6))),
          sliderInput('tettrangetdk', "Työelämän vaatimus", min = 1, max = 6, value= c(1,6)))
      ),
      hr(),
      plotOutput("Hajontakuva_tdk", width="100%", height="700px"),
      hr(),
      htmlOutput("lyhensel")
    ),
    
    tabPanel(title="Hajontakuva, oppialaryhmä", id="Hajontakuva_koulutusala",
      fluidRow(
        column(3,
          selectInput(inputId="kouala",
            label = "Valitse oppialaryhmä",
            choices=oppialaryhmat, selected=12)
        ),
        column(2,
          radioButtons(inputId = "vuosi_kouala", label="Valmistumisvuosi", choices=list(2011, 2009, 2007, 2005, 2003), selected = 2011)    
        ),
        column(3,
          textInput('filenameKouala', "Tiedostonimi"),
          downloadButton('savePlotKouala', 'Lataa kuvaaja')
        ),
        column(3,
          fluidRow(
          sliderInput('yokehrangekouala', "Yliopisto-opiskelu kehitti", min = 1, max = 6, value= c(1,6))),
          sliderInput('tettrangekouala', "Työelämän vaatimus", min = 1, max = 6, value= c(1,6)))
      ),
      hr(),
      plotOutput("Hajontakuva_kouala", width="100%", height="700px"),
      hr(),
      htmlOutput("lyhenselKouala")
    ),
    tabPanel(title = "Viivakuva, tiedekunta", id="Viivakuva_tdk",
      fluidRow(
        column(6,
        selectInput(inputId = "viivaTdk",
          label = "Valitse tiedekunta",
          choices=tiedekunnat, selected=12)
        ),
        column(6,
        selectInput(inputId = "viivaMuutTdk",
          label = "Valitse muuttuja",
          choices=taidot, selected=1)
        )
      ),
      fluidRow(
        column(6,
          plotOutput("Viivakuva_tdk")
        ),
        column(6,
          DT::dataTableOutput('taulukko_tdk') 
        )
      )
    ),
    tabPanel(title = "Viivakuva, oppialaryhmä", id="Viivakuva_kouala",
      fluidRow(
        column(6,
        selectInput(inputId = "viivaKouala",
          label = "Valitse oppialaryhmä",
          choices=oppialaryhmat, selected=12)
        ),
        column(6,
                selectInput(inputId = "viivaMuutKouala",
                  label = "Valitse muuttuja",
                  choices=taidot, selected=1)
        )
      ),
    fluidRow (
      column(6,
        plotOutput("Viivakuva_kouala")
      ),
      column(6,
        DT::dataTableOutput('taulukko_kouala')
      )
    )
  ),
  tabPanel(title = "Ohjeet ja tiedot", id="seliteteksti",
           mainPanel(
           h1("Yleistiedot aineistosta ja kyselystä"),
           p("Helsingin yliopisto on mukana kansallisessa uraseurantakyselytutkimuksessa, jossa tutkitaan korkeakouluista valmistuneiden työuraa ensimmäisen viiden vuoden ajan valmistumisen jälkeen.
           Perusjoukkoon kuuluu pääsäntöisesti ylemmän korkeakoulututkinnon suorittaneita, mutta myös eläinlääketieteen, hammaslääketieteen tai lääketieteen lisensiaatit sekä
           lastentarhanopettajat ja farmaseutit kuuluvat kyselyn piiriin. Tämä sovellus on rakennettu uraseuranta-aineistojen yhden osa-alueen tarkastelua varten.
           Aineistossa on vain Helsingin yliopistosta valmistuneet opiskelijat."),
           p("Tämän sovelluksen aineistoon kuuluvat vuosina 2003, 2005, 2007, 2009 ja 2011 Helsingin yliopistosta valmistuneet. Kyselyt on aina toteutettu
           viisi vuotta valmistumisen jälkeen. Uusin aineisto on kerätty syksyllä 2016."),
           p("Tässä sovelluksessa keskitytään työelämän tarpeisiin ja siihen kuinka hyvin yliopisto-opiskelu kehitti tiettyjä osaamisalueita. Vastaaja on saanut jokaisen muuttujan
           kohdalla arvioida asteikolla 1-6 kyseisen osaamisalueen hyödyllisyyttä työelämässä ja kuinka hyvin yliopisto-opiskelu kehitti kyseistä taitoa. On huomioitava,
           että muuttujat poikkeavat hieman vuosien välillä, joten kaikkina vuosina ei ole kysytty samoja muuttujia."),
           p("Lisätietoja uraseurantakyselyistä ja -aineistoista saa"), a(href="https://www.helsinki.fi/fi/opiskelu/yhteystiedot/urapalvelut", "Helsingin yliopiston urapalveluista"),
           h1("Selityksiä kuviin ja luokituksiin"),
           h3("Kuvat"),
           p("Sovelluksessa on kahdenlaisia kuvia. Hajontakuvissa pystyy vuosittain tarkastelemaan halutussa luokassa kaikkia sinä vuonna kyseltyjä muuttujia.
           Hajontakuviin on piirretty oletusarvoisesti apuviivoja. Sekä y- että x-akselille on piirretty viivat luvun neljä kohdalle havainnollistamaan muuttujien tarpeellisuutta työelämässä
           tai koulutuksen kehittävyyttä. Näiden lisäksi kuvassa kulkee kolme apuviivaa vinottain. Kaikkien värillisten apuviivojen kulmakerroin on 1, ainoana erona on
           leikkauspiste y-akselin kanssa. Ensimmäinen, vihreä viiva kulkee origon kautta ja kuvaa tilannetta, jossa yliopisto-opiskelu on kehittänyt yhtä paljon tiettyä osa-aluetta
           kuin työelämän tarpeeksi on arvioitu. Toinen, oranssi viiva on 'yhden' mitan päässä vihreästä keskiviivasta. Jos jokin muuttuja on oranssilla viivalla, on työelämän tarve
           yhden enemmän kuin kuinka hyvin yliopistokoulutus kehittää. Punaisella viivalla etäisyys on kaksi. Mitä kauempana muuttujan etäisyys vihreästä viivasta on,
           sitä kehnommin yliopisto-opiskelu kohtaa työelämän tarpeen. Jos halutaan arvioida kuinka kaukana yliopisto-opiskelun kehittävyys on tietyn muuttujan kohdalla,
           on syytä tarkastella vaakasuoraa etäisyyttä vihreästä apuviivasta."),
           p("Toinen kuvaajatyyppi on viivakuvaaja. Tämän avulla voidaan tarkastella yhden osaamisalueen kehitystä tietyssä luokassa. On huomioitava, että sovellus
           ei ota kantaa onko vuosien välinen muutos tilastollisesti merkittävä. Tämä joudutaan tarkastelemaan erikseen aineistosta."),
           h3("Luokat"),
           p("Sovelluksessa on käytetty kahta luokitusta. Aineistoa voidaan tarkastella tiedekunnittain. On huomioitava, että tiedekuntajako vastaa vuoden 2017 tiedekuntajakoa. Tämä vaikuttaa myös vanhoihin tuloksiin.
           Oppialaryhmittely on aineistopohjainen luokittelu, jonka avulla päästään kiinni hienojakoisempaan tietoon. Oppialaryhmittely perustuu tiedekuntarajoihin paitsi filosofian ja tilastotieteen kohdalla. Lisätietoja
           oppialaryhmittelystä Helsingin yliopiston Urapalveluista. Oppialaryhmien sisältämät pääaineet löytää"),a(href="https://github.com/tvkangas/osaamistutka/blob/master/osaamistutkaHYOppialaryhmat.xlsx", "täältä"),
           h1("Vastausäärät ja vastausprosentit"),
           p("Vastaajamäärät ja vastausprosentit löytää"), a(href="https://github.com/tvkangas/osaamistutka/blob/master/osaamistutkaVastausmaaratVastausprosentit.xlsx", "täältä"),
           h1("Aineiston suojaus"),
           p("Koska sovellus pyörii ulkopuolisela palvelimella, on vastaajien anonymiteetista jouduttu huolehtimaan. Ulkoiselle palvelimelle ei ole viety 'raakadataa', vaan 
           valmiiksi laskettu aineisto. Näin ollaan vältytty vastauskohtaisen aineiston viemiseltä. Aineistosta ei voida päätellä yksittäisen vastauksen tuloksia."),
           h1("Palaute"),
           p("Palautetta voi lähettää suoraan sähköpostiosoitteeseen tuukka.kangas@helsinki.fi"),
           h1("Yhteystiedot ja toteutus"),
           p("Sovelluksen on kehittänyt Tuukka Kangas (tuukkavkangas@gmail.com). Sovellus on tehty R-kielellä. Sovelluksessa on hyödynnetty avointa shiny-pakettia sekä muita tarpeellisia
           paketteja. Sovelluksen lähdekoodi on julkaistu"), a(href="https://github.com/tvkangas/osaamistutka/blob/master/app.R", "täällä."), p("Sovellus on kehitetty harjoittelujakson aikana Helsingin yliopiston urapalveluissa"),
           h1("Päivitykset"),
           p("3.2. Ensimmäinen versio viety ulkoiselle palvelimelle."),
           p("1.3. Päivitys, bugien korjausta."),
           p("3.3. Stilistisiä muutoksia, oppialaryhmien päivitys."),
           p("28.3. Oppialaryhmien päivitys, vastaajamäärät ja vastausprosentit, oppialaryhmien selitykset."),
           p("4.4. Uusi toiminnallisuus."),
           p("10.4. Päivitys")
           )
           
  )
  )
)

#mydataHajonta = read.csv("Z:/Desktop/Urapalvelut/tietotaitovastavuusbarometri/tyoelamataidotHajontaVuosittain.csv", header = TRUE, sep = ";")
#mydataViiva = read.csv("Z:/Desktop/Urapalvelut/tietotaitovastavuusbarometri/tyoelamataidotViivaVuosittain.csv", header = TRUE, sep = ";")

mydataHajonta = read.csv(file="tyoelamataidotHajontaVuosittain.csv", header = TRUE, sep = ";", encoding="UTF-8")
mydataViiva = read.csv(file="tyoelamataidotViivaVuosittain.csv", header = TRUE, sep = ";", encoding="UTF-8")

server <- function(input, output) {
  
  #Hajontakuva, tiedekunta

  output$Hajontakuva_tdk <- renderPlot ({
    print(plotInputTDK())
  })
  
  plotInputTDK = function() {
    yoAlaraja <- input$yokehrangetdk[1]
    yoYlaraja <- input$yokehrangetdk[2]
    tettAlaraja <- input$tettrangetdk[1]
    tettYlaraja <- input$tettrangetdk[2]
    
    newdata <- mydataHajonta[ which(mydataHajonta$tdk==input$tdk
                            & mydataHajonta$valmvuosi == input$vuosi_tdk & mydataHajonta$yo >= yoAlaraja 
                            & mydataHajonta$yo <= yoYlaraja & mydataHajonta$tett >= tettAlaraja
                            & mydataHajonta$tett <= tettYlaraja), ]
    p <- ggplot(newdata, aes(yo, tett)) +
    geom_vline(xintercept = 4, color='black', lty=2) +
      geom_hline(yintercept = 4, color='black', lty=2) +
      geom_point(color='black', size = 5, shape=1) +
      theme_classic(base_size=20) +
      scale_x_continuous(breaks = c(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0), limits=c(1,6)) +
      scale_y_continuous(breaks = c(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0), limits=c(1,6)) +
      geom_abline(intercept=0, slope=1, color = 'green', size=1.2, lty=2) + #Keskiviiva
      geom_abline(intercept=1, slope=1, color = 'orange', lty=2, size=1.2) + #Etäisyydellä 1
      geom_abline(intercept=2, slope=1, color = 'red', lty=2, size=1.2) + #Etäisyydellä 2
      geom_text_repel(aes(label=muuttuja), force = 6, size = 6) +
      xlab("Yliopisto-opiskelu kehitti") +
      ylab("Työelämä vaatii") +
      labs(caption = paste0("Katso lyhenteet kuvan alta\n",newdata$tdknimi, "\n", input$vuosi_tdk, "\n"))+
      theme(plot.caption = element_text(size = 14))+
      coord_fixed(ratio=1) +
      ggtitle("Osaamistutka" )
  }

  
  output$savePlotTdk <- downloadHandler(
    filename = function() { paste0(input$filenameTdk, ".png")},
    content = function(file) {
      ggsave(file, plot = plotInputTDK(), device = "png", width = 15, height = 10)
    }
  )

    lyhenteet <- c('Tieteidenvälisyys', 'Tieteidenvälisyys/moniammatillisissa ryhmissä toimiminen',
                   'Viestintä suomeksi', 'Suomen kielinen viestintä',
                   'Viestintä ruotsiksi', 'Ruotsin kielinen viestintä',
                   'Viestintä englanniksi', 'Englannin kielinen viestintä',
                   'Analyyt., syst. ajattelu', 'Analyyttinen, systemaattinen ajattelu',
                   'Käytännön osaaminen opinnoista', 'Opinnoista saatu käytännön osaaminen',
                   'Toimiminen mon.kult. ympäristössä', 'Toimiminen monikulttuurisessa ympäristössä',
                   'Liiketoiminnan/taloushallinnon tuntemus', 'Liiketoiminnan/taloushallinnon perusteiden tuntemus'
    )

  output$lyhensel <- renderUI({
    otsikko <- "Lyhenteiden selitykset"
    kaikille <- HTML(paste(h4(otsikko),"<br/>",lyhenteet[3],"&ensp;", lyhenteet[4],"<br/>",lyhenteet[5],"&ensp;", lyhenteet[6],
                           "<br/>",lyhenteet[7],"&ensp;", lyhenteet[8],"<br/>",lyhenteet[9],"&ensp;",lyhenteet[10]))
    if (input$vuosi_tdk == 2011) {
      HTML(paste(kaikille,"<br/>",lyhenteet[1],"&ensp;",lyhenteet[2],"<br/>",lyhenteet[11],"&ensp;", lyhenteet[12],"<br/>",lyhenteet[13],"&ensp;", lyhenteet[14],
                 "<br/>",lyhenteet[15],"&ensp;", lyhenteet[16]))
    } else if (input$vuosi_tdk == 2009) {
      HTML(paste(kaikille,"<br/>",lyhenteet[13],"&ensp;",lyhenteet[14]))
    } else {
      kaikille
    }
    
  })
    
  #Hajontakuva, oppialaryhmittäin
  
  output$Hajontakuva_kouala <- renderPlot ({
    print(plotInputKouala())
  })
  
  plotInputKouala = function() {
    yoAlaraja <- input$yokehrangekouala[1]
    yoYlaraja <- input$yokehrangekouala[2]
    tettAlaraja <- input$tettrangekouala[1]
    tettYlaraja <- input$tettrangekouala[2]

    newdata <- mydataHajonta[ which(mydataHajonta$kouala==input$kouala
                                    & mydataHajonta$valmvuosi == input$vuosi_kouala & mydataHajonta$yo >= yoAlaraja 
                                    & mydataHajonta$yo <= yoYlaraja & mydataHajonta$tett >= tettAlaraja
                                    & mydataHajonta$tett <= tettYlaraja), ]
    p <- ggplot(newdata, aes(yo, tett)) +
      geom_vline(xintercept = 4, color='black', lty=2) +
      geom_hline(yintercept = 4, color='black', lty=2) +
      geom_point(color='black', size = 5, shape=1) +
      theme_classic(base_size=20) +
      scale_x_continuous(breaks = c(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0), limits=c(1,6)) +
      scale_y_continuous(breaks = c(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0), limits=c(1,6)) +
      geom_abline(intercept=0, slope=1, color = 'green', size=1.2, lty=2) + #Keskiviiva
      geom_abline(intercept=1, slope=1, color = 'orange', lty=2, size=1.2) + #Etäisyydellä 1
      geom_abline(intercept=2, slope=1, color = 'red', lty=2, size=1.2) + #Etäisyydellä 2
      geom_text_repel(aes(label=muuttuja), force = 4, size = 6) +
      xlab("Yliopisto-opiskelu kehitti") +
      ylab("Työelämä vaatii") +
      labs(caption = paste0("Katso lyhenteet kuvan alta\n", newdata$koualanimi, "\n", input$vuosi_kouala) )+
      theme(plot.caption = element_text(size = 14))+
      coord_fixed(ratio=1) +
      ggtitle(paste0("Osaamistutka" ) )
  }
  
  
  output$savePlotKouala <- downloadHandler(
    filename = function() { paste0(input$filenameKouala, ".png")},
    content = function(file) {
      ggsave(file, plot = plotInputKouala(), device = "png", width = 15, height = 10)
    }
  )


    output$lyhenselKouala <- renderUI({
      otsikko <- "Lyhenteiden selitykset"
      kaikille <- HTML(paste(h4(otsikko),"<br/>",lyhenteet[3],"&ensp;", lyhenteet[4],"<br/>",lyhenteet[5],"&ensp;", lyhenteet[6],
                  "<br/>",lyhenteet[7],"&ensp;", lyhenteet[8],"<br/>",lyhenteet[9],"&ensp;",lyhenteet[10]))
      if (input$vuosi_kouala == 2011) {
        HTML(paste(kaikille,"<br/>",lyhenteet[1],"&ensp;",lyhenteet[2],"<br/>",lyhenteet[11],"&ensp;", lyhenteet[12],
                   "<br/>",lyhenteet[13],"&ensp;", lyhenteet[14],"<br/>",lyhenteet[15],"&ensp;", lyhenteet[16]))
      } else if (input$vuosi_kouala == 2009) {
        HTML(paste(kaikille,"<br/>",lyhenteet[13],"&ensp;",lyhenteet[14]))
      } else {
        kaikille
      }
    })
    
    #viivakuva, tiedekunnittain
    
    lineInputTdk <- reactive({
      newdata <- mydataViiva[ which(mydataViiva$tdk==input$viivaTdk
                                     & mydataViiva$muuttuja == input$viivaMuutTdk), ]
      
      p <- ggplot(newdata, aes(x=valmvuosi, y=arvo, color=group)) +
           geom_line(aes(color=group), size=1) +
           geom_point(color='black', size = 5, shape=1) +
           theme_classic(base_size=16) +
           scale_y_continuous(name="Keskiarvo", breaks = c(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0), limits=c(1,6)) +
           scale_x_continuous(name="Valmistumisvuosi", breaks = c(2003, 2005, 2007, 2009, 2011), limits=c(2002.5,2011.5)) +
           ggtitle(paste0("Osaamisalueen kehitys vuosittain, ", newdata$tdknimi, "\n(", newdata$muuttujanimi, ")")) +
           coord_fixed(ratio=1.5) 
    })
    
    output$Viivakuva_tdk <- reactivePlot(function() {
      print(lineInputTdk())  
    })
    
    output$taulukko_tdk <- DT::renderDataTable({
      newdata <- mydataViiva[ which(( mydataViiva$tdk==input$viivaTdk | mydataViiva$tdk== 12)
                                    & mydataViiva$muuttuja == input$viivaMuutTdk), ]
      newdata <- newdata[,-c(0,4,5,6,7)]
      
      tulostettavaData <- c("Kohta", 2003, 2005, 2007, 2009, 2011)
      
      tdknimi = ""
      
      if (nrow(newdata) < 9) tdknimi = newdata[1,3] else tdknimi = newdata[9,3] #hakee tiedekunnan nimen. if tarvitaan tarkistamaan onko koko yliopisto
      
      #Tarvitaan apumuuttuja, koska muuten bioymp ja elöinlöökis nökyvöt vöörin
      aputiedekuntaNro <- paste("^",input$viivaTdk,"$", sep="")
      
      kohdeTdkArvot <- newdata[grep(aputiedekuntaNro,newdata$tdk,ignore.case=T),]
      kohdeTdkTyoelama <- kohdeTdkArvot[grep("Tyoelaman vaatimus", kohdeTdkArvot$group,ignore.case=T), ]
      kohdeTdkYliopisto <- kohdeTdkArvot[grep("Yliopisto-opiskelu kehitti", kohdeTdkArvot$group,ignore.case=T), ]
      
      ensimmainenRivi = c(paste0(tdknimi,", tyoelämän vaatimus"))
      
      #Käydään läpi kaikki vuodet ja haetaan riville työelämän vaatimukset
      for(i in 2003:2011) {
        if (!i%%2) {
          next
        }
        tmpDataTyoelama <- kohdeTdkTyoelama[grep(i,kohdeTdkTyoelama$valmvuosi,ignore.case=T),]
        ensimmainenRivi <- c(ensimmainenRivi,tmpDataTyoelama[,"arvo"])
      }
      
      
      #Lisätään rivi tulostettavaan dataan
      tulostettavaData <- rbind(tulostettavaData, ensimmainenRivi)
      
      #Yliopisto-opiskelu kehitti
      
      toinenRivi = c(paste0(tdknimi,", yliopisto-opiskelu kehitti"))
      
      #Käydään läpi kaikki vuodet ja haetaan riville työelämän vaatimukset
      for(i in 2003:2011) {
        if (!i%%2) {
          next
        }
        tmpDataYliopisto <- kohdeTdkYliopisto[grep(i,kohdeTdkTyoelama$valmvuosi,ignore.case=T),]
        toinenRivi <- c(toinenRivi,tmpDataYliopisto[,"arvo"])
      }
      
      
      #Lisätään rivi tulostettavaan dataan
      tulostettavaData <- rbind(tulostettavaData, toinenRivi)
      
      #Otetaan otsikot ensimmäiseltä riviltä ja poistetaan rivi
      colnames(tulostettavaData) = tulostettavaData[1,]
      tulostettavaData <- tulostettavaData[-1,]
      
      #Lisätään koko yliopiston arvot
      
      kokoYliopistoArvot <- newdata[grep("^12$",newdata$tdk,ignore.case=T),]
      kokoYliopistoTyoelama <- kokoYliopistoArvot[grep("Tyoelaman vaatimus", kokoYliopistoArvot$group,ignore.case=T), ]
      kokoYliopistoYliopisto <- kokoYliopistoArvot[grep("Yliopisto-opiskelu kehitti", kokoYliopistoArvot$group,ignore.case=T), ]
      
      kolmasRivi = c("koko yliopisto, tyoelämän vaatimus")
      
      #Käydään läpi kaikki vuodet ja haetaan riville työelämän vaatimukset
      for(i in 2003:2011) {
        if (!i%%2) {
          next
        }
        tmpDataTyoelama <- kokoYliopistoTyoelama[grep(i,kokoYliopistoTyoelama$valmvuosi,ignore.case=T),]
        kolmasRivi <- c(kolmasRivi, tmpDataTyoelama[,"arvo"])
      }

      #Lisätään rivi tulostettavaan dataan
      tulostettavaData <- rbind(tulostettavaData, kolmasRivi)
      
      #Yliopisto-opiskelu kehitti
      
      neljasRivi = c("koko yliopisto, yliopisto-opiskelu kehitti")
      
      #Käydään läpi kaikki vuodet ja haetaan riville työelämän vaatimukset
      for(i in 2003:2011) {
        if (!i%%2) {
          next
        }
        tmpDataYliopisto <- kokoYliopistoYliopisto[grep(i,kokoYliopistoYliopisto$valmvuosi,ignore.case=T),]
        neljasRivi <- c(neljasRivi,tmpDataYliopisto[,"arvo"])
      }
      
      #Lisätään rivi tulostettavaan dataan
      tulostettavaData <- rbind(tulostettavaData, neljasRivi)
      
      DT::datatable(tulostettavaData, rownames=FALSE, options = list(
        pageLength = 4))
      
    })
    
    #viivakuva, oppialaryhmittäin
    
    lineInputKouala <- reactive({
      newdata <- mydataViiva[ which(mydataViiva$kouala==input$viivaKouala
                                    & mydataViiva$muuttuja == input$viivaMuutKouala), ]
      
      p <- ggplot(newdata, aes(x=valmvuosi, y=arvo, group=group, color=group)) +
        geom_line(aes(linetype=group), size=1) +
        geom_point(color='black', size = 5, shape=1) +
        theme_classic(base_size=16) +
        scale_y_continuous(name="Keskiarvo", breaks = c(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0), limits=c(1,6)) +
        scale_x_continuous(name="Valmistumisvuosi", breaks = c(2003, 2005, 2007, 2009, 2011), limits=c(2002.5,2011.5)) +
        ggtitle(paste0("Osaamisalueen kehitys vuosittain, ", newdata$koualanimi, "\n(", newdata$muuttujanimi, ")")) +
        coord_fixed(ratio=1.5) 
    })
    
    output$Viivakuva_kouala <- reactivePlot(function() {
      print(lineInputKouala())
    })
    
    
    
    output$taulukko_kouala <- DT::renderDataTable({
      newdata <- mydataViiva[ which((mydataViiva$kouala==input$viivaKouala | mydataViiva$kouala == 12)
                                    & mydataViiva$muuttuja == input$viivaMuutKouala), ]
      newdata <- newdata[,-c(0,2,3,6,7)]
      
      koualanimidata <- dplyr::filter(newdata, kouala > 12)  
      
      koualanimidata <- koualanimidata[1,]
      
      tulostettavaData <- c("Kohta", 2003, 2005, 2007, 2009, 2011)
      
      if (nrow(newdata) < 9) koualanimi = newdata[1,3] else koualanimi = newdata[9,3] #hakee koulutusalannimen nimen. if tarvitaan tarkistamaan onko koko yliopisto
      
      kohdeTdkArvot <- newdata[grep(input$viivaKouala,newdata$kouala,ignore.case=T),]
      kohdeTdkTyoelama <- kohdeTdkArvot[grep("Tyoelaman vaatimus", kohdeTdkArvot$group,ignore.case=T), ]
      kohdeTdkYliopisto <- kohdeTdkArvot[grep("Yliopisto-opiskelu kehitti", kohdeTdkArvot$group,ignore.case=T), ]
      
      if (is.na(koualanimidata$koualanimi) ) {
        ensimmainenRivi = "koko yliopisto, työelämän vaatimus"
        toinenRivi = "koko yliopisto, yliopisto-opiskelu kehitti"
      } else {
        ensimmainenRivi = c(paste0(koualanimidata$koualanimi,", tyoelämän vaatimus"))
        toinenRivi = c(paste0(koualanimidata$koualanimi,", yliopisto-opiskelu kehitti"))
      }
      
      #Käydään läpi kaikki vuodet ja haetaan riville työelämän vaatimukset
      for(i in 2003:2011) {
        if (!i%%2) {
          next
        }
        tmpDataTyoelama <- kohdeTdkTyoelama[grep(i,kohdeTdkTyoelama$valmvuosi,ignore.case=T),]
        ensimmainenRivi <- c(ensimmainenRivi,tmpDataTyoelama[,"arvo"])
      }
      
      #Lisätään rivi tulostettavaan dataan
      tulostettavaData <- rbind(tulostettavaData, ensimmainenRivi)

      #Käydään läpi kaikki vuodet ja haetaan riville työelämän vaatimukset
      for(i in 2003:2011) {
        if (!i%%2) {
          next
        }
        tmpDataYliopisto <- kohdeTdkYliopisto[grep(i,kohdeTdkTyoelama$valmvuosi,ignore.case=T),]
        toinenRivi <- c(toinenRivi,tmpDataYliopisto[,"arvo"])
      }
      
      #Lisätään rivi tulostettavaan dataan
      tulostettavaData <- rbind(tulostettavaData, toinenRivi)
      
      #Otetaan otsikot ensimmäiseltä riviltä ja poistetaan rivi
      colnames(tulostettavaData) = tulostettavaData[1,]
      tulostettavaData <- tulostettavaData[-1,]
      
      #Lisätään koko yliopiston arvot
      
      kokoYliopistoArvot <- newdata[grep("^12$",newdata$kouala,ignore.case=F),]
      kokoYliopistoTyoelama <- kokoYliopistoArvot[grep("Tyoelaman vaatimus", kokoYliopistoArvot$group,ignore.case=T), ]
      kokoYliopistoYliopisto <- kokoYliopistoArvot[grep("Yliopisto-opiskelu kehitti", kokoYliopistoArvot$group,ignore.case=T), ]
      
      kolmasRivi = c("koko yliopisto, tyoelämän vaatimus")
      
      #Käydään läpi kaikki vuodet ja haetaan riville työelämän vaatimukset
      for(i in 2003:2011) {
        if (!i%%2) {
          next
        }
        tmpDataTyoelama <- kokoYliopistoTyoelama[grep(i,kokoYliopistoTyoelama$valmvuosi,ignore.case=T),]
        kolmasRivi <- c(kolmasRivi, tmpDataTyoelama[,"arvo"])
      }
      
      
      #Lisätään rivi tulostettavaan dataan
      tulostettavaData <- rbind(tulostettavaData, kolmasRivi)
      
      #Yliopisto-opiskelu kehitti
      
      neljasRivi = c("koko yliopisto, yliopisto-opiskelu kehitti")
      
      #Käydään läpi kaikki vuodet ja haetaan riville työelämän vaatimukset
      for(i in 2003:2011) {
        if (!i%%2) {
          next
        }
        tmpDataYliopisto <- kokoYliopistoYliopisto[grep(i,kokoYliopistoYliopisto$valmvuosi,ignore.case=T),]
        neljasRivi <- c(neljasRivi,tmpDataYliopisto[,"arvo"])
      }
      
      #Lisätään rivi tulostettavaan dataan
      tulostettavaData <- rbind(tulostettavaData, neljasRivi)
      
      DT::datatable(tulostettavaData, rownames=FALSE, options = list(
        pageLength = 4))
      
    })
  
}

shinyApp(ui = ui, server = server)
