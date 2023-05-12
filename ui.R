

library(shiny)
library(dplyr)
library(plotly)
library(shinycssloaders)

id <- 186624
x <- 1:id
fi <- ((((sqrt(5) + 1) / 2)) * x) %% 1
exx <- (exp(1) * x) %% 1
pix <- (pi * x) %% 1
exm <- ((x / exp(1))) %% 1
pim <- (x / pi) %% 1
lf <- log(fi)
laf <- abs(log(fi))
lex <- log(exx)
lem <- log(exm)
laex <- abs(log(exx))
laem <- abs(log(exm))
lpx <- log(pix)
lpm <- log(pim)
lapx <- abs(log(pix))
lapm <- abs(log(pim))
red <- 1 + ((x %% 9) - 1)
dec2 <- ntile(x, 2)
dec9 <- ntile(x, 9)
dec10 <- ntile(x, 10)


df <-as_tibble(x,fi,exx,exm,pix,pim,lf,laf,lex,lem,laex,laem,lpx,lpm,lapx,lapm,red,dec9,dec2,dec10) %>%
  mutate(red = as.factor(red),dec9 = as.factor(dec9),dec2 = as.factor(dec2),dec10 = as.factor(dec10)) %>%
  mutate(x = as.numeric(x),fi = as.numeric(fi),exx = as.numeric(exx),exm = as.numeric(exm),pix = as.numeric(pix),
    pim = as.numeric(pim),lf = as.numeric(lf),laf = as.numeric(laf),lex = as.numeric(lex),laex = as.numeric(laex),
    laem = as.numeric(laem),lpx = as.numeric(lpx),lpm = as.numeric(lpm),lapx = as.numeric(lapx),lapm = as.numeric(lapm))

cc<-c("x","fi","exx","exm","pix","pim","lf","laf","lex","lem","laex","laem","lpx","lpm","lapx","lapm")

pais_df <- read.csv("paises.csv")



#############################################################################################################

ui <- fluidPage(
  # Application title
  titlePanel(
    title = div(" ♔ ♕ ♖ ♗ ♘ ♙ Chess Mining ♟ ♞ ♝ ♜ ♛ ♚", align = "center"),
    windowTitle = ("Chess Mining")
  ),
  
  navbarPage(title = "CM", position ="static-top", collapsible =T,
  #img(src = 'mini.png', height = '28px')",
  
  
  tabPanel("Inicio",
           mainPanel(
               width = 9,
               h1("Las ideas principales son:", align ="center"),
               br(),
               br(),
               h2("1. Construye un Blockchain entre todφs", align ="center"),
               hr(),
               h2("2. Almacena información inmutable", align ="center"),
               hr(),
               h2("3. Crea dinero con nuestros datos", align ="center"),
               hr(),
               h2("4. Inteligencia Artificial para la Humanidad", align ="center"),
               hr(),
               h2("5. Irracionales= φ, e y π, velocidad de la luz", align ="center"),
               hr(),
               h2("6. Unidad tecnológica y social",  align ="center"),
               hr(),
               br()
               
               
               
             ),
             
             sidebarPanel(
               width = 3,
               textOutput("greeting"),
               h2(" Regístrate ", align = "center"),
               h5("¡comienza a minar hoy!", align ="center"),
               textInput("nam", "Nombre(s):", ""),
               textInput("sur", "Apellido(s):", ""),
               dateInput(
                 "date",
                 "Fecha de nacimiento:",
                 startview = "decade",
                 value = "2012-12-21",
                 format = "dd / M / yyyy",
                 width = "40%"
               ),
               selectInput("pais", "País", choices = pais_df$nombre, width = "60%"),
               actionButton("calculate", "¡Vamos!", align =
                              "center"),
               br(),
               hr(),
               helpText(
                 "*Tu información es privada, valiosa y escriptada. ♫ Recuérdalo ♪",
                 align = "center"
               )
             )
           ),
  #----
  #Irracionales
  navbarMenu("Matemáticas",
             tabPanel(
               "Irracionales | φ | e | π |",
               
               fluidPage(
                 titlePanel("Gráfica polar"),
                 sidebarLayout(
                   sidebarPanel(width=3,
                                selectInput(
                                  "irra",
                                  "Elige tu irracional",
                                  c(
                                    "φ" = "fi",
                                    "ε multiplicación [*]" = "exx",
                                    "ε división [÷]" = "exm",
                                    "π  multiplicación [*]" = "pix",
                                    "π  división [÷]" = "pim",
                                    "Log φ" = "lf",
                                    "Log ε*" = "lex",
                                    "Log ε÷" = "lem",
                                    "Log π*" = "lpx",
                                    "Log π÷" = "lpm",
                                    "Log+Abs φ" = "laf",
                                    "Log+Abs ε*" = "laex",
                                    "Log+Abs ε÷" = "laem",
                                    "Log+Abs π*" = "lapx",
                                    "Log+Abs π÷" = "lapm"
                                  ),
                                  selected = "fi"
                                ),
                                hr(),
                                
                                sliderInput(
                                  "num",
                                  "Número de observaciones",
                                  min = 0,
                                  max = 186624,
                                  value = 186624/3,
                                  step = 12
                                ),
                                hr(),
                                sliderInput(
                                  "tra",
                                  "Transparencia (%)",
                                  min = 0,
                                  max = 1,
                                  value = .72,
                                  step = 0.016
                                ),
                                hr(),
                                sliderInput(
                                  "tam",
                                  "Tamaño del punto (px)",
                                  min = 0,
                                  max = 12.6,
                                  value = 1,
                                  step = 0.1
                                ),
                                #actionButton("resetBtn", "Reajustar"),
                                sliderInput(
                                  "alt",
                                  "Alto (px)",
                                  min = 21,
                                  max = 1728,
                                  value = 864,
                                  step = 72
                                ),
                                sliderInput(
                                  "anh",
                                  "Ancho (px)",
                                  min = 21,
                                  max = 1728,
                                  value = 864,
                                  step = 72
                                ),
                                h3("Irracionales | φ | e | π |"),
                                textOutput("onum"),
                                br()),
                   mainPanel(width = 9,
                             div(plotlyOutput("p", height = "100%"),align = "center"),
                             br(),br())
                 )
               ),
               
               
                 ###//////////////////////////////////////////////////////////
               fluidPage(
                 titlePanel("Cuadrado Mágico"),
                 sidebarLayout(
                   mainPanel(width = 9,
                             div(plotlyOutput("cm", height = "100%"), align = "center")),
                   sidebarPanel(width = 3, position="rigth",
                                sliderInput(
                                  "ncc",
                                  "Número de observaciones",
                                  min = 0,
                                  max = 186624,
                                  value = 186624,
                                  step = 12
                                ),
                                selectInput(
                                  "siz",
                                  "Tamaño del punto (px)",
                                  c(
                                    "0.5",
                                    "1",
                                    "φ = 1.618" = "1.61801",
                                    "2",
                                    "e = 2.718" = "2.7182",
                                    "3",
                                    "π = 3.1415" = "3.1415",
                                    "4",
                                    "5",
                                    "6",
                                    "7"
                                  ), selected = "0.5"
                                ),
                                sliderInput(
                                  "tra1",
                                  "Transparencia (%)",
                                  min = 0,
                                  max = 1,
                                  value = 1,
                                  step = 0.11
                                ),
                                sliderInput(
                                  "alt1",
                                  "Alto (px)",
                                  min = 21,
                                  max = 1728,
                                  value = 864,
                                  step = 72
                                ),
                                sliderInput(
                                  "anh1",
                                  "Ancho (px)",
                                  min = 21,
                                  max = 1728,
                                  value = 864,
                                  step = 72
                                ))
                   
                 )
               ),
               
               
               
               
                
                 
                 ###//////////////////////////////////////////////////////////
               
               fluidPage(
                 titlePanel("Mapa de calor"),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                sliderInput(
                                  "n2d",
                                  "Número de observaciones",
                                  min = 0,
                                  max = 186624/96,
                                  value = 186624/96,
                                  step = 12
                                ),
                                selectInput(
                                  "irra2d",
                                  "X",c("x","fi","exx","exm","pix","pim","lf","laf","lex","lem","laex","laem","lpx","lpm","lapx","lapm"), selected = "fi"),
                                selectInput(
                                  "irra2dy",
                                  "Y",c("fi","exx","exm","pix","pim","lf","laf","lex","lem","laex","laem","lpx","lpm","lapx","lapm"), selected = "pim"),
                               ),
                   mainPanel(width = 9,
                             div(plotlyOutput("hex2d", height = "864px", width="1296px"), align = "center"))
                 )
               )
             ),
               
               
               
           
  
  
  #----
  # C= Luz
  tabPanel("Velocidad de la Luz = 432²", ),
  
  #----
  # 963 a Primos
  tabPanel("Números Primos = 963 vs. 875421", ),
  
),

tabPanel(
  "Minería",
  br(),
  
  h1("5 Reglas del Juego", align = "center"),
  br(),
  
  h2("1. Protocolo de consenso: 1+1...", align = "center"),
  br(),
  h2("2. Elige tu color: negro, blanco o dorado", align = "center"),
  br(),
  h2("3. Tamaño de bloque <= 1 Quijote (1mb)", align = "center"),
  br(),
  h2("4. Tiempo: no más de 1000 bloques", align = "center"),
  br(),
  h2("5. Creación de monedas", align = "center"),
  br(),
  p(
    "Para poder minar será necesario contar con algún dispositivo
                        capaz de realizar la regla 1 y conectarse a una red celular o WiFi
                        en el día que el bloque sea minado",
    align = "center"
  ),
  br(),
),


tabPanel("NFT", )

))
