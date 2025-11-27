packages=c("shiny",
           "shinydashboard", 
           "shinyjs",
           "shinydashboardPlus", 
           "shinyWidgets", 
           "shinythemes",
           "leaflet",
           "readxl",
           "dplyr", 
           "sf", 
           "raster",
           "readr",
           "tidyr",
           "ggplot2",
           "lubridate",
           "magrittr",
           "plotly",
           "stringr",
           "showtext")

package.check <- lapply(packages, FUN=function(x) {
  if (!require(x, character.only=TRUE)) {
    install.packages(x, dependencies=TRUE,repos='http://cran.rediris.es')
    library(x, character.only=TRUE)
  }
})

# CARGA BASE DE DATOS ALIMENTOS
archivo_excel <- "datos/recetas.xlsx"

# Cargar la primera hoja del archivo Excel
recetas <- read_excel(archivo_excel)
recetas$id = 1:dim(recetas)[1]

nutr <- recetas[,c(5,4)]
nutr$informacion_nutricional[1]

# Inicializa un data.frame vacío para almacenar los valores
df_nutricional <- data.frame(
  kcal = numeric(),
  grasa = numeric(),
  carbohidratos = numeric(),
  proteinas = numeric(),
  stringsAsFactors = FALSE
)

# Procesa cada cadena de texto de la lista
for (texto in nutr$informacion_nutricional) {
  # Limpia la cadena de texto
  texto <- str_remove_all(texto, "\\[|\\]")
  texto_limpio <- str_trim(texto)
  
  # Inicializa un vector para almacenar los valores extraídos
  valores <- c(kcal = NA, grasa = NA, carbohidratos = NA, proteinas = NA)
  
  # Divide la cadena de texto en pares clave-valor
  pares <- strsplit(texto_limpio, ",")[[1]]
  pares <- str_trim(pares)
  
  # Procesa cada par clave-valor
  for (par in pares) {
    # Divide el par en clave y valor
    partes <- strsplit(par, ":")[[1]]
    clave <- str_trim(partes[1])
    valor <- str_trim(partes[2])
    
    # Asigna el valor a la clave correspondiente
    if (clave == "'calorías (kcal)") {
      valores["kcal"] <- as.numeric(sub("'", "", valor))
    } else if (clave == "'grasa (fat)") {
      # Elimina la 'g' del final y convierte a número
      valores["grasa"] <- as.numeric(sub("g'", "", valor))
    } else if (clave == "'hidratos de carbono (carbs)") {
      # Elimina la 'g' del final y convierte a número
      valores["carbohidratos"] <- as.numeric(sub("g'", "", valor))
    } else if (clave == "'proteínas (protein)") {
      # Elimina la 'g' del final y convierte a número
      valores["proteinas"] <- as.numeric(sub("g'", "", valor))
    }
  }
  
  # Añade los valores extraídos como una fila al `data.frame`
  df_nutricional <- rbind(df_nutricional, valores)
}

df_nutricional$id <- 1:dim(df_nutricional)[1]
df_nutricional <- na.omit(df_nutricional)
recetas <- recetas[df_nutricional$id,]
df_nutricional$id <- 1:dim(df_nutricional)[1]
recetas$id <- 1:dim(recetas)[1]
names(df_nutricional) <- (c("Kcal", "Grasa", "Carbohidratos", "Proteinas", "id"))

# CARGA BASE DE DATOS EJERCICIOS

ejercicios <- data.frame(
  Nombre = c(
    "Sentadillas", "Flexiones", "Correr", "Plancha", "Salto de cuerda",
    "Estocadas", "Curl de bíceps", "Zancadas", "Burpees", "Remo con mancuerna",
    "Prensa de pierna", "Elevación lateral", "Curl de piernas", "Fondos",
    "Flexiones de tríceps", "Peso muerto", "Elevación de piernas", "Salto de caja",
    "Remo invertido", "Extensión de tríceps", "Press de banca", "Press de hombros",
    "Salto en estrella", "Press militar", "Puente de glúteos", "Marcha del granjero",
    "Tijeras laterales", "Tijeras frontales", "Zancadas caminando", "Rotaciones de torso",
    "Press inclinado", "Curl con barra", "Escalador de montaña", "Crunch",
    "Superman", "Sentadillas búlgaras", "Elevación de talones", "Remo con barra",
    "Levantamiento de piernas colgado", "Elevación frontal", "Remo con banda de resistencia",
    "Crunch oblicuo", "Remo con TRX", "Press Arnold", "Salto en tijera", "Plancha lateral",
    "Remo de pie", "Kickboxing", "Levantamiento lateral con polea", "Tijeras de patada",
    "Remo sentado con polea", "Press de banca inclinado", "Sentadillas con salto",
    "Levantamiento de rodillas", "Salto en largo", "Estocadas inversas", "Sentadillas con salto de rodillas",
    "Levantamiento lateral con mancuernas", "Salto en tijera lateral", "Sentadillas con salto hacia adelante",
    "Estiramientos de hombros", "Trote", "Caminata", "Zancadas con salto",
    "Salto en cruz"
  ),
  Ejecucion = c(
    "De pie, baja como si te sentaras, luego vuelve a subir.",
    "Boca abajo, baja el cuerpo hasta cerca del suelo y luego sube.",
    "Corre a un ritmo constante.",
    "Boca abajo, con los antebrazos y pies en el suelo, mantén el cuerpo recto.",
    "Salta la cuerda de forma continua.",
    "Da un paso hacia adelante, baja la rodilla trasera, y regresa.",
    "De pie, levanta una pesa hacia el hombro y baja lentamente.",
    "Da un paso largo hacia adelante, baja hasta ángulos de 90 grados.",
    "Desde de pie, agáchate, haz una flexión y salta.",
    "De pie, inclínate hacia adelante y rema una mancuerna.",
    "Empuja las piernas contra una plataforma con peso.",
    "Levanta pesas hacia afuera desde los lados del cuerpo.",
    "Desde sentado, dobla las piernas hacia atrás.",
    "Baja los codos para levantar tu cuerpo.",
    "De pie, baja el cuerpo hacia atrás y levántalo.",
    "Levanta una barra con pesas desde el suelo.",
    "Acostado, levanta las piernas hacia arriba.",
    "Salta sobre una plataforma elevada.",
    "Sujeta una barra con peso y levántala.",
    "Extiende los brazos hacia atrás con pesas.",
    "Acuéstate y empuja pesas hacia arriba.",
    "Levanta pesas por encima de la cabeza.",
    "Salta abriendo y cerrando las piernas y brazos.",
    "Levanta pesas por encima de la cabeza desde un banco.",
    "Acuéstate con los pies en el suelo y levanta las caderas.",
    "Camina sosteniendo pesas en ambas manos.",
    "Da un paso lateral y baja el cuerpo.",
    "Desde una posición de pie, da pasos hacia adelante alternando piernas.",
    "Da pasos largos hacia adelante mientras mantienes el cuerpo recto.",
    "Agáchate y rota el torso de lado a lado con peso.",
    "Levanta una barra con pesas desde una inclinación.",
    "Levanta una barra de pesas hacia los hombros.",
    "Corre en el lugar, alternando rodillas hacia arriba.",
    "Acuéstate y levanta las rodillas hacia el pecho.",
    "Acuéstate boca abajo y levanta brazos y piernas.",
    "Apoya un pie en una banca y baja el otro hacia abajo.",
    "Eleva los talones hacia arriba mientras estás de pie.",
    "Inclínate hacia adelante y rema una barra con pesas.",
    "Levanta las piernas hacia arriba desde una barra.",
    "Levanta pesas hacia adelante desde los lados del cuerpo.",
    "Tira de una banda de resistencia hacia el pecho.",
    "Levanta el torso girando hacia un lado.",
    "Sujeta una banda TRX y rema hacia ti.",
    "Levanta pesas por encima de la cabeza desde una posición neutral.",
    "Salta alternando piernas hacia adelante y hacia atrás.",
    "Levanta el cuerpo de lado apoyándote en un codo.",
    "Tira de una cuerda hacia el pecho.",
    "Practica movimientos de kickboxing (patadas y puñetazos).",
    "Tira de una polea desde abajo hacia los lados del cuerpo.",
    "Salta y patea hacia adelante.",
    "Tira de una cuerda hacia tu pecho mientras estás sentado.",
    "Levanta pesas hacia arriba desde un banco inclinado.",
    "Realiza una sentadilla y salta hacia arriba con fuerza.",
    "Levanta las rodillas hacia arriba mientras te desplazas.",
    "Salta hacia adelante desde una posición estática.",
    "Da un paso hacia atrás y baja la rodilla, luego regresa.",
    "Realiza una sentadilla y salta hacia arriba con las rodillas hacia el pecho.",
    "Levanta pesas hacia los lados desde una posición de pie.",
    "Salta hacia los lados alternando piernas.",
    "Realiza una sentadilla y salta hacia adelante.",
    "Estira los hombros hacia los lados y hacia arriba.",
    "Corre a un ritmo moderado.",
    "Camina a un ritmo moderado.",
    "Da un paso largo hacia adelante y salta con fuerza.",
    "Salta abriendo y cerrando brazos y piernas."
    
  ),
  Tiempo = c(
    "10-15 repeticiones", "10-15 repeticiones", "30 minutos", "30-60 segundos",
    "10-15 minutos", "10 repeticiones por pierna", "10-15 repeticiones por brazo",
    "10 repeticiones por pierna", "10-15 repeticiones", "10-15 repeticiones por brazo",
    "10-15 repeticiones", "10-15 repeticiones por brazo", "10-15 repeticiones",
    "10 repeticiones", "10-15 repeticiones", "10-15 repeticiones", "10 repeticiones",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones por pierna",
    "10-15 repeticiones por pierna", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones por pierna", "10-15 repeticiones por pierna",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones por pierna", "10-15 repeticiones",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones",
    "10-15 repeticiones por pierna", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones",
    "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones", "10-15 repeticiones"
  ),
  Kcal = c(
    80, 85, 350, 25, 150, 60, 45, 65, 95, 60,
    110, 35, 80, 50, 40, 120, 55, 90, 70, 80,
    140, 100, 115, 110, 60, 75, 80, 65, 75, 110,
    125, 50, 300, 100, 65, 75, 65, 75, 110, 120,
    65, 90, 115, 125, 50, 55, 80, 90, 70, 100,
    100, 90, 125, 65, 75, 60, 80, 90, 70, 75,
    80, 65, 90, 70, 100
  ),
  Objetivo = c(
    "Ganar masa muscular", "Ganar masa muscular", "Perder peso",
    "Ganar resistencia", "Perder peso", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Perder peso",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar resistencia",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar masa muscular",
    "Ganar masa muscular", "Perder peso", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Perder peso",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Ganar masa muscular",
    "Perder peso", "Ganar masa muscular", "Ganar masa muscular",
    "Ganar masa muscular", "Perder peso", "Ganar masa muscular",
    "Ganar masa muscular", "Ganar masa muscular", "Perder peso",
    "Perder peso", "Perder peso", "Perder peso", "Perder peso",
    "Perder peso", "Perder peso", "Perder peso", "Perder peso",
    "Perder peso", "Perder peso", "Perder peso"
  )
)

# Definir usuarios y contraseñas 
usuarios <- c("1", "usuario2", "usuario3")
contrasenas <- c("1", "contrasena2", "contrasena3")

# Definir la interfaz de usuario para la página de inicio de sesión
ui_login <- fluidPage(
  tags$style(
    HTML("
    body {
      text-align: justify;
      font-family: 'Calibri', sans-serif;
      font-size: 18px; 
      margin-right: 20px;
      color: #000000 !important;
      background-color: #F4E1F4 !important;
    }
  ")
  ),
  titlePanel("Iniciar sesión"),
  textInput("usuario", "Usuario:"),
  passwordInput("contrasena", "Contraseña:"),
  actionButton("login", "Iniciar sesión")
)

ui_cuestionario <- fluidPage(
  # Estilos para la página de inicio de sesión y el cuestionario
  tags$style(
    HTML("
    body {
      text-align: justify;
      font-family: 'Calibri', sans-serif;
      font-size: 18px; 
      margin-right: 20px;
      color: #000000 !important;
      background-color: #F4E1F4 !important;
    }
  ")
  ),
  titlePanel("CUESTIONARIO INICIAL"),
  p('¡Bienvenido a FitFusion! Estamos emocionados de acompañarte 
    en tu viaje hacia un estilo de vida más saludable y equilibrado.'),
  p('Antes de comenzar, nos gustaría conocerte un poco mejor para adaptar nuestras
    recomendaciones a tus necesidades individuales. '),
  p('Por favor, tómate unos minutos para responder estas preguntas:'),
  selectInput("genero", "Género:", choices = c("Mujer" = 1, "Hombre" = 0)),
  textInput("edad", "Edad:"),
  textInput("altura", "Altura en centímetros:"),
  textInput("peso", "Peso en kilogramos:"),
  selectInput("objetivo", "Objetivo:", choices = c("Perder peso" = 1, "Ganar masa muscular" = 0)),
  radioButtons("nivel", "Selecciona tu nivel de actividad:", 
               choices = c("Bajo" = 1.2, "Medio" = 1.5, "Alto" = 1.725),inline = TRUE),
  actionButton("continuar", "Continuar")
)


# Definir la función para crear la interfaz de usuario de la página principal
ui_principal <- function(genero, edad, altura, peso, objetivo, nivel) {
  dashboardPage(
    dashboardHeader(title = tags$img(src = 'logo.png', height = "55px", width = "auto")),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebar",
        menuItem("Inicio", tabName = "inicio", icon = icon("home")),
        menuItem("Ejercicios", tabName = "ejercicios", icon = icon("dumbbell")),
        menuItem("Alimentos", tabName = "alimentos", icon = icon("utensils")),
        menuItem("Social", tabName = "corazon", icon = icon("heart")),
        menuItem("Soporte y ayuda", tabName = "atencion", icon = icon("circle-question"))
      ),
      tags$style(
        HTML("
             .main-sidebar {
                background-color: #F4E1F4 !important;
             }
             .skin-black .main-sidebar .sidebar .sidebar-menu .menu-text {
                color: #000000 !important;
             }
             ")
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(HTML("
        /* Justificar todo el texto y ajustar tamaño de letra */
      body {
        text-align: justify;
        font-family: 'Calibri', sans-serif;
        font-size: 18px; 
        margin-right: 20px;
        color: #000000 !important;
        background-color: #F4E1F4 !important;
      }
      
      /* Cambiar la fuente y tamaño de los títulos */
      h1 {
        font-family: 'Calibri', sans-serif; 
        font-size: 32px; 
        margin-right: 20px;
        color: #AA16D6 !important;
        font-weight: bold;
      }
      
      h3 {
        font-family: 'Calibri', sans-serif; 
        font-size: 24px;
        margin-right: 20px;
        color: #000000 !important;
        font-weight: bold;
      }
      
      h4 {
        font-family: 'Calibri', sans-serif; 
        font-size: 20px;
        margin-right: 20px;
        color: #000000 !important;
        font-weight: bold;
      }
      
      p {
      margin-right: 20px; 
      }

      /* Establecer color de fondo del contenido */
      .content-wrapper {
            background-color: #FFFFFF !important; 
      }
      "))
      ),
      tabItems(
        tabItem(tabName = "inicio",
                h1("NOVEDADES!!!"),
                p("FitFusion no solo recomienda ejercicios 
                  y dietas a través de una plataforma, sino que también ofrece 
                  un espacio interactivo que permite a los usuarios conectarse y 
                  compartir experiencias con otros miembros. Esta diferenciación 
                  en la experiencia del usuario no solo mejora el compromiso del 
                  cliente, sino que también nos destaca frente a la competencia, 
                  atrayendo así a un mayor número de usuarios potenciales."),
                br(),
                h1("FRASE DEL DIA"),
                p('"El éxito no es definitivo, el fracaso no es fatal: es el coraje
                  para continuar lo que cuenta." - Winston Churchill'),
                br(),
                h1("FOTOS DE LOS USUARIOS"), 
                fluidRow(
                  column(3, img(src = "imagen1.jpg", style = "max-width: 100%;")),
                  column(3, img(src = "imagen2.jpg", style = "max-width: 100%;")),
                  column(3, img(src = "imagen3.jpg", style = "max-width: 100%;")),
                  column(3, img(src = "imagen4.jpg", style = "max-width: 100%;"))
                ),
                br(),
                h1("TESTIMONIOS DE USUARIOS"),
                # Sección de testimonios de usuarios
                fluidRow(
                  column(12,
                         div(
                           style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px;",
                           h4("Testimonio de Juan Pérez"),
                           p("¡Gracias a FitFusion he logrado perder 10 kilos en solo 3 meses! La variedad de ejercicios 
                             y la comunidad de apoyo me han mantenido motivado todo el tiempo.")
                         )
                  ),
                  column(12,
                         div(
                           style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px;",
                           h4("Testimonio de María González"),
                           p("FitFusion ha cambiado mi vida. Con su ayuda, he adoptado un estilo de vida más saludable y
                             he alcanzado mis metas de fitness. ¡Lo recomiendo a todos mis amigos!")
                         )
                  )
                ),
        ),
        tabItem(tabName = "ejercicios",
                h1("EJERCICIOS PARA HOY"),
                p('A partir de los datos obtenidos en el cuestionario inicial se han generado 
                  una serie de ejercicios que te ayudarán a conseguir el objetivo fijado.'),
                h3("EJERCICIOS PROPUESTOS:"),
                uiOutput("ejercicios")
        ),
        tabItem(tabName = "alimentos",
                h1("DESAYUNO DE HOY:", textOutput('nombre_desayuno')),
                h3('Ingredientes'),
                textOutput('ingrediente_desayuno'),
                h3('Valor nutricional'),
                textOutput('valores_desayuno'),
                h3('Preparación'),
                textOutput('preparacion_desayuno'),
                br(),
                h1("COMIDA DE HOY", textOutput('nombre_comida')),
                h3('Ingredientes'),
                textOutput('ingrediente_comida'),
                h3('Valor nutricional'),
                textOutput('valores_comida'),
                h3('Preparación'),
                textOutput('preparacion_comida'),
                br(),
                h1("CENA DE HOY", textOutput('nombre_cena')),
                h3('Ingredientes'),
                textOutput('ingrediente_cena'),
                h3('Valor nutricional'),
                textOutput('valores_cena'),
                h3('Preparación'),
                textOutput('preparacion_cena')
        ),
        tabItem(tabName = "corazon",
                h1("COMPARTE TU PROGRESO Y TUS LOGROS"),
                # Sección para subir imágenes
                fluidRow(
                  column(12,
                         fileInput("imagen", "Publica tu recuerdo", accept = c('image/png', 'image/jpeg'))
                  )
                ),
                # Sección para visualizar publicaciones
                fluidRow(
                  column(12,
                         # Publicación de ejemplo
                         div(
                           style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px;",
                           img(src = "ejercicio_redsocial.jpg", style = "max-width: 100%;"),
                           p("¡Espectacular día para hacer ejercicio! 💪🌞"),
                           p("👍 15 Likes   💬 5 Comentarios")
                         )
                  )
                ),
                # Sección para comentarios y likes
                fluidRow(
                  column(6,
                         actionButton("like", icon("heart"), style = "width: 10%; margin-right: 5px; color: red;"),
                         actionButton("agregar_comentario", icon("comment"), style = "width: 10%; color: blue;")
                  )
                )
        ),   
        tabItem(tabName = "atencion",
                h1("SOPORTE Y AYUDA"),
                h3("Contáctanos"),
                p("Si tienes alguna pregunta o necesitas ayuda, por favor completa el formulario de contacto."),
                br(),
                textInput("nombre_contacto", "Nombre:"),
                textInput("correo_contacto", "Correo electrónico:"),
                textAreaInput("mensaje_contacto", "Mensaje:"),
                actionButton("enviar_mensaje", "Enviar mensaje"),
                br(),
                h3("Preguntas frecuentes"),
                fluidRow(
                  column(6,
                         h4("¿Cómo puedo restablecer mi contraseña?"),
                         p("Para restablecer tu contraseña, ve a la página de inicio de sesión y haz clic en '¿Olvidaste tu contraseña?' 
                           para recibir instrucciones."),
                         br(),
                         h4("¿Cómo puedo actualizar mi información de perfil?"),
                         p("Puedes actualizar tu información de perfil en la página de configuración de tu cuenta."),
                         br(),
                         h4("¿Qué hago si tengo problemas técnicos con la aplicación?"),
                         p("Si experimentas problemas técnicos, por favor contáctanos utilizando el formulario de contacto 
                           y nuestro equipo de soporte te ayudará lo antes posible.")
                  )
                ),
                br(),
                h3("Información de contacto"),
                p("Si necesitas comunicarte con nosotros por otros medios, aquí tienes nuestra información de contacto:"),
                p("Correo electrónico: support@fitfusion.com"),
                p("Teléfono: 626 38 90 14"),
                p("Síguenos en redes sociales:"),
                p(HTML("<a href='https://www.facebook.com/fitfusion'>Facebook</a>, 
                       <a href='https://twitter.com/fitfusion'>Twitter</a>, <a href='https://www.instagram.com/fitfusion'>Instagram</a>"))
        )
        
      )
    )
  )
}

server <- function(input, output, session) {
  # Variable de estado para controlar la etapa del flujo de la aplicación
  etapa <- reactiveVal("inicio_sesion")
  
  # Variable reactiva para almacenar las respuestas del cuestionario
  respuestas <- reactiveValues(
    genero = NULL,
    edad = NULL,
    altura = NULL,
    peso = NULL,
    objetivo = NULL,
    nivel = NULL
  )
  
  observeEvent(input$login, {
    usuario <- input$usuario
    contrasena <- input$contrasena
    
    # Verificar si el usuario y la contraseña son válidos
    if (usuario %in% usuarios && contrasena %in% contrasenas) {
      # Si son válidos, avanzar a la siguiente etapa (cuestionario)
      etapa("cuestionario")
    } else {
      # Si no son válidos, mostrar un mensaje de error
      showModal(modalDialog(
        title = "Error de inicio de sesión",
        "Usuario o contraseña incorrectos. Por favor, inténtalo de nuevo.",
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$continuar, {
    # Guardar las respuestas del cuestionario
    respuestas$genero <- input$genero
    respuestas$edad <- as.numeric(input$edad)
    respuestas$altura <- as.numeric(input$altura)
    respuestas$peso <- as.numeric(input$peso)
    respuestas$objetivo <- input$objetivo
    respuestas$nivel <- as.numeric(input$nivel)
    
    # MODELO PARA LAS RECETAS
    if (respuestas$genero == 0) {
      TMB <- (10 * respuestas$peso) + (6.25 * respuestas$altura) - (5 * respuestas$edad) + 5
    } else {
      TMB <- (10 * respuestas$peso) + (6.25 * respuestas$altura) - (5 * respuestas$edad) - 161
    }
    
    # Calcular el GET
    GET <- TMB * respuestas$nivel
    
    # Ajustar GET según el objetivo
    if (respuestas$objetivo == 0) {
      calorias_objetivo <- GET + 300  # Añadir un superávit de 300 calorías
    } else {
      calorias_objetivo <- GET - 300  # Aplicar un déficit de 300 calorías
    }
    
    # Establecer gramaje de proteínas por kilogramo de peso corporal según el objetivo
    if (respuestas$objetivo == 0) {
      proteinas_por_kg <- 1.6  # Puedes ajustar entre 1.6 - 2.2 según el objetivo
    } else {
      proteinas_por_kg <- 1.2  # Puedes ajustar entre 1.2 - 2.0 según el objetivo
    }
    
    # Calcular proteínas totales en gramos
    proteinas_totales <- respuestas$peso * proteinas_por_kg
    
    # Calcular calorías de proteínas
    calorias_proteinas <- proteinas_totales * 4  # 1 gramo de proteína = 4 kcal
    
    # Establecer el porcentaje de calorías de grasas (30% como ejemplo)
    porcentaje_grasas <- 0.3
    
    # Calcular calorías de grasas
    calorias_grasas <- calorias_objetivo * porcentaje_grasas
    
    # Calcular gramos de grasas
    grasas_totales <- calorias_grasas / 9  # 1 gramo de grasa = 9 kcal
    
    # Calcular calorías de carbohidratos restantes
    calorias_carbohidratos <- calorias_objetivo - calorias_proteinas - calorias_grasas
    
    # Calcular gramos de carbohidratos
    carbohidratos_totales <- calorias_carbohidratos / 4  # 1 gramo de carbohidrato = 4 kcal
    
    numeros_aleatorios <- sample(1:dim(df_nutricional)[1], size = 50)
    
    # Definir los objetivos de calorías, grasas, carbohidratos y proteínas
    objetivos <- c(
      kcal_objetivo = calorias_objetivo,
      grasa_objetivo = grasas_totales,
      hidratos_objetivo = carbohidratos_totales,
      proteina_objetivo = proteinas_totales
    )
    
    # Crear una función para calcular la desviación entre las recetas seleccionadas y los objetivos
    calcular_desviacion <- function(recetas, objetivos) {
      total <- colSums(recetas)
      desviacion <- sum((total - objetivos)^2)
      return(desviacion)
    }
    
    # Calcular todas las combinaciones de 3 recetas posibles
    combinaciones <- combn(numeros_aleatorios, 3, simplify = FALSE)
    
    # Inicializar un data frame para guardar las combinaciones y sus desviaciones
    resultados <- data.frame(
      recetas = I(vector("list", length(combinaciones))),
      desviacion = numeric(length(combinaciones))
    )
    
    # Iterar sobre cada combinación de recetas
    for (i in seq_along(combinaciones)) {
      # Seleccionar las recetas correspondientes a la combinación
      recetas_seleccionadas <- df_nutricional[combinaciones[[i]], c("Kcal", "Grasa", "Carbohidratos", "Proteinas")]
      
      # Calcular la desviación respecto a los objetivos
      desviacion <- calcular_desviacion(recetas_seleccionadas, objetivos)
      
      # Guardar la combinación de recetas y su desviación en el data frame
      resultados$recetas[[i]] <- combinaciones[[i]]
      resultados$desviacion[i] <- desviacion
    }
    
    
    # Ordenar las combinaciones por desviación (menor desviación primero)
    resultados <- resultados %>%
      arrange(desviacion)
    
    # Seleccionar las 100 mejores combinaciones
    mejores_combinaciones <- resultados %>%
      head(100)
    
    receta <- mejores_combinaciones[sample(1:100),'recetas'][[1]]
    
    nombre_desayuno <- recetas[recetas$id == receta[1],'nombre'][[1]]
    ingrediente_desayuno <- recetas[recetas$id == receta[1],'ingredientes'][[1]]
    preparacion_desayuno <- recetas[recetas$id == receta[1],'preparacion'][[1]]
    valores_desayuno <- recetas[recetas$id == receta[1],'informacion_nutricional'][[1]]
    
    output$nombre_desayuno <- renderText(nombre_desayuno)
    output$ingrediente_desayuno <- renderText(ingrediente_desayuno)
    output$valores_desayuno <- renderText(valores_desayuno)
    output$preparacion_desayuno <- renderText(preparacion_desayuno)
    
    nombre_comida <- recetas[recetas$id == receta[2],'nombre'][[1]]
    ingrediente_comida <- recetas[recetas$id == receta[2],'ingredientes'][[1]]
    preparacion_comida <- recetas[recetas$id == receta[2],'preparacion'][[1]]
    valores_comida <- recetas[recetas$id == receta[2],'informacion_nutricional'][[1]]
    
    output$nombre_comida <- renderText(nombre_comida)
    output$ingrediente_comida <- renderText(ingrediente_comida)
    output$valores_comida <- renderText(valores_comida)
    output$preparacion_comida <- renderText(preparacion_comida)
    
    nombre_cena <- recetas[recetas$id == receta[3],'nombre'][[1]]
    ingrediente_cena <- recetas[recetas$id == receta[3],'ingredientes'][[1]]
    preparacion_cena <- recetas[recetas$id == receta[3],'preparacion'][[1]]
    valores_cena <- recetas[recetas$id == receta[3],'informacion_nutricional'][[1]]
    
    output$nombre_cena <- renderText(nombre_cena)
    output$ingrediente_cena <- renderText(ingrediente_cena)
    output$valores_cena <- renderText(valores_cena)
    output$preparacion_cena <- renderText(preparacion_cena)
    
    
    # MODELO PARA LOS EJERCICIOS
    output$ejercicios <- renderUI({
      # Mapear los valores del selectInput a los valores reales del objetivo
      objetivo_seleccionado <- ifelse(input$objetivo == 1, 'Perder peso', 'Ganar masa muscular')
      
      # Filtrar los ejercicios basados en el objetivo seleccionado
      ejercicios_filtrados <- ejercicios[ejercicios$Objetivo == objetivo_seleccionado, ]
      
      # Obtener el número total de ejercicios que cumplen con el objetivo
      num_ejercicios <- nrow(ejercicios_filtrados)
      
      # Si hay menos de 5 ejercicios que cumplen con el objetivo, ajustar para mostrar todos los ejercicios disponibles
      num_a_mostrar <- min(5, num_ejercicios)
      
      # Seleccionar aleatoriamente 5 ejercicios
      indices_aleatorios <- sample(1:num_ejercicios, num_a_mostrar)
      ejercicios_mostrar <- ejercicios_filtrados[indices_aleatorios, ]
      
      # Crear una lista de etiquetas para mostrar la información de cada ejercicio
      etiquetas_ejercicios <- lapply(1:num_a_mostrar, function(i) {
        ejercicio <- ejercicios_mostrar[i, ]
        ejercicio_info <- paste(
          tags$span(tags$strong("Nombre del ejercicio: "), ejercicio$Nombre, style = "font-size: larger;"), "<br/>",
          "<strong>Ejecución:</strong> ", ejercicio$Ejecucion, "<br/>",
          "<strong>Tiempo de ejecución:</strong> ", ejercicio$Tiempo, "<br/>",
          "<strong>Kcal:</strong> ", ejercicio$Kcal, "<br/>",
          "<br/>"
        )
        tags$div(HTML(ejercicio_info))
      })
      
      # Convertir la lista de etiquetas en un objeto de UI
      do.call(tagList, etiquetas_ejercicios)
    })
      

    
    # Avanzar a la siguiente etapa (principal)
    etapa("principal")
  })
  
  # Observar cambios en la variable de etapa y renderizar dinámicamente la interfaz de usuario
  output$ui <- renderUI({
    if (etapa() == "inicio_sesion") {
      # Mostrar la página de inicio de sesión
      ui_login
    } else if (etapa() == "cuestionario") {
      # Mostrar el cuestionario
      ui_cuestionario
    } else if (etapa() == "principal") {
      # Mostrar la página principal
      ui_principal(respuestas$genero, respuestas$edad, respuestas$altura, 
                   respuestas$peso, respuestas$objetivo, respuestas$nivel)
    }
  })
}

# Ejecutar la aplicación
shinyApp(ui = function() { uiOutput("ui") }, server)