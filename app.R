library(shiny)
library(shinyjs)
library(tibble)

# ---------------------------
# Parámetros del juego
# ---------------------------
Numero_Cartas <- min(Numero_Cartas, nrow(qa_bank)*2)
# Debe ser par. Máximo: 2 * nrow(qa_bank)

# ---------------------------
# Banco de Preguntas/Respuestas - Interacción de la Radiación
# ---------------------------
qa_bank <- tibble::tribble(
  ~question,                                 ~answer,
  "¿Tipos de radiación?",                        "Partículas pesadas, electrones, neutrones, gamma.",
  "¿Con qué interactúan partículas pesadas?",    "Con electrones (ionización) y núcleos (elásticas).",
  "¿Qué son rayos δ?",                           "Electrones secundarios que ionizan.",
  "¿Qué describe Bethe-Bloch?",                  "Pérdida de energía por colisión.",
  "¿Qué es la curva de Bragg?",                  "Stopping power vs profundidad (pico al final).",
  "¿Importancia de Bremsstrahlung en e⁻?",       "Muy importante desde pocos MeV.",
  "¿Tres procesos de gamma?",                    "Fotoeléctrico, Compton, producción de pares.",
  "¿Proceso que absorbe todo el fotón?",         "Efecto fotoeléctrico.",
  "¿Ley de atenuación de gamma?",                "I = I₀ e^(-μx)",
  "¿Por qué neutrones interactúan poco?",        "Sin carga, solo fuerza nuclear (alcance corto).",
  "¿Ejemplo de reacción para detectar neutrones?", "³He(n,p)³H o ¹⁰B(n,α)⁷Li",
  "¿Qué es moderación?",                         "Neutrones pierden energía con núcleos ligeros.",
  "¿Buen moderador?",                            "Material ligero y con mucho hidrógeno.",
  "¿Cómo interactúan partículas cargadas?",      "De forma continua.",
  "¿Cómo interactúan los gamma?",                "Procesos discretos (poco probables).",
  "¿Material para espectroscopia gamma?",        "Alto Z (fotoeléctrico >> otros).",
  "¿Qué producen en el detector?",               "Electrones libres (rayos δ).",
  "¿Función de respuesta ideal?",                "Pico delta de Dirac.",
  "¿Qué causa la anchura del pico?",             "Fluctuaciones estadísticas.",
  "¿Qué es resolución energética (R)?",          "R = ΔE / E (FWHM / Energía).",
  "¿Ventaja semiconductores?",                   "Mejor resolución (Factor Fano < 1).",
  "¿Eficiencia intrínseca?",                     "Eventos detectados / eventos que llegan.",
  "¿Qué es pile-up?",                            "Señales que se superponen.",
  "¿Qué es dead time?",                          "Tiempo que el sistema no puede medir."
)

# ---------------------------
# UI
# ---------------------------
game_ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body, html { 
        background-color: #CCFF00 !important; 
        color: #0f172a !important; 
        font-family: 'Verdana', sans-serif; 
      }
      .board { display:flex; flex-wrap:wrap; justify-content:center; }
      .tile.btn {
        margin: 12px;
        width: 120px; height: 180px;
        border-radius: 12px;
        border: none;
        box-shadow: 2px 4px 10px rgba(0,0,0,0.25);
        transition: transform .12s ease, box-shadow .12s ease, background-color .15s ease;
        font-size: 18px;
        display:flex; align-items:center; justify-content:center;
        text-align:center;
        padding: 5px;
        white-space: normal;
      }
      .tile.btn:hover { transform: translateY(-2px); box-shadow: 4px 8px 18px rgba(0,0,0,0.25); }
      .tile.covered  { background-color: #334155 !important; }
      .tile.revealed { background-color: #10b981 !important; }
      .tile.matched  { background-color: #f59e0b !important; }
      .btn-primary {
        background: #2563eb !important;
        border: none !important;
        border-radius: 10px;
        padding: 10px 18px;
        font-family: 'Verdana', sans-serif;
      }
      .btn-primary:hover { filter: brightness(1.1); }
      .footer { text-align: center; font-size: 16px; margin-top: 30px; font-family: 'Verdana', sans-serif; }
    "))
  ),
  tags$h2("Memorama de Radiación"),
  tags$p("Haz match entre la pregunta y su respuesta correcta."),
  fluidRow(uiOutput("gameBoard")),
  div(style = "text-align: center; margin-top: 20px;",
      actionButton("reset_game", "Reiniciar Juego", class = "btn-primary")),
  div(style = "text-align: center; font-size: 24px; margin-top: 20px;",
      textOutput("game_end_message")),
  div(class="footer", "Juego creado por José Martín Valdez López")
)

# ---------------------------
# Server
# ---------------------------
game_server <- function(input, output, session) {
  
  make_deck <- function(n_pairs_wanted) {
    total_pairs <- nrow(qa_bank)
    n_pairs <- min(n_pairs_wanted, total_pairs)
    picked <- qa_bank[sample(total_pairs, n_pairs), ]
    picked <- cbind(picked, pair = seq_len(n_pairs))
    deck <- rbind(
      data.frame(idx=1:n_pairs, pair=picked$pair, face_type="Q", label=picked$question),
      data.frame(idx=(n_pairs+1):(2*n_pairs), pair=picked$pair, face_type="A", label=picked$answer)
    )
    deck[sample(nrow(deck)), ]
  }
  
  if (Numero_Cartas %% 2 != 0) stop("Numero_Cartas debe ser par.")
  requested_pairs <- Numero_Cartas / 2
  
  init_state <- function() {
    deck <- make_deck(requested_pairs)
    list(
      board      = deck,
      revealed   = rep(FALSE, nrow(deck)),
      matched    = rep(FALSE, nrow(deck)),
      first_pick = NULL,
      second_pick= NULL,
      lock_board = FALSE,
      game_won   = FALSE
    )
  }
  
  gs <- reactiveValues(!!!init_state())
  
  output$gameBoard <- renderUI({
    tiles <- lapply(seq_len(nrow(gs$board)), function(i) {
      is_revealed <- gs$revealed[i]
      is_matched  <- gs$matched[i]
      tile_class <- paste("tile",
                          if (is_matched) "matched" else if (is_revealed) "revealed" else "covered")
      label_text <- if (is_revealed || is_matched) gs$board$label[i] else "☢️"
      actionButton(inputId=paste0("tile_", i),
                   label=label_text,
                   class=tile_class)
    })
    div(class="board", do.call(tagList, tiles))
  })
  
  observeEvent(lapply(seq_len(nrow(gs$board)), function(i) input[[paste0("tile_", i)]]), {
    if (gs$lock_board) return()
    clicked_idx <- which(sapply(seq_len(nrow(gs$board)), function(i) input[[paste0("tile_", i)]] > 0))
    if (!length(clicked_idx)) return()
    idx <- clicked_idx[length(clicked_idx)]
    if (gs$revealed[idx] || gs$matched[idx]) return()
    if (is.null(gs$first_pick)) {
      gs$first_pick <- idx
      gs$revealed[idx] <- TRUE
      return()
    } else if (is.null(gs$second_pick) && idx != gs$first_pick) {
      gs$second_pick <- idx
      gs$revealed[idx] <- TRUE
    } else return()
    
    if (!is.null(gs$first_pick) && !is.null(gs$second_pick)) {
      gs$lock_board <- TRUE
      isolate({
        i1 <- gs$first_pick; i2 <- gs$second_pick
        same_pair <- gs$board$pair[i1] == gs$board$pair[i2]
        different_faces <- gs$board$face_type[i1] != gs$board$face_type[i2]
        if (same_pair && different_faces) { gs$matched[i1] <- TRUE; gs$matched[i2] <- TRUE }
        delay(900, {
          if (!gs$matched[i1]) gs$revealed[i1] <- FALSE
          if (!gs$matched[i2]) gs$revealed[i2] <- FALSE
          gs$first_pick <- NULL; gs$second_pick <- NULL
          gs$lock_board <- FALSE
          if (all(gs$matched)) gs$game_won <- TRUE
        })
      })
    }
  }, ignoreInit=TRUE)
  
  output$game_end_message <- renderText({
    if (gs$game_won) "¡Felicidades! Has completado el juego." else ""
  })
  
  observeEvent(input$reset_game, {
    new <- init_state()
    gs$board      <- new$board
    gs$revealed   <- new$revealed
    gs$matched    <- new$matched
    gs$first_pick <- new$first_pick
    gs$second_pick<- new$second_pick
    gs$lock_board <- new$lock_board
    gs$game_won   <- new$game_won
  })
}

# ---------------------------
# Lanzar app
# ---------------------------
shinyApp(ui=game_ui, server=game_server)


