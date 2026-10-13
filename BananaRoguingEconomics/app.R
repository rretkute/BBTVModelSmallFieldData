
library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(bslib)
library(scales)
library(data.table)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# ------------------------------------------------------------
# DATA
# ------------------------------------------------------------

surv_data  <- as.data.table(readRDS("BBTV_management_simulations.rds"))
surv_data1 <- as.data.table(readRDS("BBTV_management_simulations_annual.rds"))

NN <- 651
number_years <- 4


compute_net_benefit <- function(H, NN, W, P, N_inspect, T_s, T_r, L, R) {
  revenue <- H * W * P
  cost_surveillance <- N_inspect * NN * T_s * L
  cost_roguing <- R * T_r * L
  revenue - (cost_surveillance + cost_roguing)
}

freq_table <- data.table(
  freq_name = c("Annual", "Biannual", "Quarterly", "Monthly"),
  insp_freq = c(365, 180, 90, 30)
)

freq_lookup <- c(
  Annual = 365,
  Biannual = 180,
  Quarterly = 90,
  Monthly = 30
)

freq_to_ninspect <- function(f) {
  if (f == 365) return(1 * number_years)
  if (f == 180) return(2 * number_years - 1)
  if (f == 90)  return(4 * number_years - 1)
  if (f == 30)  return(12 * number_years - 1)
}


size_values   <- c(small = 5,    medium = 10,  large = 18)
price_values  <- c(low   = 0.15, average = 0.35, high = 0.6)
labour_values <- c(free  = 0,    some = 0.5,   hired = 2)
detect_values <- c(low   = 0.2,  mid = 0.5,    high = 0.9)

nearest_value <- function(x, choices) choices[which.min(abs(choices - x))]


safe_plot <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      ggplot() +
        theme_void() +
        annotate(
          "text", x = 0.5, y = 0.5,
          label = "Chart will appear once the window is a little larger.",
          size = 5, colour = "#6b7280"
        ) +
        xlim(0, 1) + ylim(0, 1)
    }
  )
}

freq_plain_key <- c(
  Annual    = "freq_annual",
  Biannual  = "freq_biannual",
  Quarterly = "freq_quarterly",
  Monthly   = "freq_monthly"
)


translations <- list(
  en = list(
    lang_button = "Fran\u00e7ais",
    title = "Banana Disease Helper",
    tab_farmer = "For Farmers",
    tab1 = "Advanced: Outcomes Explorer",
    tab2 = "Advanced: Annual Breakdown",

    # --- Farmer tab -------------------------------------------------
    farmer_card_header = "About your farm",
    farmer_intro = "Answer four quick questions about your farm. We will suggest how often to check your plants for Banana Bunchy Top Disease (BBTD).",
    size_label = "How big are your banana bunches?",
    size_small_l = "Small bunch", size_small_d = "about 5 kg",
    size_medium_l = "Medium bunch", size_medium_d = "about 10 kg",
    size_large_l = "Large bunch", size_large_d = "about 18 kg",
    price_label = "What is the banana price like right now?",
    price_low_l = "Low price", price_low_d = "about $0.15/kg",
    price_average_l = "Average price", price_average_d = "about $0.35/kg",
    price_high_l = "High price", price_high_d = "about $0.60/kg",
    labour_label = "Who checks and removes sick plants?",
    labour_free_l = "I do it myself", labour_free_d = "no cash cost",
    labour_some_l = "I sometimes pay someone", labour_some_d = "occasional hired help",
    labour_hired_l = "I regularly hire help", labour_hired_d = "paid labour every time",
    detect_label = "How confident are you at spotting sick plants?",
    detect_low_l = "Not very confident", detect_low_d = "I often miss early signs",
    detect_mid_l = "Somewhat confident", detect_mid_d = "I usually notice",
    detect_high_l = "Very confident", detect_high_d = "trained / expert-level",
    farmer_result_header = "Our suggestion for you",
    farmer_chart_title = "Money kept after 4 years, by checking schedule",
    farmer_chart_x = "Estimated net revenue (US$, over 4 years)",
    farmer_recommend_prefix = "Recommended: check your plants",
    farmer_recommend_gain = "Over 4 years, this could leave you with about %s more than the least effective schedule shown here.",
    farmer_value_line = "Estimated result: about %s over 4 years, per plant plot of this size.",
    show_advanced = "Show exact numbers (for extension staff)",
    freq_annual = "once a year",
    freq_biannual = "every 6 months",
    freq_quarterly = "every 3 months",
    freq_monthly = "every month",
    freq_annual_short = "Once a year",
    freq_biannual_short = "Every 6 months",
    freq_quarterly_short = "Every 3 months",
    freq_monthly_short = "Every month",

    econ_params = "Economic Parameters",
    bunch_weight = "Bunch weight (kg):",
    price = "Banana price (US$/kg):",
    labour = "Labour cost (US$ per hour):",
    inspect_time = "Time to inspect a plant (minutes):",
    remove_time = "Time to remove a plant (minutes):",
    detection_prob = "Detection probability:",
    download = "Download Results",
    plot1_title = "Net Revenue vs Detection Probability",
    plot2_title = "Yearly economic outcomes across inspection strategies",
    x_det = "Detection Probability",
    y_rev = "Farmer Net Revenue (US$)",
    freq = "Inspection Frequency",
    year = "Year",
    econ_value = "Economic value (US$)",
    metric = "Metric",
    helptext1 = "This panel explores the profitability of BBTD roguing strategies. Results show expected net revenue across detection probabilities for different inspection frequencies over a four-year horizon.",
    econ_indicators = "Economic indicators",
    healthy_profit_t = "Healthy Profit",
    healthy_profit_d = "Revenue generated from healthy banana plants: Healthy plants \u00d7 bunch weight \u00d7 banana price.",
    control_cost_t = "Control Cost",
    control_cost_d = "Labour costs associated with disease management, including plantation inspection and roguing of infected plants.",
    disease_loss_t = "Disease Loss",
    disease_loss_d = "Revenue that would have been obtained if all plants were healthy but is lost because of BBTD infection.",
    net_benefit_t = "Net Benefit",
    net_benefit_d = "Overall economic outcome for farmers: Healthy Profit minus Control Cost.",
    ds_plot_title = "Annual economic performance under fixed detection probability"
  ),

  fr = list(
    lang_button = "English",
    title = "Aide contre la maladie de la banane",
    tab_farmer = "Pour les agriculteurs",
    tab1 = "Avanc\u00e9 : Exploration des r\u00e9sultats \u00e9conomiques",
    tab2 = "Avanc\u00e9 : Analyse \u00e9conomique annuelle",

    farmer_card_header = "\u00c0 propos de votre exploitation",
    farmer_intro = "R\u00e9pondez \u00e0 quatre questions simples sur votre exploitation. Nous vous sugg\u00e9rerons \u00e0 quelle fr\u00e9quence surveiller vos plants contre la maladie des\u00a0bunchy\u00a0top\u00a0(BBTD).",
    size_label = "Quelle est la taille de vos r\u00e9gimes de bananes ?",
    size_small_l = "Petit r\u00e9gime", size_small_d = "environ 5 kg",
    size_medium_l = "R\u00e9gime moyen", size_medium_d = "environ 10 kg",
    size_large_l = "Grand r\u00e9gime", size_large_d = "environ 18 kg",
    price_label = "Quel est le prix actuel de la banane ?",
    price_low_l = "Prix bas", price_low_d = "environ 0,15 $/kg",
    price_average_l = "Prix moyen", price_average_d = "environ 0,35 $/kg",
    price_high_l = "Prix \u00e9lev\u00e9", price_high_d = "environ 0,60 $/kg",
    labour_label = "Qui inspecte et arrache les plants malades ?",
    labour_free_l = "Je le fais moi-m\u00eame", labour_free_d = "aucun co\u00fbt en argent",
    labour_some_l = "Je paie parfois quelqu'un", labour_some_d = "aide occasionnelle",
    labour_hired_l = "J'embauche r\u00e9guli\u00e8rement", labour_hired_d = "main-d'\u0153uvre pay\u00e9e \u00e0 chaque fois",
    detect_label = "Quelle est votre confiance pour reconna\u00eetre les plants malades ?",
    detect_low_l = "Peu confiant", detect_low_d = "je manque souvent les premiers signes",
    detect_mid_l = "Assez confiant", detect_mid_d = "je remarque g\u00e9n\u00e9ralement",
    detect_high_l = "Tr\u00e8s confiant", detect_high_d = "form\u00e9 / niveau expert",
    farmer_result_header = "Notre suggestion pour vous",
    farmer_chart_title = "Argent conserv\u00e9 apr\u00e8s 4 ans, selon le calendrier de contr\u00f4le",
    farmer_chart_x = "Revenu net estim\u00e9 (US$, sur 4 ans)",
    farmer_recommend_prefix = "Recommandation\u00a0: v\u00e9rifiez vos plants",
    farmer_recommend_gain = "Sur 4 ans, cela pourrait vous laisser environ %s de plus que le calendrier le moins efficace pr\u00e9sent\u00e9 ici.",
    farmer_value_line = "R\u00e9sultat estim\u00e9\u00a0: environ %s sur 4 ans, pour une parcelle de cette taille.",
    show_advanced = "Afficher les chiffres exacts (pour les agents de vulgarisation)",
    freq_annual = "une fois par an",
    freq_biannual = "tous les 6 mois",
    freq_quarterly = "tous les 3 mois",
    freq_monthly = "tous les mois",
    freq_annual_short = "Une fois par an",
    freq_biannual_short = "Tous les 6 mois",
    freq_quarterly_short = "Tous les 3 mois",
    freq_monthly_short = "Tous les mois",

    econ_params = "Param\u00e8tres \u00e9conomiques",
    bunch_weight = "Poids du r\u00e9gime (kg) :",
    price = "Prix de la banane (US$/kg) :",
    labour = "Co\u00fbt de la main-d'\u0153uvre (US$ par heure) :",
    inspect_time = "Temps d'inspection d'un plant (minutes) :",
    remove_time = "Temps d'arrachage d'un plant (minutes) :",
    detection_prob = "Probabilit\u00e9 de d\u00e9tection :",
    download = "T\u00e9l\u00e9charger les r\u00e9sultats",
    plot1_title = "Revenu net en fonction de la probabilit\u00e9 de d\u00e9tection",
    plot2_title = "R\u00e9sultats \u00e9conomiques annuels selon les strat\u00e9gies d'inspection",
    x_det = "Probabilit\u00e9 de d\u00e9tection",
    y_rev = "Revenu net de l'exploitation (US$)",
    freq = "Fr\u00e9quence d'inspection",
    year = "Ann\u00e9e",
    econ_value = "Valeur \u00e9conomique (US$)",
    metric = "Indicateur",
    helptext1 = "Ce panneau explore la rentabilit\u00e9 des strat\u00e9gies d'arrachage sanitaire du BBTD. Les r\u00e9sultats montrent le revenu net attendu selon diff\u00e9rentes probabilit\u00e9s de d\u00e9tection et fr\u00e9quences d'inspection sur un horizon de quatre ans.",
    econ_indicators = "Indicateurs \u00e9conomiques",
    healthy_profit_t = "Profit sain",
    healthy_profit_d = "Revenu g\u00e9n\u00e9r\u00e9 par les plants de bananiers sains : plants sains \u00d7 poids du r\u00e9gime \u00d7 prix de la banane.",
    control_cost_t = "Co\u00fbt de contr\u00f4le",
    control_cost_d = "Co\u00fbts de main-d'\u0153uvre li\u00e9s \u00e0 la gestion de la maladie, incluant l'inspection des plantations et l'arrachage des plants infect\u00e9s.",
    disease_loss_t = "Perte due \u00e0 la maladie",
    disease_loss_d = "Revenu potentiel si tous les plants \u00e9taient sains, mais perdu en raison de l'infection par le BBTD.",
    net_benefit_t = "B\u00e9n\u00e9fice net",
    net_benefit_d = "R\u00e9sultat \u00e9conomique global pour les producteurs : profit sain moins co\u00fbt de contr\u00f4le.",
    ds_plot_title = "Performance \u00e9conomique annuelle \u00e0 probabilit\u00e9 de d\u00e9tection fixe"
  )
)

tr <- function(key, lang) translations[[lang]][[key]]

ui <- page_navbar(
  id = "main_nav",
  title = uiOutput("app_title", inline = TRUE),
  window_title = "Banana Disease Helper / Aide banane",
  selected = "farmer",

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1f3c88",
     base_font = font_collection(
      "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"
    )
  ) %>%
    bs_add_rules("
      .farmer-choice .btn { font-size: 1.05rem; padding: 0.65rem 0.9rem; }
      .farmer-choice label { width: 100%; }
      .farmer-choice .radio-inline, .farmer-choice .checkbox-inline { margin-right: 0; }
      .form-group.shiny-input-radiogroup label.radio { display: block; margin-bottom: .5rem; padding: .5rem .75rem; border: 1px solid #dbe1ea; border-radius: 8px; }
      .form-group.shiny-input-radiogroup label.radio:hover { background-color: #f4f7fb; }
      .farmer-recommend { font-size: 1.15rem; padding: 14px 18px; border-radius: 10px; background-color: #eaf6ec; border: 1px solid #bfe3c6; margin-top: 10px; }
      @media (max-width: 575px) {
        .lang-logo-bar { position: static !important; margin: 8px 12px 0 auto !important; justify-content: flex-end !important; }
      }
      #farmer_plot, #fnr_plot, #ds_compare_plot {
        min-width: 260px;
        min-height: 260px;
      }
      #lang_toggle, #lang_toggle:focus {
        background-color: #ffffff !important;
        color: #1f3c88 !important;
        border: 1.5px solid #1f3c88 !important;
        font-weight: 600;
        box-shadow: 0 1px 3px rgba(0,0,0,0.15);
      }
      #lang_toggle:hover {
        background-color: #1f3c88 !important;
        color: #ffffff !important;
      }
    "),

  header = tagList(
    tags$div(
      class = "lang-logo-bar",
      style = "
        position: fixed;
        top: 10px;
        right: 15px;
        z-index: 9999;
        display: flex;
        align-items: center;
        gap: 10px;
      ",
      actionButton("lang_toggle", label = uiOutput("lang_button_label", inline = TRUE),
                   class = "btn btn-sm"),
      tags$img(src = "CU_logo.jpg", height = "40px")
    )
  ),

  nav_panel(
    title = uiOutput("tab_farmer_label", inline = TRUE),
    value = "farmer",
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        class = "farmer-choice",
        uiOutput("farmer_card_header_ui"),
        uiOutput("farmer_intro_ui"),
        hr(),
        uiOutput("size_ui"),
        uiOutput("price_ui"),
        uiOutput("labour_ui"),
        uiOutput("detect_ui")
      ),
      card(
        full_screen = TRUE,
        card_header(uiOutput("farmer_result_header_ui")),
        plotOutput("farmer_plot", height = 380),
        uiOutput("farmer_recommendation"),
        hr(),
        checkboxInput("show_advanced_numbers", label = uiOutput("show_advanced_ui", inline = TRUE), value = FALSE),
        conditionalPanel(
          "input.show_advanced_numbers == true",
          tableOutput("farmer_table")
        )
      )
    )
  ),

  nav_panel(
    title = uiOutput("tab1_label", inline = TRUE),
    value = "advanced1",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        uiOutput("econ_params_header"),
        sliderInput("W", NULL, min = 1, max = 120, value = 10, step = 1),
        sliderInput("P", NULL, min = 0.1, max = 1, value = 0.2, step = 0.01),
        sliderInput("L", NULL, min = 0, max = 3, value = 0.2, step = 0.05),
        sliderInput("Ts", NULL, min = 1, max = 60, value = 1, step = 1),
        sliderInput("Tr", NULL, min = 1, max = 60, value = 15, step = 1),
        hr(),
        uiOutput("helptext1_ui")
      ),
      card(
        full_screen = TRUE,
        card_header(uiOutput("plot1_title_ui")),
        plotOutput("fnr_plot", height = 600),
        br(),
        downloadButton("download_data", label = NULL)
      )
    )
  ),

  nav_panel(
    title = uiOutput("tab2_label", inline = TRUE),
    value = "advanced2",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        sliderInput("ds_det", NULL, min = 0.1, max = 1, value = 0.9, step = 0.05),
        hr(),
        uiOutput("econ_indicators_header"),
        uiOutput("econ_indicators_text")
      ),
      card(
        full_screen = TRUE,
        card_header(uiOutput("plot2_title_ui")),
        plotOutput("ds_compare_plot", height = 550)
      )
    )
  )
)


server <- function(input, output, session) {

    lang_rv <- reactiveVal("en")

  observeEvent(input$lang_toggle, {
    lang_rv(if (lang_rv() == "en") "fr" else "en")
  })

  output$app_title       <- renderUI(tr("title", lang_rv()))
  output$lang_button_label <- renderUI(tr("lang_button", lang_rv()))
  output$tab_farmer_label  <- renderUI(tr("tab_farmer", lang_rv()))
  output$tab1_label        <- renderUI(tr("tab1", lang_rv()))
  output$tab2_label        <- renderUI(tr("tab2", lang_rv()))

  observe({
    l <- lang_rv()
    updateSliderInput(session, "W",  label = tr("bunch_weight", l))
    updateSliderInput(session, "P",  label = tr("price", l))
    updateSliderInput(session, "L",  label = tr("labour", l))
    updateSliderInput(session, "Ts", label = tr("inspect_time", l))
    updateSliderInput(session, "Tr", label = tr("remove_time", l))
    updateSliderInput(session, "ds_det", label = tr("detection_prob", l))
  })

  output$econ_params_header    <- renderUI(h5(tr("econ_params", lang_rv())))
  output$helptext1_ui          <- renderUI(helpText(tr("helptext1", lang_rv())))
  output$plot1_title_ui         <- renderUI(tr("plot1_title", lang_rv()))
  output$plot2_title_ui         <- renderUI(tr("plot2_title", lang_rv()))
  output$econ_indicators_header <- renderUI(h5(tr("econ_indicators", lang_rv())))
  output$econ_indicators_text   <- renderUI(tagList(
    p(strong(tr("healthy_profit_t", lang_rv())), tr("healthy_profit_d", lang_rv())),
    p(strong(tr("control_cost_t", lang_rv())), tr("control_cost_d", lang_rv())),
    p(strong(tr("disease_loss_t", lang_rv())), tr("disease_loss_d", lang_rv())),
    p(strong(tr("net_benefit_t", lang_rv())), tr("net_benefit_d", lang_rv()))
  ))

  output$farmer_card_header_ui <- renderUI(h5(tr("farmer_card_header", lang_rv())))
  output$farmer_intro_ui       <- renderUI(p(tr("farmer_intro", lang_rv())))
  output$farmer_result_header_ui <- renderUI(tr("farmer_result_header", lang_rv()))
  output$show_advanced_ui      <- renderUI(tr("show_advanced", lang_rv()))

  output$size_ui <- renderUI({
    l <- lang_rv()
    radioButtons(
      "farmer_size", tr("size_label", l),
      choiceNames = list(
        tagList(strong(tr("size_small_l", l)),  " \u2014 ", tags$small(tr("size_small_d", l))),
        tagList(strong(tr("size_medium_l", l)), " \u2014 ", tags$small(tr("size_medium_d", l))),
        tagList(strong(tr("size_large_l", l)),  " \u2014 ", tags$small(tr("size_large_d", l)))
      ),
      choiceValues = list("small", "medium", "large"),
      selected = isolate(input$farmer_size) %||% "medium"
    )
  })

  output$price_ui <- renderUI({
    l <- lang_rv()
    radioButtons(
      "farmer_price", tr("price_label", l),
      choiceNames = list(
        tagList(strong(tr("price_low_l", l)),     " \u2014 ", tags$small(tr("price_low_d", l))),
        tagList(strong(tr("price_average_l", l)), " \u2014 ", tags$small(tr("price_average_d", l))),
        tagList(strong(tr("price_high_l", l)),    " \u2014 ", tags$small(tr("price_high_d", l)))
      ),
      choiceValues = list("low", "average", "high"),
      selected = isolate(input$farmer_price) %||% "average"
    )
  })

  output$labour_ui <- renderUI({
    l <- lang_rv()
    radioButtons(
      "farmer_labour", tr("labour_label", l),
      choiceNames = list(
        tagList(strong(tr("labour_free_l", l)),  " \u2014 ", tags$small(tr("labour_free_d", l))),
        tagList(strong(tr("labour_some_l", l)),  " \u2014 ", tags$small(tr("labour_some_d", l))),
        tagList(strong(tr("labour_hired_l", l)), " \u2014 ", tags$small(tr("labour_hired_d", l)))
      ),
      choiceValues = list("free", "some", "hired"),
      selected = isolate(input$farmer_labour) %||% "some"
    )
  })

  output$detect_ui <- renderUI({
    l <- lang_rv()
    radioButtons(
      "farmer_detect", tr("detect_label", l),
      choiceNames = list(
        tagList(strong(tr("detect_low_l", l)),  " \u2014 ", tags$small(tr("detect_low_d", l))),
        tagList(strong(tr("detect_mid_l", l)),  " \u2014 ", tags$small(tr("detect_mid_d", l))),
        tagList(strong(tr("detect_high_l", l)), " \u2014 ", tags$small(tr("detect_high_d", l)))
      ),
      choiceValues = list("low", "mid", "high"),
      selected = isolate(input$farmer_detect) %||% "mid"
    )
  })

  farmer_inputs <- reactive({
    list(
      W   = size_values[[input$farmer_size %||% "medium"]],
      P   = price_values[[input$farmer_price %||% "average"]],
      L   = labour_values[[input$farmer_labour %||% "some"]],
      det = detect_values[[input$farmer_detect %||% "mid"]]
    )
  })

  farmer_results <- reactive({
    fi <- farmer_inputs()
    det_choices <- sort(unique(surv_data$det.prob))
    det_use <- nearest_value(fi$det, det_choices)
    Ts_hr <- 1 / 60
    Tr_hr <- 15 / 60

    out <- rbindlist(lapply(seq_len(nrow(freq_table)), function(i) {
      freq_name <- freq_table$freq_name[i]
      insp_freq <- freq_table$insp_freq[i]
      N_inspect <- freq_to_ninspect(insp_freq)
      dt <- surv_data[insp.freq == insp_freq & abs(det.prob - det_use) < 1e-9]
      net <- compute_net_benefit(dt$n.H, NN, fi$W, fi$P, N_inspect, Ts_hr, Tr_hr, fi$L, dt$n.R)
      data.table(freq_name = freq_name, insp_freq = insp_freq, mean_net = mean(net, na.rm = TRUE))
    }))
    out[, is_best := mean_net == max(mean_net)]
    out
  })

  output$farmer_plot <- renderPlot({
    safe_plot({
      l <- lang_rv()
      df <- farmer_results()
      df[, freq_label := sapply(freq_name, function(f) tr(paste0("freq_", tolower(f), "_short"), l))]
      df$freq_label <- factor(df$freq_label, levels = df$freq_label[order(-df$insp_freq)])

      ggplot(df, aes(x = freq_label, y = mean_net, fill = is_best)) +
        geom_col(width = 0.65) +
        geom_text(aes(label = dollar(round(mean_net / 10) * 10)), hjust = -0.1, size = 5.5) +
        coord_flip(clip = "off") +
        scale_fill_manual(values = c(`TRUE` = "#2e7d46", `FALSE` = "#9aa7bd"), guide = "none") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.28)), labels = dollar_format()) +
        labs(title = tr("farmer_chart_title", l), x = NULL, y = tr("farmer_chart_x", l)) +
        theme_minimal(base_size = 17) +
        theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
    })
  })

  output$farmer_recommendation <- renderUI({
    l <- lang_rv()
    df <- farmer_results()
    best <- df[is_best == TRUE][1]
    worst <- df[which.min(mean_net)]
    gain <- best$mean_net - worst$mean_net
    freq_word <- tr(paste0("freq_", tolower(best$freq_name)), l)

    div(
      class = "farmer-recommend",
      p(strong(paste0("\u2705 ", tr("farmer_recommend_prefix", l), " ", freq_word, "."))),
      p(sprintf(tr("farmer_recommend_gain", l), dollar(round(gain / 10) * 10))),
      p(sprintf(tr("farmer_value_line", l), dollar(round(best$mean_net / 10) * 10)))
    )
  })

  output$farmer_table <- renderTable({
    l <- lang_rv()
    df <- farmer_results()
    df[, Schedule := sapply(freq_name, function(f) tr(paste0("freq_", tolower(f), "_short"), l))]
    df[, .(Schedule, `Net revenue (US$, 4 yr)` = round(mean_net))]
  })

   results_data <- reactive({
    Ts_hr <- input$Ts / 60
    Tr_hr <- input$Tr / 60
    rbindlist(lapply(1:nrow(freq_table), function(i) {
      freq_name <- freq_table$freq_name[i]
      insp_freq <- freq_table$insp_freq[i]
      N_inspect <- freq_to_ninspect(insp_freq)
      dt <- surv_data[insp.freq == insp_freq]
      dt[, net := compute_net_benefit(
        n.H, NN,
        input$W, input$P,
        N_inspect,
        Ts_hr, Tr_hr,
        input$L,
        n.R
      )]
      dt[, .(
        mean = mean(net, na.rm = TRUE),
        lower = quantile(net, 0.025, na.rm = TRUE),
        upper = quantile(net, 0.975, na.rm = TRUE)
      ), by = det.prob][, freq := freq_name]
    }))
  })

  output$fnr_plot <- renderPlot({
    safe_plot({
      l <- lang_rv()
      df <- results_data()
      df$freq <- factor(df$freq, levels = c("Annual", "Biannual", "Quarterly", "Monthly"))
      ggplot(df, aes(det.prob, mean, colour = freq, fill = freq)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
        geom_line(linewidth = 1.3) +
        scale_colour_viridis(discrete = TRUE) +
        scale_fill_viridis(discrete = TRUE) +
        theme_minimal(base_size = 18) +
        xlim(0.1, 1) +
        scale_y_continuous(labels = dollar_format()) +
        labs(
          x = tr("x_det", l),
          y = tr("y_rev", l),
          colour = tr("freq", l),
          fill = tr("freq", l)
        )
    })
  })

  ds_data <- reactive({
    Ts_hr <- input$Ts / 60
    Tr_hr <- input$Tr / 60
    dt <- surv_data1[abs(Det.Prob - input$ds_det) < 1e-6]
    dt[, freq := factor(
      Insp.Freq,
      levels = c(365, 180, 90, 30),
      labels = c("Annual", "Biannual", "Quarterly", "Monthly")
    )]
    dt[, N_inspect := sapply(Insp.Freq, freq_to_ninspect)]
    dt[, N_inspect_year := N_inspect / number_years]
    dt[, revenue := n.H * input$W * input$P]
    dt[, cost_surveillance := N_inspect_year * NN * Ts_hr * input$L]
    dt[, cost_roguing := n.R * Tr_hr * input$L]
    dt[, control_cost := cost_surveillance + cost_roguing]
    dt[, disease_loss := (NN * input$W * input$P) - revenue]
    dt[, net := revenue - control_cost]
    dt
  })

  output$ds_compare_plot <- renderPlot({
    safe_plot({
      l <- lang_rv()
      dt <- ds_data()
      summary_dt <- dt[, .(
        Healthy_Profit = mean(revenue),
        Control_Cost = mean(control_cost),
        Disease_Loss = mean(disease_loss),
        Net_Benefit = mean(net)
      ), by = .(year, freq)]
      plot_dt <- melt(summary_dt, id.vars = c("year", "freq"), variable.name = "metric")
      ggplot(plot_dt, aes(year, value, colour = metric, group = metric)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2.5) +
        facet_wrap(~freq, nrow = 1) +
        scale_colour_viridis(discrete = TRUE) +
        theme_minimal(base_size = 17) +
        scale_y_continuous(labels = dollar_format()) +
        scale_x_continuous(breaks = 1:4) +
        labs(
          x = tr("year", l),
          y = tr("econ_value", l),
          colour = tr("metric", l),
          title = tr("ds_plot_title", l)
        )
    })
  })

  output$download_data <- downloadHandler(
    filename = function() "BBTV_net_revenue_results.csv",
    content = function(file) write.csv(results_data(), file, row.names = FALSE)
  )

}

shinyApp(ui, server)
