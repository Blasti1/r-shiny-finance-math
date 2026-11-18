# ATLAS R LAB — учебный практикум по финансам, производным и статистике.
# Запуск: install.packages("shiny") (один раз), затем shiny::runApp()
# Необходим только внешний пакет shiny; расчёты и графики выполнены средствами Base R.
library(shiny)

# ============================= Вспомогательные функции =============================
fmt_num <- function(x, digits = 2) {
  if (length(x) != 1 || is.na(x) || !is.finite(x)) return("—")
  formatC(x, format = "f", digits = digits, big.mark = " ", decimal.mark = ",")
}
fmt_rub <- function(x) paste0(fmt_num(x), " ₽")

kpi <- function(label, value, note = "") {
  tags$div(class = "kpi",
    tags$div(class = "kpi-label", label),
    tags$div(class = "kpi-value", value),
    tags$div(class = "kpi-note", note)
  )
}

section_title <- function(num, kicker, title, description) {
  tags$div(class = "section-title",
    tags$div(class = "section-overline", paste(num, "/", kicker)),
    tags$h2(title),
    tags$p(description)
  )
}

parse_series <- function(value) {
  chunks <- unlist(strsplit(value, "[,;[:space:]]+"))
  chunks <- chunks[nzchar(chunks)]
  if (length(chunks) == 0) return(numeric(0))
  suppressWarnings(as.numeric(chunks))
}

# Безопасный разбор математического выражения без выполнения произвольного R-кода.
safe_math_expr <- function(text) {
  if (nchar(text) > 140) stop("Слишком длинное выражение (максимум 140 символов).")
  parsed <- parse(text = text, keep.source = FALSE)
  if (length(parsed) != 1) stop("Введите одну функцию от переменной x.")
  allowed_ops <- c("+", "-", "*", "/", "^", "sin", "cos", "tan", "exp", "log", "sqrt")
  valid_tree <- function(node) {
    if (is.numeric(node)) return(length(node) == 1 && is.finite(node))
    if (is.name(node)) return(as.character(node) %in% c("x", "pi"))
    if (!is.call(node)) return(FALSE)
    fn <- node[[1]]
    if (!is.name(fn)) return(FALSE)
    opname <- as.character(fn)
    if (!(opname %in% allowed_ops)) return(FALSE)
    args <- as.list(node)[-1]
    arity <- length(args)
    if (opname %in% c("+", "-") && !(arity %in% c(1, 2))) return(FALSE)
    if (opname %in% c("*", "/", "^") && arity != 2) return(FALSE)
    if (opname %in% c("sin", "cos", "tan", "exp", "log", "sqrt") && arity != 1) return(FALSE)
    all(vapply(args, valid_tree, logical(1)))
  }
  if (!valid_tree(parsed[[1]])) {
    stop("Разрешены только x, числа, pi, +, -, *, /, ^, sin, cos, tan, exp, log и sqrt.")
  }
  parsed[[1]]
}

calc_expression <- function(expr, x) {
  scope <- list2env(list(x = x, pi = base::pi), parent = baseenv())
  y <- suppressWarnings(eval(expr, envir = scope))
  if (!is.numeric(y)) stop("Результат функции должен быть числом.")
  if (length(y) == 1) return(rep(as.numeric(y), length(x)))
  if (length(y) != length(x)) stop("Не удалось вычислить значения функции.")
  as.numeric(y)
}

# ============================= Интерфейс =============================
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("ATLAS — аналитический практикум на R Shiny"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tags$div(class = "page",
    tags$header(class = "topbar",
      tags$div(class = "brand",
        tags$div(class = "brand-mark", "A"),
        tags$div(tags$strong("ATLAS"), tags$small("R / LAB"))
      ),
      tags$div(class = "topbar-right",
        tags$span(class = "online-indicator"),
        tags$span("Учебная лаборатория"),
        tags$span(class = "topbar-index", "2026 · R SHINY")
      )
    ),
    tags$section(class = "masthead",
      tags$div(class = "masthead-copy",
        tags$div(class = "eyebrow", "ИНТЕРАКТИВНЫЙ ПРАКТИКУМ  /  03 ДИСЦИПЛИНЫ"),
        tags$h1("Числа становятся", tags$br(), tags$em("понятнее.")),
        tags$p("Финансовые модели, производные и статистика — в одном пространстве. Меняйте данные и исследуйте расчёты на графиках."),
        tags$div(class = "masthead-pills",
          tags$span("01 Финансы"), tags$span("02 Производные"), tags$span("03 Статистика")
        )
      ),
      tags$div(class = "hero-visual", `aria-hidden` = "true",
        tags$div(class = "hero-grid"),
        tags$div(class = "hero-line line-one"),
        tags$div(class = "hero-line line-two"),
        tags$div(class = "hero-disc", "f(x)"),
        tags$div(class = "hero-small", "DATA / THINK / SOLVE")
      )
    ),
    tags$div(class = "content-layout",
      tags$aside(class = "sidebar",
        tags$div(class = "sidebar-caption", "НАВИГАЦИЯ"),
        tags$div(class = "sidebar-highlight", "3", tags$small("учебных модуля")),
        tags$div(class = "sidebar-rule"),
        tags$div(class = "sidebar-note",
          tags$strong("Как работать"),
          tags$p("1. Выберите раздел."),
          tags$p("2. Введите параметры задачи."),
          tags$p("3. Изучите результат и визуализацию.")
        ),
        tags$div(class = "sidebar-bottom", "Работает локально в RStudio", tags$span("●"))
      ),
      tags$main(class = "main-content",
        tabsetPanel(id = "section", type = "pills", selected = "fin",
          tabPanel(title = "01  Финансовые задачи", value = "fin",
            section_title("01", "ФИНАНСОВЫЕ МОДЕЛИ", "Финансы без лишних формул",
                          "Вклад с ежемесячным пополнением, кредит или инвестиционный проект — выберите модель и получите расчёт."),
            tags$div(class = "section-grid",
              tags$div(class = "form-card",
                tags$div(class = "card-kicker", "ПАРАМЕТРЫ"),
                selectInput("fin_kind", "Тип финансовой задачи",
                  choices = c("Вклад и пополнение" = "deposit", "Аннуитетный кредит" = "credit", "NPV инвестиционного проекта" = "npv")),
                conditionalPanel("input.fin_kind == 'deposit'",
                  numericInput("dep_initial", "Начальная сумма, ₽", 100000, min = 0, max = 1e9, step = 1000),
                  numericInput("dep_rate", "Ставка в год, %", 12, min = 0, max = 100, step = .5),
                  numericInput("dep_years", "Срок, лет", 5, min = 1, max = 40, step = 1),
                  numericInput("dep_add", "Пополнение в конце месяца, ₽", 5000, min = 0, max = 1e8, step = 500)
                ),
                conditionalPanel("input.fin_kind == 'credit'",
                  numericInput("credit_sum", "Сумма кредита, ₽", 900000, min = 1, max = 1e9, step = 10000),
                  numericInput("credit_rate", "Годовая ставка, %", 15, min = 0, max = 100, step = .5),
                  numericInput("credit_months", "Срок, месяцев", 36, min = 1, max = 480, step = 1)
                ),
                conditionalPanel("input.fin_kind == 'npv'",
                  numericInput("npv_cost", "Первоначальные вложения, ₽", 160000, min = 0, max = 1e9, step = 10000),
                  numericInput("npv_rate", "Ставка дисконтирования, %", 11, min = 0, max = 100, step = .5),
                  textAreaInput("npv_flows", "Денежные потоки по годам, ₽", "50000, 60000, 70000, 80000", rows = 3),
                  tags$p(class = "hint", "Разделяйте значения запятой или точкой с запятой. Можно вводить отрицательные потоки.")
                ),
                tags$div(class = "live-note", tags$span(class="live-dot"), "Расчёт обновляется автоматически")
              ),
              tags$div(class = "results-panel",
                uiOutput("fin_metrics"),
                tags$div(class = "chart-card",
                  tags$div(class = "card-head", tags$strong("Динамика показателей"), tags$span("ГРАФИК")),
                  plotOutput("fin_plot", height = "285px")
                ),
                tags$div(class = "data-card",
                  tags$div(class = "card-head", tags$strong("Таблица расчёта"), tags$span("ДАННЫЕ")),
                  tableOutput("fin_table"),
                  downloadButton("fin_export", "Скачать таблицу CSV", class = "outline-button")
                )
              )
            )
          ),
          tabPanel(title = "02  Производные", value = "deriv",
            section_title("02", "МАТЕМАТИЧЕСКИЙ АНАЛИЗ", "Производная в деталях",
                          "Введите функцию от x, укажите точку — приложение найдёт производную и построит касательную."),
            tags$div(class = "section-grid",
              tags$div(class = "form-card",
                tags$div(class = "card-kicker", "ЗАДАЙТЕ ФУНКЦИЮ"),
                textInput("f_expr", "Функция f(x)", "x^3 - 3*x^2 + 2"),
                numericInput("f_point", "Точка x₀", 1, min = -100, max = 100, step = 0.5),
                fluidRow(
                  column(6, numericInput("f_min", "График: от x", -2, step = 1)),
                  column(6, numericInput("f_max", "До x", 4, step = 1))
                ),
                tags$div(class = "examples",
                  tags$strong("Примеры записи"),
                  tags$code("x^2 + 2*x"),
                  tags$code("sin(x) + x"),
                  tags$code("exp(x) / 2")
                ),
                tags$p(class = "hint", "Поддерживаются арифметические операции, sin, cos, tan, exp, log, sqrt и число pi. Возведение в степень — через ^.")
              ),
              tags$div(class = "results-panel",
                uiOutput("deriv_metrics"),
                tags$div(class = "formula-card", uiOutput("deriv_formula")),
                tags$div(class = "chart-card",
                  tags$div(class = "card-head", tags$strong("Функция и касательная"), tags$span("ГРАФИК")),
                  plotOutput("deriv_plot", height = "320px")
                )
              )
            )
          ),
          tabPanel(title = "03  Статистика", value = "stats",
            section_title("03", "СТАТИСТИЧЕСКИЙ АНАЛИЗ", "Из данных — в выводы",
                          "Введите числовую выборку. Получите описательные показатели, 95% доверительный интервал и визуализацию распределения."),
            tags$div(class = "section-grid",
              tags$div(class = "form-card",
                tags$div(class = "card-kicker", "НАБОР ДАННЫХ"),
                textAreaInput("stat_values", "Числа выборки", "12, 15, 18, 18, 21, 24, 26, 31, 35, 40, 42, 45", rows = 7),
                tags$p(class = "hint", "Разделяйте числа пробелами, запятыми, точкой с запятой или переносами строк. Для десятичных дробей используйте точку."),
                tags$div(class = "sidebar-rule"),
                tags$div(class = "mini-info", "Дисперсия вычисляется по выборочной формуле (деление на n − 1)."),
                downloadButton("stats_export", "Скачать показатели CSV", class = "outline-button")
              ),
              tags$div(class = "results-panel",
                uiOutput("stat_metrics"),
                tags$div(class = "data-card",
                  tags$div(class = "card-head", tags$strong("Описательная статистика"), tags$span("СВОДКА")),
                  tableOutput("stat_table")
                ),
                tags$div(class = "plot-pair",
                  tags$div(class = "chart-card",
                    tags$div(class = "card-head", tags$strong("Гистограмма")),
                    plotOutput("hist_plot", height = "230px")
                  ),
                  tags$div(class = "chart-card",
                    tags$div(class = "card-head", tags$strong("Диаграмма размаха")),
                    plotOutput("box_plot", height = "230px")
                  )
                )
              )
            )
          )
        ),
        tags$footer(class = "footer", "ATLAS R / LAB", tags$span("Учебный проект · R Shiny · расчёты локально"))
      )
    )
  )
)

# ============================= Серверная логика =============================
server <- function(input, output, session) {
  financial <- reactive({
    req(input$fin_kind)
    mode <- input$fin_kind
    if (mode == "deposit") {
      req(input$dep_initial, input$dep_rate, input$dep_years, input$dep_add)
      validate(need(input$dep_initial >= 0 && input$dep_add >= 0 &&
                    input$dep_rate >= 0 && input$dep_rate <= 100 &&
                    input$dep_years >= 1 && input$dep_years <= 40,
                    "Проверьте сумму вклада, ставку и срок."))
      n <- as.integer(round(input$dep_years * 12))
      r <- input$dep_rate / 1200
      balance <- numeric(n + 1)
      balance[1] <- input$dep_initial
      for (m in seq_len(n)) balance[m + 1] <- balance[m] * (1 + r) + input$dep_add
      invested <- input$dep_initial + n * input$dep_add
      contribution <- c(input$dep_initial, input$dep_initial + seq_len(n) * input$dep_add)
      raw <- data.frame(Месяц = 0:n, Баланс = round(balance, 2), Внесено = round(contribution, 2), check.names = FALSE)
      idx <- seq.int(1, n + 1, by = 12)
      if (tail(idx, 1) != n + 1) idx <- c(idx, n + 1)
      display <- data.frame(Месяц = raw[[1]][idx], `Баланс, ₽` = vapply(raw[[2]][idx], fmt_rub, character(1)),
                            `Внесено, ₽` = vapply(raw[[3]][idx], fmt_rub, character(1)), check.names = FALSE)
      list(mode = mode, primary = c("Итоговая сумма", fmt_rub(tail(balance, 1))),
           second = c("Внесено", fmt_rub(invested)),
           third = c("Начисленные проценты", fmt_rub(tail(balance, 1) - invested)),
           x = 0:n, y = balance, table = display, export = raw)
    } else if (mode == "credit") {
      req(input$credit_sum, input$credit_rate, input$credit_months)
      validate(need(input$credit_sum > 0 && input$credit_rate >= 0 &&
                    input$credit_rate <= 100 && input$credit_months >= 1 &&
                    input$credit_months <= 480, "Проверьте параметры кредита."))
      n <- as.integer(round(input$credit_months))
      r <- input$credit_rate / 1200
      payment <- if (r == 0) input$credit_sum / n else input$credit_sum * r / (1 - (1 + r)^(-n))
      balance <- numeric(n + 1)
      interest <- numeric(n)
      principal <- numeric(n)
      payments <- numeric(n)
      balance[1] <- input$credit_sum
      for (m in seq_len(n)) {
        interest[m] <- balance[m] * r
        principal[m] <- min(balance[m], max(0, payment - interest[m]))
        payments[m] <- principal[m] + interest[m]
        balance[m + 1] <- max(0, balance[m] - principal[m])
      }
      raw <- data.frame(Месяц = seq_len(n), Платёж = round(payments, 2),
                        Проценты = round(interest, 2), Остаток = round(balance[-1], 2), check.names = FALSE)
      display <- data.frame(Месяц = head(raw[[1]], 12),
        `Платёж, ₽` = vapply(head(raw[[2]], 12), fmt_rub, character(1)),
        `Проценты, ₽` = vapply(head(raw[[3]], 12), fmt_rub, character(1)),
        `Остаток, ₽` = vapply(head(raw[[4]], 12), fmt_rub, character(1)), check.names = FALSE)
      list(mode = mode, primary = c("Платёж в месяц", fmt_rub(payment)),
           second = c("Всего выплат", fmt_rub(sum(payments))),
           third = c("Переплата", fmt_rub(sum(interest))),
           x = 0:n, y = balance, table = display, export = raw)
    } else {
      req(input$npv_cost, input$npv_rate, input$npv_flows)
      flows <- parse_series(input$npv_flows)
      validate(need(input$npv_cost >= 0 && input$npv_rate >= 0 &&
                    input$npv_rate <= 100, "Проверьте сумму вложений и ставку."),
               need(length(flows) >= 1 && length(flows) <= 30 &&
                    all(is.finite(flows)), "Введите от 1 до 30 числовых денежных потоков через запятую."))
      t <- seq_along(flows)
      pv <- flows / (1 + input$npv_rate / 100)^t
      cumulative <- c(-input$npv_cost, -input$npv_cost + cumsum(pv))
      raw <- data.frame(Год = t, Поток = round(flows, 2),
        `Приведённая стоимость` = round(pv, 2),
        `Накопленный NPV` = round(cumulative[-1], 2), check.names = FALSE)
      display <- data.frame(Год = raw[[1]],
        `Поток, ₽` = vapply(raw[[2]], fmt_rub, character(1)),
        `PV, ₽` = vapply(raw[[3]], fmt_rub, character(1)),
        `Накопленный NPV, ₽` = vapply(raw[[4]], fmt_rub, character(1)), check.names = FALSE)
      total <- tail(cumulative, 1)
      list(mode = mode, primary = c("NPV проекта", fmt_rub(total)),
           second = c("Приведённые потоки", fmt_rub(sum(pv))),
           third = c("Первоначальные вложения", fmt_rub(input$npv_cost)),
           x = 0:length(flows), y = cumulative, table = display, export = raw)
    }
  })

  output$fin_metrics <- renderUI({
    z <- financial()
    tags$div(class = "metrics",
      kpi(z$primary[1], z$primary[2], "Основной результат"),
      kpi(z$second[1], z$second[2], "Расчёт по исходным данным"),
      kpi(z$third[1], z$third[2], "Дополнительный показатель")
    )
  })

  output$fin_plot <- renderPlot({
    z <- financial()
    par(mar = c(4.2, 4.2, 0.8, 1.2), bg = "white", fg = "#233446",
        col.axis = "#677480", col.lab = "#233446", family = "sans")
    xl <- if (z$mode == "npv") "Год" else "Месяц"
    yl <- if (z$mode == "credit") "Остаток, ₽" else if (z$mode == "npv") "Накопленный NPV, ₽" else "Баланс, ₽"
    plot(z$x, z$y, type = "n", xlab = xl, ylab = yl, bty = "n", las = 1)
    abline(h = pretty(z$y), col = "#E9E7E1", lty = 1)
    if (z$mode == "npv") abline(h = 0, col = "#8F9CA9", lty = 2)
    lines(z$x, z$y, col = "#D66B50", lwd = 3)
    points(tail(z$x, 1), tail(z$y, 1), pch = 19, col = "#1F7880", cex = 1.3)
  }, res = 110)

  output$fin_table <- renderTable({ financial()$table }, striped = TRUE, hover = TRUE, spacing = "xs", rownames = FALSE)
  output$fin_export <- downloadHandler(
    filename = function() paste0("atlas_finance_", input$fin_kind, ".csv"),
    content = function(file) utils::write.csv2(financial()$export, file = file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  derivative <- reactive({
    req(input$f_expr, input$f_point, input$f_min, input$f_max)
    validate(need(is.finite(input$f_point) && is.finite(input$f_min) &&
                  is.finite(input$f_max) && input$f_min < input$f_max &&
                  input$f_max - input$f_min <= 200,
                  "Проверьте точку и границы графика (отрезок до 200 единиц)."))
    expr <- tryCatch(safe_math_expr(input$f_expr), error = function(e) e)
    validate(need(!inherits(expr, "error"), if (inherits(expr, "error")) conditionMessage(expr) else ""))
    d_expr <- tryCatch(D(expr, "x"), error = function(e) e)
    validate(need(!inherits(d_expr, "error"),
                  if (inherits(d_expr, "error")) paste("Не удалось найти производную:", conditionMessage(d_expr)) else ""))
    y0 <- tryCatch(calc_expression(expr, input$f_point)[1], error = function(e) NA_real_)
    slope <- tryCatch(calc_expression(d_expr, input$f_point)[1], error = function(e) NA_real_)
    validate(need(is.finite(y0) && is.finite(slope),
                  "В указанной точке функция или производная не определена."))
    xs <- seq(input$f_min, input$f_max, length.out = 450)
    ys <- tryCatch(calc_expression(expr, xs), error = function(e) rep(NA_real_, length(xs)))
    ok <- is.finite(ys)
    validate(need(sum(ok) >= 2, "На выбранном интервале нет значений для построения графика."))
    tangent <- y0 + slope * (xs - input$f_point)
    list(expr = expr, dexpr = d_expr, x0 = input$f_point,
         y0 = y0, slope = slope, xs = xs, ys = ys, tangent = tangent, ok = ok)
  })

  output$deriv_metrics <- renderUI({
    d <- derivative()
    tags$div(class = "metrics",
      kpi("Точка x₀", fmt_num(d$x0), "Аргумент функции"),
      kpi("Значение f(x₀)", fmt_num(d$y0), "Координата точки"),
      kpi("Наклон f′(x₀)", fmt_num(d$slope), "Угловой коэффициент")
    )
  })
  output$deriv_formula <- renderUI({
    d <- derivative()
    deriv_text <- paste(deparse(d$dexpr, width.cutoff = 500), collapse = "")
    tags$div(
      tags$div(class = "card-kicker", "АНАЛИТИЧЕСКОЕ РЕШЕНИЕ"),
      tags$p(tags$strong("Производная: "), tags$code(paste0("f'(x) = ", deriv_text))),
      tags$p(tags$strong("Касательная: "),
             tags$code(paste0("y = ", fmt_num(d$y0), " + (", fmt_num(d$slope), ") * (x - (", fmt_num(d$x0), "))"))),
      tags$p(class = "hint", "Производная рассчитывается встроенной функцией R D().")
    )
  })
  output$deriv_plot <- renderPlot({
    d <- derivative()
    par(mar = c(4.2, 4.2, 0.8, 1.2), bg = "white", fg = "#233446",
        col.axis = "#677480", col.lab = "#233446", family = "sans")
    plot(d$xs, d$ys, type = "n", xlab = "x", ylab = "y", bty = "n", las = 1)
    abline(h = pretty(d$ys[d$ok]), v = pretty(d$xs), col = "#EDEBE7")
    lines(d$xs, ifelse(d$ok, d$ys, NA_real_), col = "#1F7880", lwd = 3)
    lines(d$xs, d$tangent, col = "#D66B50", lwd = 2.5, lty = 2)
    points(d$x0, d$y0, pch = 19, col = "#D66B50", cex = 1.25)
    legend("topleft", legend = c("f(x)", "Касательная"),
           col = c("#1F7880", "#D66B50"), lwd = c(3, 2.5),
           lty = c(1, 2), bty = "n", cex = .9)
  }, res = 110)

  statistics <- reactive({
    req(input$stat_values)
    vals <- parse_series(input$stat_values)
    validate(need(length(vals) >= 2 && length(vals) <= 10000 && all(is.finite(vals)),
                  "Введите минимум 2 и максимум 10 000 корректных чисел."))
    n <- length(vals)
    m <- mean(vals)
    s <- sd(vals)
    half <- if (s == 0) 0 else qt(.975, n - 1) * s / sqrt(n)
    q <- as.numeric(quantile(vals, c(.25, .75)))
    metrics <- data.frame(
      Показатель = c("Количество наблюдений", "Среднее арифметическое", "Медиана", "Выборочная дисперсия",
                     "Стандартное отклонение", "Минимум", "Q1 (25%)", "Q3 (75%)", "Максимум",
                     "95% ДИ среднего — нижняя граница", "95% ДИ среднего — верхняя граница"),
      Значение = c(as.character(n), fmt_num(m), fmt_num(median(vals)), fmt_num(var(vals)),
                   fmt_num(s), fmt_num(min(vals)), fmt_num(q[1]), fmt_num(q[2]), fmt_num(max(vals)),
                   fmt_num(m - half), fmt_num(m + half)), check.names = FALSE)
    list(values = vals, n = n, mean = m, median = median(vals), sd = s,
         interval = c(m - half, m + half), table = metrics)
  })
  output$stat_metrics <- renderUI({
    s <- statistics()
    tags$div(class = "metrics",
      kpi("Наблюдения", as.character(s$n), "Объём выборки"),
      kpi("Среднее", fmt_num(s$mean), "Арифметическое"),
      kpi("Медиана", fmt_num(s$median), "Центр распределения")
    )
  })
  output$stat_table <- renderTable({ statistics()$table }, striped = TRUE, hover = TRUE, spacing = "xs", rownames = FALSE)
  output$hist_plot <- renderPlot({
    s <- statistics()
    par(mar = c(4.2, 3.6, 0.8, 0.8), bg = "white", fg = "#233446",
        col.axis = "#677480", col.lab = "#233446", family = "sans")
    hist(s$values, breaks = "Sturges", col = "#A8D1C9", border = "white",
         xlab = "Значение", ylab = "Частота", main = "", las = 1)
    abline(v = s$mean, col = "#D66B50", lwd = 2.4, lty = 2)
  }, res = 110)
  output$box_plot <- renderPlot({
    s <- statistics()
    par(mar = c(4.2, 3.6, 0.8, 0.8), bg = "white", fg = "#233446",
        col.axis = "#677480", col.lab = "#233446", family = "sans")
    boxplot(s$values, horizontal = TRUE, col = "#F2CABD", border = "#C96A53",
            xlab = "Значение", main = "", axes = TRUE)
    points(s$mean, 1, pch = 19, col = "#1F7880", cex = 1.2)
  }, res = 110)
  output$stats_export <- downloadHandler(
    filename = function() "atlas_statistics.csv",
    content = function(file) utils::write.csv2(statistics()$table, file = file, row.names = FALSE, fileEncoding = "UTF-8")
  )
}

shinyApp(ui = ui, server = server)
