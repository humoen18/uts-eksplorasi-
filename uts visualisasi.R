# =====================================================================
# UTS EKSPLORASI & VISUALISASI DATA MULTIVARIAT (26.1)
# Topik   : Faktor-faktor yang memengaruhi harga mobil bekas
# Dataset : used_car.csv (609 observasi, 20 variabel)
# Aplikasi: R Shiny  ->  "CARLENS"
# =====================================================================

# =====================================================================
# BAGIAN 0 : PAKET, PALET WARNA, DAN PERSIAPAN DATA
# =====================================================================
pkgs <- c("shiny", "bslib", "dplyr", "plotly", "DT")
miss <- setdiff(pkgs, rownames(installed.packages()))
if (length(miss)) install.packages(miss)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Palet "Aurora Noir" --------------------------------------------
bg        <- "#0B0F1A"   # latar
surface   <- "#131A2B"   # latar
edge      <- "#26314D"   # garis
grid_col  <- "#2A3553"   # garis
ink       <- "#E6EDF7"   # teks primer
muted     <- "#8A97B2"   # teks sekunder
mint      <- "#34E0B5"
pink      <- "#FF5C8A"
amber     <- "#FFC857"
violet    <- "#8B7CFF"
sky       <- "#4CC9F0"
cols      <- c(mint, pink, amber, violet, sky)

# ---- Identitas mahasiswa (UTS dikerjakan individual) -----------------
tim <- c("Nama Mahasiswa", "NIM")      # <-- GANTI dengan nama & NIM Anda

# ---- Baca data -------------------------------------------------------
file_data <- "used_car.csv"
if (!file.exists(file_data)) file_data <- file.choose()
raw <- read.csv(file_data, check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(ncol(raw) == 20)

# Nama kolom diseragamkan (tanpa spasi) agar mudah dipakai
names(raw) <- c("NAMA", "MEREK", "TAHUN", "KM_RIBU", "LOKASI", "TRANSMISI",
                "PLAT", "KAM_BELAKANG", "SUNROOF", "SPION_LIPAT",
                "REM_PARKIR", "NAVIGASI", "VSC", "KEYLESS", "SPORT_MODE",
                "KAMERA_360", "PINTU_GESER", "CRUISE", "HARGA", "ANGSURAN")

fitur_cols <- c("Kamera belakang" = "KAM_BELAKANG", "Sunroof" = "SUNROOF",
                "Spion lipat otomatis" = "SPION_LIPAT",
                "Rem parkir elektrik" = "REM_PARKIR",
                "Navigasi peta" = "NAVIGASI",
                "Vehicle stability control" = "VSC",
                "Keyless push start" = "KEYLESS", "Sports mode" = "SPORT_MODE",
                "Kamera 360" = "KAMERA_360", "Pintu geser listrik" = "PINTU_GESER",
                "Auto cruise control" = "CRUISE")

merek_top <- c("Toyota", "Honda", "Daihatsu", "Mitsubishi", "Suzuki")

# Pengkodean variabel + variabel turunan
df <- raw %>%
  mutate(
    HARGA_JT    = HARGA / 1e6,                    # harga (Rp juta)
    ANGSURAN_JT = ANGSURAN / 1e6,                 # angsuran (Rp juta/bulan)
    UMUR        = 2026 - TAHUN,                   # umur mobil (tahun)
    JML_FITUR   = rowSums(across(all_of(unname(fitur_cols)))),
    TRANS_LBL = factor(ifelse(TRANSMISI == "Automatic", "Otomatis", "Manual"),
                       levels = c("Manual", "Otomatis")),
    PLAT_LBL  = factor(ifelse(PLAT == "even plate", "Genap", "Ganjil"),
                       levels = c("Ganjil", "Genap")),
    MEREK_GRP = factor(ifelse(MEREK %in% merek_top, MEREK, "Lainnya"),
                       levels = c(merek_top, "Lainnya")),
    WILAYAH = factor(
      case_when(grepl("^Jakarta", LOKASI) ~ "Jakarta",
                LOKASI %in% c("Bogor", "Depok", "Bekasi",
                              "Tangerang Selatan") ~ "Bodetabek",
                LOKASI == "Bandung" ~ "Bandung",
                LOKASI %in% c("Surabaya", "Malang") ~ "Jawa Timur",
                TRUE ~ "Tidak diketahui"),
      levels = c("Jakarta", "Bodetabek", "Bandung", "Jawa Timur",
                 "Tidak diketahui")),
    ERA = cut(TAHUN, breaks = c(0, 2016, 2018, 2020, Inf),
              labels = c("<=2016", "2017-2018", "2019-2020", "2021+"))
  )

# Daftar variabel (dipakai di eksplorasi & dashboard)
num_raw  <- c("HARGA_JT", "ANGSURAN_JT", "KM_RIBU", "UMUR", "JML_FITUR")
num_vars <- c("Harga (Rp juta)"            = "HARGA_JT",
              "Angsuran (Rp juta/bln)"     = "ANGSURAN_JT",
              "Jarak tempuh (ribu km)"     = "KM_RIBU",
              "Umur mobil (tahun)"         = "UMUR",
              "Jumlah fitur (0-11)"        = "JML_FITUR")
cat_vars <- c("Transmisi"      = "TRANS_LBL",
              "Merek"          = "MEREK_GRP",
              "Wilayah"        = "WILAYAH",
              "Jenis plat"     = "PLAT_LBL",
              "Kelompok tahun" = "ERA")
lab <- function(v) {
  all <- c(num_vars, cat_vars)
  out <- names(all)[match(v, all)]
  ifelse(is.na(out), v, out)
}

# Pilihan prediktor regresi
# (ANGSURAN sengaja tidak ditawarkan: korelasinya dengan HARGA ~ 1,00)
rg_choices <- c(num_vars[c("Jarak tempuh (ribu km)", "Umur mobil (tahun)",
                           "Jumlah fitur (0-11)")],
                cat_vars[c("Transmisi", "Merek", "Wilayah", "Jenis plat")])
rg_num <- num_vars[c("Jarak tempuh (ribu km)", "Umur mobil (tahun)",
                     "Jumlah fitur (0-11)")]


# =====================================================================
# BAGIAN 1 : FUNGSI BANTU
# =====================================================================
fmt <- function(x, d = 0)
  format(round(x, d), big.mark = ".", decimal.mark = ",",
         nsmall = d, trim = TRUE)
pfmt <- function(p) sapply(p, function(z)
  if (is.na(z)) "-" else if (z < .001) "< 0,001" else fmt(z, 3))

group_colors <- function(lv) setNames(rep_len(cols, length(lv)), lv)

rgba <- function(hex, a = 0.5) {
  r <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", r[1], r[2], r[3], a)
}

kpi <- function(label, value, color) {
  div(class = "kpi", style = paste0("--accent:", color),
      div(class = "kpi-label", label),
      div(class = "kpi-value", value))
}

team_chips <- function() lapply(tim, function(n) span(class = "team-chip", n))

legend_chips <- function(lv, gc) {
  div(class = "chips",
      lapply(lv, function(l)
        span(class = "chip", span(class = "dot",
                                  style = paste0("background:", gc[[l]])), l)))
}

# Tema dasar untuk semua grafik plotly
base_plotly <- function(p) {
  p %>% layout(paper_bgcolor = "rgba(0,0,0,0)",
               plot_bgcolor  = "rgba(0,0,0,0)",
               font   = list(color = ink, size = 12),
               legend = list(bgcolor = "rgba(0,0,0,0)"),
               margin = list(t = 30, r = 15)) %>%
    config(displaylogo = FALSE)
}
style_plotly <- function(p) {
  base_plotly(p) %>%
    layout(xaxis = list(gridcolor = grid_col, zerolinecolor = grid_col),
           yaxis = list(gridcolor = grid_col, zerolinecolor = grid_col))
}

# ---- Ukuran statistik ------------------------------------------------
skew <- function(x) {
  m <- mean(x); s2 <- mean((x - m)^2)
  if (s2 == 0) return(NA_real_)
  mean((x - m)^3) / s2^1.5
}
kurt <- function(x) {
  m <- mean(x); s2 <- mean((x - m)^2)
  if (s2 == 0) return(NA_real_)
  mean((x - m)^4) / s2^2
}
modus_num <- function(x) {
  tb <- table(x)
  if (max(tb) == 1) return("Tidak ada modus")
  paste(head(names(tb)[tb == max(tb)], 3), collapse = ", ")
}
fence <- function(x) {
  q <- quantile(x, c(.25, .75), names = FALSE)
  i <- q[2] - q[1]
  c(lo = q[1] - 1.5 * i, hi = q[2] + 1.5 * i)
}
ringkas_num <- function(x) {
  q <- quantile(x, c(.25, .5, .75), names = FALSE)
  data.frame(
    Kelompok = c("Umum", rep("Tendensi sentral", 3), rep("Dispersi", 6),
                 rep("Kuartil", 3), rep("Bentuk", 2)),
    Ukuran = c("Jumlah data (N)", "Mean", "Median", "Modus",
               "Varians", "Std. deviasi", "Minimum", "Maksimum",
               "Range", "IQR (Q3-Q1)", "Q1", "Q2 (Median)", "Q3",
               "Skewness", "Kurtosis (normal=3)"),
    Nilai = c(fmt(length(x)), fmt(mean(x), 2), fmt(median(x), 2), modus_num(x),
              fmt(var(x), 2), fmt(sd(x), 2), fmt(min(x), 2), fmt(max(x), 2),
              fmt(max(x) - min(x), 2), fmt(q[3] - q[1], 2),
              fmt(q[1], 2), fmt(q[2], 2), fmt(q[3], 2),
              fmt(skew(x), 2), fmt(kurt(x), 2))
  )
}
kekuatan <- function(r) {
  a <- abs(r)
  if (a < .2) "sangat lemah" else if (a < .4) "lemah" else
    if (a < .6) "sedang" else if (a < .8) "kuat" else "sangat kuat"
}

# ---- Fungsi bantu regresi -------------------------------------------
vif_cols <- function(fit) {
  X <- model.matrix(fit)[, -1, drop = FALSE]
  if (ncol(X) < 2) return(setNames(rep(NA_real_, ncol(X)), colnames(X)))
  v <- sapply(seq_len(ncol(X)), function(j) {
    r2 <- summary(lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared
    1 / (1 - r2)
  })
  names(v) <- colnames(X); v
}
bp_test <- function(fit) {       # Breusch-Pagan (versi Koenker, tanpa paket lmtest)
  e2 <- resid(fit)^2
  r2 <- summary(lm(e2 ~ fitted(fit)))$r.squared
  st <- length(e2) * r2
  c(stat = st, p = pchisq(st, 1, lower.tail = FALSE))
}
nice_term <- function(tm, preds, short = FALSE) {
  if (tm == "(Intercept)") return("Intersep")
  v <- preds[startsWith(tm, preds)][1]
  if (is.na(v)) return(tm)
  rest <- substring(tm, nchar(v) + 1)
  if (short) return(if (nzchar(rest)) paste0(v, "[", rest, "]") else v)
  if (nzchar(rest)) paste0(lab(v), " = ", rest) else lab(v)
}
unit_lbl <- c(UMUR = "1 tahun umur mobil", KM_RIBU = "1 ribu km jarak tempuh",
              JML_FITUR = "1 fitur tambahan")


# =====================================================================
# BAGIAN 2 : TEMA & CSS
# =====================================================================
app_theme <- bs_theme(
  version = 5, bg = bg, fg = ink,
  primary = mint, secondary = violet, success = mint,
  info = sky, warning = amber, danger = pink,
  base_font    = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("Trebuchet MS", "Segoe UI", "sans-serif")
)

css <- sprintf("
body{background:radial-gradient(1200px 600px at 85%% -10%%, #1b2345 0%%, %s 60%%) fixed;}
.navbar{background:%s !important;border-bottom:1px solid %s;}
.navbar-brand{font-weight:800;letter-spacing:.5px;color:%s !important;}
.navbar .nav-link{color:%s !important;font-weight:600;}
.navbar .nav-link.active{color:%s !important;border-bottom:2px solid %s;}
.bslib-sidebar-layout > .sidebar{background:%s !important;border-right:1px solid %s;}
.card{background:%s !important;border:1px solid %s !important;border-radius:16px;}
.card-header{background:transparent !important;border-bottom:1px solid %s;
             font-weight:700;color:%s;}
.kpi{background:%s;border:1px solid %s;border-radius:16px;padding:16px 20px;
     position:relative;overflow:hidden;}
.kpi:before{content:'';position:absolute;left:0;top:0;bottom:0;width:5px;
            background:var(--accent);}
.kpi-label{font-size:.72rem;letter-spacing:1.4px;text-transform:uppercase;color:%s;}
.kpi-value{font-size:1.9rem;font-weight:800;color:var(--accent);line-height:1.2;}
.chips{display:flex;flex-wrap:wrap;gap:8px;margin:6px 0;}
.chip{background:%s;border:1px solid %s;border-radius:999px;padding:3px 12px;
      font-size:.8rem;color:%s;}
.dot{display:inline-block;width:9px;height:9px;border-radius:50%%;margin-right:6px;}
.note{color:%s;font-size:.85rem;}
.section-title{font-weight:800;color:%s;margin:4px 0 2px;}
.table{--bs-table-bg:transparent;--bs-table-color:%s;}
table.dataTable tbody tr{background-color:transparent !important;color:%s;}
.dataTables_wrapper,.dataTables_info,.dataTables_length,.dataTables_filter{color:%s !important;}
.btn-primary{color:#07130f;font-weight:700;}
.team-title{font-size:.72rem;letter-spacing:2px;text-transform:uppercase;color:%s;margin-bottom:6px;}
.team-chip{display:inline-block;background:linear-gradient(90deg,%s,%s);color:#07130f;
           font-weight:700;border-radius:999px;padding:3px 12px;margin:0 6px 6px 0;font-size:.82rem;}
.team-footer{margin-top:28px;padding:16px 20px;border:1px dashed %s;border-radius:16px;}
",
               bg, surface, edge, mint, ink, mint, mint, surface, edge,
               surface, edge, edge, ink, surface, edge, muted, bg, edge, ink,
               muted, ink, ink, ink, ink,
               muted, mint, sky, edge)

# Tambahan CSS untuk kotak interpretasi & teks keluaran
css <- paste0(css, "
.interp{background:", bg, ";border-left:4px solid ", mint, ";border-radius:10px;
        padding:10px 14px;margin-top:10px;color:", ink, ";font-size:.9rem;}
.interp ul{margin:6px 0 0 0;padding-left:18px;}
.interp li{margin-bottom:4px;}
pre,.shiny-text-output{background:", bg, " !important;color:", ink, " !important;
        border:1px solid ", edge, " !important;border-radius:10px;font-size:.8rem;}
.eq{background:", bg, ";border:1px solid ", edge, ";border-radius:10px;padding:10px 14px;
    font-family:Consolas,monospace;font-size:.85rem;color:", amber, ";margin-bottom:10px;}
")

team_footer <- function() {
  div(class = "team-footer",
      div(class = "team-title", "Dashboard ini dikerjakan oleh"),
      div(team_chips()))
}

# ---- Tabel variabel (Soal 1) -----------------------------------------
fitur_def <- c("kamera belakang", "sunroof", "spion lipat otomatis",
               "rem parkir elektrik", "navigasi peta",
               "vehicle stability control (VSC)", "keyless push start",
               "sports mode", "kamera 360", "pintu geser listrik",
               "auto cruise control")

# (a) 20 variabel asli pada dataset
var_asli <- data.frame(
  No = 1:20,
  `Variabel pada data` = c("car name", "brand", "year", "mileage (km)", "location",
                           "transmission", "plate type",
                           "rear camera", "sun roof", "auto retract mirror",
                           "electric parking brake", "map navigator",
                           "vehicle stability control", "keyless push start",
                           "sports mode", "360 camera view", "power sliding door",
                           "auto cruise control",
                           "price (Rp)", "instalment (Rp|Monthly)"),
  Tipe = c("Nominal", "Nominal", "Numerik (diskrit)", "Numerik (rasio)",
           "Nominal", "Nominal (biner)", "Nominal (biner)",
           rep("Biner (0/1)", 11), "Numerik (rasio)", "Numerik (rasio)"),
  Definisi = c("Nama dan varian mobil (172 nama unik)",
               "Merek mobil (13 merek)",
               "Tahun pembuatan mobil",
               "Jarak tempuh mobil; nilai asli bertitik (mis. 10.508) dibaca sebagai ribuan km",
               "Kota lokasi penjualan (13 kota, termasuk 'Unknown')",
               "Jenis transmisi: Manual / Automatic",
               "Jenis plat nomor: ganjil / genap",
               paste0("Ketersediaan ", fitur_def, " (1 = ada, 0 = tidak)"),
               "Harga jual mobil (Rupiah)",
               "Cicilan per bulan (Rupiah); hampir proporsional dengan harga"),
  `Peran dalam analisis` = c(
    "Label pada tooltip grafik; tidak dianalisis",
    "Dikelompokkan menjadi MEREK_GRP; PREDIKTOR regresi",
    "Diturunkan menjadi UMUR (PREDIKTOR) dan ERA; filter tahun",
    "Menjadi KM_RIBU; PREDIKTOR regresi",
    "Dikelompokkan menjadi WILAYAH; prediktor opsional",
    "Menjadi TRANS_LBL; PREDIKTOR regresi",
    "Menjadi PLAT_LBL; prediktor opsional",
    rep("Dijumlahkan menjadi JML_FITUR (PREDIKTOR regresi)", 11),
    "Menjadi HARGA_JT; VARIABEL RESPONS regresi",
    "Menjadi ANGSURAN_JT; eksplorasi saja, tidak masuk regresi (kolinear dengan harga)"),
  check.names = FALSE, stringsAsFactors = FALSE
)

# (b) variabel turunan hasil pengolahan
var_turunan <- data.frame(
  Variabel = c("HARGA_JT", "ANGSURAN_JT", "KM_RIBU", "UMUR", "JML_FITUR",
               "TRANS_LBL", "PLAT_LBL", "MEREK_GRP", "WILAYAH", "ERA"),
  Tipe = c("Numerik (rasio)", "Numerik (rasio)", "Numerik (rasio)",
           "Numerik (rasio)", "Numerik (diskrit)",
           "Nominal", "Nominal", "Nominal", "Nominal", "Ordinal"),
  `Aturan pembentukan` = c(
    "price (Rp) / 1.000.000 (harga dalam juta rupiah)",
    "instalment / 1.000.000 (juta rupiah per bulan)",
    "= kolom mileage (ribu km)",
    "2026 - year",
    "Jumlah dari 11 fitur biner (0 sampai 11)",
    "Manual / Otomatis",
    "Ganjil / Genap",
    "Toyota, Honda, Daihatsu, Mitsubishi, Suzuki, Lainnya",
    "Jakarta, Bodetabek, Bandung, Jawa Timur, Tidak diketahui",
    "<=2016, 2017-2018, 2019-2020, 2021+"),
  `Digunakan pada` = c(
    "Univariat, bivariat, multivariat; respons regresi (dalam ln)",
    "Univariat, bivariat, multivariat (tidak di regresi)",
    "Univariat, bivariat, multivariat; prediktor regresi",
    "Univariat, bivariat, multivariat; prediktor regresi",
    "Univariat, bivariat, multivariat; prediktor regresi",
    "Univariat, bivariat, multivariat; prediktor regresi",
    "Univariat, bivariat; prediktor opsional",
    "Univariat, bivariat, multivariat; prediktor regresi",
    "Univariat, bivariat; prediktor opsional",
    "Univariat, bivariat, multivariat (heatmap)"),
  check.names = FALSE, stringsAsFactors = FALSE
)


# =====================================================================
# BAGIAN 3 : UI
# =====================================================================
ui <- page_navbar(
  title = "CARLENS",
  id = "nav", theme = app_theme, fillable = FALSE,
  header = tags$head(tags$style(HTML(css))),
  
  # --- Sidebar global (filter) ---
  sidebar = sidebar(
    width = 290, open = "desktop",
    h5("FILTER DATA", class = "section-title"),
    checkboxGroupInput("f_trans", "Transmisi",
                       choices = levels(df$TRANS_LBL),
                       selected = levels(df$TRANS_LBL)),
    checkboxGroupInput("f_merek", "Merek",
                       choices = levels(df$MEREK_GRP),
                       selected = levels(df$MEREK_GRP)),
    checkboxGroupInput("f_wil", "Wilayah",
                       choices = levels(df$WILAYAH),
                       selected = levels(df$WILAYAH)),
    sliderInput("f_thn", "Tahun pembuatan",
                min = min(df$TAHUN), max = max(df$TAHUN),
                value = c(min(df$TAHUN), max(df$TAHUN)), step = 1, sep = ""),
    actionButton("f_reset", "Reset filter", class = "btn-primary w-100"),
    hr(),
    div(class = "note", textOutput("f_info")),
    hr(),
    div(class = "team-title", "Dikerjakan oleh"),
    div(team_chips())
  ),
  
  # ===== TAB LATAR BELAKANG (Soal 1) =====
  nav_panel(
    "Latar Belakang",
    h3("Latar Belakang Analisis Harga Mobil Bekas", class = "section-title"),
    card(card_header("1. Latar Belakang"),
         p("Mobil adalah kebutuhan transportasi utama bagi banyak keluarga di Indonesia.
            Karena harga mobil baru relatif tinggi, mobil bekas menjadi alternatif yang
            banyak dipilih. Pasar mobil bekas sangat beragam: dua mobil dengan merek yang
            sama dapat memiliki harga yang jauh berbeda, bergantung pada tahun pembuatan,
            jarak tempuh, jenis transmisi, kelengkapan fitur, dan lokasi penjualan."),
         p("Keragaman ini menyulitkan pembeli untuk menilai apakah harga tawaran sudah
            wajar, dan menyulitkan penjual maupun platform iklan untuk menetapkan harga
            yang kompetitif. Harga yang terlalu tinggi membuat unit lama terjual,
            sedangkan harga yang terlalu rendah merugikan penjual. Karena itu diperlukan
            analisis data untuk memahami faktor apa saja yang berkaitan dengan harga
            mobil bekas dan seberapa besar kaitannya."),
         p("Penelitian ini memakai data iklan mobil bekas (609 unit) dan menerapkan
            statistika deskriptif, eksplorasi dan visualisasi data (univariat, bivariat,
            multivariat), serta regresi linear berganda, yang hasilnya disajikan dalam
            dashboard interaktif.")),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("2. Rumusan Masalah"),
           tags$ol(
             tags$li("Bagaimana karakteristik mobil bekas pada data (harga, umur,
                       jarak tempuh, transmisi, merek, wilayah, dan fitur)?"),
             tags$li("Bagaimana hubungan harga dengan umur, jarak tempuh, transmisi,
                       dan merek?"),
             tags$li("Bagaimana pola harga jika beberapa variabel dilihat bersamaan?"),
             tags$li("Variabel apa yang berpengaruh signifikan terhadap harga dan
                       seberapa baik model regresi menjelaskannya?"))),
      card(card_header("3. Tujuan Analisis"),
           tags$ol(
             tags$li("Menggambarkan karakteristik mobil bekas (univariat)."),
             tags$li("Menelusuri hubungan harga dengan variabel lain (bivariat)."),
             tags$li("Menemukan pola gabungan beberapa variabel (multivariat)."),
             tags$li("Membangun model regresi linear berganda untuk harga dan
                       menafsirkan koefisiennya."),
             tags$li("Menyimpulkan faktor yang paling berkaitan dengan harga serta
                       memberi rekomendasi.")))
    ),
    card(card_header("4. Manfaat"),
         tags$ul(
           tags$li("Pembeli: acuan menilai kewajaran harga tawaran."),
           tags$li("Penjual/dealer: dasar penetapan harga berdasarkan faktor yang terbukti berpengaruh."),
           tags$li("Platform iklan: dasar membuat estimator harga wajar."))),
    card(card_header("5. Gambaran Singkat Data"),
         uiOutput("info_latar"),
         uiOutput("info_data"),
         p(class = "note", "Rincian variabel ada di tab 'Data & Variabel'.")),
    team_footer()
  ),
  
  # ===== TAB DATA & VARIABEL (Soal 1) =====
  nav_panel(
    "Data & Variabel",
    h3("Dataset dan Variabel yang Digunakan", class = "section-title"),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Sumber Data & Pengolahan"),
           p(class = "note",
             "Sumber: dataset 'used_car' (iklan mobil bekas). ",
             "CATATAN: isi nama platform/tautan asal dataset (mis. Kaggle) di sini."),
           h6("Pengolahan data", class = "section-title"),
           uiOutput("info_olah")),
      card(card_header("Penetrasi Fitur (% mobil memiliki fitur)"),
           plotlyOutput("ov_fitur", height = "360px"))
    ),
    card(card_header("Variabel yang Digunakan dalam Analisis"),
         uiOutput("info_pakai")),
    card(card_header("Daftar 20 Variabel pada Dataset"),
         tableOutput("tbl_asli")),
    card(card_header("Variabel Turunan Hasil Pengolahan"),
         tableOutput("tbl_turun")),
    team_footer()
  ),
  
  # ===== TAB 1 : RINGKASAN =====
  nav_panel(
    "Ringkasan",
    h3("Ringkasan Pasar Mobil Bekas", class = "section-title"),
    p(paste0("Gambaran umum data sesuai filter di sisi kiri.  |  Oleh: ",
             paste(tim, collapse = " • ")), class = "note"),
    uiOutput("kpis"),
    br(),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("Komposisi Transmisi"),
           plotlyOutput("ov_trans", height = "300px")),
      card(card_header("Jumlah Mobil per Merek"),
           plotlyOutput("ov_merek", height = "300px")),
      card(card_header("Jumlah Mobil per Wilayah"),
           plotlyOutput("ov_wil", height = "300px"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Sebaran Harga per Kelompok Tahun"),
           plotlyOutput("ov_era", height = "320px")),
      card(card_header("Kualitas Data & Outlier (IQR)"),
           tableOutput("ov_quality"))
    ),
    team_footer()
  ),
  
  # ===== TAB 2 : UNIVARIAT =====
  nav_panel(
    "Univariat",
    navset_card_tab(
      id = "uni_tabs",
      
      # ---------- Data kategorik ----------
      nav_panel("Data Kategorik (Nominal & Ordinal)",
                layout_columns(
                  col_widths = c(8, 4),
                  card(card_header("Visualisasi Data Kategorik"),
                       layout_columns(
                         col_widths = c(6, 6),
                         selectInput("uk_var", "Variabel", cat_vars),
                         selectInput("uk_type", "Jenis grafik",
                                     c("Bar chart", "Pie chart", "Pareto chart"))),
                       plotlyOutput("uk_plot", height = "360px")),
                  card(card_header("Frekuensi, Proporsi & Modus"),
                       tableOutput("uk_tbl"),
                       uiOutput("uk_interp")))),
      
      # ---------- Data numerik ----------
      nav_panel("Data Numerik (Interval & Rasio)",
                layout_columns(
                  col_widths = c(8, 4),
                  card(card_header("Distribusi Variabel Numerik"),
                       layout_columns(
                         col_widths = c(3, 3, 3, 3),
                         selectInput("un_var", "Variabel", num_vars),
                         selectInput("un_type", "Jenis grafik",
                                     c("Histogram", "Density plot", "Boxplot",
                                       "Stem-and-leaf")),
                         sliderInput("un_bins", "Jumlah bin", 5, 40, 15),
                         numericInput("un_n", "Sampel stem-and-leaf", 50, 10, 500, 10)),
                       conditionalPanel("input.un_type != 'Stem-and-leaf'",
                                        plotlyOutput("un_plot", height = "340px")),
                       conditionalPanel("input.un_type == 'Stem-and-leaf'",
                                        verbatimTextOutput("un_stem"))),
                  card(card_header("Statistik Deskriptif"),
                       tableOutput("un_tbl"),
                       uiOutput("un_interp"))),
                layout_columns(
                  col_widths = c(7, 5),
                  card(card_header("Deteksi Outlier (aturan 1,5 x IQR)"),
                       plotlyOutput("un_out_plot", height = "260px")),
                  card(card_header("Batas Outlier & Dampaknya"),
                       tableOutput("un_out_tbl"),
                       tableOutput("un_dampak"))))
    )
  ),
  
  # ===== TAB 3 : BIVARIAT =====
  nav_panel(
    "Bivariat",
    navset_card_tab(
      id = "bi_tabs",
      
      # ---------- Kategorik vs Kategorik ----------
      nav_panel("Kategorik vs Kategorik",
                layout_columns(
                  col_widths = c(8, 4),
                  card(card_header("Diagram Batang Dua Variabel Kategorik"),
                       layout_columns(
                         col_widths = c(4, 4, 4),
                         selectInput("kk_a", "Kategori utama (X)", cat_vars,
                                     selected = "MEREK_GRP"),
                         selectInput("kk_b", "Dipecah menurut", cat_vars,
                                     selected = "TRANS_LBL"),
                         selectInput("kk_type", "Jenis grafik",
                                     c("Stacked bar chart", "Grouped bar chart",
                                       "Segmented bar chart (persen)"))),
                       plotlyOutput("kk_plot", height = "360px")),
                  card(card_header("Uji Chi-Square"),
                       verbatimTextOutput("kk_chi_txt"),
                       uiOutput("kk_interp"))),
                layout_columns(
                  col_widths = c(6, 6),
                  card(card_header("Tabel Kontingensi (Frekuensi)"),
                       tableOutput("kk_tab")),
                  card(card_header("Persentase Baris (%)"),
                       tableOutput("kk_prop")))),
      
      # ---------- Kuantitatif vs Kuantitatif ----------
      nav_panel("Kuantitatif vs Kuantitatif",
                layout_columns(
                  col_widths = c(8, 4),
                  card(card_header("Hubungan Dua Variabel Numerik"),
                       selectInput("kn_type", "Jenis grafik",
                                   c("Scatterplot",
                                     "Scatterplot + garis linear",
                                     "Scatterplot + garis kuadratik",
                                     "Scatterplot + garis nonparametrik (LOESS)",
                                     "Line plot (rata-rata Y per nilai X)",
                                     "Plot korelasi")),
                       conditionalPanel(
                         "input.kn_type != 'Plot korelasi'",
                         layout_columns(
                           col_widths = c(4, 4, 4),
                           selectInput("kn_x", "Sumbu X", num_vars, selected = "UMUR"),
                           selectInput("kn_y", "Sumbu Y", num_vars, selected = "HARGA_JT"),
                           selectInput("kn_color", "Warna / kelompok",
                                       c("Tanpa warna" = "none", cat_vars)))),
                       conditionalPanel(
                         "input.kn_type == 'Plot korelasi'",
                         layout_columns(
                           col_widths = c(8, 4),
                           checkboxGroupInput("cor_vars", "Variabel", num_vars,
                                              selected = num_vars, inline = TRUE),
                           radioButtons("cor_method", "Metode",
                                        c("Spearman" = "spearman",
                                          "Pearson" = "pearson")))),
                       plotlyOutput("kn_plot", height = "400px")),
                  card(card_header("Kekuatan Hubungan"),
                       uiOutput("kn_side")))),
      
      # ---------- Kuantitatif vs Kategorik ----------
      nav_panel("Kuantitatif vs Kategorik",
                layout_columns(
                  col_widths = c(8, 4),
                  card(card_header("Variabel Numerik menurut Kelompok"),
                       layout_columns(
                         col_widths = c(3, 3, 3, 3),
                         selectInput("kq_y", "Variabel numerik (Y)", num_vars),
                         selectInput("kq_x", "Kelompok (X)", cat_vars,
                                     selected = "MEREK_GRP"),
                         selectInput("kq_g", "Kelompok kedua (Mean/SEM)",
                                     c("Tanpa" = "none", cat_vars), selected = "TRANS_LBL"),
                         selectInput("kq_type", "Jenis grafik",
                                     c("Bar chart (rata-rata)",
                                       "Grouped kernel density plot",
                                       "Box plot",
                                       "Violin plot",
                                       "Box plot + Violin plot",
                                       "Ridgeline plot",
                                       "Mean/SEM plot",
                                       "Strip plot",
                                       "Cleveland dot chart",
                                       "Box plot + titik data (jitter)"))),
                       plotlyOutput("kq_plot", height = "400px")),
                  card(card_header("Ringkasan per Kelompok"),
                       tableOutput("kq_tbl"),
                       uiOutput("kq_interp"))))
    )
  ),
  
  # ===== TAB 4 : MULTIVARIAT =====
  nav_panel(
    "Multivariat",
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Eksplorasi Tiga Variabel atau Lebih"),
           selectInput("mv_type", "Jenis grafik",
                       c("Bubble chart (4 variabel)",
                         "Heatmap rata-rata (3 variabel)",
                         "Parallel coordinates",
                         "Scatterplot 3D",
                         "Scatterplot matrix (SPLOM)")),
           conditionalPanel(
             "input.mv_type == 'Bubble chart (4 variabel)'",
             layout_columns(
               col_widths = c(3, 3, 3, 3),
               selectInput("mv_x", "Sumbu X", num_vars, selected = "KM_RIBU"),
               selectInput("mv_y", "Sumbu Y", num_vars, selected = "HARGA_JT"),
               selectInput("mv_size", "Ukuran gelembung", num_vars,
                           selected = "JML_FITUR"),
               selectInput("mv_col", "Warna (kategori)", cat_vars,
                           selected = "TRANS_LBL"))),
           conditionalPanel(
             "input.mv_type == 'Heatmap rata-rata (3 variabel)'",
             layout_columns(
               col_widths = c(4, 4, 4),
               selectInput("mv_hr", "Baris (kategori)", cat_vars, selected = "ERA"),
               selectInput("mv_hc", "Kolom (kategori)", cat_vars, selected = "MEREK_GRP"),
               selectInput("mv_hv", "Nilai rata-rata dari", num_vars,
                           selected = "HARGA_JT"))),
           conditionalPanel(
             "input.mv_type == 'Parallel coordinates' || input.mv_type == 'Scatterplot matrix (SPLOM)'",
             layout_columns(
               col_widths = c(8, 4),
               checkboxGroupInput("mv_vars", "Variabel (min. 3)", num_vars,
                                  selected = c("HARGA_JT", "KM_RIBU", "UMUR",
                                               "JML_FITUR"), inline = TRUE),
               conditionalPanel(
                 "input.mv_type == 'Scatterplot matrix (SPLOM)'",
                 selectInput("mv_col2", "Warna (kategori)", cat_vars,
                             selected = "TRANS_LBL")))),
           conditionalPanel(
             "input.mv_type == 'Scatterplot 3D'",
             layout_columns(
               col_widths = c(3, 3, 3, 3),
               selectInput("mv_3x", "Sumbu X", num_vars, selected = "UMUR"),
               selectInput("mv_3y", "Sumbu Y", num_vars, selected = "KM_RIBU"),
               selectInput("mv_3z", "Sumbu Z", num_vars, selected = "HARGA_JT"),
               selectInput("mv_col3", "Warna (kategori)", cat_vars,
                           selected = "TRANS_LBL"))),
           plotlyOutput("mv_plot", height = "460px")),
      card(card_header("Interpretasi"),
           uiOutput("mv_interp"),
           p(class = "note",
             "Gunakan filter di sisi kiri untuk membandingkan subkelompok data."))
    ),
    team_footer()
  ),
  
  # ===== TAB 5 : REGRESI =====
  nav_panel(
    "Regresi",
    h3("Regresi Linear Berganda: Harga Mobil Bekas", class = "section-title"),
    p(class = "note",
      "Respons: harga mobil (Rp juta). Model dihitung ulang pada data sesuai filter."),
    layout_columns(
      col_widths = c(4, 8),
      card(card_header("Spesifikasi Model"),
           checkboxInput("rg_log", "Transformasi log pada respons (ln Harga)", TRUE),
           checkboxGroupInput("rg_pred", "Prediktor (min. 2)", rg_choices,
                              selected = c("UMUR", "KM_RIBU", "JML_FITUR",
                                           "TRANS_LBL", "MEREK_GRP")),
           div(class = "interp",
               "Alasan pemilihan: harga adalah luaran yang ingin dijelaskan.
                Prediktor dipilih karena secara logis memengaruhi harga.
                Angsuran bulanan TIDAK dipakai karena berkorelasi ~1,00 dengan harga
                (sekadar turunan harga, bukan penyebabnya).
                Level acuan kategori = level pertama (Manual, Toyota, Jakarta, Ganjil).")),
      card(card_header("Visualisasi Hasil Model"),
           layout_columns(
             col_widths = c(6, 6),
             selectInput("rg_viz", "Jenis grafik",
                         c("Koefisien + selang kepercayaan 95%",
                           "Kontribusi prediktor (|t|)",
                           "Aktual vs prediksi",
                           "Residual vs fitted",
                           "Q-Q plot residual",
                           "Histogram residual",
                           "Efek parsial prediktor numerik",
                           "Cook's distance")),
             conditionalPanel(
               "input.rg_viz == 'Efek parsial prediktor numerik'",
               selectInput("rg_eff", "Prediktor numerik", rg_num))),
           plotlyOutput("rg_plot", height = "400px"),
           uiOutput("rg_viz_interp"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Persamaan Model & Estimasi Koefisien"),
           uiOutput("rg_eq"),
           tableOutput("rg_coef")),
      card(card_header("Ukuran Kecocokan & Uji Asumsi"),
           tableOutput("rg_stat"))
    ),
    card(card_header("Interpretasi Koefisien"), uiOutput("rg_interp")),
    card(card_header("Keterbatasan Model"), uiOutput("rg_limit")),
    team_footer()
  ),
  
  # ===== TAB 6 : KESIMPULAN =====
  nav_panel(
    "Kesimpulan",
    h3("Kesimpulan & Rekomendasi", class = "section-title"),
    p(class = "note",
      "Angka pada kesimpulan dihitung dari seluruh data (tanpa filter)."),
    uiOutput("ks_text"),
    team_footer()
  ),
  
  # ===== TAB 7 : DATA =====
  nav_panel(
    "Data",
    card(card_header("Tabel Data (sesuai filter)"),
         DT::DTOutput("tbl"),
         downloadButton("dl", "Unduh CSV", class = "btn-primary mt-2")),
    team_footer()
  )
)


# =====================================================================
# BAGIAN 4 : SERVER
# =====================================================================
server <- function(input, output, session) {
  
  # ---------- Data terfilter ----------
  dat <- reactive({
    d <- df %>%
      filter(TRANS_LBL %in% input$f_trans,
             MEREK_GRP %in% input$f_merek,
             WILAYAH %in% input$f_wil,
             TAHUN >= input$f_thn[1], TAHUN <= input$f_thn[2])
    validate(need(nrow(d) > 0,
                  "Tidak ada data pada filter ini. Longgarkan filter di sisi kiri."))
    d
  })
  
  output$f_info <- renderText({
    n <- nrow(df %>% filter(TRANS_LBL %in% input$f_trans,
                            MEREK_GRP %in% input$f_merek,
                            WILAYAH %in% input$f_wil,
                            TAHUN >= input$f_thn[1], TAHUN <= input$f_thn[2]))
    paste0(n, " dari ", nrow(df), " observasi terpilih")
  })
  
  observeEvent(input$f_reset, {
    updateCheckboxGroupInput(session, "f_trans", selected = levels(df$TRANS_LBL))
    updateCheckboxGroupInput(session, "f_merek", selected = levels(df$MEREK_GRP))
    updateCheckboxGroupInput(session, "f_wil",   selected = levels(df$WILAYAH))
    updateSliderInput(session, "f_thn", value = range(df$TAHUN))
  })
  
  # helper: data frame jumlah per kategori
  count_df <- function(d, v) {
    t <- table(d[[v]])
    data.frame(k = names(t), n = as.integer(t))
  }
  # helper: kelompok (faktor) untuk pewarnaan
  grp <- function(d, v) {
    if (v == "none") factor(rep("Semua", nrow(d))) else droplevels(d[[v]])
  }
  
  # ======================= LATAR BELAKANG & DATA =======================
  output$info_latar <- renderUI({
    n   <- nrow(raw)
    mk  <- sort(table(raw$MEREK), decreasing = TRUE)
    tagList(
      p(paste0("Dataset ini berisi ", n, " iklan mobil bekas (satu baris = satu unit mobil) ",
               "dari ", length(unique(raw$MEREK)), " merek yang dijual di ",
               length(unique(raw$LOKASI)), " lokasi. Tahun pembuatan berkisar ",
               min(raw$TAHUN), "-", max(raw$TAHUN), " dan harga berkisar Rp ",
               fmt(min(df$HARGA_JT), 0), " - ", fmt(max(df$HARGA_JT), 0),
               " juta (median Rp ", fmt(median(df$HARGA_JT), 1), " juta).")),
      p(paste0("Merek terbanyak adalah ", names(mk)[1], " (", mk[[1]], " unit, ",
               fmt(100 * mk[[1]] / n, 1), "%). Sebanyak ",
               fmt(100 * mean(df$TRANS_LBL == "Otomatis"), 1),
               "% mobil bertransmisi otomatis. Setiap iklan memuat identitas mobil, ",
               "spesifikasi (tahun, jarak tempuh, transmisi, plat), lokasi, 11 fitur ",
               "kelengkapan, serta harga dan angsuran bulanan.")),
      p(class = "note",
        "Karena harga yang tercatat adalah harga tawaran iklan, hasil analisis ",
        "menggambarkan pasar yang diiklankan, bukan harga transaksi akhir.")
    )
  })
  
  output$info_data <- renderUI({
    div(class = "chips",
        span(class = "chip", paste0("Observasi: ", nrow(raw))),
        span(class = "chip", paste0("Variabel asli: ", ncol(raw))),
        span(class = "chip", "Numerik: 4 (tahun, jarak tempuh, harga, angsuran)"),
        span(class = "chip", "Kategorik: 5 (nama, merek, lokasi, transmisi, plat)"),
        span(class = "chip", "Biner: 11 fitur"),
        span(class = "chip", paste0("Variabel turunan: ", nrow(var_turunan))))
  })
  
  output$info_pakai <- renderUI({
    baris <- function(judul, warna, isi, ket)
      div(style = "margin-bottom:10px;",
          div(style = paste0("font-weight:700;color:", warna, ";"), judul),
          div(class = "chips",
              lapply(isi, function(z) span(class = "chip", z))),
          div(class = "note", ket))
    tagList(
      baris("Variabel respons (Y)", pink, "HARGA_JT (harga, Rp juta)",
            "Variabel yang ingin dijelaskan; pada regresi dipakai dalam bentuk ln(harga)."),
      baris("Prediktor numerik (X)", mint,
            c("UMUR (tahun)", "KM_RIBU (ribu km)", "JML_FITUR (0-11)"),
            "Umur mobil, jarak tempuh, dan jumlah fitur yang dimiliki."),
      baris("Prediktor kategorik (X)", violet,
            c("TRANS_LBL (transmisi)", "MEREK_GRP (merek)", "WILAYAH", "PLAT_LBL"),
            "Dikodekan sebagai variabel dummy; level acuan = level pertama. Transmisi dan merek menjadi default model."),
      baris("Variabel eksplorasi saja", amber,
            c("ANGSURAN_JT", "ERA (kelompok tahun)", "11 fitur biner", "TAHUN (filter)"),
            "Dipakai pada grafik univariat/bivariat/multivariat dan filter. ANGSURAN_JT tidak masuk regresi karena berkorelasi ~1,00 dengan harga."),
      baris("Tidak dianalisis", muted, "NAMA (car name)",
            "Hanya sebagai label pada tooltip grafik (172 nama unik).")
    )
  })
  
  output$info_olah <- renderUI({
    na_tot <- sum(is.na(raw))
    n_unk  <- sum(raw$LOKASI == "Unknown")
    tags$ul(
      tags$li(paste0("Data hilang (NA): ", na_tot, " sel. Lokasi bernilai 'Unknown' ",
                     "pada ", n_unk, " mobil dipertahankan sebagai kategori sendiri.")),
      tags$li("Pencilan: ditandai dengan aturan 1,5 x IQR (tab Univariat) dan TIDAK dibuang, ",
              "karena merupakan mobil yang sah (mis. mobil mewah/baru)."),
      tags$li("Transformasi: harga dibagi 1 juta; jarak tempuh dibaca sebagai ribu km; ",
              "ln(harga) dipakai pada regresi untuk memperbaiki kemencengan."),
      tags$li("Pengelompokan: merek (5 teratas + Lainnya), kota menjadi wilayah, tahun menjadi 4 kelompok."))
  })
  
  output$tbl_asli  <- renderTable(var_asli,    width = "100%")
  output$tbl_turun <- renderTable(var_turunan, width = "100%")
  
  output$ov_fitur <- renderPlotly({
    p <- sapply(fitur_cols, function(v) 100 * mean(df[[v]]))
    o <- order(p)
    plot_ly(x = as.numeric(p[o]), y = names(p)[o], type = "bar", orientation = "h",
            text = paste0(fmt(p[o], 1), "%"), textposition = "outside",
            marker = list(color = sky)) %>%
      style_plotly() %>%
      layout(xaxis = list(title = "% mobil memiliki fitur"),
             yaxis = list(title = "", categoryorder = "array",
                          categoryarray = names(p)[o]),
             margin = list(l = 150, r = 40))
  })
  
  # ======================= RINGKASAN =======================
  output$kpis <- renderUI({
    d <- dat()
    layout_columns(
      col_widths = c(3, 3, 2, 2, 2),
      kpi("Jumlah mobil",        fmt(nrow(d)),                         mint),
      kpi("Rata-rata harga",     paste("Rp", fmt(mean(d$HARGA_JT), 1), "jt"), sky),
      kpi("Rata-rata umur",      paste(fmt(mean(d$UMUR), 1), "th"),    amber),
      kpi("Jarak tempuh",        paste(fmt(mean(d$KM_RIBU), 1), "rb km"), pink),
      kpi("Otomatis",            paste0(fmt(mean(d$TRANS_LBL == "Otomatis") * 100, 1), "%"), violet)
    )
  })
  
  output$ov_trans <- renderPlotly({
    cd <- count_df(dat(), "TRANS_LBL")
    plot_ly(cd, labels = ~k, values = ~n, type = "pie", hole = 0.62,
            marker = list(colors = c(sky, pink),
                          line = list(color = surface, width = 3)),
            textinfo = "label+percent", insidetextfont = list(color = bg)) %>%
      base_plotly() %>% layout(showlegend = FALSE)
  })
  
  output$ov_merek <- renderPlotly({
    cd <- count_df(dat(), "MEREK_GRP")
    plot_ly(cd, x = ~n, y = ~k, type = "bar", orientation = "h",
            marker = list(color = rep_len(cols, nrow(cd)))) %>%
      style_plotly() %>%
      layout(xaxis = list(title = "Jumlah"),
             yaxis = list(title = "", categoryorder = "array",
                          categoryarray = rev(levels(df$MEREK_GRP))))
  })
  
  output$ov_wil <- renderPlotly({
    cd <- count_df(dat(), "WILAYAH")
    plot_ly(cd, x = ~k, y = ~n, type = "bar",
            marker = list(color = rep_len(cols, nrow(cd)))) %>%
      style_plotly() %>%
      layout(xaxis = list(title = "", categoryorder = "array",
                          categoryarray = levels(df$WILAYAH)),
             yaxis = list(title = "Jumlah"))
  })
  
  output$ov_era <- renderPlotly({
    d <- dat()
    g <- droplevels(d$ERA)
    gc <- group_colors(levels(g))
    p <- plot_ly()
    for (lv in levels(g)) {
      p <- add_trace(p, y = d$HARGA_JT[g == lv], type = "violin", name = lv,
                     box = list(visible = TRUE), meanline = list(visible = TRUE),
                     line = list(color = gc[[lv]]),
                     fillcolor = gc[[lv]], opacity = 0.55)
    }
    style_plotly(p) %>%
      layout(showlegend = FALSE, yaxis = list(title = "Harga (Rp juta)"),
             xaxis = list(title = "Kelompok tahun"))
  })
  
  output$ov_quality <- renderTable({
    d <- dat()
    data.frame(
      Variabel = lab(num_vars),
      Missing  = sapply(num_vars, function(v) sum(is.na(d[[v]]))),
      Outlier  = sapply(num_vars, function(v) length(boxplot.stats(d[[v]])$out)),
      row.names = NULL
    )
  }, striped = FALSE, width = "100%")
  
  # =========================================================
  # UNIVARIAT - KATEGORIK
  # =========================================================
  uk_freq <- reactive({
    d <- dat(); v <- input$uk_var
    t <- table(d[[v]])
    out <- data.frame(k = names(t), n = as.integer(t), stringsAsFactors = FALSE)
    out$k <- factor(out$k, levels = levels(d[[v]]))
    out <- out[out$n > 0, ]
    out$pct <- 100 * out$n / sum(out$n)
    out
  })
  
  output$uk_plot <- renderPlotly({
    t <- uk_freq(); cl <- rep_len(cols, nrow(t))
    if (input$uk_type == "Bar chart") {
      plot_ly(t, x = ~k, y = ~n, type = "bar", text = ~n, textposition = "outside",
              marker = list(color = cl)) %>%
        style_plotly() %>%
        layout(xaxis = list(title = lab(input$uk_var)),
               yaxis = list(title = "Frekuensi"))
    } else if (input$uk_type == "Pie chart") {
      plot_ly(t, labels = ~k, values = ~n, type = "pie",
              marker = list(colors = cl, line = list(color = surface, width = 3)),
              textinfo = "label+percent") %>%
        base_plotly()
    } else {
      tp <- t[order(-t$n), ]
      tp$kum <- cumsum(tp$pct)
      kat <- as.character(tp$k)
      plot_ly() %>%
        add_trace(x = kat, y = tp$n, type = "bar", name = "Frekuensi",
                  marker = list(color = violet)) %>%
        add_trace(x = kat, y = tp$kum, type = "scatter", mode = "lines+markers",
                  name = "% kumulatif", yaxis = "y2",
                  line = list(color = amber, width = 3),
                  marker = list(color = amber, size = 9)) %>%
        style_plotly() %>%
        layout(xaxis = list(title = lab(input$uk_var), categoryorder = "array",
                            categoryarray = kat),
               yaxis = list(title = "Frekuensi"),
               yaxis2 = list(overlaying = "y", side = "right", range = c(0, 105),
                             title = "% kumulatif", showgrid = FALSE),
               legend = list(orientation = "h", y = -0.2))
    }
  })
  
  output$uk_tbl <- renderTable({
    t <- uk_freq(); t <- t[order(-t$n), ]
    data.frame(Kategori = as.character(t$k),
               Frekuensi = t$n,
               `Persen (%)` = fmt(t$pct, 1),
               `Kumulatif (%)` = fmt(cumsum(t$pct), 1),
               check.names = FALSE)
  }, width = "100%")
  
  output$uk_interp <- renderUI({
    t <- uk_freq(); i <- which.max(t$n)
    div(class = "interp",
        paste0("Kategori dominan (modus) pada ", lab(input$uk_var), " adalah \"",
               t$k[i], "\" dengan ", t$n[i], " mobil (", fmt(t$pct[i], 1), "%)."))
  })
  
  # =========================================================
  # UNIVARIAT - NUMERIK
  # =========================================================
  xnum <- reactive({
    x <- dat()[[input$un_var]]
    validate(need(length(x) > 2, "Data terlalu sedikit."))
    x
  })
  
  output$un_plot <- renderPlotly({
    x <- xnum(); v <- input$un_var
    mn <- mean(x); md <- median(x)
    vline <- function(val, col)
      list(type = "line", x0 = val, x1 = val, y0 = 0, y1 = 1, yref = "paper",
           line = list(color = col, dash = "dash", width = 2))
    ann <- list(
      list(x = mn, y = 1, yref = "paper", text = "Mean", showarrow = FALSE,
           yanchor = "bottom", font = list(color = pink)),
      list(x = md, y = 1.07, yref = "paper", text = "Median", showarrow = FALSE,
           yanchor = "bottom", font = list(color = sky)))
    shp <- list(vline(mn, pink), vline(md, sky))
    
    if (input$un_type == "Histogram") {
      plot_ly(x = x, type = "histogram", nbinsx = input$un_bins,
              marker = list(color = mint, line = list(color = bg, width = 1))) %>%
        style_plotly() %>%
        layout(shapes = shp, annotations = ann, showlegend = FALSE,
               xaxis = list(title = lab(v)), yaxis = list(title = "Frekuensi"),
               margin = list(t = 50))
    } else if (input$un_type == "Density plot") {
      validate(need(length(unique(x)) > 1, "Nilai variabel konstan."))
      dn <- density(x)
      plot_ly(x = dn$x, y = dn$y, type = "scatter", mode = "lines",
              fill = "tozeroy", line = list(color = mint, width = 3),
              fillcolor = "rgba(52,224,181,.25)") %>%
        style_plotly() %>%
        layout(shapes = shp, annotations = ann, showlegend = FALSE,
               xaxis = list(title = lab(v)), yaxis = list(title = "Densitas"),
               margin = list(t = 50))
    } else {
      plot_ly(x = x, type = "box", name = "", boxpoints = "outliers",
              marker = list(color = pink), line = list(color = mint),
              fillcolor = "rgba(52,224,181,.20)") %>%
        style_plotly() %>%
        layout(showlegend = FALSE, xaxis = list(title = lab(v)))
    }
  })
  
  output$un_stem <- renderPrint({
    x <- dat()[[input$un_var]]
    req(input$un_n)
    set.seed(1)
    xs <- if (length(x) > input$un_n) sample(x, input$un_n) else x
    cat("Stem-and-leaf (", length(xs), " data sampel): ", lab(input$un_var), "\n", sep = "")
    stem(xs)
  })
  
  output$un_tbl <- renderTable(ringkas_num(xnum()), width = "100%")
  
  output$un_interp <- renderUI({
    x <- xnum(); sk <- skew(x); ku <- kurt(x)
    validate(need(!is.na(sk), "Nilai variabel konstan."))
    bentuk <- if (abs(sk) < 0.5) "cenderung simetris"
    else if (sk > 0) "miring ke kanan (right-skewed)"
    else "miring ke kiri (left-skewed)"
    pk <- if (length(unique(x)) > 1) {
      d <- density(x, adjust = 2); sum(diff(sign(diff(d$y))) == -2)
    } else 1
    modal <- if (pk <= 1) "unimodal (satu puncak)" else paste0("multimodal (", pk, " puncak)")
    f <- fence(x); n_out <- sum(x < f[1] | x > f[2])
    div(class = "interp",
        paste0("Distribusi ", bentuk, " (skewness = ", fmt(sk, 2), "), ", modal,
               ". Mean ", if (mean(x) > median(x)) "lebih besar" else "tidak lebih besar",
               " dari median. Kurtosis ", fmt(ku, 2),
               if (ku > 3) " (leptokurtik)." else " (platikurtik).",
               " Ada ", n_out, " outlier menurut aturan 1,5 x IQR."))
  })
  
  output$un_out_plot <- renderPlotly({
    x <- xnum(); f <- fence(x)
    set.seed(1)
    out <- x < f[1] | x > f[2]
    yj <- runif(length(x), -0.3, 0.3)
    shp <- lapply(f, function(val)
      list(type = "line", x0 = val, x1 = val, y0 = 0, y1 = 1, yref = "paper",
           line = list(color = amber, dash = "dash", width = 2)))
    p <- plot_ly() %>%
      add_trace(x = x[!out], y = yj[!out], type = "scatter", mode = "markers",
                name = "Normal",
                marker = list(color = mint, size = 8, opacity = .75)) %>%
      add_trace(x = x[out], y = yj[out], type = "scatter", mode = "markers",
                name = "Outlier",
                marker = list(color = pink, size = 10, opacity = .95))
    style_plotly(p) %>%
      layout(shapes = unname(shp),
             xaxis = list(title = paste0(lab(input$un_var),
                                         "  (garis putus = batas bawah & atas)")),
             yaxis = list(title = "", showticklabels = FALSE, range = c(-1, 1)),
             legend = list(orientation = "h", y = -0.3))
  })
  
  output$un_out_tbl <- renderTable({
    x <- xnum(); q <- quantile(x, c(.25, .75), names = FALSE); f <- fence(x)
    n_out <- sum(x < f[1] | x > f[2])
    data.frame(
      Ukuran = c("Q1", "Q3", "IQR", "Batas bawah (Q1 - 1,5 IQR)",
                 "Batas atas (Q3 + 1,5 IQR)", "Jumlah outlier", "Persentase outlier"),
      Nilai = c(fmt(q[1], 2), fmt(q[2], 2), fmt(q[2] - q[1], 2), fmt(f[1], 2),
                fmt(f[2], 2), fmt(n_out), paste0(fmt(100 * n_out / length(x), 1), "%")))
  }, width = "100%")
  
  output$un_dampak <- renderTable({
    x <- xnum(); f <- fence(x); xt <- x[x >= f[1] & x <= f[2]]
    validate(need(length(xt) > 2, ""))
    ukur <- function(v) c(mean(v), median(v), sd(v), min(v), max(v))
    data.frame(Ukuran = c("Rata-rata", "Median", "Std. deviasi", "Minimum", "Maksimum"),
               `Tanpa outlier` = fmt(ukur(xt), 2),
               `Dengan outlier` = fmt(ukur(x), 2),
               check.names = FALSE)
  }, width = "100%")
  
  # =========================================================
  # BIVARIAT - KATEGORIK vs KATEGORIK
  # =========================================================
  kk_tb <- reactive({
    d <- dat(); a <- input$kk_a; b <- input$kk_b
    validate(need(a != b, "Pilih dua variabel kategorik yang berbeda."))
    tb <- table(droplevels(d[[a]]), droplevels(d[[b]]))
    validate(need(nrow(tb) >= 2 && ncol(tb) >= 2,
                  "Setiap variabel butuh minimal 2 kategori pada data terfilter."))
    tb
  })
  
  output$kk_plot <- renderPlotly({
    tb <- kk_tb()
    tl <- as.data.frame(tb); names(tl) <- c("A", "B", "n")
    tl$pct <- ave(tl$n, tl$A, FUN = function(z) if (sum(z) == 0) 0 else 100 * z / sum(z))
    gc <- group_colors(levels(tl$B))
    seg <- input$kk_type == "Segmented bar chart (persen)"
    p <- plot_ly()
    for (lv in levels(tl$B)) {
      s <- tl[tl$B == lv, ]
      p <- add_trace(p, x = s$A, y = if (seg) s$pct else s$n, type = "bar", name = lv,
                     text = if (seg) paste0(round(s$pct), "%") else s$n,
                     textposition = "auto",
                     marker = list(color = gc[[lv]]))
    }
    style_plotly(p) %>%
      layout(barmode = if (input$kk_type == "Grouped bar chart") "group" else "stack",
             xaxis = list(title = lab(input$kk_a)),
             yaxis = if (seg) list(title = "Persentase (%)", range = c(0, 100))
             else list(title = "Frekuensi"),
             legend = list(title = list(text = lab(input$kk_b))))
  })
  
  output$kk_tab <- renderTable({
    m <- as.data.frame.matrix(kk_tb())
    cbind(Kategori = rownames(m), m)
  }, width = "100%")
  
  output$kk_prop <- renderTable({
    m <- as.data.frame.matrix(round(100 * prop.table(kk_tb(), 1), 1))
    cbind(Kategori = rownames(m), m)
  }, width = "100%")
  
  kk_chi <- reactive({
    set.seed(2026)
    suppressWarnings(chisq.test(kk_tb(), simulate.p.value = TRUE, B = 5000))
  })
  
  output$kk_chi_txt <- renderPrint({
    tb <- kk_tb(); res <- kk_chi()
    print(res)
    k <- min(dim(tb)) - 1
    cat("Cramer's V:", round(sqrt(unname(res$statistic) / (sum(tb) * k)), 4), "\n")
  })
  
  output$kk_interp <- renderUI({
    res <- kk_chi()
    pv <- if (res$p.value < .001) "< 0,001" else fmt(res$p.value, 3)
    div(class = "interp",
        if (res$p.value < 0.05)
          paste0("p-value ", pv, " < 0,05: ada hubungan yang signifikan antara ",
                 lab(input$kk_a), " dan ", lab(input$kk_b), ".")
        else
          paste0("p-value ", pv, " >= 0,05: tidak ada bukti hubungan yang signifikan antara ",
                 lab(input$kk_a), " dan ", lab(input$kk_b), "."),
        " (p-value simulasi Monte Carlo agar aman untuk sel yang jarang.)")
  })
  
  # =========================================================
  # BIVARIAT - KUANTITATIF vs KUANTITATIF
  # =========================================================
  cor_mat <- reactive({
    v <- input$cor_vars
    validate(need(length(v) >= 2, "Pilih minimal 2 variabel."))
    d <- dat(); validate(need(nrow(d) > 3, "Data terlalu sedikit."))
    X <- d[v]
    validate(need(all(sapply(X, sd) > 0), "Ada variabel yang konstan."))
    cor(X, method = input$cor_method)
  })
  
  line_data <- function(d, x, y, g) {
    xs <- d[[x]]
    bin <- if (length(unique(xs)) > 25) {
      br <- unique(quantile(xs, seq(0, 1, length.out = 16), names = FALSE))
      cut(xs, breaks = br, include.lowest = TRUE)
    } else factor(xs)
    data.frame(g = g, bin = bin, xs = xs, ys = d[[y]]) %>%
      group_by(g, bin) %>%
      summarise(X = mean(xs), Y = mean(ys), .groups = "drop") %>%
      arrange(g, X)
  }
  
  output$kn_plot <- renderPlotly({
    tp <- input$kn_type
    
    # ---- Plot korelasi (heatmap) ----
    if (tp == "Plot korelasi") {
      cm <- cor_mat(); nm <- lab(colnames(cm)); n <- length(nm)
      ann <- lapply(seq_len(n * n), function(k) {
        i <- (k - 1) %% n + 1; j <- (k - 1) %/% n + 1
        list(x = nm[i], y = nm[j], text = sprintf("%.2f", cm[j, i]),
             showarrow = FALSE, font = list(color = ink, size = 13))
      })
      return(
        plot_ly(x = nm, y = nm, z = cm, type = "heatmap", zmin = -1, zmax = 1,
                xgap = 3, ygap = 3,
                colorscale = list(list(0, pink), list(0.5, surface), list(1, mint)),
                colorbar = list(title = "r", tickfont = list(color = ink))) %>%
          base_plotly() %>%
          layout(annotations = ann,
                 xaxis = list(title = "", tickangle = -30),
                 yaxis = list(title = "", autorange = "reversed"),
                 margin = list(l = 140, b = 110))
      )
    }
    
    d <- dat(); x <- input$kn_x; y <- input$kn_y
    validate(need(x != y, "Pilih dua variabel yang berbeda."))
    g <- grp(d, input$kn_color); gc <- group_colors(levels(g))
    
    # ---- Line plot ----
    if (grepl("^Line plot", tp)) {
      ld <- line_data(d, x, y, g)
      p <- plot_ly()
      for (lv in levels(g)) {
        s <- ld[ld$g == lv, ]
        if (nrow(s) == 0) next
        p <- add_trace(p, x = s$X, y = s$Y, type = "scatter", mode = "lines+markers",
                       name = lv,
                       line = list(color = if (input$kn_color == "none") mint else gc[[lv]],
                                   width = 3),
                       marker = list(size = 9,
                                     color = if (input$kn_color == "none") mint else gc[[lv]]))
      }
      return(style_plotly(p) %>%
               layout(xaxis = list(title = lab(x)),
                      yaxis = list(title = paste("Rata-rata", lab(y)))))
    }
    
    # ---- Scatterplot (+ garis) ----
    p <- plot_ly()
    for (lv in levels(g)) {
      i <- g == lv
      p <- add_trace(p, x = d[[x]][i], y = d[[y]][i], type = "scatter",
                     mode = "markers", name = lv,
                     marker = list(color = if (input$kn_color == "none") mint else gc[[lv]],
                                   size = 10, opacity = .8,
                                   line = list(color = bg, width = 1)))
    }
    xs <- d[[x]]; ys <- d[[y]]; o <- order(xs)
    garis <- NULL; nama <- NULL
    if (tp == "Scatterplot + garis linear") {
      validate(need(length(unique(xs)) >= 2, "Variabel X konstan."))
      garis <- fitted(lm(ys ~ xs)); nama <- "Garis linear"
    } else if (tp == "Scatterplot + garis kuadratik") {
      validate(need(length(unique(xs)) >= 3, "Butuh minimal 3 nilai X berbeda."))
      garis <- fitted(lm(ys ~ poly(xs, 2))); nama <- "Garis kuadratik"
    } else if (tp == "Scatterplot + garis nonparametrik (LOESS)") {
      validate(need(length(unique(xs)) >= 5, "Butuh minimal 5 nilai X berbeda."))
      fit <- tryCatch(suppressWarnings(loess(ys ~ xs)), error = function(e) NULL)
      validate(need(!is.null(fit), "LOESS gagal dihitung pada data ini."))
      garis <- fitted(fit); nama <- "Garis LOESS"
    }
    if (!is.null(garis)) {
      p <- add_trace(p, x = xs[o], y = garis[o], type = "scatter", mode = "lines",
                     name = nama, line = list(color = amber, dash = "dash", width = 3))
    }
    style_plotly(p) %>% layout(xaxis = list(title = lab(x)),
                               yaxis = list(title = lab(y)))
  })
  
  output$kn_side <- renderUI({
    tp <- input$kn_type
    if (tp == "Plot korelasi") {
      return(tagList(h6("Pasangan terkuat", class = "section-title"),
                     tableOutput("kn_top"),
                     p(class = "note", "Mint = korelasi positif, merah muda = negatif."),
                     p(class = "note",
                       "Perhatikan: Harga dan Angsuran berkorelasi ~1,00 (angsuran turunan harga), ",
                       "sehingga tidak dipakai bersamaan dalam regresi.")))
    }
    d <- dat(); x <- d[[input$kn_x]]; y <- d[[input$kn_y]]
    validate(need(input$kn_x != input$kn_y, "Pilih dua variabel yang berbeda."),
             need(length(x) > 3, "Data terlalu sedikit."),
             need(sd(x) > 0 && sd(y) > 0, "Salah satu variabel konstan."))
    if (grepl("^Line plot", tp)) {
      return(p(class = "note",
               "Line plot menampilkan rata-rata Y pada setiap nilai X (X yang kontinu dikelompokkan menjadi 15 interval)."))
    }
    pe <- cor.test(x, y, method = "pearson")
    sp <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
    pv <- function(p) if (p < .001) "< 0,001" else fmt(p, 3)
    r2l <- summary(lm(y ~ x))$r.squared
    r2q <- if (length(unique(x)) >= 3) summary(lm(y ~ poly(x, 2)))$r.squared else NA
    tagList(
      kpi("Pearson r",  fmt(pe$estimate, 3), mint), br(),
      kpi("Spearman rho", fmt(sp$estimate, 3), violet), br(),
      p(class = "note",
        sprintf("Hubungan %s (%s). p-value Pearson = %s, Spearman = %s.",
                kekuatan(pe$estimate),
                if (pe$estimate >= 0) "positif" else "negatif",
                pv(pe$p.value), pv(sp$p.value))),
      p(class = "note",
        sprintf("R² model linear = %s; R² model kuadratik = %s.",
                fmt(r2l, 3), if (is.na(r2q)) "-" else fmt(r2q, 3)))
    )
  })
  
  output$kn_top <- renderTable({
    cm <- cor_mat(); ix <- which(upper.tri(cm), arr.ind = TRUE)
    out <- data.frame(Pasangan = paste(colnames(cm)[ix[, 1]], "-", colnames(cm)[ix[, 2]]),
                      r = cm[ix])
    out <- out[order(-abs(out$r)), ][seq_len(min(5, nrow(out))), ]
    out$r <- fmt(out$r, 2); out
  }, width = "100%")
  
  # =========================================================
  # BIVARIAT - KUANTITATIF vs KATEGORIK
  # =========================================================
  output$kq_plot <- renderPlotly({
    d <- dat(); v <- input$kq_y; y <- d[[v]]
    xg <- droplevels(d[[input$kq_x]]); lv <- levels(xg)
    gc <- group_colors(lv); tp <- input$kq_type
    ttl <- list(title = lab(v))
    
    if (tp == "Bar chart (rata-rata)") {
      m <- tapply(y, xg, mean)
      plot_ly(x = names(m), y = as.numeric(m), type = "bar",
              text = fmt(as.numeric(m), 1), textposition = "outside",
              marker = list(color = unname(gc[names(m)]))) %>%
        style_plotly() %>%
        layout(xaxis = list(title = lab(input$kq_x), categoryorder = "array",
                            categoryarray = lv),
               yaxis = list(title = paste("Rata-rata", lab(v))))
      
    } else if (tp == "Grouped kernel density plot") {
      p <- plot_ly()
      for (l in lv) {
        z <- y[xg == l]
        if (length(z) < 2 || length(unique(z)) < 2) next
        dn <- density(z, from = min(y), to = max(y))
        p <- add_trace(p, x = dn$x, y = dn$y, type = "scatter", mode = "lines",
                       fill = "tozeroy", name = l, line = list(color = gc[[l]], width = 2),
                       fillcolor = rgba(gc[[l]], .3))
      }
      style_plotly(p) %>%
        layout(xaxis = list(title = lab(v)), yaxis = list(title = "Densitas"),
               legend = list(orientation = "h", y = -0.2))
      
    } else if (tp == "Box plot") {
      p <- plot_ly()
      for (l in lv)
        p <- add_trace(p, y = y[xg == l], type = "box", name = l,
                       boxpoints = "outliers", marker = list(color = gc[[l]]),
                       line = list(color = gc[[l]]), fillcolor = rgba(gc[[l]], .25))
      style_plotly(p) %>% layout(showlegend = FALSE, yaxis = ttl,
                                 xaxis = list(title = lab(input$kq_x)))
      
    } else if (tp == "Violin plot") {
      p <- plot_ly()
      for (l in lv)
        p <- add_trace(p, y = y[xg == l], type = "violin", name = l,
                       line = list(color = gc[[l]]), fillcolor = rgba(gc[[l]], .35))
      style_plotly(p) %>% layout(showlegend = FALSE, yaxis = ttl,
                                 xaxis = list(title = lab(input$kq_x)))
      
    } else if (tp == "Box plot + Violin plot") {
      p <- plot_ly()
      for (l in lv)
        p <- add_trace(p, y = y[xg == l], type = "violin", name = l,
                       box = list(visible = TRUE, fillcolor = rgba(amber, .9),
                                  line = list(color = ink)),
                       meanline = list(visible = FALSE),
                       line = list(color = gc[[l]]), fillcolor = rgba(gc[[l]], .4))
      style_plotly(p) %>% layout(showlegend = FALSE, yaxis = ttl,
                                 xaxis = list(title = lab(input$kq_x)))
      
    } else if (tp == "Ridgeline plot") {
      rg <- range(y); k <- length(lv)
      validate(need(diff(rg) > 0, "Nilai variabel konstan."))
      dens <- lapply(lv, function(l) {
        z <- y[xg == l]
        if (length(z) > 1 && length(unique(z)) > 1) density(z, from = rg[1], to = rg[2]) else NULL
      })
      mx <- max(sapply(dens, function(z) if (is.null(z)) 0 else max(z$y)))
      p <- plot_ly()
      for (i in rev(seq_len(k))) {
        dn <- dens[[i]]; if (is.null(dn)) next
        yy <- i + 1.6 * dn$y / mx
        p <- add_trace(p, x = c(dn$x, rev(dn$x)), y = c(yy, rep(i, length(yy))),
                       type = "scatter", mode = "lines", fill = "toself", name = lv[i],
                       line = list(color = gc[[lv[i]]], width = 2),
                       fillcolor = rgba(gc[[lv[i]]], .55), hoverinfo = "name")
      }
      style_plotly(p) %>%
        layout(showlegend = FALSE, xaxis = list(title = lab(v)),
               yaxis = list(title = "", tickmode = "array", tickvals = seq_len(k),
                            ticktext = lv, range = c(0.5, k + 2)))
      
    } else if (tp == "Mean/SEM plot") {
      gname <- input$kq_g
      validate(need(gname == "none" || gname != input$kq_x,
                    "Pilih kelompok kedua yang berbeda dari kelompok X."))
      gg <- if (gname == "none") factor(rep("Semua", nrow(d))) else droplevels(d[[gname]])
      gcc <- group_colors(levels(gg))
      m <- data.frame(g = gg, x = xg, y = y) %>%
        group_by(g, x) %>%
        summarise(rata = mean(y), se = sd(y) / sqrt(n()), .groups = "drop") %>%
        mutate(se = ifelse(is.na(se), 0, se))
      p <- plot_ly()
      for (l in levels(gg)) {
        s <- m[m$g == l, ]
        p <- add_trace(p, x = as.character(s$x), y = s$rata, type = "scatter",
                       mode = "lines+markers", name = l,
                       line = list(color = gcc[[l]], width = 3),
                       marker = list(color = gcc[[l]], size = 9),
                       error_y = list(type = "data", array = s$se, color = gcc[[l]],
                                      thickness = 2, width = 5))
      }
      style_plotly(p) %>%
        layout(xaxis = list(title = lab(input$kq_x), categoryorder = "array",
                            categoryarray = lv),
               yaxis = list(title = paste("Rata-rata", lab(v), "± SEM")),
               legend = list(orientation = "h", y = -0.2))
      
    } else if (tp == "Strip plot") {
      set.seed(1)
      p <- plot_ly()
      for (i in seq_along(lv)) {
        z <- y[xg == lv[i]]
        p <- add_trace(p, x = z, y = i + runif(length(z), -0.18, 0.18),
                       type = "scatter", mode = "markers", name = lv[i],
                       marker = list(color = gc[[lv[i]]], size = 8, opacity = .7))
      }
      style_plotly(p) %>%
        layout(showlegend = FALSE, xaxis = list(title = lab(v)),
               yaxis = list(title = "", tickmode = "array", tickvals = seq_along(lv),
                            ticktext = lv, range = c(0.4, length(lv) + 0.6)))
      
    } else if (tp == "Cleveland dot chart") {
      m <- sort(tapply(y, xg, mean))
      awal <- min(m) - 0.05 * max(diff(range(m)), 1e-9)
      plot_ly(x = as.numeric(m), y = names(m), type = "scatter", mode = "markers",
              marker = list(color = sky, size = 13),
              error_x = list(type = "data", symmetric = FALSE, array = rep(0, length(m)),
                             arrayminus = as.numeric(m) - awal, color = muted,
                             thickness = 1, width = 0)) %>%
        style_plotly() %>%
        layout(xaxis = list(title = paste("Rata-rata", lab(v))),
               yaxis = list(title = "", categoryorder = "array",
                            categoryarray = names(m)))
      
    } else {   # Box plot + titik data (jitter)
      p <- plot_ly()
      for (l in lv)
        p <- add_trace(p, x = y[xg == l], y = rep(l, sum(xg == l)), type = "box",
                       orientation = "h", name = l, boxpoints = "all", jitter = .5,
                       pointpos = 0, fillcolor = "rgba(0,0,0,0)",
                       marker = list(color = gc[[l]], opacity = .6),
                       line = list(color = gc[[l]], width = 2))
      style_plotly(p) %>%
        layout(showlegend = FALSE, xaxis = list(title = lab(v)),
               yaxis = list(title = ""))
    }
  })
  
  output$kq_tbl <- renderTable({
    d <- dat(); gname <- input$kq_g
    pakai_g <- gname != "none" && gname != input$kq_x
    t <- data.frame(x = droplevels(d[[input$kq_x]]), y = d[[input$kq_y]])
    if (pakai_g) t$g <- droplevels(d[[gname]])
    grp_cols <- if (pakai_g) c("x", "g") else "x"
    out <- t %>% group_by(across(all_of(grp_cols))) %>%
      summarise(N = n(), Mean = mean(y), Median = median(y), SD = sd(y),
                SE = sd(y) / sqrt(n()), Min = min(y), Max = max(y), .groups = "drop")
    out$x <- as.character(out$x)
    names(out)[names(out) == "x"] <- lab(input$kq_x)
    if (pakai_g) {
      out$g <- as.character(out$g)
      names(out)[names(out) == "g"] <- lab(gname)
    }
    out %>% mutate(across(c(Mean, Median, SD, SE, Min, Max), ~ round(.x, 2)))
  }, width = "100%")
  
  output$kq_interp <- renderUI({
    d <- dat(); xg <- droplevels(d[[input$kq_x]])
    m <- tapply(d[[input$kq_y]], xg, mean)
    div(class = "interp",
        paste0("Rata-rata ", lab(input$kq_y), " tertinggi pada kelompok \"",
               names(m)[which.max(m)], "\" (", fmt(max(m), 2),
               ") dan terendah pada \"", names(m)[which.min(m)], "\" (",
               fmt(min(m), 2), ")."))
  })
  
  # =========================================================
  # MULTIVARIAT
  # =========================================================
  output$mv_plot <- renderPlotly({
    d <- dat(); tp <- input$mv_type
    validate(need(nrow(d) > 5, "Data terlalu sedikit."))
    
    # ---- Bubble chart: X, Y, ukuran, warna ----
    if (tp == "Bubble chart (4 variabel)") {
      x <- input$mv_x; y <- input$mv_y; s <- input$mv_size
      validate(need(x != y, "Pilih sumbu X dan Y yang berbeda."))
      g <- droplevels(d[[input$mv_col]]); gc <- group_colors(levels(g))
      sv <- d[[s]]; rg <- range(sv)
      sz <- if (diff(rg) == 0) rep(12, length(sv)) else 6 + 22 * (sv - rg[1]) / diff(rg)
      p <- plot_ly()
      for (lv in levels(g)) {
        i <- g == lv
        p <- add_trace(p, x = d[[x]][i], y = d[[y]][i], type = "scatter",
                       mode = "markers", name = lv,
                       text = paste0(d$NAMA[i], " (", d$TAHUN[i], ")<br>",
                                     lab(s), ": ", fmt(sv[i], 1)),
                       hoverinfo = "text",
                       marker = list(color = gc[[lv]], size = sz[i],
                                     sizemode = "diameter", opacity = .65,
                                     line = list(color = bg, width = 1)))
      }
      return(style_plotly(p) %>%
               layout(xaxis = list(title = lab(x)), yaxis = list(title = lab(y)),
                      legend = list(title = list(text = lab(input$mv_col)))))
    }
    
    # ---- Heatmap rata-rata: baris x kolom -> nilai ----
    if (tp == "Heatmap rata-rata (3 variabel)") {
      validate(need(input$mv_hr != input$mv_hc, "Pilih baris dan kolom yang berbeda."))
      a <- droplevels(d[[input$mv_hr]]); b <- droplevels(d[[input$mv_hc]])
      v <- d[[input$mv_hv]]
      m  <- tapply(v, list(a, b), mean)
      nn <- tapply(v, list(a, b), length)
      ann <- list()
      for (i in seq_len(nrow(m))) for (j in seq_len(ncol(m)))
        if (!is.na(m[i, j]))
          ann[[length(ann) + 1]] <- list(
            x = colnames(m)[j], y = rownames(m)[i],
            text = paste0(fmt(m[i, j], 1), "<br>n=", nn[i, j]),
            showarrow = FALSE, font = list(color = ink, size = 11))
      return(
        plot_ly(x = colnames(m), y = rownames(m), z = m, type = "heatmap",
                xgap = 3, ygap = 3,
                colorscale = list(list(0, "#1b2345"), list(0.5, violet), list(1, mint)),
                colorbar = list(title = "Rata-rata", tickfont = list(color = ink))) %>%
          base_plotly() %>%
          layout(annotations = ann,
                 xaxis = list(title = lab(input$mv_hc), categoryorder = "array",
                              categoryarray = colnames(m)),
                 yaxis = list(title = lab(input$mv_hr), autorange = "reversed",
                              categoryorder = "array", categoryarray = rownames(m)),
                 margin = list(l = 110, b = 70))
      )
    }
    
    # ---- Parallel coordinates ----
    if (tp == "Parallel coordinates") {
      vs <- input$mv_vars
      validate(need(length(vs) >= 3, "Pilih minimal 3 variabel."))
      dims <- lapply(vs, function(v) list(label = lab(v), values = d[[v]]))
      return(
        plot_ly(type = "parcoords",
                line = list(color = d$HARGA_JT, showscale = TRUE,
                            colorscale = list(c(0, pink), c(0.5, amber), c(1, mint)),
                            colorbar = list(title = "Harga",
                                            tickfont = list(color = ink))),
                dimensions = dims,
                labelfont = list(color = ink), tickfont = list(color = ink),
                rangefont = list(color = muted)) %>%
          base_plotly() %>% layout(margin = list(t = 60, l = 70, r = 60))
      )
    }
    
    # ---- Scatterplot 3D ----
    if (tp == "Scatterplot 3D") {
      validate(need(length(unique(c(input$mv_3x, input$mv_3y, input$mv_3z))) == 3,
                    "Pilih tiga variabel yang berbeda."))
      g <- droplevels(d[[input$mv_col3]]); gc <- group_colors(levels(g))
      p <- plot_ly()
      for (lv in levels(g)) {
        i <- g == lv
        p <- add_trace(p, x = d[[input$mv_3x]][i], y = d[[input$mv_3y]][i],
                       z = d[[input$mv_3z]][i], type = "scatter3d", mode = "markers",
                       name = lv,
                       marker = list(color = gc[[lv]], size = 3.5, opacity = .8))
      }
      ax <- function(v) list(title = lab(v), gridcolor = grid_col, color = ink)
      return(base_plotly(p) %>%
               layout(scene = list(xaxis = ax(input$mv_3x), yaxis = ax(input$mv_3y),
                                   zaxis = ax(input$mv_3z)),
                      legend = list(title = list(text = lab(input$mv_col3)))))
    }
    
    # ---- SPLOM ----
    vs <- input$mv_vars
    validate(need(length(vs) >= 3, "Pilih minimal 3 variabel."))
    g <- droplevels(d[[input$mv_col2]]); gc <- group_colors(levels(g))
    dims <- lapply(vs, function(v) list(label = lab(v), values = d[[v]]))
    plot_ly(type = "splom", dimensions = dims,
            text = as.character(g),
            marker = list(color = unname(gc[as.character(g)]), size = 5, opacity = .6,
                          line = list(color = bg, width = 0.5)),
            diagonal = list(visible = FALSE), showupperhalf = FALSE) %>%
      base_plotly() %>% layout(margin = list(t = 30, l = 60, b = 60))
  })
  
  output$mv_interp <- renderUI({
    d <- dat(); tp <- input$mv_type
    txt <- NULL; extra <- NULL
    
    if (tp == "Bubble chart (4 variabel)") {
      validate(need(input$mv_x != input$mv_y, "Pilih sumbu X dan Y yang berbeda."))
      g <- droplevels(d[[input$mv_col]]); x <- d[[input$mv_x]]; y <- d[[input$mv_y]]
      r <- sapply(levels(g), function(l) {
        i <- g == l
        if (sum(i) > 3 && sd(x[i]) > 0 && sd(y[i]) > 0) cor(x[i], y[i]) else NA_real_
      })
      txt <- paste0("Korelasi ", lab(input$mv_x), " dengan ", lab(input$mv_y),
                    " per kelompok ", lab(input$mv_col), ": ",
                    paste0(names(r), " (r = ", fmt(r, 2), ")", collapse = "; "),
                    ". Ukuran gelembung menunjukkan ", lab(input$mv_size),
                    ". Bandingkan posisi tiap warna: kelompok yang berada lebih tinggi pada ",
                    "sumbu Y memiliki nilai ", lab(input$mv_y), " yang lebih besar pada X yang sama.")
    } else if (tp == "Heatmap rata-rata (3 variabel)") {
      validate(need(input$mv_hr != input$mv_hc, "Pilih baris dan kolom yang berbeda."))
      a <- droplevels(d[[input$mv_hr]]); b <- droplevels(d[[input$mv_hc]])
      m <- tapply(d[[input$mv_hv]], list(a, b), mean)
      imx <- which(m == max(m, na.rm = TRUE), arr.ind = TRUE)[1, ]
      imn <- which(m == min(m, na.rm = TRUE), arr.ind = TRUE)[1, ]
      txt <- paste0("Rata-rata ", lab(input$mv_hv), " tertinggi pada kombinasi ",
                    rownames(m)[imx[1]], " x ", colnames(m)[imx[2]], " (",
                    fmt(max(m, na.rm = TRUE), 1), ") dan terendah pada ",
                    rownames(m)[imn[1]], " x ", colnames(m)[imn[2]], " (",
                    fmt(min(m, na.rm = TRUE), 1), "). Sel kosong = tidak ada mobil ",
                    "pada kombinasi itu; perhatikan n kecil karena rata-ratanya kurang stabil.")
    } else {
      vs <- if (tp == "Scatterplot 3D") c(input$mv_3x, input$mv_3y, input$mv_3z) else input$mv_vars
      validate(need(length(vs) >= 3, "Pilih minimal 3 variabel."))
      cm <- cor(d[vs], method = "spearman")
      ix <- which(upper.tri(cm), arr.ind = TRUE)
      k <- ix[which.max(abs(cm[ix])), ]
      txt <- paste0("Pasangan variabel dengan hubungan terkuat (Spearman): ",
                    lab(colnames(cm)[k[1]]), " dan ", lab(colnames(cm)[k[2]]),
                    " (rho = ", fmt(cm[k[1], k[2]], 2), ", ", kekuatan(cm[k[1], k[2]]), "). ",
                    if (tp == "Parallel coordinates")
                      "Pada parallel coordinates, garis hijau = harga tinggi dan merah muda = harga rendah; garis yang berkelompok menandakan profil mobil yang serupa. Seret pada sumbu untuk menyaring rentang nilai."
                    else if (tp == "Scatterplot 3D")
                      "Putar grafik untuk melihat apakah kelompok warna terpisah dalam ruang tiga dimensi."
                    else "Setiap panel menunjukkan satu pasang variabel; warna membedakan kelompok.")
    }
    div(class = "interp", txt)
  })
  
  # =========================================================
  # REGRESI LINEAR BERGANDA
  # =========================================================
  reg_fit <- reactive({
    d <- droplevels(dat()); pr <- input$rg_pred
    validate(need(length(pr) >= 2, "Pilih minimal 2 prediktor."))
    ok <- sapply(pr, function(v)
      if (is.factor(d[[v]])) nlevels(d[[v]]) >= 2 else sd(d[[v]]) > 0)
    validate(need(all(ok), paste0("Ada prediktor yang konstan pada data terfilter: ",
                                  paste(lab(pr[!ok]), collapse = ", "),
                                  ". Longgarkan filter atau lepas prediktor itu.")))
    validate(need(nrow(d) > length(pr) + 15, "Data terlalu sedikit untuk model ini."))
    y <- if (isTRUE(input$rg_log)) "log(HARGA_JT)" else "HARGA_JT"
    lm(as.formula(paste(y, "~", paste(pr, collapse = " + "))), data = d)
  })
  
  output$rg_eq <- renderUI({
    fit <- reg_fit(); b <- coef(fit); pr <- input$rg_pred
    yname <- if (isTRUE(input$rg_log)) "ln(Harga)" else "Harga"
    suku <- sapply(seq_along(b)[-1], function(i)
      paste0(if (b[i] >= 0) " + " else " - ", fmt(abs(b[i]), 4), "·",
             nice_term(names(b)[i], pr, short = TRUE)))
    div(class = "eq", paste0(yname, " = ", fmt(b[1], 4), paste(suku, collapse = "")))
  })
  
  output$rg_coef <- renderTable({
    fit <- reg_fit(); pr <- input$rg_pred
    co <- summary(fit)$coefficients; ci <- confint(fit)
    vf <- vif_cols(fit)[rownames(co)]
    star <- ifelse(co[, 4] < .001, "***", ifelse(co[, 4] < .01, "**",
                                                 ifelse(co[, 4] < .05, "*",
                                                        ifelse(co[, 4] < .1, ".", ""))))
    data.frame(
      Prediktor = sapply(rownames(co), nice_term, preds = pr),
      Koefisien = fmt(co[, 1], 4),
      `Std. Error` = fmt(co[, 2], 4),
      t = fmt(co[, 3], 2),
      `p-value` = pfmt(co[, 4]),
      Sig. = star,
      `CI 2,5%` = fmt(ci[, 1], 4),
      `CI 97,5%` = fmt(ci[, 2], 4),
      VIF = ifelse(is.na(vf), "-", fmt(vf, 2)),
      check.names = FALSE, row.names = NULL
    )
  }, width = "100%")
  
  output$rg_stat <- renderTable({
    fit <- reg_fit(); sm <- summary(fit); d <- droplevels(dat())
    fs <- sm$fstatistic
    pF <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    yobs <- d$HARGA_JT
    yhat <- if (isTRUE(input$rg_log)) exp(fitted(fit)) else fitted(fit)
    rmse <- sqrt(mean((yobs - yhat)^2)); mae <- mean(abs(yobs - yhat))
    mape <- 100 * mean(abs(yobs - yhat) / yobs)
    bp <- bp_test(fit)
    sh <- tryCatch(shapiro.test(resid(fit))$p.value, error = function(e) NA_real_)
    data.frame(
      Ukuran = c("Jumlah observasi", "R²", "R² adjusted",
                 "Residual std. error (skala model)",
                 "Uji F simultan (F)", "p-value uji F",
                 "RMSE (Rp juta, skala harga)", "MAE (Rp juta)", "MAPE (%)",
                 "AIC", "BIC",
                 "Breusch-Pagan (homoskedastisitas) p", "Shapiro-Wilk residual p",
                 "VIF maksimum"),
      Nilai = c(fmt(nobs(fit)), fmt(sm$r.squared, 4), fmt(sm$adj.r.squared, 4),
                fmt(sm$sigma, 4), fmt(fs[1], 2), pfmt(pF),
                fmt(rmse, 2), fmt(mae, 2), fmt(mape, 1),
                fmt(AIC(fit), 1), fmt(BIC(fit), 1),
                pfmt(bp["p"]), pfmt(sh),
                fmt(max(vif_cols(fit), na.rm = TRUE), 2))
    )
  }, width = "100%")
  
  output$rg_plot <- renderPlotly({
    fit <- reg_fit(); tp <- input$rg_viz; pr <- input$rg_pred
    d <- droplevels(dat())
    
    if (tp == "Koefisien + selang kepercayaan 95%") {
      co <- summary(fit)$coefficients[-1, , drop = FALSE]
      ci <- confint(fit)[-1, , drop = FALSE]
      nm <- sapply(rownames(co), nice_term, preds = pr)
      sig <- co[, 4] < .05
      o <- order(co[, 1])
      plot_ly(x = co[o, 1], y = nm[o], type = "scatter", mode = "markers",
              marker = list(size = 11, color = ifelse(sig[o], mint, muted)),
              error_x = list(type = "data", symmetric = FALSE,
                             array = ci[o, 2] - co[o, 1],
                             arrayminus = co[o, 1] - ci[o, 1],
                             color = muted, thickness = 2, width = 5)) %>%
        style_plotly() %>%
        layout(shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1,
                                  yref = "paper",
                                  line = list(color = amber, dash = "dash", width = 2))),
               xaxis = list(title = "Koefisien (selang 95%) — hijau = signifikan"),
               yaxis = list(title = "", categoryorder = "array", categoryarray = nm[o]),
               margin = list(l = 190))
      
    } else if (tp == "Kontribusi prediktor (|t|)") {
      co <- summary(fit)$coefficients[-1, , drop = FALSE]
      nm <- sapply(rownames(co), nice_term, preds = pr)
      tv <- abs(co[, 3]); o <- order(tv)
      plot_ly(x = tv[o], y = nm[o], type = "bar", orientation = "h",
              marker = list(color = ifelse(co[o, 4] < .05, mint, muted))) %>%
        style_plotly() %>%
        layout(shapes = list(list(type = "line", x0 = 1.96, x1 = 1.96, y0 = 0, y1 = 1,
                                  yref = "paper",
                                  line = list(color = amber, dash = "dash", width = 2))),
               xaxis = list(title = "|t-value| (garis putus = 1,96)"),
               yaxis = list(title = "", categoryorder = "array", categoryarray = nm[o]),
               margin = list(l = 190))
      
    } else if (tp == "Aktual vs prediksi") {
      yobs <- d$HARGA_JT
      yhat <- if (isTRUE(input$rg_log)) exp(fitted(fit)) else fitted(fit)
      rg <- range(c(yobs, yhat))
      plot_ly() %>%
        add_trace(x = yhat, y = yobs, type = "scatter", mode = "markers",
                  name = "Mobil",
                  text = paste0(d$NAMA, " (", d$TAHUN, ")"), hoverinfo = "text",
                  marker = list(color = mint, size = 8, opacity = .6,
                                line = list(color = bg, width = 1))) %>%
        add_trace(x = rg, y = rg, type = "scatter", mode = "lines",
                  name = "Garis ideal (y = x)",
                  line = list(color = amber, dash = "dash", width = 2)) %>%
        style_plotly() %>%
        layout(xaxis = list(title = "Harga prediksi (Rp juta)"),
               yaxis = list(title = "Harga aktual (Rp juta)"),
               legend = list(orientation = "h", y = -0.2))
      
    } else if (tp == "Residual vs fitted") {
      fv <- fitted(fit); rs <- rstandard(fit)
      lo <- tryCatch(loess(rs ~ fv), error = function(e) NULL)
      o <- order(fv)
      p <- plot_ly() %>%
        add_trace(x = fv, y = rs, type = "scatter", mode = "markers", name = "Residual",
                  marker = list(color = sky, size = 8, opacity = .6,
                                line = list(color = bg, width = 1)))
      if (!is.null(lo))
        p <- add_trace(p, x = fv[o], y = fitted(lo)[o], type = "scatter", mode = "lines",
                       name = "LOESS", line = list(color = amber, width = 3))
      style_plotly(p) %>%
        layout(shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                                  y0 = 0, y1 = 0,
                                  line = list(color = pink, dash = "dash", width = 2))),
               xaxis = list(title = "Nilai fitted"),
               yaxis = list(title = "Residual terstandar"),
               legend = list(orientation = "h", y = -0.2))
      
    } else if (tp == "Q-Q plot residual") {
      rs <- rstandard(fit); qq <- qqnorm(rs, plot.it = FALSE)
      qx <- quantile(rs, c(.25, .75), names = FALSE); tx <- qnorm(c(.25, .75))
      sl <- diff(qx) / diff(tx); ic <- qx[1] - sl * tx[1]
      xr <- range(qq$x)
      plot_ly() %>%
        add_trace(x = qq$x, y = qq$y, type = "scatter", mode = "markers",
                  name = "Residual",
                  marker = list(color = violet, size = 8, opacity = .7)) %>%
        add_trace(x = xr, y = ic + sl * xr, type = "scatter", mode = "lines",
                  name = "Garis normal", line = list(color = amber, dash = "dash", width = 2)) %>%
        style_plotly() %>%
        layout(xaxis = list(title = "Kuantil teoretis"),
               yaxis = list(title = "Kuantil residual terstandar"),
               legend = list(orientation = "h", y = -0.2))
      
    } else if (tp == "Histogram residual") {
      rs <- resid(fit)
      plot_ly(x = rs, type = "histogram", nbinsx = 25,
              marker = list(color = mint, line = list(color = bg, width = 1))) %>%
        style_plotly() %>%
        layout(showlegend = FALSE, xaxis = list(title = "Residual"),
               yaxis = list(title = "Frekuensi"))
      
    } else if (tp == "Efek parsial prediktor numerik") {
      numv <- pr[sapply(pr, function(v) is.numeric(d[[v]]))]
      validate(need(length(numv) > 0, "Pilih minimal satu prediktor numerik."))
      v <- if (!is.null(input$rg_eff) && input$rg_eff %in% numv) input$rg_eff else numv[1]
      grid <- seq(min(d[[v]]), max(d[[v]]), length.out = 50)
      nd <- lapply(pr, function(p)
        if (is.numeric(d[[p]])) rep(mean(d[[p]]), 50)
        else factor(rep(names(which.max(table(d[[p]]))), 50), levels = levels(d[[p]])))
      names(nd) <- pr; nd <- as.data.frame(nd); nd[[v]] <- grid
      pd <- predict(fit, nd, interval = "confidence")
      if (isTRUE(input$rg_log)) pd <- exp(pd)
      plot_ly() %>%
        add_trace(x = d[[v]], y = d$HARGA_JT, type = "scatter", mode = "markers",
                  name = "Data aktual",
                  marker = list(color = muted, size = 6, opacity = .35)) %>%
        add_trace(x = c(grid, rev(grid)), y = c(pd[, "upr"], rev(pd[, "lwr"])),
                  type = "scatter", mode = "lines", fill = "toself",
                  name = "Selang 95%", line = list(color = "rgba(0,0,0,0)"),
                  fillcolor = "rgba(52,224,181,.25)", hoverinfo = "none") %>%
        add_trace(x = grid, y = pd[, "fit"], type = "scatter", mode = "lines",
                  name = "Prediksi rata-rata", line = list(color = mint, width = 3)) %>%
        style_plotly() %>%
        layout(xaxis = list(title = lab(v)),
               yaxis = list(title = "Harga (Rp juta)"),
               legend = list(orientation = "h", y = -0.2))
      
    } else {   # Cook's distance
      cd <- cooks.distance(fit); n <- length(cd); th <- 4 / n
      plot_ly(x = seq_len(n), y = cd, type = "bar",
              marker = list(color = ifelse(cd > th, pink, mint)),
              text = paste0(d$NAMA, " (", d$TAHUN, ")"), hoverinfo = "text+y") %>%
        style_plotly() %>%
        layout(shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                                  y0 = th, y1 = th,
                                  line = list(color = amber, dash = "dash", width = 2))),
               showlegend = FALSE,
               xaxis = list(title = "Indeks observasi"),
               yaxis = list(title = "Cook's distance (garis putus = 4/n)"))
    }
  })
  
  output$rg_viz_interp <- renderUI({
    fit <- reg_fit(); sm <- summary(fit); tp <- input$rg_viz
    bp <- bp_test(fit)
    txt <- switch(tp,
      "Koefisien + selang kepercayaan 95%" =
        "Titik hijau = koefisien signifikan (selang 95% tidak memotong 0). Koefisien berbeda skala, sehingga bandingkan arah & signifikansi, bukan panjang batang.",
      "Kontribusi prediktor (|t|)" =
        "Batang yang melewati garis 1,96 signifikan pada α = 5%. Makin besar |t|, makin kuat bukti pengaruh prediktor tersebut.",
      "Aktual vs prediksi" =
        paste0("Titik yang rapat di sekitar garis y = x menunjukkan prediksi akurat. R² adjusted = ",
               fmt(sm$adj.r.squared, 3), ". Sebaran yang melebar pada harga tinggi menandakan model kurang baik untuk mobil mahal."),
      "Residual vs fitted" =
        paste0("Residual idealnya acak di sekitar nol tanpa pola. Uji Breusch-Pagan p = ", pfmt(bp["p"]),
               if (bp["p"] < .05) " (ada indikasi heteroskedastisitas)." else " (tidak ada indikasi heteroskedastisitas)."),
      "Q-Q plot residual" =
        "Titik yang mengikuti garis menandakan residual mendekati normal. Penyimpangan di ujung menandakan ekor distribusi yang berat / pencilan.",
      "Histogram residual" =
        "Histogram residual yang simetris dan berbentuk lonceng mendukung asumsi normalitas galat.",
      "Efek parsial prediktor numerik" =
        "Garis = prediksi harga rata-rata saat prediktor lain ditahan pada rata-rata (kategori: modus). Pita = selang kepercayaan 95%.",
      "Cook's distance" =
        "Batang merah muda = observasi berpengaruh besar (> 4/n) terhadap estimasi koefisien; periksa apakah itu mobil yang tidak biasa.")
    div(class = "interp", txt)
  })
  
  output$rg_interp <- renderUI({
    fit <- reg_fit(); pr <- input$rg_pred; lg <- isTRUE(input$rg_log)
    sm <- summary(fit); co <- sm$coefficients; d <- droplevels(dat())
    fs <- sm$fstatistic; pF <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    
    efek <- function(b) {
      if (lg) {
        pc <- (exp(b) - 1) * 100
        paste0(if (pc >= 0) "lebih tinggi sekitar " else "lebih rendah sekitar ",
               fmt(abs(pc), 1), "%")
      } else {
        paste0(if (b >= 0) "lebih tinggi sekitar Rp " else "lebih rendah sekitar Rp ",
               fmt(abs(b), 2), " juta")
      }
    }
    
    items <- lapply(seq_len(nrow(co))[-1], function(i) {
      tm <- rownames(co)[i]; b <- co[i, 1]; p <- co[i, 4]
      v <- pr[startsWith(tm, pr)][1]
      sig <- if (p < .05) paste0("signifikan (p ", pfmt(p), ")")
      else paste0("TIDAK signifikan (p ", pfmt(p), ")")
      if (tm == v) {
        u <- if (!is.na(unit_lbl[v])) unit_lbl[v] else paste("1 satuan", lab(v))
        tags$li(paste0("Setiap penambahan ", u, ", harga rata-rata ",
                       sub("lebih tinggi", "naik", sub("lebih rendah", "turun", efek(b))),
                       ", dengan variabel lain tetap — ", sig, "."))
      } else {
        lvl <- substring(tm, nchar(v) + 1); ref <- levels(d[[v]])[1]
        tags$li(paste0("Mobil dengan ", lab(v), " = ", lvl, " memiliki harga rata-rata ",
                       efek(b), " dibanding ", ref, ", dengan variabel lain tetap — ", sig, "."))
      }
    })
    
    div(class = "interp",
        paste0("Model ", if (lg) "(respons ln Harga) " else "(respons Harga) ",
               "secara simultan ", if (pF < .05) "signifikan" else "tidak signifikan",
               " (uji F, p ", pfmt(pF), "); R² adjusted = ", fmt(sm$adj.r.squared, 3),
               ", artinya sekitar ", fmt(100 * sm$adj.r.squared, 1),
               "% keragaman ", if (lg) "ln(harga)" else "harga",
               " dijelaskan oleh prediktor terpilih."),
        tags$ul(
          tags$li(paste0("Intersep = ", fmt(co[1, 1], 3),
                         ": nilai ", if (lg) "ln(harga)" else "harga",
                         " untuk mobil acuan dengan semua prediktor numerik = 0 ",
                         "(hanya sebagai titik acuan, bukan interpretasi praktis).")),
          items,
          tags$li("Catatan: koefisien menunjukkan asosiasi, bukan hubungan sebab-akibat.")))
  })
  
  output$rg_limit <- renderUI({
    div(class = "interp",
        tags$ul(
          tags$li("Harga pada data adalah harga tawaran iklan, bukan harga transaksi akhir; bisa lebih tinggi dari harga yang benar-benar terjadi."),
          tags$li("Data bersifat cross-sectional (satu waktu) dan hanya 609 mobil, sebagian besar di Jabodetabek, sehingga belum tentu berlaku untuk daerah lain."),
          tags$li("Tipe/varian mobil (172 nama) tidak masuk model; hanya merek yang dikelompokkan, sehingga perbedaan harga antar-model dalam satu merek tidak tertangkap."),
          tags$li("Umur dan jarak tempuh saling berkorelasi (lihat VIF); koefisiennya dapat tidak stabil. Angsuran dikeluarkan karena kolinear sempurna dengan harga."),
          tags$li("Fitur seperti sunroof, kamera 360, navigasi dimiliki sangat sedikit mobil, sehingga efek individualnya tidak dimodelkan terpisah (dirangkum dalam jumlah fitur)."),
          tags$li("Asumsi regresi (linearitas, homoskedastisitas, normalitas galat) perlu dicek lewat grafik diagnostik; pelanggaran memengaruhi uji hipotesis dan selang kepercayaan."),
          tags$li("Pencilan/observasi berpengaruh (Cook's distance) tidak dibuang sehingga dapat memengaruhi estimasi."),
          tags$li("Nilai 'jarak tempuh' dibaca sebagai ribuan km berdasarkan format data; bila satuan aslinya berbeda, besaran koefisien perlu disesuaikan."),
          tags$li("Model dihitung pada data terfilter; hasil bisa berbeda bila filter diubah."),
          tags$li("Prediksi di luar rentang data (mis. mobil sangat tua atau sangat mewah) bersifat ekstrapolasi dan tidak andal.")))
  })
  
  # ======================= KESIMPULAN =======================
  output$ks_text <- renderUI({
    d <- df
    r_um <- cor(d$HARGA_JT, d$UMUR, method = "spearman")
    r_km <- cor(d$HARGA_JT, d$KM_RIBU, method = "spearman")
    r_ft <- cor(d$HARGA_JT, d$JML_FITUR, method = "spearman")
    mt <- tapply(d$HARGA_JT, d$TRANS_LBL, mean)
    mm <- tapply(d$HARGA_JT, d$MEREK_GRP, mean)
    fit <- lm(log(HARGA_JT) ~ UMUR + KM_RIBU + JML_FITUR + TRANS_LBL + MEREK_GRP, data = d)
    sm <- summary(fit); co <- sm$coefficients[-1, , drop = FALSE]
    pr <- c("UMUR", "KM_RIBU", "JML_FITUR", "TRANS_LBL", "MEREK_GRP")
    top <- rownames(co)[which.max(abs(co[, 3]))]
    sigs <- rownames(co)[co[, 4] < .05]
    tagList(
      card(card_header("Kesimpulan Utama"),
           div(class = "interp", tags$ul(
             tags$li(paste0("Dataset memuat ", nrow(d), " mobil bekas dengan harga rata-rata Rp ",
                            fmt(mean(d$HARGA_JT), 1), " juta (median Rp ", fmt(median(d$HARGA_JT), 1),
                            " juta); distribusi harga cenderung ",
                            if (skew(d$HARGA_JT) > 0.5) "miring ke kanan" else "simetris",
                            " dan ", length(boxplot.stats(d$HARGA_JT)$out),
                            " mobil terdeteksi sebagai pencilan harga.")),
             tags$li(paste0("Hubungan bivariat (Spearman) dengan harga: umur rho = ", fmt(r_um, 2),
                            ", jarak tempuh rho = ", fmt(r_km, 2), ", jumlah fitur rho = ",
                            fmt(r_ft, 2), ".")),
             tags$li(paste0("Mobil otomatis rata-rata Rp ", fmt(mt["Otomatis"], 1),
                            " juta vs manual Rp ", fmt(mt["Manual"], 1), " juta (selisih Rp ",
                            fmt(mt["Otomatis"] - mt["Manual"], 1), " juta).")),
             tags$li(paste0("Merek dengan rata-rata harga tertinggi: ", names(mm)[which.max(mm)],
                            " (Rp ", fmt(max(mm), 1), " juta); terendah: ", names(mm)[which.min(mm)],
                            " (Rp ", fmt(min(mm), 1), " juta).")),
             tags$li(paste0("Regresi linear berganda (respons ln Harga; prediktor: umur, jarak tempuh, ",
                            "jumlah fitur, transmisi, merek) memiliki R² adjusted = ",
                            fmt(sm$adj.r.squared, 3), ". Prediktor dengan bukti terkuat: ",
                            nice_term(top, pr), "."),
                     if (length(sigs)) paste0(" Koefisien signifikan (α = 5%): ",
                                              paste(sapply(sigs, nice_term, preds = pr), collapse = "; "), ".")),
             tags$li("Sisa keragaman harga yang tidak terjelaskan menunjukkan faktor lain (tipe/varian, kondisi fisik, riwayat servis) berperan dan tidak tersedia di data.")))),
      card(card_header("Rekomendasi"),
           div(class = "interp", tags$ul(
             tags$li("Pembeli: bandingkan harga tawaran dengan kisaran rata-rata untuk umur, transmisi, dan merek yang sama (gunakan heatmap & model regresi), bukan hanya jarak tempuh."),
             tags$li("Penjual/dealer: tetapkan harga berdasarkan faktor yang terbukti signifikan pada model (umur, transmisi, merek), dan tonjolkan kelengkapan fitur."),
             tags$li("Platform iklan: sediakan estimator harga wajar dan tandai iklan yang jauh menyimpang dari harga prediksi (residual besar)."),
             tags$li("Pengembangan analisis: tambahkan variabel tipe/varian, kondisi, dan harga transaksi; uji model non-linear atau regularisasi (Ridge/Lasso) untuk mengatasi multikolinearitas.")))),
      team_footer()
    )
  })
  
  # ======================= DATA =======================
  output$tbl <- DT::renderDT({
    DT::datatable(dat() %>%
                    select(NAMA, MEREK, TAHUN, all_of(c("KM_RIBU", "HARGA_JT", "ANGSURAN_JT",
                                                        "UMUR", "JML_FITUR")),
                           LOKASI, TRANS_LBL, PLAT_LBL, MEREK_GRP, WILAYAH, ERA) %>%
                    mutate(across(c(KM_RIBU, HARGA_JT, ANGSURAN_JT), ~ round(.x, 3))),
                  filter = "top", rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$dl <- downloadHandler(
    filename = function() paste0("carlens_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(dat(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
