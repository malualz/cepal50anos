# ====== Estilo CEPAL: gráfico de publicações por ano (dados brutos) ======
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr))   install.packages("dplyr")
if (!require(tidyr))   install.packages("tidyr")
if (!require(scales))  install.packages("scales")
if (!require(sysfonts)) install.packages("sysfonts")
if (!require(showtext)) install.packages("showtext")

library(ggplot2); library(dplyr); library(tidyr); library(scales)
library(sysfonts); library(showtext)

# --------------------------------------------------------------------
# 1) Tipografia (prioriza Source Sans Pro; alternativa Lato; fallback "sans")
# --------------------------------------------------------------------
# Tente carregar automaticamente (Google Fonts)
try({ font_add_google("Source Sans Pro", "Source Sans Pro")  }, silent = TRUE)
try({ font_add_google("Lato",           "Lato")              }, silent = TRUE)

showtext_auto()

font_family <- if ("Source Sans Pro" %in% font_families()) "Source Sans Pro" else
  if ("Lato" %in% font_families())             "Lato" else "sans"

# --------------------------------------------------------------------
# 2) Preparação dos dados (brutos, completos por ano)
# --------------------------------------------------------------------
df_wos <- r_wos$yearly |> tidyr::complete(PY = tidyr::full_seq(PY, 1), fill = list(Docs = 0)) |> mutate(Source = "WoS")
df_sco <- r_sco$yearly |> tidyr::complete(PY = tidyr::full_seq(PY, 1), fill = list(Docs = 0)) |> mutate(Source = "Scopus")
df_mrg <- r_mrg$yearly |> tidyr::complete(PY = tidyr::full_seq(PY, 1), fill = list(Docs = 0)) |> mutate(Source = "Merged")

year_min <- min(c(df_wos$PY, df_sco$PY, df_mrg$PY), na.rm = TRUE)
year_max <- max(c(df_wos$PY, df_sco$PY, df_mrg$PY), na.rm = TRUE)
x_breaks <- seq(floor(year_min/5)*5, ceiling(year_max/5)*5, by = 5)

y_max <- max(c(df_wos$Docs, df_sco$Docs, df_mrg$Docs), na.rm = TRUE)
y_step <- dplyr::case_when(
  y_max <= 50  ~ 10,
  y_max <= 100 ~ 20,
  y_max <= 300 ~ 50,
  y_max <= 600 ~ 100,
  TRUE         ~ 200
)
y_top    <- ceiling(y_max / y_step) * y_step
y_breaks <- seq(0, y_top, by = y_step)

# --------------------------------------------------------------------
# 3) Paleta e tema (CEPAL-like)
#    - Azul principal para WoS
#    - Azul-petróleo/teal para Scopus
#    - Cinza claro para Merged (fundo)
# --------------------------------------------------------------------
col_wos    <- "#005B9A"  # azul profundo
col_scopus <- "#2A9D8F"  # teal/azulado
col_merged <- "#E6ECF2"  # cinza muito claro (área)
grid_gray  <- "#D9DEE3"  # grid horizontal sutil
text_gray  <- "#2B2B2B"

theme_cepal <- theme_minimal(base_size = 13, base_family = font_family) +
  theme(
    plot.title      = element_text(face = "bold", color = text_gray, size = 20, margin = margin(b = 6)),
    plot.subtitle   = element_text(color = text_gray, margin = margin(b = 16)),
    plot.caption    = element_text(color = text_gray, size = 16),
    axis.title      = element_text(color = text_gray, size = 16),
    axis.text       = element_text(color = text_gray),
    legend.position = "top",
    legend.title    = element_text(face = "bold"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = grid_gray, linewidth = 0.4),
    plot.margin     = margin(10, 12, 10, 12)
  )

# --------------------------------------------------------------------
# 4) Gráfico
# --------------------------------------------------------------------
p_cepal <- ggplot() +
  # Área preenchida do merged ao fundo
  geom_area(data = df_mrg, aes(PY, Docs),
            fill = col_merged, alpha = 1, show.legend = FALSE) +
  # WoS (linha contínua, cor institucional)
  geom_line(data = df_wos, aes(PY, Docs, color = "WoS"),
            linewidth = 0.9, linetype = "solid") +
  # Scopus (linha tracejada, um pouco mais fina)
  geom_line(data = df_sco, aes(PY, Docs, color = "Scopus"),
            linewidth = 0.9, linetype = "dashed") +
  scale_color_manual(values = c("WoS" = col_wos, "Scopus" = col_scopus),
                     name = "Dataset") +
  scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, y_top), breaks = y_breaks,
                     labels = scales::comma_format(accuracy = 1), expand = c(0, 0)) +
  labs(
    title = "Publications by Year (raw counts)",
    x = "Publication year",
    y = "Number of documents"
    # , caption = "Source: WoS, Scopus; author’s calculations."
  ) +
  theme_cepal

print(p_cepal)


# --------------------------------------------------------------------
# 5) Exportação (padrão editorial)
# --------------------------------------------------------------------
dir.create("outputs_cepal", showWarnings = FALSE)
ggsave("outputs_cepal/fig_pub_trends_cepal_style.png", p_cepal,
       width = 7.2, height = 5.1, dpi = 300, bg = "white")
ggsave("outputs_cepal/fig_pub_trends_cepal_style.pdf", p_cepal,
       width = 7.2, height = 5.1, device = cairo_pdf, bg = "white")

showtext::showtext_opts(dpi = 300)
ggsave("outputs_cepal/fig_pub_trends_cepal_style.png",
       p_cepal, width = 7.2, height = 5.1, units = "in",
       dpi = 300, bg = "white")   # usa device padrão; manter dpi igual ao showtext
