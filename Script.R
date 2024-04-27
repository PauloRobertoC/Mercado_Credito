# Analise do Mercado de Credito com R
# Autor: Paulo Roberto Carneiro
# Referencias: F. J. C. Carvalho, F. E. P. Souza, J. Sicsu, L. F. R. Paula, and R. Studart.
# Economia Monetária e Financeira - Teoria e Política. Elsevier, Rio de Janeiro,
# sétima edition, 2000.



# Pacotes -----------------------------------------------------------------

# Carregar pacotes
library(GetBCBData)
library(dplyr)
library(ggplot2)
library(sidrar)
library(lubridate)
library(ggseas)
library(tidyr)



# Coleta de dados ---------------------------------------------------------


# Parâmetros e códigos para coleta de dados
codigos <- c(
  # Concessões de crédito - Total - R$ (milhões)
  "Concessões de crédito - Total" = 20631,

  # Concessões de crédito - Pessoas jurídicas - Total - R$ (milhões)
  "Concessões de crédito - PJ" = 20632,

  # Concessões de crédito - Pessoas físicas - Total	- R$ (milhões)
  "Concessões de crédito - PF" = 20633,

  # Concessões de crédito com recursos livres - Total	- R$ (milhões)
  "Concessões de crédito - Livre" = 20634,

  # Concessões de crédito com recursos direcionados - Total	- R$ (milhões)
  "Concessões de crédito - Direcionado" = 20685,

  # Saldo da carteira de crédito - Total - R$ (milhões)
  "Saldo da carteira de crédito - Total" = 20539,

  # PIB acumulado dos últimos 12 meses - Valores correntes - (R$ milhões)
  "PIB acumulado dos últimos 12 meses" = 4382,

  # Saldos das operações de crédito sob controle privado - Total	- (R$ milhões)
  "Saldos de crédito - Privado" = 2043,

  # Saldos das operações de crédito sob controle público - Total	- (R$ milhões)
  "Saldos de crédito - Público" = 2007,

  # Taxa média de juros das operações de crédito - Total - % a.a.
  "Taxa média de juros das operações de crédito" = 20714,

  # Spread médio das operações de crédito - Total	- p.p.
  "Spread médio das operações de crédito" = 20783,

  # Inadimplência da carteira de crédito - Total - %
  "Inadimplência da carteira de crédito" = 21082,

  # Saldo das operações de credito do Sistema Financeiro Nacional - Maranhão - PF (R$ Milhões)
  "Saldo das Operaões de Credito do Sistema Financeiro Nacional - Maranhão - Pessoa Física" = 14011,

  # Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %
  "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %" = 15870,

  # Saldo das operações de crédito do Sistema Financeiro Nacional - Piauí - PF (R$ Milhões)
  "Saldo das operações de crédito do Sistema Financeiro Nacional - Piauí - PF (R$ Milhões)" = 14019,

  # Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %
  "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %" = 15878

)


# Importa dados do SGS/BCB
dados <- GetBCBData::gbcbd_get_series(
  id = codigos,
  first.date = "2012-01-01",
  use.memoise = FALSE
)

# Importa dados do IPCA
ipca <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2266/p/all/d/v2266%2013")


# Tratamento de dados -----------------------------------------------------

# Seleção de colunas do BCB
dados %<>%
  dplyr::select("data" = "ref.date", "serie" = "series.name", "valor" = "value") %>%
  dplyr::as_tibble()

# Seleção e tratamento de colunas do IPCA
ipca %<>%
  dplyr::mutate(
    data = lubridate::ym(`Mês (Código)`),
    ipca = Valor,
    .keep = "none"
  ) %>%
  dplyr::as_tibble()



# Visualização de dados ---------------------------------------------------


# Objeto com fonte dos dados
fonte <- "Dados: BCB | Elaboração: Paulo Roberto - Serviços Financeiros"


# |-- Concessões ----

# Total - Nominal
dados %>%
  dplyr::filter(serie == "Concessões de crédito - Total") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor / 1000) +
  ggplot2::geom_line(size = 1, color = "red") +
  ggplot2::labs(
    title = "Concessões de crédito - Total",
    subtitle = "Valores nominais",
    y = "R$ bilhões",
    x = NULL,
    caption = fonte
  )

# PF/PJ e Livre/Direcionado - Nominal
dados %>%
  dplyr::filter(
    serie %in% c(
      "Concessões de crédito - PJ",
      "Concessões de crédito - PF",
      "Concessões de crédito - Livre",
      "Concessões de crédito - Direcionado"
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor / 1000, color = serie) +
  ggplot2::geom_line(size = 1) +
  ggplot2::facet_wrap(facets = ~serie, scales = "free") +
  ggplot2::labs(
    title = "Concessões de crédito",
    subtitle = "Valores nominais",
    y = "R$ bilhões",
    x = NULL,
    caption = fonte
  ) +
  ggplot2::theme(legend.position = "none")


# Total - Deflacionado e dessazonalidado
dados %>%
  dplyr::filter(serie == "Concessões de crédito - Total") %>%
  dplyr::left_join(y = ipca, by = "data") %>%
  dplyr::mutate(
    deflacionado = ipca[data == max(dados$data)] / ipca * valor
  ) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y = deflacionado / 1000) +
  ggseas::stat_seas(
    frequency = 12,
    start = c(2012, 01),
    size = 1,
    color = "navyblue"
  ) +
  ggplot2::labs(
    title = "Concessões de crédito - Total",
    subtitle = paste0(
      "Valores deflacionados pelo IPCA a preços de ",
      format(max(dados$data), "%b/%Y"),
      " e ajustado sazonalmente pelo X13-SEATS-ARIMA."
    ),
    y = "R$ bilhões",
    x = NULL,
    caption = "Dados: BCB/IBGE | Elaborações: Paulo Roberto - Serviços Financeiros"
  )



# |-- Estoque ----

# Saldo % do PIB
dados %>%
  dplyr::filter(
    serie %in% c(
      "Saldo da carteira de crédito - Total",
      "PIB acumulado dos últimos 12 meses"
    )
  ) %>%
  tidyr::pivot_wider(id_cols = "data", names_from = "serie", values_from = "valor") %>%
  dplyr::mutate(
    saldo_pib = `Saldo da carteira de crédito - Total` / `PIB acumulado dos últimos 12 meses`
  ) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  saldo_pib * 100) +
  ggplot2::geom_area(fill = "navyblue") +
  ggplot2::labs(
    title = "Estoque de Crédito",
    y = "% do PIB",
    x = NULL,
    caption = fonte
  )


# Privado/Publico % do Total
dados %>%
  dplyr::filter(
    serie %in% c(
      "Saldos de crédito - Privado",
      "Saldos de crédito - Público",
      "Saldo da carteira de crédito - Total"
    )
  ) %>%
  tidyr::pivot_wider(id_cols = "data", names_from = "serie", values_from = "valor") %>%
  dplyr::mutate(
    data = data,
    Privado = `Saldos de crédito - Privado` / `Saldo da carteira de crédito - Total` * 100,
    `Público` = `Saldos de crédito - Público` / `Saldo da carteira de crédito - Total` * 100,
    .keep = "none"
  ) %>%
  tidyr::pivot_longer(cols = -"data", names_to = "serie", values_to = "valor") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y = valor, fill = serie) +
  ggplot2::geom_area() +
  ggplot2::labs(
    title = "Estoque de Crédito",
    y = "% do Total",
    x = NULL,
    fill = NULL,
    caption = fonte
  )



# |-- Taxa de juros ----

# Gráfico de linha
dados %>%
  dplyr::filter(serie == "Taxa média de juros das operações de crédito") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "navyblue") +
  ggplot2::labs(
    title = "Taxa de juros - Mercado de Crédito - Brasil",
    y = "% a.a.",
    x = NULL,
    caption = fonte
  )


# |-- Spread ----

# Gráfico de linha
dados %>%
  dplyr::filter(serie == "Spread médio das operações de crédito") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "navyblue") +
  ggplot2::labs(
    title = "Spread bancário - Mercado de Crédito - Brasil",
    y = "p.p.",
    x = NULL,
    caption = fonte
  )


# |-- Inadimplência ----

# Gráfico de linha
dados %>%
  dplyr::filter(serie == "Inadimplência da carteira de crédito") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "navyblue") +
  ggplot2::labs(
    title = "Inadimplência - Mercado de Crédito - Brasil",
    y = "%",
    x = NULL,
    caption = fonte
  )

### Analise Regional
# Inadimplencia MA

dados %>%
  dplyr::filter(serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "navyblue") +
  ggplot2::labs(
    title = "Inadimplência - Mercado de Crédito - Maranhão",
    y = "%",
    x = NULL,
    caption = fonte
  )

# Inadimplencia MA

dados %>%
  dplyr::filter(serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "navyblue") +
  ggplot2::labs(
    title = "Inadimplência - Mercado de Crédito - Piauí",
    y = "%",
    x = NULL,
    caption = fonte
  )

# Filtrando os dados para inadimplência no Brasil, Maranhão e Piauí
dados_inadimplencia <- dados %>%
  filter(  serie == "Inadimplência da carteira de crédito - Total - %" |
           serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %" |
           serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %")

# Ajustando nomes das séries para facilitar a identificação
dados_inadimplencia1$serie <- factor(dados_inadimplencia$serie,
                                    levels = c("Inadimplência da carteira de crédito - Total - %",
                                               "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %",
                                               "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %"),
                                    labels = c("Brasil", "Maranhão", "Piauí"))

# Plotando o gráfico
ggplot(dados_inadimplencia1, aes(x = data, y = valor, color = serie)) +
  geom_line(size = 1) +
  labs(title = "Inadimplência - Mercado de Crédito",
       y = "%",
       x = NULL,
       caption = "Fonte: Banco Central do Brasil | Elaboração: Paulo Roberto - Serviços Financeiros") +
  theme_minimal()


##############################

# Filtrando os dados para as séries desejadas
dados_inadimplencia3 <- dados %>%
  filter(
    serie == "Inadimplência da carteira de crédito" |
      serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %" |
      serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %"
  )

# Modificando os nomes das séries para simplificar as legendas
dados_inadimplencia3$serie <- recode(dados_inadimplencia3$serie,
                                    "Inadimplência da carteira de crédito" = "Brasil",
                                    "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %" = "Maranhão",
                                    "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %" = "Piauí")

# Plotando o gráfico
ggplot(dados_inadimplencia3, aes(x = data, y = valor, color = serie)) +
  geom_line(size = 1) +
  labs(
    title = "Inadimplência - Mercado de Crédito",
    y = "%",
    x = NULL,
    caption = "Fonte: Banco Central do Brasil | Elaboração: Paulo Roberto - Serviços Financeiros"
  ) +
  theme_minimal()


###############################################################################################################################

# Total - Nominal
dados %>%
  dplyr::filter(serie == "Concessões de crédito - Total") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor / 1000) +
  ggplot2::geom_line(size = 1, color = "red") +
  ggplot2::labs(
    title = "Concessões de crédito - Total",
    subtitle = "Valores nominais",
    y = "R$ bilhões",
    x = NULL,
    caption = fonte
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, lineheight = 1.2),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, lineheight = 1.2)
  )

# PF/PJ e Livre/Direcionado - Nominal
dados %>%
  dplyr::filter(
    serie %in% c(
      "Concessões de crédito - PJ",
      "Concessões de crédito - PF",
      "Concessões de crédito - Livre",
      "Concessões de crédito - Direcionado"
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor / 1000, color = serie) +
  ggplot2::geom_line(size = 1) +
  ggplot2::facet_wrap(facets = ~serie, scales = "free") +
  ggplot2::labs(
    title = "Concessões de crédito",
    subtitle = "Valores nominais",
    y = "R$ bilhões",
    x = NULL,
    caption = fonte
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, lineheight = 1.2),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, lineheight = 1.2),
    legend.position = "none"
  )

# Total - Deflacionado e dessazonalidado
dados %>%
  dplyr::filter(serie == "Concessões de crédito - Total") %>%
  dplyr::left_join(y = ipca, by = "data") %>%
  dplyr::mutate(
    deflacionado = ipca[data == max(dados$data)] / ipca * valor
  ) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y = deflacionado / 1000) +
  ggseas::stat_seas(
    frequency = 12,
    start = c(2012, 01),
    size = 1,
    color = "red"
  ) +
  ggplot2::labs(
    title = "Concessões de crédito - Total",
    subtitle = paste0(
      "Valores deflacionados pelo IPCA a preços de ",
      format(max(dados$data), "%b/%Y"),
      " e ajustado sazonalmente pelo X13-SEATS-ARIMA."
    ),
    y = "R$ bilhões",
    x = NULL,
    caption = "Dados: BCB/IBGE | Elaborações: Paulo Roberto - Serviços Financeiros"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, lineheight = 1.2),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, lineheight = 1.2)
  )

# Privado/Publico % do Total
dados %>%
  dplyr::filter(
    serie %in% c(
      "Saldos de crédito - Privado",
      "Saldos de crédito - Público",
      "Saldo da carteira de crédito - Total"
    )
  ) %>%
  tidyr::pivot_wider(id_cols = "data", names_from = "serie", values_from = "valor") %>%
  dplyr::mutate(
    data = data,
    Privado = `Saldos de crédito - Privado` / `Saldo da carteira de crédito - Total` * 100,
    `Público` = `Saldos de crédito - Público` / `Saldo da carteira de crédito - Total` * 100,
    .keep = "none"
  ) %>%
  tidyr::pivot_longer(cols = -"data", names_to = "serie", values_to = "valor") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y = valor, fill = serie) +
  ggplot2::geom_area() +
  ggplot2::labs(
    title = "Estoque de Crédito",
    subtitle = "Percentual do Total",
    y = "% do Total",
    x = NULL,
    fill = NULL,
    caption = fonte
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, lineheight = 1.2),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, lineheight = 1.2)
  )

# |-- Taxa de juros ----

# Gráfico de linha
dados %>%
  dplyr::filter(serie == "Taxa média de juros das operações de crédito") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "red") +
  ggplot2::labs(
    title = "Taxa de juros - Mercado de Crédito - Brasil",
    y = "% a.a.",
    x = NULL,
    caption = fonte
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# |-- Spread ----

# Gráfico de linha
dados %>%
  dplyr::filter(serie == "Spread médio das operações de crédito") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "red") +
  ggplot2::labs(
    title = "Spread bancário - Mercado de Crédito - Brasil",
    y = "p.p.",
    x = NULL,
    caption = fonte
  )  +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5)
  )


# |-- Inadimplência ----

# Gráfico de linha
dados %>%
  dplyr::filter(serie == "Inadimplência da carteira de crédito") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "red") +
  ggplot2::labs(
    title = "Inadimplência - Mercado de Crédito - Brasil",
    y = "%",
    x = NULL,
    caption = fonte
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

### Analise Regional
# Inadimplencia MA

dados %>%
  dplyr::filter(serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "red") +
  ggplot2::labs(
    title = "Inadimplência no Mercado de Crédito - Maranhão (2012/2024)",
    y = "%",
    x = NULL,
    caption = fonte
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# Inadimplencia MA

dados %>%
  dplyr::filter(serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = data, y =  valor) +
  ggplot2::geom_line(size = 1, color = "red") +
  ggplot2::labs(
    title = "Inadimplência no Mercado de Crédito - Piauí (2012/2024)",
    y = "%",
    x = NULL,
    caption = fonte
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# Filtrando os dados para inadimplência no Brasil, Maranhão e Piauí
dados_inadimplencia <- dados %>%
  filter(  serie == "Inadimplência da carteira de crédito - Total - %" |
             serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %" |
             serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %")

# Ajustando nomes das séries para facilitar a identificação
dados_inadimplencia1$serie <- factor(dados_inadimplencia$serie,
                                     levels = c("Inadimplência da carteira de crédito - Total - %",
                                                "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %",
                                                "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %"),
                                     labels = c("Brasil", "Maranhão", "Piauí"))

# Plotando o gráfico
ggplot(dados_inadimplencia1, aes(x = data, y = valor, color = serie)) +
  geom_line(size = 1) +
  labs(title = "Inadimplência no Mercado de Crédito (2012/2024)",
       y = "%",
       x = NULL,
       caption = "Fonte: Banco Central do Brasil | Elaboração: Paulo Roberto - Serviços Financeiros") +
  theme_minimal()


##############################

# Filtrando os dados para as séries desejadas
dados_inadimplencia3 <- dados %>%
  filter(
    serie == "Inadimplência da carteira de crédito" |
      serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %" |
      serie == "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %"
  )

# Modificando os nomes das séries para simplificar as legendas
dados_inadimplencia3$serie <- recode(dados_inadimplencia3$serie,
                                     "Inadimplência da carteira de crédito" = "Brasil",
                                     "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Maranhão - Pessoas físicas - %" = "Maranhão",
                                     "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Piauí - PF - %" = "Piauí")

# Plotando o gráfico
ggplot(dados_inadimplencia3, aes(x = data, y = valor, color = serie)) +
  geom_line(size = 1) +
  labs(
    title = "Inadimplência no Mercado de Crédito (2012/2024)",
    y = "%",
    x = NULL,
    caption = "Fonte: Banco Central do Brasil | Elaboração: Paulo Roberto - Serviços Financeiros"
  )
