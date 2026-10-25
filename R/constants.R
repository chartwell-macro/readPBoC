.PBOC_BASE <- "https://www.pbc.gov.cn"

.PBOC_DATA_INDEX <- c(
  zh = paste0(.PBOC_BASE, "/diaochatongjisi/116219/116319/index.html"),
  en = paste0(.PBOC_BASE, "/en/3688247/3688975/index.html")
)

.PBOC_RELEASE_INDEX <- list(
  zh = c(
    data_interpretation = paste0(
      .PBOC_BASE, "/diaochatongjisi/116219/116225/index.html"
    )
  ),
  en = c(
    general = paste0(
      .PBOC_BASE, "/en/3688247/3688978/3732405/index.html"
    ),
    financial_markets = paste0(
      .PBOC_BASE, "/en/3688247/3688978/3709134/index.html"
    ),
    financial_statistics = paste0(
      .PBOC_BASE, "/en/3688247/3688978/3709137/index.html"
    ),
    aggregate_financing = paste0(
      .PBOC_BASE, "/en/3688247/3688978/3709140/index.html"
    ),
    other = paste0(
      .PBOC_BASE, "/en/3688247/3688978/3709143/index.html"
    )
  )
)

