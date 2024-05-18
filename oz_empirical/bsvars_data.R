
# interpolate quarterly inflation and gdp to monthly
############################################################
create_interpolated_series <- function(data, start_date, end_date) {
  # function to interpolate quarterly to monthly data
  # start_date and end_date need to be given as a character with format "YYYY-MM-DD"
  
  # data object to merge by data all time series
  df <- data.frame(date = seq(as.Date(start_date), as.Date(end_date), by = "months"))
  df <- dplyr::left_join(df, data, by = "date")  
  ## interpolate GDP series
  # first shift GDP series such that Q1 is assigned to March etc
  df$X   = c(rep(NA,2), df$value[1:(length(df$value) - 2)])
  # now interpolate, taking into account leading and trailing NA values
  df <- dplyr::mutate(df, value_out = c(rep(NA, min(which(!is.na(df$X))) - 1),
                                        zoo::na.approx(df$X),
                                        rep(NA, length(df$date) - max(which(!is.na(df$X))))
  )
  )
  df <- dplyr::select(df, c(date, value_out))
  df
}


# colours 
############################################################
N           = 4
bsvars_pink = "#ff69b4"
bsvars_yell = "#ffd700"
bsvars_grad = grDevices::colorRampPalette(c("#ff69b4", "#ffd700"))(N)

# Australian data
############################################################
start_date = "1950-01-01"
end_date   = "2024-05-01"

# QUARTERLY
# Real Gross Domestic Product for Australia (Domestic Currency, Seasonally Adjusted)
aud_gdp   = fredr::fredr("NGDPRSAXDCAUQ")
df_au_gdp = create_interpolated_series(aud_gdp, start_date, end_date)
aud_gdp   = xts::xts(df_au_gdp$value, df_au_gdp$date, tclass = 'yearmon')
aud_gdp   = xts::to.monthly(aud_gdp, OHLC = FALSE, drop.time = TRUE)
aud_gdp   = na.omit(12 * 100 * diff(log(aud_gdp)))
rm(df_au_gdp)

# QUARTERLY
# Consumer Price Index: All Items: Total: Total for Australia (Index 2015=100, Not Seasonally Adjusted)
aud_cpi   = fredr::fredr("AUSCPIALLQINMEI")
df_au_cpi = create_interpolated_series(aud_cpi, start_date, end_date)
aud_cpi   = xts::xts(df_au_cpi$value, df_au_cpi$date, tclass = 'yearmon')
aud_cpi   = xts::to.monthly(aud_cpi, OHLC = FALSE, drop.time = TRUE)
aud_pi   = na.omit(12 * 100 * diff(log(aud_cpi)))
rm(df_au_cpi)

# 3-Month or 90-day Rates and Yields: Interbank Rates for Australia (Percent, Not Seasonally Adjusted) 
aud_IR    = fredr::fredr("IR3TIB01AUM156N")
aud_IR    = xts::xts(aud_IR$value, aud_IR$date, tclass = 'yearmon')

# exchange rate
aud_USD   = readrba::read_rba(series_id = "FXRUSD")
aud_USD   = xts::xts(aud_USD$value, aud_USD$date, tclass = 'yearmon')
aud_USD   = xts::to.monthly(aud_USD, OHLC = FALSE, drop.time = TRUE)

y               = na.omit(merge(aud_gdp, aud_pi, aud_IR, aud_USD))
colnames(y)     = c("gdp", "pi", "IR", "USD")




# US data
############################################################
# QUARTERLY
# real gdp
gdp       = fredr::fredr("GDPC1")
df_gdp    = create_interpolated_series(gdp, start_date, end_date)
gdp       = xts::xts(df_gdp$value_out, df_gdp$date, tclass = 'yearmon')
gdp       = xts::to.monthly(gdp, OHLC = FALSE, drop.time = TRUE)
usa_gdp   = na.omit(12 * 100 * diff(log(gdp)))
rm(df_gdp)

# Consumer Price Index: All Items for the United States
cpi       = fredr::fredr("USACPIALLMINMEI")
cpi       = xts::xts(cpi$value, cpi$date, tclass = 'yearmon')
usa_pi    = na.omit(12 * 100 * diff(log(cpi)))

# Federal Funds Effective Rate
FFR       = fredr::fredr("FEDFUNDS")
usa_FFR   = xts::xts(FFR$value, FFR$date, tclass = 'yearmon')



start_date = "1950-01-01"
end_date   = "2024-05-01"

aud_tot   = fredr::fredr("AUSLOCOTTNOSTSAM")
aud_tot   = na.omit(xts::xts(aud_tot$value, aud_tot$date, tclass = 'yearmon'))
colnames(aud_tot) = "aud_tot"

x         = na.omit(merge(aud_tot, usa_gdp, usa_pi, usa_FFR))
x         = na.omit(merge(x, lag(x)))
x         = x["1969-07/2023-09"]
save(y, x, file = "oz_empirical/bsvars_data.rda")

# y
# plot(
#   y, 
#   main = "A Small Australian Monetary System",
#   col = bsvars_grad,
#   legend.loc = "topright",
# )
# 
# fUnitRoots::adfTest(aud_gdp, lags = 15)
# fUnitRoots::adfTest(aud_pi, lags = 15)
# fUnitRoots::adfTest(aud_IR, lags = 15)
# fUnitRoots::adfTest(aud_USD, lags = 15)