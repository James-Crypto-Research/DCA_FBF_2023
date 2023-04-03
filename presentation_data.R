# This script loads and manipulates the data to be ready
# for the Quarto presentation

library(plyr)
library(tidyverse)
library(lubridate)
library(GlassnodeR)
library(RefiLlama)
library(CMCR)
library(arrow)
# where to save the data

data_file <- "data/presentation_data.Rdata"

# For BTC address
btc_add <- read_csv("/Users/jame/Dropbox (Bank of Canada)/Data/Crypto/Chainalysis/BTC/net-new-entities.csv")
btc_add$BTC_tot_add <- cumsum(btc_add$net_new_entities)
btc_add$date <- as.POSIXct(btc_add$time)
btc_add <- btc_add |> select(date,BTC_tot_add)

# For BTC HODL
BTC_hodl <- get_lost_coins()
BTC_supply <- get_circulable_supply("BTC")
BTC_data <- BTC_hodl |> left_join(BTC_supply, by="date") |>
  mutate(btc_is = BTC_hodl/BTC)

# For BTC HODL waves
btc_hodl_wave <- get_hodl_wave("BTC")
eth_hodl_wave <- get_hodl_wave("ETH")


#  For Duts to Dust


# For miner concentration
miners <- read_csv("data/miners.csv",col_types = "cddd") |>
  filter(pool != "NETWORK")


# For BTC dominanceo
global_metrics <- get_global_metrics(time_start = "2013-01-01",
                                    time_end="2023-03-01") |> as_tibble()
dominance <- global_metrics |> select(date,btc_dominance,eth_dominance) |>
                    mutate(date=as.Date(date))

active <- global_metrics |> select(date,active_cryptocurrencies,active_exchanges) |>
            rename(cryptos = active_cryptocurrencies, exchanges = active_exchanges)
rm(global_metrics)


# For ETH address
eth_add <- read_csv("/Users/jame/Dropbox (Bank of Canada)/Data/Crypto/Chainalysis/ETH/net-new-entities.csv")
eth_add$eth_tot_add <- cumsum(eth_add$net_new_entities)
eth_add$date <- as.POSIXct(eth_add$time)
eth_add <- eth_add |> select(date,eth_tot_add)

# For ICO shares


# For token entry and exit
x <- get_currency_map()
x <- x |> filter(!is.na(first_historical_data),!is.na(last_historical_data))
x <- x |> mutate(last_month = as.Date(round_date(last_historical_data,"month")))
x <- x |> mutate(
  first_month = as.Date(round_date(first_historical_data,"month"))
)


births <- x |> group_by(first_month) |> summarize(starts=n()) |>
                rename(date = first_month) |> select(date,starts)
deaths <- x |> group_by(last_month) |> summarize(ends=n()) |>
                rename(date = last_month) |> select(date,ends)
BD_tokens <- births |> full_join(deaths, by="date") |> replace_na(list(starts = 0, ends= 0))
rm(x,births,deaths)



# For exchange entry and exit
x <- get_exchange_map()
x <- x |> filter(!is.na(first_historical_data),!is.na(last_historical_data))
x <- x |> mutate(last_week = as.Date(round_date(last_historical_data,"month")))
x <- x |> mutate(
            first_week = as.Date(round_date(first_historical_data,"month"))
            )
births <- x |> group_by(first_week) |> summarize(starts=n()) |>
  rename(date = first_week)
deaths <- x |> group_by(last_week) |> summarize(ends=n()) |>
  rename(date = last_week)
BD_exchange <- births |> full_join(deaths)
rm(x,births,deaths)

# tokens on listings and vice versa

crypto_data <- read_feather("/Users/jame/Dropbox (Bank of Canada)/Data/Crypto/CMC_data/crypto_data_2022_01_24.feather",
                    col_select = c("Date","exchange_name","name")) |>
                    mutate(name = str_extract(name,"^[:alnum:]+")) |>
                    distinct()
# This creates the average number of coins per exchange traded
num_traded_exchanges <- crypto_data %>%
group_by(Date,exchange_name) %>%
summarize(n = n(),.groups="drop") |>
  group_by(exchange_name) |>
  summarise(n=mean(n))

num_traded_crypto <- crypto_data %>%
  group_by(Date,name) %>%
  summarize(n = n(),.groups="drop") |>
  group_by(name) |>
  summarise(n=mean(n))
rm(crypto_data)

# For stabelcoins
# This is the list of stablecoins to get
stablecoins <- c("usdt","busd","gusd","husd","dai","usdp","eurs",
                 "sai","susd","usdc")
market_caps <- stablecoins |> map(get_marketcap) |>
  join_all(by="date",match="all") |> as_tibble()
# now get USTC from CMC
market_caps[is.na(market_caps)] <- 0
market_caps <- market_caps |> pivot_longer(-date,names_to="token",
                                           values_to = "marketcap")
ustc <- get_token_quote("USTC","2020-10-01","2023-04-01") |>
  select(date,marketcap) |>
  mutate(date=as.Date(date),token="ustc") |>
  as_tibble() |>
  relocate(token,.after = date)
market_caps <- bind_rows(market_caps,ustc)

# For updated shares

# For TVL
TVL <- get_historical_tvl() |> rename("tvl" = "totalLiquidityUSD")

# For TVL vs exchange
# error in the API. Non-available tokens ARMOR , "wNXM", "WTC"
available_tokens <- c("BTC", "ETH", "AAVE", "ABT", "ANT", "APE",
                      "BADGER", "BAL", "BAND", "BAT", "BIX", "BNT", "BOND",
                      "BRD", "BUSD", "BZRX", "CELR", "CHSB", "CND", "COMP",
                      "CREAM", "CRO", "CRV", "CVC", "CVP", "DAI", "DENT", "DGX",
                      "DHT", "DMG", "DODO", "DRGN",
                      "ELF", "ENG", "ENJ", "EURS", "FET", "FTT", "FUN", "GNO",
                      "GUSD", "HEGIC", "HOT", "HPT", "HT", "HUSD",
                      "KCS", "LAMB", "LBA", "LDO",
                      "LEO", "LINK", "LOOM", "LRC", "MANA",
                      "MATIC", "MCB", "MCO", "MFT", "MIR",
                      "MKR", "MLN", "MTA", "MTL", "MX", "NEXO",
                      "NFTX", "NMR", "Nsure", "OCEAN", "OKB",
                      "OMG", "PAY", "PERP", "PICKLE",
                      "PNK", "PNT", "POLY", "POWR", "PPT",
                      "QASH", "QKC", "QNT", "RDN", "REN",
                      "REP", "RLC", "ROOK", "RSR", "SAI",
                      "SAN", "SAND", "SHIB", "SNT", "SNX",
                      "STAKE", "STBL", "stETH", "STORJ",
                      "sUSD", "SUSHI", "TEL", "TOP", "TUSD",
                      "UBT", "UMA", "UNI", "USDC", "USDK",
                      "USDP", "USDT", "UTK", "VERI", "WaBi",
                      "WAX", "WBTC", "WETH",
                      "YFI", "ZRX")
exchange_totals <- map(available_tokens,get_exchange_balance)
exchange_totals <- exchange_totals |> map(mutate,exchange=NULL)
exchange_totals <- exchange_totals |> join_all(by="date") %>%
  rowwise() %>%
  mutate(ex_total = sum(c_across(BTC:ZRX),na.rm = TRUE)) %>%
  relocate(ex_total,.after=date)

BTC_price <- get_closing_price("BTC")
ETH_price <- get_closing_price("ETH")
ETH_supply <- get_circulable_supply("ETH")
BTC_Llight <- get_lightning_network()
BTC_tx_rate <- get_transactions_rate("BTC")
ETH_tx_rate <- get_transactions_rate("ETH")
save(list=ls(),file=data_file)
