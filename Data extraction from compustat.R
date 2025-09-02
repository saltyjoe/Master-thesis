library(RPostgres)
library(DBI)
library(dplyr)
library(tidyr)
library(rvest)
library(lubridate)
library(httr)
library(writexl)
library(DescTools)
library(tibble)

# Connect to WRDS
wrds_connect <- function(username, password) {
  con <- dbConnect(
    Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    port = 9737,
    dbname = "wrds",
    user = username,
    password = password,
    sslmode = "require"
  )
  return(con)
}

# Replace with your credentials
con <- wrds_connect(username = "saltyjoe", password = "Sdfz980123!@")

# Define query parameters
current_year <- 2024
start_year <- current_year - 15  # extra years for EPS trend calculation (need at least 5)

# 方案3: 如果你知道具体的变更日期，可以使用网络存档服务获取过去的名单
# 例如，可以尝试使用Wayback Machine (archive.org)的历史快照

historical_url <- "https://web.archive.org/web/20240201/https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
sp500_page <- read_html(historical_url)
sp500 <- html_table(sp500_page)[[1]]
tickers <- paste0("'", sp500$Symbol, "'", collapse = ",")

print(tickers)

# check columns name
check_columns <- dbGetQuery(con, "SELECT column_name FROM information_schema.columns 
                                 WHERE table_schema = 'comp' AND table_name = 'secm'")
print(check_columns)

# 检查comp.funda的列名
check_columns_funda <- dbGetQuery(con, "SELECT column_name FROM information_schema.columns 
                                      WHERE table_schema = 'comp' AND table_name = 'funda'")
print(check_columns_funda)


# Build SQL query - includes EPS and key financial variables
# Based on Chen et al. (2022), Ou and Penman (1989), Sloan (1996), Piotroski (2000)
sql_query <- paste0("
SELECT 
    -- Identification information
    a.gvkey, a.datadate, a.fyear, a.tic, a.conm, a.cik, a.sich,
    
    -- EPS related data (Chen et al. 2022)
    a.epspx,     -- Basic EPS (excluding extraordinary items)
    a.epspi,     -- Basic EPS (including extraordinary items) 
    a.epsfi,     -- Diluted EPS (including extraordinary items)
    a.epsfx,     -- Diluted EPS (excluding extraordinary items)
    
    -- Basic financial data
    a.at,       -- Total assets
    a.lt,       -- Total liabilities
    a.dltt,     -- Long-term debt
    a.dlc,      -- Debt in current liabilities
    a.ni,       -- Net income
    a.sale,     -- Sales/Revenue
    a.ceq,      -- Common equity
    a.oibdp,    -- Operating income before depreciation
    a.oiadp,    -- Operating income after depreciation
    a.ib,       -- Income before extraordinary items
    a.dp,       -- Depreciation and amortization
    a.capx,     -- Capital expenditures
    a.xrd,      -- R&D expense
    a.act,      -- Current assets
    a.lct,      -- Current liabilities
    a.che,      -- Cash and cash equivalents
    a.rect,     -- Receivables
    a.invt,     -- Inventory
    a.cogs,     -- Cost of goods sold
    a.txt,      -- Income taxes total
    a.csho,     -- Common shares outstanding
    a.prcc_f,   -- Price close - fiscal
    a.dvc,      -- Common dividends
    a.ebit,     -- Earnings before interest and taxes
    a.re,       -- Retained earnings
    a.wcap,     -- Working capital
    a.pi,       -- Pre-tax income
    a.oancf    -- Operating activities net cash flow
    
FROM 
    comp.funda a
WHERE 
    a.tic IN (", tickers, ")
    AND a.datadate BETWEEN '", start_year, "-01-01' AND '", current_year, "-12-31'
    AND a.indfmt='INDL' 
    AND a.datafmt='STD'
    AND a.popsrc='D'
    AND a.consol='C'
ORDER BY
    a.gvkey, a.datadate
")

# Execute query and get data
compustat_data <- dbGetQuery(con, sql_query)

# Ensure data is sorted by company and date
compustat_data <- compustat_data %>%
  arrange(gvkey, datadate)

# Process EPS variables - implementing Chen et al. (2022) method
# 1. Select primary EPS indicator (prioritize basic EPS excluding extraordinary items)
compustat_data <- compustat_data %>%
  mutate(
    eps = epspx,
    eps = ifelse(is.na(eps), epspi, eps),
    eps = ifelse(is.na(eps), epsfx, eps),
    eps = ifelse(is.na(eps), epsfi, eps)
  )

# 2. Calculate year-over-year EPS change - use lead as forecast y
compustat_data <- compustat_data %>%
  group_by(gvkey) %>%
  mutate(eps_next = lead(eps),
         eps_change = eps_next - eps) %>%
  ungroup()

# 3. Calculate average of past four years' EPS changes (drift term)
compustat_data <- compustat_data %>%
  group_by(gvkey) %>%
  mutate(
    eps_lag1 = lag(eps, 1),
    eps_lag2 = lag(eps, 2),
    eps_lag3 = lag(eps, 3),
    eps_lag4 = lag(eps, 4),
    eps_change_1 = eps - eps_lag1,
    eps_change_2 = eps_lag1 - eps_lag2,
    eps_change_3 = eps_lag2 - eps_lag3,
    eps_change_4 = eps_lag3 - eps_lag4
  ) %>%
  ungroup()

# Calculate the drift term (average of past changes)
compustat_data <- compustat_data %>%
  rowwise() %>%
  mutate(eps_drift = mean(c(eps_change_1, eps_change_2, eps_change_3, eps_change_4), na.rm = TRUE)) %>%
  ungroup()

# remove years to calculate drift
compustat_data <- compustat_data %>%
  filter(!(fyear >= 2008 & fyear <= 2012)) %>%
  filter(!(fyear == 2024))



# 4. Detrending - subtract drift from current EPS change
compustat_data <- compustat_data %>%
  mutate(eps_change_detrended = eps_change - eps_drift)

# 5. Create binary dependent variable from Chen et al. (2022)
compustat_data <- compustat_data %>%
  mutate(eps_increase = as.integer(eps_change_detrended > 0))

# delete NAs for dependent variable due to invisible obs
compustat_data <- compustat_data %>% filter(!is.na(eps_increase))

# fill NA sich code
compustat_data <- compustat_data %>%
  mutate(sich = case_when(
    conm == "DOVER CORP" ~ 3569,  # Dover Corp SIC
    conm == "KINDER MORGAN INC" ~ 4923,  # Kinder Morgan SIC
    TRUE ~ sich  # 保留其他公司的原 SIC 值
  ))



# Check NAs
colSums(is.na(compustat_data))

write_xlsx(compustat_data,path = "E:/me/RSM/thesis/data/compustat.xlsx")

# Calculate common financial ratios from literature
# Ou and Penman (1989), Sloan (1996), Piotroski (2000), etc.

find_ff48_industry <- function(sic_code) {
  # 遍历映射表的每一行
  for (i in 1:nrow(ff48_map)) {
    if (sic_code >= ff48_map$sic_start[i] & sic_code <= ff48_map$sic_end[i]) {
      return(ff48_map$ff48_code[i])
    }
  }
  return(NA)  # 如果没有找到匹配的区间
}

# 应用这个函数到 compustat_data 的每一行
trydata <- compustat_data %>%
  mutate(ffindustry = sapply(sich, find_ff48_industry))

# fill NA 
trydata <- trydata %>%
  mutate(ffindustry = case_when(
    conm == "HONEYWELL INTERNATIONAL INC" ~ 24,  
    conm == "BERKSHIRE HATHAWAY" ~ 45, 
    conm == "GE AEROSPACE" ~ 24,
    conm == "3M CO" ~ 14,
    TRUE ~ ffindustry  # 保留其他公司的原 SIC 值
  ))

colSums(is.na(trydata))

write_xlsx(trydata,path = "E:/me/RSM/thesis/data/trydata.xlsx")

# 设置一个函数：避免除以0
safe_divide <- function(numerator, denominator) {
  ifelse(is.na(denominator) | denominator == 0, NA, numerator / denominator)
}

# 基于行业均值插补的函数


fill_na_with_industry_mean <- function(df, var, industry_col = "ffindustry") {
  df %>%
    group_by(!!sym(industry_col)) %>%
    mutate("{var}" := ifelse(is.na(.data[[var]]), mean(.data[[var]], na.rm = TRUE), .data[[var]])) %>%
    ungroup()
}

# 先创建 CFO 变量（需要处理 NA）
trydata <- trydata %>%
  mutate(
    dp = ifelse(is.na(dp), 0, dp),  # 折旧为 NA 可视为 0
    CFO = ifelse(is.na(oancf), ni + dp, oancf),
    dltt = ifelse(is.na(dltt), 0, dltt),
    oibdp = ifelse(is.na(oibdp), 0, oibdp),
    act = ifelse(is.na(act), 0, act),
    lct = ifelse(is.na(lct), 0, lct),
    rect = ifelse(is.na(rect), 0, rect),
    capx = ifelse(is.na(capx), 0, capx),
    invt = ifelse(is.na(invt), 0, invt),
    lt = ifelse(is.na(lt), 0, lt)
  )

# Check NAs
colSums(is.na(trydata))
rm(trydata)


# 创建比率变量
trydata <- trydata %>%
  mutate(
    # Profitability
    ROA = safe_divide(ni, at),
    ROE = safe_divide(ni, ceq),
    EBIT_Margin = safe_divide(ebit, sale),
    Gross_Margin = safe_divide(sale - cogs, sale),
    CFO_to_Assets = safe_divide(CFO, at),
    Accruals = safe_divide(ni - CFO, at),
    
    # Liquidity
    Current_Ratio = safe_divide(act, lct),
    Quick_Ratio = safe_divide(act - invt, lct),
    Cash_Ratio = safe_divide(che, lct),
    
    # Leverage
    Debt_to_Equity = safe_divide(lt, ceq),
    Debt_to_Assets = safe_divide(lt, at),
    Long_Term_Debt_to_Assets = safe_divide(dltt, at),
    
    # Efficiency
    Asset_Turnover = safe_divide(sale, at),
    Inventory_Turnover = safe_divide(cogs, invt),
    Receivables_Turnover = safe_divide(sale, rect),
    
    # Market
    Market_Cap = csho * prcc_f,
    Book_Value = ceq,
    Market_to_Book = safe_divide(Market_Cap, Book_Value),
    Price_to_Earnings = safe_divide(prcc_f, safe_divide(ni, csho)),
    
    # Other
    Dividend_Yield = safe_divide(dvc, Market_Cap),
    CAPEX_to_Assets = safe_divide(capx, at)
  )


colSums(is.na(trydata))




# fill in industry mean
vars_to_fill <- c("Current_Ratio", "Quick_Ratio", "Cash_Ratio", "Inventory_Turnover", "Receivables_Turnover", "Market_Cap", "Market_to_Book","Price_to_Earnings","Price_to_Earnings","Dividend_Yield")

for (v in vars_to_fill) {
  trydata <- fill_na_with_industry_mean(trydata, v)
}

colSums(is.na(trydata))

# Winsorize all the data
ratio_vars <- c(
  "CFO", "ROA", "ROE", "EBIT_Margin", "Gross_Margin", "CFO_to_Assets", "Accruals",
  "Current_Ratio", "Quick_Ratio", "Cash_Ratio", "Debt_to_Equity", "Debt_to_Assets",
  "Long_Term_Debt_to_Assets", "Asset_Turnover", "Inventory_Turnover", "Receivables_Turnover",
  "Market_Cap", "Book_Value", "Market_to_Book", "Price_to_Earnings",
  "Dividend_Yield", "CAPEX_to_Assets"
)

winsorize_1_99 <- function(x) {
  qnt <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  x[x < qnt[1]] <- qnt[1]
  x[x > qnt[2]] <- qnt[2]
  return(x)
}
# 使用 mutate + across 一键 winsorize
finaldata <- trydata %>%
  mutate(across(
    all_of(ratio_vars),
    ~ winsorize_1_99(.)
  ))

finaldata <- finaldata %>%
  mutate(
    Current_Ratio = ifelse(is.na(Current_Ratio), 0, Current_Ratio),  # 折旧为 NA 可视为 0
    Quick_Ratio = ifelse(is.na(Quick_Ratio), 0, Quick_Ratio),
    Cash_Ratio = ifelse(is.na(Cash_Ratio), 0, Cash_Ratio)
  )

colSums(is.na(finaldata))
write_xlsx(finaldata,path = "E:/me/RSM/thesis/data/finalcompustat.xlsx")




# Create a more concise dataset with key variables only
key_variables <- c(
  # Identification information
  "gvkey", "datadate", "fyear", "tic", "conm", "cik",
  # Dependent variables
  "eps", "eps_next", "eps_change", "eps_drift", "eps_change_detrended", "eps_increase",
  # Profitability indicators
  "ROA", "ROE", "EBIT_Margin", "Gross_Margin", "CFO_to_Assets", "Accruals",
  # Liquidity indicators
  "Current_Ratio", "Quick_Ratio", "Cash_Ratio",
  # Leverage indicators
  "Debt_to_Equity", "Debt_to_Assets", "Long_Term_Debt_to_Assets",
  # Efficiency indicators
  "Asset_Turnover", "Inventory_Turnover", "Receivables_Turnover",
  # Market-related indicators
  "Market_Cap", "Market_to_Book", "Price_to_Earnings",
  # Piotroski F-Score
  "F_SCORE"
)

compustat_key_data <- compustat_data[, key_variables]
write.csv(compustat_key_data, "sp500_key_variables.csv", row.names = FALSE)

# Close the connection
dbDisconnect(con)

print("Completed! Extracted data and calculated Chen et al. (2022) EPS change metrics and other financial ratios.")