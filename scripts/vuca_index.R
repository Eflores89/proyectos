### VUCA Index for Mexico
### v0 - First try
library(banxicoR)
library(inegiR)
token_inegi <- "xxxx"

library(ggplot2)
library(eem)


# Volatility: Mexican stock exchange open and closing gaps
vuca_v <- function(token_inegi){
  
  lows <- inegi_series(series = inegi_code("15321"), 
                       token = token_inegi)
  highs <- inegi_series(series = inegi_code("15322"), 
                        token = token_inegi)
  
  d <- data.frame("v" = (lows$Values - highs$Values)/lows$Values*100, 
                  "dates" = highs$Dates)
  d
}

# Uncertainty: Difficulty to model fx rates in survey
vuca_u <- function(){
  std_dev <- banxico_series(series = "SR14880")
  fx <- banxico_series(series = "SF17909")
  
  names(fx) <- c("dates", "fx")
  names(std_dev) <- c("dates", "std")
  
  std_dev <- std_dev %>% 
    left_join(., fx) %>%
    mutate("u" = std/fx*100) %>%
    select(c("dates", "u"))
  
  std_dev
}

# Complexity: Relative prices of inputs, standard devs.
vuca_c <- function(token_inegi) {
  
  i1 <- inegiR::inegi_series(series = inegi_code("364705"), token = token_inegi)
  names(i1) <- c("s1", "dates")
  i2 <- inegiR::inegi_series(series = inegi_code("364710"), token = token_inegi)
  names(i2) <- c("s2", "dates")
  i3 <- inegiR::inegi_series(series = inegi_code("364711"), token = token_inegi)
  names(i3) <- c("s3", "dates")
  i4 <- inegiR::inegi_series(series = inegi_code("364714"), token = token_inegi)
  names(i4) <- c("s4", "dates")
  i5 <- inegiR::inegi_series(series = inegi_code("364717"), token = token_inegi)
  names(i5) <- c("s5", "dates")
  i6 <- inegiR::inegi_series(series = inegi_code("364739"), token = token_inegi)
  names(i6) <- c("s6", "dates")
  i7 <- inegiR::inegi_series(series = inegi_code("364749"), token = token_inegi)
  names(i7) <- c("s7", "dates")
  i8 <- inegiR::inegi_series(series = inegi_code("364755"), token = token_inegi)
  names(i8) <- c("s8", "dates")
  i9 <- inegiR::inegi_series(series = inegi_code("364758"), token = token_inegi)
  names(i9) <- c("s9", "dates")
  i10 <- inegiR::inegi_series(series = inegi_code("364760"), token = token_inegi)
  names(i10) <- c("s10", "dates")
  i11 <- inegiR::inegi_series(series = inegi_code("364763"), token = token_inegi)
  names(i11) <- c("s11", "dates")
  i12 <- inegiR::inegi_series(series = inegi_code("364765"), token = token_inegi)
  names(i12) <- c("s12", "dates")
  i13 <- inegiR::inegi_series(series = inegi_code("364769"), token = token_inegi)
  names(i13) <- c("s13", "dates")
  i14 <- inegiR::inegi_series(series = inegi_code("364772"), token = token_inegi)
  names(i14) <- c("s14", "dates")
  i15 <- inegiR::inegi_series(series = inegi_code("364775"), token = token_inegi)
  names(i15) <- c("s15", "dates")
  
  d <- i1 %>% 
    left_join(., i2) %>% 
    left_join(., i3) %>%
    left_join(., i4) %>%
    left_join(., i5) %>%
    left_join(., i6) %>%
    left_join(., i7) %>%
    left_join(., i8) %>%
    left_join(., i9) %>%
    left_join(., i10) %>%
    left_join(., i11) %>%
    left_join(., i12) %>%
    left_join(., i13) %>%
    left_join(., i14) %>%
    left_join(., i15) %>% 
    rowwise() %>%
    mutate("c" = sd(c(s1, s2, s3, s4, s5, s6, s7, s8,
                      s9, s10, s11, s12, s13, s14, s15))) %>%
    select(c("dates", "c"))
  
  d
}

# Ambiguity: Not sure to invest in survey
vuca_a <- function(){
  not_sure <- banxico_series("SR15035")
  names(not_sure) <- c("dates", "a")
  not_sure
}


vuca <- function(token_inegi, scales = c(0.25, 0.25, 0.25, 0.25)){
  df <- vuca_v(token_inegi = token_inegi) %>% 
    left_join(., vuca_u()) %>%
    left_join(., vuca_c(token_inegi = token_inegi)) %>%
    left_join(., vuca_a()) 

  df <- df[complete.cases(df), ]
  df <- as.tibble(df) %>% 
    mutate("v_ind" = v/first(v)*100, 
           "u_ind" = u/first(u)*100, 
           "c_ind" = c/first(c)*100, 
           "a_ind" = a/first(a)*100) %>%
    mutate("vuca" = v_ind*scales[1] + u_ind*scales[2] + c_ind*scales[3] + a_ind*scales[4] ) %>%
    tq_mutate(select = v, 
              mutate_fun = runMean, 
              n = 12, col_rename = "vuca_12m") # running 12m average
  
  df
}


# Downloading the vuca index
d <- vuca(token_inegi = token_inegi)


# graph
ggplot(d, aes(x = dates, y = vuca)) + 
  geom_line(color = eem_colors[1]) + 
  geom_smooth(color = eem_colors[3]) +
  eem::theme_eem() + 
  labs(title = "VUCA Index for Mexico", 
       x = "Dates" , y = "Index (100 = 2010/07)")





