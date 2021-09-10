library(tidyverse)
library(ggdist)

file_url = "https://raw.githubusercontent.com/wesslen/financial-decisions/master/public/returns.csv"

returns <- read_csv(file_url) %>%
  select(Year, equities_sp, treasury_10yr) %>%
  gather(key = "Fund", value = "Returns", -Year) %>%
  mutate(Fund = ifelse(Fund=="equities_sp",
                        "Stocks",
                        "Bonds"))

# correlation
cor(returns$Returns[returns$Fund=="Stocks"],returns$Returns[returns$Fund=="Bonds"])

set.seed(123)

file_url = "https://raw.githubusercontent.com/wesslen/financial-decisions/master/public/returns.csv"

raw_df <- read_csv(file_url) %>%
  select(-treasury_3mo) %>%
  rename(Stocks = equities_sp,
         Bonds = treasury_10yr)

getBootstrap <- function(df, num_year_sim = 30){
  samp1 <- df %>%
    sample_n(size = num_year_sim, replace = TRUE) %>%
    mutate(Stocks = Stocks + 1, Bonds = Bonds + 1) 
  
  # get geometric means (annualize 30 year returns)
  stocks = (prod(samp1$Stocks) ^ (1 / num_year_sim)) - 1
  bonds = (prod(samp1$Bonds) ^ (1 / num_year_sim)) - 1
  
  return_df <- data.frame(stocks = stocks, bonds = bonds)
  
  return(return_df)
}

reps = 33

boot_33_1yr_samples <- reps %>% 
  rerun(getBootstrap(raw_df, 1))%>%
  map_df(bind_rows) %>%
  gather("fund", "returns") %>%
  group_by(fund) %>%
  mutate(rank = rank(returns, ties.method = "first")) %>%
  mutate(rank = factor(rank))

g <- boot_33_1yr_samples %>%
  ggplot(aes(x = fct_reorder(rank, returns), y = returns, fill = fund)) +
  #scale_x_continuous(labels = scales::percent) + 
  cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(size=8), 
    axis.text.y = element_text(size=8),
    axis.text.x=element_blank()
  ) +
  scale_y_continuous(labels = scales::percent)

gfont <-   theme(
  text = element_text(size=8), 
  axis.text.x = element_text(size=8),
  axis.text.y = element_text(size=8)
) 

g1 <- g + geom_col(width = 0.5) + facet_wrap(~fund, ncol = 2) +
  
  labs(x = " ", y = "Annualized Returns", subtitle = "Bar Chart 1") +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme(legend.position = "none")

# change legend position if different evaluation period!

g2 <-  boot_33_1yr_samples %>%
  ggplot(aes(x = fct_reorder(rank, returns), y = returns, fill = fund)) +
  cowplot::theme_minimal_hgrid() +
  scale_y_continuous(labels = scales::percent) + 
  gfont +
  theme(legend.position = c(0.2,0.8), axis.text.x=element_blank()) + # 30 = c(0.2,0.8), 1 =  c(0.7,0.25)
  geom_bar(width = 0.5, stat="identity", position = "dodge") +
  labs(x = " ", y = "Annualized Returns",  subtitle = "Bar Chart 2", fill = " ") +
  scale_fill_manual(values=c("#999999", "#E69F00"))

## for advanced visualizations: sample n = 1000

boot_100_1yr_samples <- 100 %>% 
  rerun(getBootstrap(raw_df, 1))%>%
  map_df(bind_rows) %>%
  gather("fund", "returns") %>%
  group_by(fund) %>%
  mutate(rank = rank(returns, ties.method = "first")) %>%
  mutate(rank = factor(rank))

h <- boot_100_1yr_samples %>%
  mutate(fund = factor(fund, levels = c("bonds", "stocks")))

g3 <- ggplot(h, aes(x = returns, y = fund, color = fund, color_ramp = stat(level))) +
  stat_interval(alpha = 0.8) + 
  labs(subtitle = "Interval", y = " ", x = "Annualized Returns", fill = " ", level = " ") +
  cowplot::theme_minimal_vgrid() +
  scale_x_continuous(labels = scales::percent) + 
  theme(legend.position = "none") +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  gfont  

g4 <- ggplot(h, aes(x = returns, y = fund, color = fund)) +
  stat_pointinterval() + 
  labs(subtitle = "Point Interval", y = " ", x = "Annualized Returns") +
  cowplot::theme_minimal_vgrid() +
  scale_x_continuous(labels = scales::percent) + 
  gfont +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  theme(legend.position = "none")

g5 <- ggplot(h, aes(x = returns, y = fund, fill = fund)) +
  stat_slab(alpha = 0.5) + labs(subtitle = "Density", y = " ", x = "Annualized Returns") +
  cowplot::theme_minimal_vgrid() +
  scale_x_continuous(labels = scales::percent) + 
  gfont +
  scale_fill_manual(values=c("#E69F00", "#999999")) +
  theme(legend.position = "none")

g6 <- ggplot(h, aes(x = returns, y = fund, fill = fund)) + 
  stat_gradientinterval(interval_alpha = 0, point_alpha = 0) + 
  labs(subtitle = "Gradient Interval", y = " ", x = "Annualized Returns") +
  cowplot::theme_minimal_vgrid() +
  scale_x_continuous(labels = scales::percent) + 
  gfont +
  scale_fill_manual(values=c("#E69F00", "#999999")) +
  theme(legend.position = "none")

g7 <- ggplot(h, aes(x = returns, y = fund, fill = fund)) + 
  stat_histinterval(alpha = 0.5, interval_alpha = 0, point_alpha = 0, outline_bars = TRUE, breaks = 10, slab_color = "gray20") + 
  labs(subtitle = "Histogram Interval", y = " ", x = "Annualized Returns") +
  cowplot::theme_minimal_vgrid() +
  scale_x_continuous(labels = scales::percent) + 
  gfont +
  scale_fill_manual(values=c("#E69F00", "#999999")) +
  theme(legend.position = "none")

g8 <- ggplot(h, aes(x = returns, y = fund, fill = fund)) + 
  stat_cdfinterval(alpha = 0.5, interval_alpha = 0, point_alpha = 0) + 
  labs(subtitle = "CDF Interval", y = " ", x = "Annualized Returns") +
  cowplot::theme_minimal_vgrid() +
  scale_x_continuous(labels = scales::percent) + 
  gfont +
  scale_fill_manual(values=c("#E69F00", "#999999")) +
  theme(legend.position = "none")

g9 <- ggplot(h, aes(x = returns, y = fund, fill = fund)) + 
  stat_ccdfinterval(alpha = 0.5, interval_alpha = 0, point_alpha = 0) + 
  labs(subtitle = "CCDF Interval", y = " ", x = "Annualized Returns") +
  cowplot::theme_minimal_vgrid() +
  scale_x_continuous(labels = scales::percent) + 
  gfont +
  scale_fill_manual(values=c("#E69F00", "#999999")) +
  theme(legend.position = "none")

g10 <- ggplot(h, aes(x = returns, y = fund, fill = fund)) + 
  stat_dots(position = "dodge", quantiles = 20, color = NA) + 
  labs(subtitle = "Quantile Dotplot", y = " ", x = "Annualized Returns") +
  cowplot::theme_minimal_vgrid() +
  scale_x_continuous(labels = scales::percent) + 
  gfont +
  scale_fill_manual(values=c("#E69F00", "#999999")) +
  theme(legend.position = "none")

library(patchwork)

right_g <- (g3 / g4 / g5 / g6) | (g7 / g8 / g9 / g10)

gsave <- ( g1 / g2 ) | right_g 

gsave %>%
  ggsave(width = 10, height = 6, filename = paste0("img/design-space-display-",1,"year.png"))
