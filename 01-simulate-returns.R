library(tidyverse)

set.seed(123)

file_url = "https://raw.githubusercontent.com/wesslen/financial-decisions/master/public/returns.csv"

returns <- read_csv(file_url) %>%
  select(Year, equities_sp, treasury_10yr) %>%
  gather(key = "Fund", value = "Returns", -Year) %>%
  mutate(Fund = ifelse(Fund=="equities_sp",
                        "Stocks",
                        "Bonds"))

set.seed(123)

samplingA <- function(periods){
  z <- filter(returns, Fund == "Stocks") %>%
    dplyr::sample_n(periods, replace = TRUE) %>%
    select(Returns) %>%
    mutate(Returns = Returns + 1)
  
  return((prod(z$Returns) ^ (1 / periods)) - 1)
}

samplingB <- function(periods){
  z <- filter(returns, Fund == "Bonds") %>%
    dplyr::sample_n(periods, replace = TRUE) %>%
    select(Returns) %>%
    mutate(Returns = Returns + 1)
  
  return((prod(z$Returns) ^ (1 / periods)) - 1)
}

getReturns <- function(n, simN = 200){
  valA <- samplingA(n)
  valA <- valA[0]
  
  valB <- samplingB(n)
  valB <- valB[0]
  
  for (i in 1:simN){
    zA <- samplingA(n)
    valA <- append(valA, zA)
    
    zB <- samplingB(n)
    valB <- append(valB, zB)
  }
  
  return(list(valA, valB))
}

getOrder <- function(val, name, reps){
  tibble(returns = val) %>%
    arrange(desc(returns)) %>%
    mutate(id = rep(1:reps, each=1)) %>%
    group_by(id) %>%
    summarise(returns = mean(returns)) %>%
    mutate(id = as.factor(id),
           asset = name)
}


getCharts <- function(n){
  val <- getReturns(n, 200)
  
  bind_rows(getOrder(val[[1]], "Asset A: High risk, high return"),getOrder(val[[2]], "Asset B: Low risk, low return")) %>%
    ggplot(aes(x = fct_reorder(id, returns), y = returns)) +
    geom_col(width = 0.5) +
    facet_wrap(~asset, ncol = 2) +
    scale_y_continuous(sec.axis = dup_axis(), labels = scales::percent_format(accuracy = 0.1)) +
    cowplot::theme_minimal_hgrid() +
    theme(axis.text.x=element_blank()) +
    theme(text = element_text(size = 14)) +
    labs(x = "Annualized Returns", y = " ", title = paste0("Investing over ",n," Year(s)"))
}


n = 30
set.seed(123)
val <- getReturns(n, 33)

val2 <- getReturns(n, 100)

h <- bind_rows(getOrder(val2[[1]], "Fund A", reps = 100),getOrder(val2[[2]], "Fund B", reps = 100)) %>%
  mutate(asset = factor(asset, levels = c("Fund B", "Fund A")))

library(ggdist)

gfont <-   theme(
  text = element_text(size=8), 
  axis.text.x = element_text(size=8),
  axis.text.y = element_text(size=8)
) 


ggplot(h, aes(x = returns, y = asset, color = asset)) +
  stat_pointinterval() + 
  labs(subtitle = "Point Interval", y = " ", x = "Annualized Returns") +
  cowplot::theme_minimal_vgrid() +
  scale_x_continuous(labels = scales::percent) + 
  gfont +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  theme(legend.position = "none")


val <- getReturns(30, 200)
r <- bind_rows(getOrder(val[[1]], "Stocks", M),getOrder(val[[2]], "Bonds", M))


# run simulations
simulations=10000

r2 <- r %>% spread(asset, returns)

returns_df = tibble()

for (j in seq(0, 1, by = 0.01)){
  Apct = j 
  Bpct = 1 - Apct
  print(Apct)
  print(Bpct)
  sample = sample(1:100,
                  size = simulations,
                  replace = TRUE)
  
  r_sample = r2[sample,]
  
  expReturns = r_sample$Stocks * Apct + r_sample$Bonds * Bpct
  
  expReturns_df <- tibble("expReturns" = expReturns, "stockAllocation" = Apct, "bondsAllocation" = Bpct)
  returns_df <- dplyr::bind_rows(returns_df,expReturns_df)
  
}

returns_df <- returns_df %>%
  mutate(stockAllocation = as.factor(stockAllocation)) %>%
  select(-bondsAllocation) %>%
  mutate(percrank=rank(expReturns)/length(expReturns)) %>%
  mutate(percrank=round(percrank, 4),
         expReturns=round(expReturns, 4))

# visually validate the returns

library(ggridges)

expRet <- returns_df %>%
  mutate(stockAllocation = as.character(stockAllocation/100)) %>%
  filter(stockAllocation %in% seq(0,1,0.1)) %>%
  group_by(stockAllocation) %>%
  summarise(Mean=mean(expReturns))

sim_plot = returns_df %>%
  mutate(stockAllocation = as.character(stockAllocation/100)) %>%
  filter(stockAllocation %in% as.character(seq(0,1,0.1))) %>%
  ggplot(aes(x = expReturns, y = stockAllocation,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "", direction = -1, alpha = 0.4) +
  #ggridges::geom_density_ridges(alpha = 0.8, quantile_lines=TRUE) +
  cowplot::theme_minimal_vgrid() +
  labs(y = "Stock allocation", x = "Possible total investment rate of return",
       title = " ") +
  scale_y_discrete(labels=c("0" = "0%", 
                            "0.1" = "10%",
                            "0.2" = "20%",
                            "0.3" = "30%",
                            "0.4" = "40%",
                            "0.5" = "50%",
                            "0.6" = "60%",
                            "0.7" = "70%",
                            "0.8" = "80%",
                            "0.9" = "90%",
                            "1" = "100%"
                            )) +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  theme(text = element_text(size = 12)) 


sim_plot
pdf(file = "img/rq/sim_plot.pdf", useDingbats = FALSE, width = 6, height = 4)
sim_plot
dev.off()

## save simulated returns

readr::write_csv(returns_df, "data/simulated-returns.csv")
feather::write_feather(returns_df, "data/simulated-returns.feather")

avg_returns <- returns_df %>%
  #mutate(stockAllocation = as.factor(stockAllocation)) %>%
  group_by(stockAllocation) %>%
  summarise(
    mean=mean(expReturns),
    sd=sd(expReturns)
  ) 

pick <- function(condition){
  function(d) d %>% filter_(condition)
}


eff_frontier <- avg_returns %>%
  mutate(label = if_else(stockAllocation%%25==0,paste0(as.character(stockAllocation),"%"),"")) %>%
  ggplot(aes(x = sd, y = mean) ) +
  ggrepel::geom_label_repel(aes(label = label)) +
  #colorblindr::scale_color_OkabeIto() +
  geom_path(size=0.5) +
  geom_point(data=pick(~label != "")) +
  cowplot::theme_minimal_grid() +
  labs(title = "Efficient Frontier",
       subtitle = "10,000 bootstrap historical simulation by stock allocation",
       y = "Expected Return",
       x = "Standard Deviation") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), 
    limits = c(0.04,0.1),
    n.breaks = 8
  ) +
  scale_x_continuous(
    labels = scales::percent,
    n.breaks = 8
    )


eff_frontier
pdf(file = "img/rq/eff_frontier.pdf", useDingbats = FALSE, width = 6, height = 4)
eff_frontier
dev.off()

lm(mean ~ stockAllocation, data = avg_returns)

#returns_df <- feather::read_feather("data/simulated-returns.feather")
