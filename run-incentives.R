
## get responses

rr <- all$responses %>%
  flatten()

#rr$usertoken <- all$usertoken

tt <- rr %>% 
  bind_rows(.id = "trial") %>%
  mutate(stockAllocation = if_else(left == "stocks",allocationLeft, allocationRight)) %>%
  mutate(time = anytime::anytime(time/1000)) %>%
  mutate(n_trial = if_else(task == "task1", as.integer(trial)+1, as.integer(trial)+8)) %>%
  filter(trial < 7)


print(paste0("Number of unique users ",length(unique(all$usertoken))))

# check if unique users equal row count of 14 * usercount
assertthat::are_equal(length(unique(all$usertoken))*14,nrow(tt))

## not good practice -- need better
tt$usertoken <- rep(all$usertoken, 7)
tt$treatment <- rep(all$treatment, 7)

### get past granted

granted <- read_csv("data/incentives.csv") 

### remove anyone who has not completed everything

usersComplete <- tt %>%
  filter(!is.na(stockAllocation))  %>%
  filter(!is.na(usertoken)) %>%
  count(usertoken) %>%
  filter(n > 13)

arrays <- tt %>%
  filter(usertoken %in% usersComplete$usertoken) %>%
  filter(!(usertoken %in% granted$usertoken))

## calculate incentives

df <- feather::read_feather("data/simulated-returns.feather")

df$stockAllocation <- as.character(df$stockAllocation)


getSimulation <- function(stock){
  
  samp <- df[df$stockAllocation == as.character(stock),]
  samp <- samp[sample(nrow(samp), 1), ]
  samp$decile <- as.integer(cut(samp$percrank, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,Inf), ))
  samp$payment <-
    if(samp$decile < 6 ){
      "$0.00"
    } else if(samp$decile == 6 ){
      "$0.05"
    } else if(samp$decile == 7 ){
      "$0.1"
    } else if(samp$decile == 8 ){
      "$0.15"
    } else if(samp$decile == 9 ){
      "$0.20"
    } else if(samp$decile == 10 ){
      "$0.25"
    } else {"$0.00"}
  
  samp$payment2 <-
    if(samp$decile < 6 ){
      0
    } else if(samp$decile == 6 ){
      0.05
    } else if(samp$decile == 7 ){
      0.1
    } else if(samp$decile == 8 ){
      0.15
    } else if(samp$decile == 9 ){
      0.2
    } else if(samp$decile == 10 ){
      0.25
    } else {0}
  
  return(samp)
}

# run the incentives
incentives <- bind_cols(arrays %>% select(-stockAllocation),
                        purrr::map_df(arrays$stockAllocation,getSimulation))

sumIncentives <- incentives %>%
  group_by(usertoken) %>%
  summarise(total_incentives = sum(payment2))

write_csv(sumIncentives, "data/temp-incentives.csv")
#### 


sumIncentives$paid <- "Yes" ### change this to "yes" after payment; also update the csv file

bind_rows(granted,sumIncentives) %>%
  write_csv("data/incentives.csv")
