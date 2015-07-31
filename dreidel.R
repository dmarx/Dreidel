# dreidel

game = function(players, money){
  pool = players
  wallets = rep(floor(money/players), players)
  live = 1:players
  turn = 0
  turncount = 0
  while(length(live) >1){
    turn = ifelse(turn==players, 1, turn + 1)
    if (wallets[turn]<1){
      next # continue
    }
    turncount = turncount + 1
    roll = sample(4, 1)
    if(roll==1){
      # player gets pool, all live players donate a token
      wallets[turn] = pool
      pool = length(live)
      wallets[live] = wallets[live] - 1
      live = which(wallets>0)      
      
    } else if(roll==2){
      # Player gets half of pool
      earn = floor(pool/2)
      pool = pool-earn
      wallets[turn] = wallets[turn] + pool
    } else if(roll==3){
      # Player donates 3
      loss = min(wallets[turn], 3)
      wallets[turn] = wallets[turn] - loss
      pool = pool + loss
      live = which(wallets>0)      
    }
    # Treat "4" as "0" - No action
  }
  turncount
}


# Game implemented in 10 minutes

# simulation implemented... really fast
mean(replicate(1e2,game(6, 100)))

# Simulation wrapped ~ 10min

results = c()
k=1e2
players=4:15
pots=c((4:10)*10, (2:5)*100)
for(n in players){
  for(p in pots){
    p = floor(p/n)*n
    #sim = mean(replicate(k,game(n, p)))
    #results = rbind(results, c(n,p,sim))
    for(i in 1:k){
      results = rbind(results, c(n, p, game(n, p)))
    }
  }
}

df = as.data.frame(results)
names(df) = c("players", "pot", "duration")

unique(df$players)
plot(duration~pot, df[df$players==5,])
plot(duration~pot, df[df$players==10,])
plot(duration~players, df[df$pot>=400,])

mod = lm(duration ~ pot + players, df)
summary(mod)
#mod = lm(duration ~ poly(pot,2) + poly(players,2), df)
#summary(mod)

anova(mod)
