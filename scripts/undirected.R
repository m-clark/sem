library(dplyr); library(magrittr)

d3heatmap::d3heatmap(cor(state.x77))
d = data.frame(state.x77) 
d %<>%
  select(Life.Exp, Income, HS.Grad, Illiteracy)

distmat = as.matrix(dist(scale(d), diag=T, upper=T))
adj = 1/distmat; diag(distmat) = 1

# networkd3 
library(networkD3)
library(scales)
?forceNetwork
stateLinks = reshape2::melt(adj) %>% 
  rename(Source=Var1,
         Target=Var2,
         Value=value) %>% 
  mutate(Source = as.numeric(Source)-1,
         Target = as.numeric(Target)-1) %>% #Value = round(Value*10)
  filter(Value!=Inf, Value>=.6)
stateNodes = data.frame(ID=rownames(d), Region=as.numeric(factor(state.region)), 
                        Division=as.numeric(factor(state.division)))
forceNetwork(Links=stateLinks, Nodes=stateNodes, Source='Source', Target='Target', 
             Value='Value', NodeID='ID', Group='Region', linkColour='#BFBFBF', #alpha('gray90',1)
             opacity=.8, colourScale='d3.scale.category10()')

save(stateLinks, stateNodes, file='data/statesNetwork')
