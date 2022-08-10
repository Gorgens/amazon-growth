A apresentar o grupo de interesse comercial...

```{r}
commercialSpecies = read.csv('auxiliar/comercial.csv')
commercial = commercialSpecies %>% group_by(comercial) %>% summarise(Species = n())
kable(commercial, col.names = c('Commecial interest', 'Number of species'), caption = 'Number of species associated to commercial intesrest.')
```

```{r}
incComercial = invMerged %>%
  drop_na(comercial) %>%
  group_by(area, plot, subplot, tree, scientific.name, comercial) %>%
  summarise(med = n(),
            cc = min(cc), 
            minDBH = min(DBH), 
            maxDBH = max(DBH), 
            inc = max(DBH) - min(DBH), 
            intervMed = max(year) - min(year), 
            incAnual = ifelse(inc == 0, 0.01, inc / intervMed),
            rInc = incAnual / minDBH) %>%
  filter(med > 1)
```


kable(eco, col.names = c('Ecological groups', 'Number of species'), caption = 'Number of species associated to each ecological group.')

parcelasArea = invMerged %>%
  group_by(area, plot, subplot, year) %>%
  summarise(obs = n()) %>%
  group_by(area, year) %>%
  summarise(nplots = n()) %>%
  group_by(area) %>%
  summarise(nplots = mean(nplots))




Calcular o tempo de residência... para sair do 10 cm e chegar no 50 cm...

$$d_f = d_i (1+r)^t$$
  
  ```{r}

```

## Comunidade

Resumo de todo o conjunto de dados....

```{r}
numeroArvores = invMerged %>%
  filter(DBH >= 10) %>%
  group_by(area, plot, subplot, year, cc) %>% 
  summarise(ntree = sum(eqTree))  %>%
  drop_na(ntree) %>%
  group_by(area, cc) %>%
  summarise(narv = sum(ntree)) %>%
  left_join(parcelasArea) %>%
  mutate(arvha = narv / nplots) %>%
  group_by(cc) %>%
  summarise(arvha = mean(arvha))

ggplot(numeroArvores, aes(cc, arvha)) + geom_col() +
  xlab('Diameter distribution') + ylab('Trees per hectare') +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
```


```{r hipsometria}
ggplot(invMerged, aes(DBH, Htot)) +                                
  geom_point(alpha = 0.1) + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
```


Gráfico apresentando a distribuição do crescimento diamétrico relativo...

```{r graficoIncremento}
ggplot(incEco, aes(rInc)) +                                
  geom_density() +
  xlim(0, 0.5) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
```

Modelo do crescimento relativo...


```{r}
incGeneral <- stan_glm(
  rInc ~ 1,
  data = incEco,
  family = Gamma(link="log"),
)
```


```{r}
plot(incGeneral, plotfun = "areas", prob = 0.9, pars = c("(Intercept)"))
```

Distribuição dos valores de crescimento relativo do diâmetro por grupo ecologico...


```{r}
ggplot(incEco, aes(rInc, colour = GrupoEco)) +                                
  geom_density() +
  xlim(0, 0.5) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
```


### Pioneiras

```{r}
grupo = "Pioneer"

grupoEcologico = inv.paisagens %>%
  filter(GrupoEco == grupo)
```

```{r}
filogenia = grupoEcologico %>%                                                    # filogenia das spécies estudadas
  group_by(scientific.name, genera.name, family.name) %>%
  summarise(n = n()) %>%
  drop_na(family.name) %>%
  filter(!is.na(family.name)) %>%
  filter(family.name != 'NI')
```

```{r cache=TRUE}
phy = phylo.maker(filogenia[,1:3])
tree = phy$scenario.3
plotTree(tree, type='fan', fsize=0.5, lwd=1, ftype='i')
```


```{r}
grupo = "Pioneer"

grupoEcologico = inv.paisagens %>%
  filter(GrupoEco == grupo)

numeroArvores = inv.paisagens %>%
  filter(GrupoEco == grupo) %>%
  filter(DBH >= 10) %>%
  group_by(area, plot, subplot, year, cc) %>% 
  summarise(ntree = sum(eqTree))  %>%
  drop_na(ntree) %>%
  group_by(area, cc) %>%
  summarise(narv = sum(ntree)) %>%
  left_join(parcelasArea) %>%
  mutate(arvha = narv / nplots) %>%
  group_by(cc) %>%
  summarise(arvha = mean(arvha))

ggplot(numeroArvores, aes(cc, arvha)) + geom_col() +
  xlab('Diameter distribution') + ylab('Trees per hectare') +
  ggtitle(paste0(grupo)) + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
```

### Demandantes de luz

```{r}
grupo = "Light-demanding" 

grupoEcologico = inv.paisagens %>%
  filter(GrupoEco == grupo)
```

```{r}
filogenia = grupoEcologico %>%                                                    # filogenia das spécies estudadas
  group_by(scientific.name, genera.name, family.name) %>%
  summarise(n = n()) %>%
  drop_na(family.name) %>%
  filter(!is.na(family.name)) %>%
  filter(family.name != 'NI')
```

```{r cache=TRUE}
phy = phylo.maker(filogenia[,1:3])
tree = phy$scenario.3
plotTree(tree, type='fan', fsize=0.5, lwd=1, ftype='i')
```

```{r}
#for(i in unique(inv.paisagens.filtered$GrupoEco)){
numeroArvores = inv.paisagens %>%
  filter(GrupoEco == grupo) %>%
  filter(DBH >= 10) %>%
  group_by(area, plot, subplot, year, cc) %>% 
  summarise(ntree = sum(eqTree))  %>%
  drop_na(ntree) %>%
  group_by(area, cc) %>%
  summarise(narv = sum(ntree)) %>%
  left_join(parcelasArea) %>%
  mutate(arvha = narv / nplots) %>%
  group_by(cc) %>%
  summarise(arvha = mean(arvha))

ggplot(numeroArvores, aes(cc, arvha)) + geom_col() +
  xlab('Diameter distribution') + ylab('Trees per hectare') +
  ggtitle(paste0(grupo)) + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
#}
```

### Intermediárias

```{r}
grupo = "Intermediate"

grupoEcologico = inv.paisagens %>%
  filter(GrupoEco == grupo)
```

```{r}
filogenia = grupoEcologico %>%                                                    # filogenia das spécies estudadas
  group_by(scientific.name, genera.name, family.name) %>%
  summarise(n = n()) %>%
  drop_na(family.name) %>%
  filter(!is.na(family.name)) %>%
  filter(family.name != 'NI')
```

```{r cache=TRUE}
phy = phylo.maker(filogenia[,1:3])
tree = phy$scenario.3
plotTree(tree, type='fan', fsize=0.5, lwd=1, ftype='i')
```

```{r}
#for(i in unique(inv.paisagens.filtered$GrupoEco)){
numeroArvores = inv.paisagens %>%
  filter(GrupoEco == grupo) %>%
  filter(DBH >= 10) %>%
  group_by(area, plot, subplot, year, cc) %>% 
  summarise(ntree = sum(eqTree))  %>%
  drop_na(ntree) %>%
  group_by(area, cc) %>%
  summarise(narv = sum(ntree)) %>%
  left_join(parcelasArea) %>%
  mutate(arvha = narv / nplots) %>%
  group_by(cc) %>%
  summarise(arvha = mean(arvha))

ggplot(numeroArvores, aes(cc, arvha)) + geom_col() +
  xlab('Diameter distribution') + ylab('Trees per hectare') +
  ggtitle(paste0(grupo)) + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
#}
```

### Shade-tolerant

```{r}
grupo = "Shade-tolerant"

grupoEcologico = inv.paisagens %>%
  filter(GrupoEco == grupo)
```

```{r}
filogenia = grupoEcologico %>%                                                    # filogenia das spécies estudadas
  group_by(scientific.name, genera.name, family.name) %>%
  summarise(n = n()) %>%
  drop_na(family.name) %>%
  filter(!is.na(family.name)) %>%
  filter(family.name != 'NI')
```

```{r cache=TRUE}
phy = phylo.maker(filogenia[,1:3])
tree = phy$scenario.3
plotTree(tree, type='fan', fsize=0.5, lwd=1, ftype='i')
```

```{r}
#for(i in unique(inv.paisagens.filtered$GrupoEco)){
numeroArvores = inv.paisagens %>%
  filter(GrupoEco == grupo) %>%
  filter(DBH >= 10) %>%
  group_by(area, plot, subplot, year, cc) %>% 
  summarise(ntree = sum(eqTree))  %>%
  drop_na(ntree) %>%
  group_by(area, cc) %>%
  summarise(narv = sum(ntree)) %>%
  left_join(parcelasArea) %>%
  mutate(arvha = narv / nplots) %>%
  group_by(cc) %>%
  summarise(arvha = mean(arvha))

ggplot(numeroArvores, aes(cc, arvha)) + geom_col() +
  xlab('Diameter distribution') + ylab('Trees per hectare') +
  ggtitle(paste0(grupo)) + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
#}
```

### Emergente demandante de luz

```{r}
grupo = "Emergent"

grupoEcologico = inv.paisagens %>%
  filter(GrupoEco == grupo)
```

```{r}
filogenia = grupoEcologico %>%                                                    # filogenia das spécies estudadas
  group_by(scientific.name, genera.name, family.name) %>%
  summarise(n = n()) %>%
  drop_na(family.name) %>%
  filter(!is.na(family.name)) %>%
  filter(family.name != 'NI')
```

```{r cache=TRUE}
phy = phylo.maker(filogenia[,1:3])
tree = phy$scenario.3
plotTree(tree, type='fan', fsize=0.5, lwd=1, ftype='i')
```

```{r}
#for(i in unique(inv.paisagens.filtered$GrupoEco)){
numeroArvores = inv.paisagens %>%
  filter(GrupoEco == grupo) %>%
  filter(DBH >= 10) %>%
  group_by(area, plot, subplot, year, cc) %>% 
  summarise(ntree = sum(eqTree))  %>%
  drop_na(ntree) %>%
  group_by(area, cc) %>%
  summarise(narv = sum(ntree)) %>%
  left_join(parcelasArea) %>%
  mutate(arvha = narv / nplots) %>%
  group_by(cc) %>%
  summarise(arvha = mean(arvha))

ggplot(numeroArvores, aes(cc, arvha)) + geom_col() +
  xlab('Diameter distribution') + ylab('Trees per hectare') +
  ggtitle(paste0(grupo)) + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
#}
```


```{r}
posterior_vs_prior(incGrupoEco, regex_pars = c("(Intercept)", "Grupo"))
```


## Comercial

Frequência de indivíduos por interesse comercial...

```{r}
arvHaEspecie = inv.paisagens %>%
  drop_na(comercial) %>%
  filter(DBH >= 10) %>%
  group_by(area, plot, subplot, year, comercial) %>% 
  summarise(ntree = sum(eqTree)) %>%
  drop_na(ntree) %>%
  group_by(area, plot, subplot, comercial) %>%
  summarise(ntree = mean(ntree)) %>%
  group_by(area, comercial) %>%
  summarise(ntree = sum(ntree)) %>%
  left_join(parcelasArea) %>%
  mutate(arvha = ntree / nplots) %>%
  group_by(comercial) %>%
  summarise(arvha = mean(arvha))

arvHaEspecie
```


```{r}
ggplot(incComercial, aes(rInc, minDBH)) +                                
  geom_point(alpha = 0.1) + 
  xlim(0, 0.5) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black")) +
  facet_wrap(~comercial)
```

```{r}
ggplot(incComercial, aes(rInc, colour = factor(comercial))) +                                
  geom_density() +
  xlim(0, 0.5) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
```

```{r}
grupo = 1

comercial = inv.paisagens %>%
  filter(comercial == grupo)
```

```{r}
filogenia = comercial %>%                                                    # filogenia das spécies estudadas
  group_by(scientific.name, genera.name, family.name) %>%
  summarise(n = n()) %>%
  drop_na(family.name) %>%
  filter(!is.na(family.name)) %>%
  filter(family.name != 'NI')
```

```{r cache=TRUE}
phy = phylo.maker(filogenia[,1:3])
tree = phy$scenario.3
plotTree(tree, type='fan', fsize=0.5, lwd=1, ftype='i')
```

```{r}
#for(i in c(0, 1)){
numeroArvores = inv.paisagens %>%
  filter(comercial == 1) %>%
  filter(DBH >= 10) %>%
  group_by(area, plot, subplot, year, cc) %>% 
  summarise(ntree = sum(eqTree))  %>%
  drop_na(ntree) %>%
  group_by(area, cc) %>%
  summarise(narv = sum(ntree)) %>%
  left_join(parcelasArea) %>%
  mutate(arvha = narv / nplots) %>%
  group_by(cc) %>%
  summarise(arvha = mean(arvha))

ggplot(numeroArvores, aes(cc, arvha)) + geom_col() +
  xlab('Diameter distribution') + ylab('Trees per hectare') +
  ggtitle('commercial') + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))
#}
```

```{r}
incrementoGrupo = incComercial %>%
  group_by(comercial) %>%
  summarise(incDesv = IQR(rInc, na.rm = TRUE), 
            rInc = median(rInc), 
            tp = 50 / rInc)

incrementoGrupo
```

```{r}
incGrupoCom <- stan_glm(
  rInc ~ factor(comercial),
  data = incComercial,
  family = Gamma(link="log"),
  prior_intercept = normal(-4, 0.1)
)
```

```{r}
bayesplot::color_scheme_set("viridis")
plot(incGrupoCom, plotfun = "areas", prob = 0.9, regex_pars = c("(Intercept)", "factor"))
```

```{r}
posterior_vs_prior(incGrupoCom, regex_pars = c("(Intercept)", "factor"))
```



