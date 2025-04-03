
####################################################################################################################
###############################  Descritive statistics ################################################################
##################################################################################################################

library(readxl)
library(networkD3)

data <- General_data_adjusted

sankey_data <- data[, c("Stressor", "Kingdom", "Category")]
nodes <- data.frame(name = unique(c(sankey_data$Stressor, sankey_data$Kingdom, sankey_data$Category)))

# Mapear os índices dos nós para as colunas
sankey_data$source <- match(sankey_data$Stressor, nodes$name) - 1
sankey_data$target_middle <- match(sankey_data$Kingdom, nodes$name) - 1
sankey_data$target_right <- match(sankey_data$Category, nodes$name) - 1

# Criar os links (conexões)
links_middle <- data.frame(
  source = sankey_data$source,
  target = sankey_data$target_middle,
  value = 1,
  group = sankey_data$Kingdom
)

links_right <- data.frame(
  source = sankey_data$target_middle,
  target = sankey_data$target_right,
  value = 1,
  group = sankey_data$Kingdom
)

# Combinar os links
links <- rbind(links_middle, links_right)

# Criar o gráfico Sankey
sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  LinkGroup = "group",
  units = "T",
  fontSize = 12,
  nodeWidth = 30
)

# Customizar os rótulos com JavaScript
sankey <- htmlwidgets::onRender(
  sankey,
  "
  function(el, x) {
    // Ajustar rótulos da coluna Stressor (mais à esquerda)
    d3.select(el).selectAll('.node text')
      .filter(function(d) { return d.name && x.nodes[d.index].name.startsWith('Stressor'); })
      .attr('x', -100) // Move para a esquerda
      .attr('text-anchor', 'end'); // Alinha à direita

    // Ajustar rótulos da coluna Category (mais à direita)
    d3.select(el).selectAll('.node text')
      .filter(function(d) { return d.name && x.nodes[d.index].name.startsWith('Category'); })
      .attr('x', 100) // Move para a direita
      .attr('text-anchor', 'start'); // Alinha à esquerda
  }
  "
)

# Mostrar o gráfico
sankey









#################################  Category x Stressor x cases ###################################################



library(readxl)
library(ggplot2)
library(dplyr)


# Agrupar os dados por Stressor e Category, contando o número de medidas
data_summary <- data %>%
  group_by(Stressor, Category) %>%
  summarise(Count = n(), .groups = 'drop')

# Criar o gráfico de barras empilhadas com transparência e sem grade de fundo
ggplot(data_summary, aes(x = Stressor, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +  # Ajuste da transparência com alpha
  labs(
    title = NULL,
    x = "Stressor",
    y = "Number of Measures",
    fill = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.major = element_blank(),  # Remove grades principais
    panel.grid.minor = element_blank()   # Remove grades menores
  )



############################### Group x Nº cases ##########################################################


# Agrupar os dados por Group e contar a frequência
data_summary <- data %>%
  group_by(Group) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Calcular a proporção
data_summary <- data_summary %>%
  mutate(Proportion = Frequency / sum(Frequency) * 100)  # Calcular a proporção em porcentagem

# Criar gráfico de barras ordenado com cores e transparência ajustadas
ggplot(data_summary, aes(x = reorder(Group, -Frequency), y = Frequency, fill = Frequency)) +
  geom_bar(stat = "identity", alpha = 0.8) +  # Transparência ajustada
  labs(
    title = NULL,
    x = NULL,
    y = "Measures"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  # Adicionar os rótulos de proporção nas barras
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            vjust = -1, size = 3, color = "black")  # Adicionar proporção como rótulo



################################ Numero de estudos po YEAR ##########################################



# Considerar cada combinação de Author e Year como um estudo único
data_summary_count <- data %>%
  distinct(Author, Year) %>%  # Selecionar combinações únicas de Author e Year
  group_by(Year) %>%
  summarise(Number_of_Studies = n(), .groups = 'drop')

# Criar gráfico de barras com número total de estudos por ano
ggplot(data_summary_count, aes(x = as.factor(Year), y = Number_of_Studies)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(
    title = NULL,
    x = "Year",
    y = "Number of Studies"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



#################################   FRequencias de varios   ##############################################
library(ggplot2)
library(dplyr)
library(forcats)
library(gridExtra)

# Criar os dados combinados, incluindo as categorias específicas
type_data <- data %>%
  count(Type) %>%
  mutate(Proportion = n / sum(n) * 100,
         Group = paste0(Type, ""), 
         Category = "Type") 

ecosystem_data <- data %>%
  count(Ecosystem) %>%
  mutate(Proportion = n / sum(n) * 100,
         Group = paste0(Ecosystem, "."), 
         Category = "Ecosystem")

biome_data <- data %>%
  count(Biome) %>%
  mutate(Proportion = n / sum(n) * 100,
         Group = paste0(Biome, ""), 
         Category = "Biome")

# Definir paleta de cores para cada grupo
type_colors <- colorRampPalette(c("cyan", "darkturquoise"))(nrow(type_data))
ecosystem_colors <- colorRampPalette(c("green", "lightgreen"))(nrow(ecosystem_data))
biome_colors <- colorRampPalette(c("darkred", "salmon"))(nrow(biome_data))

# Combinar todas as cores em um único vetor
color_palette <- scale_fill_manual(
  values = c(
    setNames(type_colors, type_data$Group),
    setNames(ecosystem_colors, ecosystem_data$Group),
    setNames(biome_colors, biome_data$Group)
  )
)

# Combinar os três conjuntos de dados em um único dataframe
combined_data <- bind_rows(type_data, biome_data, ecosystem_data)

# Criar o gráfico
bar_plot <- ggplot() +
  # Adicionar camada de Type
  geom_bar(data = type_data, aes(x = reorder(Group, Proportion), y = Proportion, fill = Group),
           stat = "identity", width = 0.8, color = "white", size = 0.4, alpha = 0.5) +
  # Adicionar camada de Biome
  geom_bar(data = biome_data, aes(x = reorder(Group, Proportion), y = Proportion, fill = Group),
           stat = "identity", width = 0.8, color = "white", size = 0.5, alpha = 0.5) +
  # Adicionar camada de Ecosystem
  geom_bar(data = ecosystem_data, aes(x = reorder(Group, Proportion), y = Proportion, fill = Group),
           stat = "identity", width = 0.8, color = "white", size = 0.5, alpha = 0.5) +
  # Adicionar os valores percentuais nas barras
  geom_text(data = combined_data, 
            aes(x = reorder(Group, Proportion), 
                y = Proportion + max(Proportion) * 0.01,  # Pequeno deslocamento acima da barra
                label = paste0(round(Proportion, 1), "%")),
            size = 3.5, color = "black", hjust = 0) +
  coord_flip() +
  labs(
    title = NULL,
    x = NULL,  # Remover o rótulo do eixo x
    y = NULL   # Remover o rótulo do eixo y
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(combined_data$Proportion) * 1.1))+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()   
  ) +
  color_palette +
  # Adicionar linhas verticais separadoras entre as categorias
  geom_vline(xintercept = c(2.5, 6.5), color = "black", linetype = "dashed", size = 0.8)

# Exibir o gráfico
grid.arrange(bar_plot)


datos <- General_data_adjusted


######################
######################## Coluna FactorialExp ########################
# Identificar estudos únicos pela combinação de Author, Year e FactorialExp
estudos_unicos_factorial <- datos %>%
  distinct(Author, Year, FactorialExp)  # Mantém apenas combinações únicas

# Calcular frequências e porcentagens
tabla_factorial <- estudos_unicos_factorial %>%
  filter(!is.na(FactorialExp)) %>%  # Remover casos sem categorias
  count(FactorialExp, name = "Total_studies") %>%  # Contar estudos por categoria
  mutate(Total_studies_percent = round((Total_studies / sum(Total_studies)) * 100, 2))

# Adicionar a linha "Total"
total_factorial <- data.frame(
  FactorialExp = "Total",
  Total_studies = sum(tabla_factorial$Total_studies),
  Total_studies_percent = 100
)

# Combinar a tabela com o total
final_table_factorial <- bind_rows(tabla_factorial, total_factorial)

# Exibir a tabela final
print(final_table_factorial)


###########
# Novo conjunto de dados para `FactorialExp` (manual ou importado)
novo_dados_factorial <- data.frame(
  FactorialExp = c("Full-factorial", "Other type", "Total"),
  Total_studies = c(45, 65, 110)  # Valores fornecidos
)

# Combinar com os dados antigos
dados_combinados_factorial <- merge(final_table_factorial, novo_dados_factorial, by = "FactorialExp", all = TRUE)

# Soma das colunas Total_studies
dados_combinados_factorial$Total_studies_combined <- rowSums(dados_combinados_factorial[, c("Total_studies.x", "Total_studies.y")], na.rm = TRUE)

# Selecionar as colunas relevantes e renomear
tabela_final_factorial <- dados_combinados_factorial[, c("FactorialExp", "Total_studies_combined")]
names(tabela_final_factorial) <- c("FactorialExp", "Total_studies")

# Calcular o percentual de cada FactorialExp no Total_studies combinado
tabela_final_factorial <- tabela_final_factorial %>%
  mutate(
    Percentual = round((Total_studies / Total_studies[FactorialExp == "Total"]) * 100, 2)
  )

# Exibir a tabela final com percentuais
print(tabela_final_factorial)


# Carregar o pacote ggplot2
library(ggplot2)

# Remover a linha "Total" para evitar duplicação no gráfico
tabela_grafico_factorial <- tabela_final_factorial %>% 
  filter(FactorialExp != "Total")

# Criar o gráfico de barras
grafico_factorial <- ggplot(tabela_grafico_factorial, aes(x = FactorialExp, y = Percentual, fill = FactorialExp)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.5) +  # Gráfico de barras
  geom_text(aes(label = paste0(Percentual, "%")), vjust = -0.5, size = 4) +  # Adicionar rótulos
  scale_fill_manual(values = c("turquoise", "salmon"))+  
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = "Experimental Design"
  ) +
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    axis.text.x = element_blank(),  # Remove os rótulos do eixo x
    axis.ticks.x = element_blank()  # Remove as marcas do eixo x
  )

# Exibir o gráfico
print(grafico_factorial)




##################################   Escala tempotal #########################
######################## coluna Tempo experimental  ################
datos <- General_data_adjusted

# Identificar estudos únicos pela combinação de Author e Year
library(dplyr)

# Criar um dataframe com estudos únicos
estudos_unicos <- datos %>%
  distinct(Author, Year, TimeExp)  # Mantém apenas combinações únicas

# Categorizar os estudos com base em TimeExp
estudos_unicos <- estudos_unicos %>%
  mutate(Temporal_scale = cut(
    TimeExp,
    breaks = c(-Inf, 30, 365, Inf),  # Limites: <30 dias, 31-365 dias, >365 dias
    labels = c("Short-term (< month)", 
               "Mid-term (> month , < year)", 
               "Long-term (> year)")
  ))

# Calcular frequências e porcentagens
tabla <- estudos_unicos %>%
  filter(!is.na(Temporal_scale)) %>%  # Remover casos sem categorias
  count(Temporal_scale, name = "Total_studies") %>%  # Contar estudos por categoria
  mutate(Total_studies_percent = round((Total_studies / sum(Total_studies)) * 100, 2))

# Adicionar a linha "Total"
total <- data.frame(
  Temporal_scale = "Total",
  Total_studies = sum(tabla$Total_studies),
  Total_studies_percent = 100
)

# Combinar a tabela com o total
final_table <- bind_rows(tabla, total)

# Exibir o resultado final
print(final_table)


###########
# Novo conjunto de dados (manual ou importado)
novo_dados <- data.frame(
  Temporal_scale = c("Short-term (< month)", "Mid-term (> month , < year)", 
                     "Long-term (> year)", "Total"),
  Total_studies = c(43, 23, 2, 67)  # Valores fornecidos
)

# Combinar com os dados antigos
dados_combinados <- merge(final_table, novo_dados, by = "Temporal_scale", all = TRUE)

# Soma das colunas Total_studies
dados_combinados$Total_studies_combined <- rowSums(dados_combinados[, c("Total_studies.x", "Total_studies.y")], na.rm = TRUE)

# Selecionar as colunas relevantes e renomear
tabela_final <- dados_combinados[, c("Temporal_scale", "Total_studies_combined")]
names(tabela_final) <- c("Temporal_scale", "Total_studies")

# Exibir a tabela final
tabela_final


# Calcular o percentual de cada Temporal_scale no Total_studies combinado
tabela_final <- tabela_final %>%
  mutate(
    Percentual = round((Total_studies / Total_studies[Temporal_scale == "Total"]) * 100, 2)
  )

# Exibir a tabela final com percentuais
print(tabela_final)


# Carregar o pacote ggplot2
library(ggplot2)

# Remover a linha "Total" para evitar duplicação no gráfico
tabela_grafico <- tabela_final %>% 
  filter(Temporal_scale != "Total")

# Criar o gráfico de barras
grafico <- ggplot(tabela_grafico, aes(x = Temporal_scale, y = Percentual, fill = Temporal_scale)) +
  geom_bar(stat = "identity", width = 0.7, alpha= 0.8) +  # Gráfico de barras
  geom_text(aes(label = paste0(Percentual, "%")), vjust = -0.5, size = 4) +  # Adicionar rótulos
  scale_fill_manual(values = c("turquoise", "skyblue", "pink"))+  
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = "Temporal Scale"
  ) +
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    axis.text.x = element_blank(),  # Remove os rótulos do eixo x
    axis.ticks.x = element_blank()  # Remove as marcas do eixo x
  )
# Exibir o gráfico
print(grafico)

library(ggplot2)

# Criando o gráfico de rosca
grafico <- ggplot(tabela_grafico, aes(x = "", y = Percentual, fill = Temporal_scale)) +
  geom_bar(stat = "identity", width = 1) +  # Criando um gráfico de pizza
  coord_polar(theta = "y", start = 0) +  # Convertendo para coordenadas polares
  geom_text(aes(label = paste0(Percentual, "%")), 
            position = position_stack(vjust = 0.5), size = 4) +  # Adicionando rótulos ao gráfico
  scale_fill_manual(values = c("lightgray", "gray", "darkgray")) +  
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = "Temporal Scale"
  ) +
  theme_minimal(base_size = 14) +  # Aplicando um tema minimalista
  theme(
    axis.text = element_blank(),  # Removendo rótulos dos eixos
    axis.ticks = element_blank(),  # Removendo marcas dos eixos
    panel.grid = element_blank()  # Removendo linhas de grade
  ) +
  # Criando o efeito de rosca ao adicionar um círculo branco no centro
  annotate("text", x = 0, y = 0, label = "", size = 20, color = "white")

# Exibir o gráfico
print(grafico)



P = grafico_factorial + grafico
print(P)
grafico_combinado <- grafico_factorial + grafico + 
  plot_layout(ncol = 2) &  # Aplica o mesmo tema a ambos os gráficos
  theme(
    legend.text = element_text(size = 8),  # Ajusta o tamanho do texto da legenda
    legend.title = element_text(size = 8)  # Ajusta o tamanho do título da legenda
  )

grafico_combinado




# Criar o gráfico de rosca
grafico_roda <- ggplot(tabela_grafico_factorial, aes(x = "", y = Percentual, fill = FactorialExp)) +
  geom_bar(stat = "identity", width = 1) +  # Barras preenchidas
  coord_polar("y", start = 0) +  # Transformar em rosca
  geom_text(aes(label = paste0(Percentual, "%")), position = position_stack(vjust = 0.5), size = 4) +  # Rótulos no centro das fatias
  scale_fill_manual(values = c("turquoise", "darkturquoise")) +  # Mantendo as cores
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = "Experimental Design"
  ) +
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    axis.text = element_blank(),  # Remove textos dos eixos
    axis.ticks = element_blank(), # Remove marcas dos eixos
    panel.grid = element_blank()  # Remove grades de fundo
  )

# Adicionar o "furo" no centro para fazer uma rosca
grafico_roda <- grafico_roda + annotate("point", x = 0, y = 0, size = 15, color = "white")

# Exibir o gráfico
grafico_roda









library(tidyverse)
library(tidygraph)
library(ggraph)


# Carregar os dados
data <- General_data_adjusted

# Criar as conexões Group → Species garantindo que todas as relações sejam mantidas
edges_gs <- data %>%
  select(from = Group, to = Species) %>%
  distinct()  # Mantém apenas pares únicos Group-Species

# Criar lista de nós (tanto grupos quanto espécies)
nodes <- data.frame(name = unique(c(data$Group, data$Species)))

# Criar o grafo
graph_tbl <- tbl_graph(nodes = nodes, edges = edges_gs, directed = TRUE)

# Adicionar um tipo de nó (group ou species)
graph_tbl <- graph_tbl %>%
  activate(nodes) %>%
  mutate(tipo = if_else(name %in% edges_gs$from, "group", "species"))

# Criar o layout usando o algoritmo 'tree' para melhor distribuição
layout <- create_layout(graph_tbl, layout = "tree")

# Ajustar a posição dos grupos para evitar sobreposição
layout <- layout %>%
  mutate(
    x = if_else(tipo == "group", x + 2, x),  # Pequeno deslocamento horizontal
    y = if_else(tipo == "group", y * 1.05, y)  # Pequeno ajuste vertical
  )

# Criar o gráfico
ggraph(layout) +
  scale_x_reverse() +
  coord_flip(ylim = c(-3,3)) +  
  geom_edge_diagonal(color = "green") +
  geom_node_point(aes(color = tipo), size = 0.1) + 
  geom_node_text(
    aes(label = ifelse(tipo == "species", paste0("italic('", name, "')"), name)),  
    hjust = 1.1,
    nudge_x = 0.2,
    size = 0.7,
    parse = TRUE  
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_blank(),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

########################
# Se necessário, carregue o pacote scales
if (!require(scales)) {
  install.packages("scales")
  library(scales)
}

ggplot(data, aes(x = Group, fill = Category)) +
  geom_bar(position = "fill", alpha = 0.8) +
  labs(title = NULL,
       x = NULL,
       y = "Distribution (%)") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_blank(),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  ) +
  scale_fill_viridis_d(option = "F") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
  scale_y_continuous(labels = scales::percent)





# Instale o pacote leaflet (caso não esteja instalado)
install.packages("leaflet")

# Carregue o pacote
library(leaflet)

# Atualizar a tabela de pontos com a nova coordenada da Ría de Vigo
pontos <- data.frame(
  Local = c("Ría de Vigo", "Cabo de Gata", "Laguna Las Yeguas", "Charca de Suárez"),
  Latitude = c(42.14, 36.74, 37.03, 36.72),
  Longitude = c(-8.73, -2.16, -3.22, -3.52),  # Atualizar a longitude da Ría de Vigo
  Tipo = c("Marinho", "Marinho", "Água Doce", "Água Doce")  # Tipos de ecossistema
)

# Criar um mapa atualizado com rótulos acima de Ría de Vigo e Lago Las Yeguas
mapa <- leaflet(pontos) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # Mapa base minimalista
  addCircleMarkers(
    lng = ~Longitude, lat = ~Latitude,  # Coordenadas
    color = ~ifelse(Tipo == "Marinho", "blue", "green"),  # Cores por tipo
    radius = 8, fillOpacity = 0.8, stroke = FALSE
  ) %>%
  # Adiciona rótulos acima de Ría de Vigo e Lago Las Yeguas
  addLabelOnlyMarkers(
    lng = c(-8.73, -3.22), lat = c(42.30 + 0.03, 37.03 + 0.2),  # Ajustar a latitude para cima
    label = c("Ría de Vigo", "Laguna Las Yeguas"),
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = 'top',  # Direção para cima
      style = list("font-size" = "10px", 
                   "padding" = "5px", 
                   "border-radius" = "3px", 
                   "background" = "rgba(255, 255, 255, 0.8)", 
                   "border" = "1px solid black"),
      offset = c(0, 10)  # Deslocar para cima
    )
  ) %>%
  # Adicionar escala
  addScaleBar(position = "bottomleft") %>%
  # Adicionar símbolo de norte usando uma imagem
  addMarkers(
    lng = -8.73, lat = 42.3,  # Posição do símbolo de norte (ajustar conforme necessário)
    icon = makeIcon(iconUrl = "north_arrow.png",  # Caminho da imagem do símbolo de norte
                    iconWidth = 30, iconHeight = 30)
  ) %>%
  
  # Adiciona rótulos abaixo de Cabo de Gata e Charca de Suárez
  addLabelOnlyMarkers(
    lng = c(-2.16, -3.52), lat = c(36.9 - 0.03, 36.9 - 0.03),  # Ajustar a latitude para baixo
    label = c("Cabo de Gata", "Charca de Suárez"),
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = 'bottom',  # Direção para baixo
      style = list("font-size" = "10px", 
                   "padding" = "5px", 
                   "border-radius" = "2px", 
                   "background" = "rgba(255, 255, 255, 0.8)", 
                   "border" = "0.5px solid black"),
      offset = c(0, 10)  # Deslocar para baixo
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("blue", "green"),
    labels = c("Marine", "Freshwater"),
    title = "Ecosystem"
  )

# Exibir o mapa
mapa




























# Carregar o pacote ggplot2
library(ggplot2)

# Criar os dados do gráfico
time <- seq(0, 336, by = 1)  # Tempo em horas (0 a 14 dias)

# Tratamentos de temperatura
control_constant_low <- rep(20, length(time))  # Controle constante (baixo nutriente)
control_constant_high <- rep(20, length(time))  # Controle constante (alto nutriente)
control_fluctuating_low <- 20 + 2 * sin((2 * pi / 24) * time)  # Controle flutuante (20 ± 2, baixo)
control_fluctuating_high <- 20 + 2 * sin((2 * pi / 24) * time)  # Controle flutuante (20 ± 2, alto)
warmed_fluctuating_low <- 25 + 2 * sin((2 * pi / 24) * time)   # Aquecido flutuante (25 ± 2, baixo)
warmed_fluctuating_high <- 25 + 2 * sin((2 * pi / 24) * time)   # Aquecido flutuante (25 ± 2, alto)

# Combinar dados em um dataframe
data <- data.frame(
  Time = rep(time, 6),
  Temperature = c(
    control_constant_low, control_constant_high,
    control_fluctuating_low, control_fluctuating_high,
    warmed_fluctuating_low, warmed_fluctuating_high
  ),
  Treatment = factor(rep(c(
    "Control (constant, Low nutrient)", 
    "Control (constant, High nutrient)",
    "Control (fluctuating, Low nutrient)", 
    "Control (fluctuating, High nutrient)", 
    "Warmed (fluctuating, Low nutrient)", 
    "Warmed (fluctuating, High nutrient)"
  ), each = length(time)))
)

# Criar o gráfico
plot <- ggplot(data, aes(x = Time / 24, y = Temperature, color = Treatment)) +
  geom_line(size = 0.5) +  # Diminuir a espessura das linhas
  scale_color_manual(values = c("blue", "blue", "green", "green", "red", "red")) +
  scale_x_continuous(breaks = seq(0, 14, by = 2), labels = seq(0, 14, by = 2)) +
  labs(
    title = "Experimental Setup",
    x = "Time (days)", 
    y = "Temperature (°C)", 
    color = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# Exibir o gráfico
print(plot)


















# Pacote necessário
library(ggplot2)

# Criando os dados com os tratamentos
data <- data.frame(
  Temperature = rep(c("Temperature Fluctuation", "Constant Temperature", "Fluctuation + Warming"), 4),
  Nutrients = rep(c("High Nutrients", "Low Nutrients"), each = 6),
  Treatment = rep(1, 12), # 1 indica que o tratamento está presente
  Trophic_Level = rep(c("Eutrophic", "Oligotrophic"), each = 6)
)

# Criando o gráfico de blocos com os níveis tróficos
ggplot(data, aes(x = Nutrients, y = Temperature, fill = as.factor(Treatment))) +
  geom_tile(color = "white", size = 0.5) + # Blocos com bordas brancas
  scale_fill_manual(values = c("1" = "green")) + # Cor verde para tratamentos presentes
  labs(x = "Nutrient Levels", y = "Temperature Conditions", fill = "Treatment") +
  facet_wrap(~ Trophic_Level) + # Separar os gráficos por nível trófico
  theme_minimal() + # Tema limpo
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )






##############################################################################################################
#####################################################################################
#################################   FRequencias de varios   ##############################################
library(ggplot2)
library(dplyr)
library(forcats)
library(gridExtra)

# Criar os dados combinados, incluindo as categorias específicas
type_data <- data %>%
  count(Type) %>%
  mutate(Proportion = n / sum(n) * 100,
         Group = paste0(Type, ""), 
         Category = "Type") 

ecosystem_data <- data %>%
  count(Ecosystem) %>%
  mutate(Proportion = n / sum(n) * 100,
         Group = paste0(Ecosystem, "."), 
         Category = "Ecosystem")

biome_data <- data %>%
  count(Biome) %>%
  mutate(Proportion = n / sum(n) * 100,
         Group = paste0(Biome, ""), 
         Category = "Biome")

# Definir paleta de cores para cada grupo
type_colors <- colorRampPalette(c("cyan", "darkturquoise"))(nrow(type_data))
ecosystem_colors <- colorRampPalette(c("green", "lightgreen"))(nrow(ecosystem_data))
biome_colors <- colorRampPalette(c("darkred", "salmon"))(nrow(biome_data))

# Combinar todas as cores em um único vetor
color_palette <- scale_fill_manual(
  values = c(
    setNames(type_colors, type_data$Group),
    setNames(ecosystem_colors, ecosystem_data$Group),
    setNames(biome_colors, biome_data$Group)
  )
)

# Combinar os três conjuntos de dados em um único dataframe
combined_data <- bind_rows(type_data, biome_data, ecosystem_data)

# Criar o gráfico
bar_plot <- ggplot() +
  # Adicionar camada de Type
  geom_bar(data = type_data, aes(x = reorder(Group, Proportion), y = Proportion, fill = Group),
           stat = "identity", width = 0.8, color = "white", size = 0.4, alpha = 0.5) +
  # Adicionar camada de Biome
  geom_bar(data = biome_data, aes(x = reorder(Group, Proportion), y = Proportion, fill = Group),
           stat = "identity", width = 0.8, color = "white", size = 0.5, alpha = 0.5) +
  # Adicionar camada de Ecosystem
  geom_bar(data = ecosystem_data, aes(x = reorder(Group, Proportion), y = Proportion, fill = Group),
           stat = "identity", width = 0.8, color = "white", size = 0.5, alpha = 0.5) +
  # Adicionar os valores percentuais nas barras
  geom_text(data = combined_data, 
            aes(x = reorder(Group, Proportion), 
                y = Proportion + max(Proportion) * 0.01,  # Pequeno deslocamento acima da barra
                label = paste0(round(Proportion, 1), "%")),
            size = 3.5, color = "black", hjust = 0) +
  coord_flip() +
  labs(
    title = NULL,
    x = NULL,  # Remover o rótulo do eixo x
    y = NULL   # Remover o rótulo do eixo y
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(combined_data$Proportion) * 1.1))+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()   
  ) +
  color_palette +
  # Adicionar linhas verticais separadoras entre as categorias
  geom_vline(xintercept = c(2.5, 6.5), color = "black", linetype = "dashed", size = 0.8)

# Exibir o gráfico
grid.arrange(bar_plot)



####################################################################################################################
###############################   General stressors ################################################################
##################################################################################################################


library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(metafor)
library(ggridges)
library(tidyr)


data <- General_data_adjusted


# Separar os grupos com base nas categorias de Stressor
data1 <- data %>%
  filter(Stressor %in% c("A", "W", "Hypoxia", "Hyperoxia"))

data2 <- data %>%
  filter(Stressor %in% c("F_temp", "F_light", "F_nut", "F_pH", "F_pCO2", "F_hypoxia", "F_water"))

data3 <- data %>%
  filter(Stressor %in% c("F_A", "F_W", "F_hiperoxia"))




############################# Calculo do lnRR global #################################

data_lnRR1 <- escalc(
  measure = "ROM", 
  m1i = me, sd1i = sde, n1i = ne, 
  m2i = mc, sd2i = sdc, n2i = nc, 
  data = data1 
)
overallresult_lnRR1 <- rma(yi, vi, data = data_lnRR1)  # Meta-análise
summary(overallresult_lnRR1)

##########################################
data_lnRR2 <- escalc(
  measure = "ROM", 
  m1i = me, sd1i = sde, n1i = ne, 
  m2i = mc, sd2i = sdc, n2i = nc, 
  data = data2 )
overallresult_lnRR2 <- rma(yi, vi, data = data_lnRR2) # Meta-análise
summary(overallresult_lnRR2)

##########################################
data_lnRR3 <- escalc(
  measure = "ROM", 
  m1i = me, sd1i = sde, n1i = ne, 
  m2i = mc, sd2i = sdc, n2i = nc, 
  data = data3 )
overallresult_lnRR3 <- rma(yi, vi, data = data_lnRR3) # Meta-análise
summary(overallresult_lnRR3)

########################### Calculo do lnCVR global #####################################

# Determinar as funçoes de acordo com (Nikagawa et al. 2015)

Calc.lnCVR <- function(CMean, CSD, CN, EMean, ESD, EN) {
  ES <- log(ESD) - log(EMean) + 1 / (2 * (EN - 1)) - 
    (log(CSD) - log(CMean) + 1 / (2 * (CN - 1)))
  return(ES)
}


Calc.var.lnCVR <- function(CMean, CSD, CN, EMean, ESD, EN, Equal.E.C.Corr=T) {
  if(Equal.E.C.Corr == T) {
    mvcorr <- cor.test(log(c(CMean, EMean)), log(c(CSD, ESD)))$estimate
    S2 <- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 
      2 * mvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + 
      ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 
      2 * mvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))
  } else {
    Cmvcorr <- cor.test(log(CMean), log(CSD))$estimate
    Emvcorr <- cor.test(log(EMean), log(ESD))$estimate
    S2 <- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 
      2 * Cmvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + 
      ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 
      2 * Emvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))
  }
  return(S2)
}


# Aplicar as funções Calc.lnCVR e Calc.var.lnCVR aos dados
data_lnCVR1 <- data1 %>%
  mutate(yi = Calc.lnCVR(CMean = mc, CSD = sdc, CN = nc,
                         EMean = me, ESD = sde, EN = ne),
         vi = Calc.var.lnCVR(CMean = mc, CSD = sdc, CN = nc,
                             EMean = me, ESD = sde, EN = ne))

overallresult_lnCVR1 <- rma(yi, vi, data = data_lnCVR1) # Meta-análise com rma
summary(overallresult_lnCVR1)

##############
data_lnCVR2 <- data2 %>%
  mutate(yi = Calc.lnCVR(CMean = mc, CSD = sdc, CN = nc,
                         EMean = me, ESD = sde, EN = ne),
         vi = Calc.var.lnCVR(CMean = mc, CSD = sdc, CN = nc,
                             EMean = me, ESD = sde, EN = ne))

overallresult_lnCVR2 <- rma(yi, vi, data = data_lnCVR2) # Meta-análise com rma
summary(overallresult_lnCVR2)

####################
data_lnCVR3 <- data3 %>%
  mutate(yi = Calc.lnCVR(CMean = mc, CSD = sdc, CN = nc,
                         EMean = me, ESD = sde, EN = ne),
         vi = Calc.var.lnCVR(CMean = mc, CSD = sdc, CN = nc,
                             EMean = me, ESD = sde, EN = ne))

overallresult_lnCVR3 <- rma(yi, vi, data = data_lnCVR3) # Meta-análise com rma
summary(overallresult_lnCVR3)

###########################  Ecosystem moderate - Metaregression ###################################################

# Convertendo a coluna Ecosystem para fator

data_lnRR1$Ecosystem <- as.factor(data_lnRR1$Ecosystem)
data_lnRR2$Ecosystem <- as.factor(data_lnRR2$Ecosystem)
data_lnRR3$Ecosystem <- as.factor(data_lnRR3$Ecosystem)

data_lnCVR1$Ecosystem <- as.factor(data_lnCVR1$Ecosystem)
data_lnCVR2$Ecosystem <- as.factor(data_lnCVR2$Ecosystem)
data_lnCVR3$Ecosystem <- as.factor(data_lnCVR3$Ecosystem)



# Metarregressão para lnRR com Ecosystem como moderador
metareg_lnRR_Ecosystem1 <- rma(yi = yi, vi = vi, mods = ~ Ecosystem-1,knha = TRUE, data = data_lnRR1, method = "REML")
summary(metareg_lnRR_Ecosystem1)
metareg_lnRR_Ecosystem2 <- rma(yi = yi, vi = vi, mods = ~ Ecosystem-1,knha = TRUE, data = data_lnRR2, method = "REML")
summary(metareg_lnRR_Ecosystem2)
metareg_lnRR_Ecosystem3 <- rma(yi = yi, vi = vi, mods = ~ Ecosystem-1,knha = TRUE, data = data_lnRR3, method = "REML")
summary(metareg_lnRR_Ecosystem3)


# Metarregressão para lnCVR com Ecosystem como moderador
metareg_lnCVR_Ecosystem1 <- rma(yi = yi, vi = vi, mods = ~ Ecosystem-1,knha = TRUE, data = data_lnCVR1, method = "REML")
summary(metareg_lnCVR_Ecosystem1)
metareg_lnCVR_Ecosystem2 <- rma(yi = yi, vi = vi, mods = ~ Ecosystem-1,knha = TRUE, data = data_lnCVR2, method = "REML")
summary(metareg_lnCVR_Ecosystem2)
metareg_lnCVR_Ecosystem3 <- rma(yi = yi, vi = vi, mods = ~ Ecosystem-1,knha = TRUE, data = data_lnCVR3, method = "REML")
summary(metareg_lnCVR_Ecosystem3)


#########################################################
################   GRAPHs #############################

ggplot(data_lnRR1, aes(x = yi, color = Ecosystem, fill = Ecosystem)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown"),
    guide = guide_legend(title = "Ecosystem")
  ) +
  scale_color_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown")
  ) +
  geom_point(aes(x = 0.1808, y = -0.05, color = "Freshwater"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0099, xmax = 0.3714, y = -0.05), 
                 color = "green", height = 0.06) +
  
  # Ponto e barras de IC para Marine
  geom_point(aes(x = 0.0714, y = -0.12, color = "Marine"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0222, xmax = 0.1650, y = -0.12), 
                 color = "blue", height = 0.06) +
  
  # Ponto e barras de IC para Terrestrial
  geom_point(aes(x = -0.2214, y = -0.19, color = "Terrestrial"), size = 4) +
  geom_errorbarh(aes(xmin = -0.4127, xmax = -0.0300, y = -0.19), 
                 color = "brown", height = 0.06) +
  
  # Linha de referência para zero
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), 
               size = 0.3, color = "black") +
  
  # Títulos e legendas
  labs(
    x = NULL, 
    y = NULL,
    color = "Ecosystem",    # Título da legenda para as linhas
    fill = "Ecosystem"      # Título da legenda para preenchimento
  ) +
  xlim(-2, 2)+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
    
  )

################ lnRR Ecosystem ########
ggplot(data_lnRR2, aes(x = yi, color = Ecosystem, fill = Ecosystem)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown", "NA" = "gray"),
    guide = guide_legend(title = "Ecosystem")
  ) +
  scale_color_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown", "NA" = "gray")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), size = 0.3, color = "black") +  # Linha para zero
  # Pontos com intervalos de confiança
  geom_point(aes(x = 0.0165, y = -0.07, color = "Freshwater"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0469, xmax = 0.0799, y = -0.07, color = "Freshwater"), height = 0.1) +
  geom_point(aes(x = -0.1414, y = -0.14, color = "Marine"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1988, xmax = -0.0840, y = -0.14, color = "Marine"), height = 0.1) +
  geom_point(aes(x = -0.0415, y = -0.21, color = "Terrestrial"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1104, xmax = 0.0273, y = -0.21, color = "Terrestrial"), height = 0.1) +
  geom_point(aes(x = 0.4778, y = -0.29, color = "NA"), size = 4) +
  geom_errorbarh(aes(xmin = 0.1067, xmax = 0.8490, y = -0.29, color = "NA"), height = 0.1) +
  # Títulos e labels
  labs(
    x = NULL, 
    y = NULL,
    color = "Ecosystem",    # Definir título da legenda para as linhas de densidade
    fill = "Ecosystem"      # Definir título da legenda para o preenchimento das áreas de densidade
  ) +
  xlim(-1, 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, vjust = -1),
    panel.background = element_blank(), 
    plot.background = element_blank(),
    
  )

#####################
ggplot(data_lnRR3, aes(x = yi, color = Ecosystem, fill = Ecosystem)) +
  geom_density(alpha = 0.4, adjust = 2) +  # Curvas de densidade
  scale_fill_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown"),
    guide = guide_legend(title = "Ecosystem")
  ) +
  scale_color_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), size = 0.3, color = "black") +  # Linha para zero
  # Pontos com intervalos de confiança
  geom_point(aes(x = 0.0266, y = -0.05, color = "Freshwater"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1032, xmax = 0.1564, y = -0.05, color = "Freshwater"), height = 0.1) +
  geom_point(aes(x = 0.0953, y = -0.15, color = "Marine"), size = 4) +
  geom_errorbarh(aes(xmin = 0.0167, xmax = 0.1740, y = -0.15, color = "Marine"), height = 0.1) +
  geom_point(aes(x = -0.3857, y = -0.19, color = "Terrestrial"), size = 4) +
  geom_errorbarh(aes(xmin = -0.5291, xmax = -0.2423, y = -0.19, color = "Terrestrial"), height = 0.1) +
  # Títulos e labels
  labs(
    x = NULL, 
    y = NULL,
    color = "Ecosystem",    # Definir título da legenda para as linhas de densidade
    fill = "Ecosystem"      # Definir título da legenda para o preenchimento das áreas de densidade
  ) +
  xlim(-1.5, 1.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, vjust = -1),
    panel.background = element_blank(), 
    plot.background = element_blank(),
    legend.position = "none"
    
  )
E7 <- E1 + E2 + E3
print(E7)


############################

ggplot(data_lnCVR1, aes(x = yi, color = Ecosystem, fill = Ecosystem)) +
  geom_density(alpha = 0.4, adjust = 2) +  # Curvas de densidade
  scale_fill_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown"),
    guide = guide_legend(title = "Ecosystem")
  ) +
  scale_color_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), size = 0.3, color = "black") +  # Linha para zero
  # Pontos com intervalos de confiança
  geom_point(aes(x = 0.8960, y = -0.02, color = "Freshwater"), size = 5) +
  geom_errorbarh(aes(xmin = -0.2922, xmax = 2.0842, y = -0.02, color = "Freshwater"), height = 0.03) +
  geom_point(aes(x = 0.3612, y = -0.05, color = "Marine"), size = 5) +
  geom_errorbarh(aes(xmin = -0.2082, xmax = 0.9307, y = -0.05, color = "Marine"), height = 0.03) +
  geom_point(aes(x = 0.0299, y = -0.08, color = "Terrestrial"), size = 5) +
  geom_errorbarh(aes(xmin = -1.1790, xmax = 1.2388, y = -0.08, color = "Terrestrial"), height = 0.03) +
  # Títulos e labels
  labs(
    x = NULL, 
    y = NULL,
    color = "Ecosystem",    # Definir título da legenda para as linhas de densidade
    fill = "Ecosystem"      # Definir título da legenda para o preenchimento das áreas de densidade
  ) +
  xlim(-3, 3) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, vjust = -1),
    panel.background = element_blank(), 
    plot.background = element_blank(),
    legend.position = "none"  )

############################
ggplot(data_lnCVR2, aes(x = yi, color = Ecosystem, fill = Ecosystem)) +
  geom_density(alpha = 0.4, adjust = 2) +  # Curvas de densidade
  scale_fill_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown", "NA" = "gray"),
    guide = guide_legend(title = "Ecosystem")
  ) +
  scale_color_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown", "NA" = "gray")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), size = 0.3, color = "black") +  # Linha para zero
  # Pontos com intervalos de confiança
  geom_point(aes(x = -0.0395, y = -0.02, color = "Freshwater"), size = 5) +
  geom_errorbarh(aes(xmin = -0.1835, xmax = 0.1046, y = -0.02, color = "Freshwater"), height = 0.03) +
  geom_point(aes(x = 0.2263, y = -0.05, color = "Marine"), size = 5) +
  geom_errorbarh(aes(xmin = 0.0945, xmax = 0.3580, y = -0.05, color = "Marine"), height = 0.03) +
  geom_point(aes(x = 0.6150, y = -0.08, color = "NA"), size = 5) +
  geom_errorbarh(aes(xmin = -0.2521, xmax = 1.4820, y = -0.08, color = "NA"), height = 0.03) +
  geom_point(aes(x = 0.1916, y = -0.13, color = "Terrestrial"), size = 5) +
  geom_errorbarh(aes(xmin = 0.0293, xmax = 0.3538, y = -0.13, color = "Terrestrial"), height = 0.03) +
  # Títulos e labels
  labs(
    x = NULL, 
    y = NULL,
    color = "Ecosystem",    # Definir título da legenda para as linhas de densidade
    fill = "Ecosystem"      # Definir título da legenda para o preenchimento das áreas de densidade
  ) +
  xlim(-3, 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, vjust = -1),
    panel.background = element_blank(), 
    plot.background = element_blank(),  
    legend.position = "none"
  )

############################
ggplot(data_lnCVR3, aes(x = yi, color = Ecosystem, fill = Ecosystem)) +
  geom_density(alpha = 0.4, adjust = 2) +  # Curvas de densidade
  scale_fill_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown"),
    guide = guide_legend(title = "Ecosystem")
  ) +
  scale_color_manual(
    values = c("Marine" = "blue", "Freshwater" = "green", "Terrestrial" = "brown")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), size = 0.3, color = "black") +  # Linha para zero
  # Pontos com intervalos de confiança
  geom_point(aes(x = 0.6957, y = -0.05, color = "Freshwater"), size = 5) +
  geom_errorbarh(aes(xmin = 0.1949, xmax = 1.1965, y = -0.05, color = "Freshwater"), height = 0.03) +
  geom_point(aes(x = -0.0432, y = -0.1, color = "Marine"), size = 5) +
  geom_errorbarh(aes(xmin = -0.3382, xmax = 0.2517, y = -0.1, color = "Marine"), height = 0.03) +
  geom_point(aes(x = 0.1309, y = -0.15, color = "Terrestrial"), size = 5) +
  geom_errorbarh(aes(xmin = -0.4241, xmax = 0.6859, y = -0.15, color = "Terrestrial"), height = 0.03) +
  # Títulos e labels
  labs(
    x = NULL,
    y = NULL,
    color = "Ecosystem",    # Definir título da legenda para as linhas de densidade
    fill = "Ecosystem"      # Definir título da legenda para o preenchimento das áreas de densidade
  ) +
  xlim(-2, 2) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, vjust = -1),
    panel.background = element_blank(), 
    plot.background = element_blank(), 
    legend.position = "none"
  )




##############################################################################################

################################################

###########################  Category moderate - Metaregression ###################################################



# Metarregressão para lnRR com Category como moderador
metareg_lnRR_Category1 <- rma(yi = yi, vi = vi, mods = ~ Category-1,knha = TRUE, data = data_lnRR1, method = "REML")
summary(metareg_lnRR_Category1)
metareg_lnRR_Category2 <- rma(yi = yi, vi = vi, mods = ~ Category-1,knha = TRUE, data = data_lnRR2, method = "REML")
summary(metareg_lnRR_Category2)
metareg_lnRR_Category3 <- rma(yi = yi, vi = vi, mods = ~ Category-1,knha = TRUE, data = data_lnRR3, method = "REML")
summary(metareg_lnRR_Category3)


# Metarregressão para lnCVR com Category como moderador
metareg_lnCVR_Category1 <- rma(yi = yi, vi = vi, mods = ~ Category-1,knha = TRUE, data = data_lnCVR1, method = "REML")
summary(metareg_lnCVR_Category1)
metareg_lnCVR_Category2 <- rma(yi = yi, vi = vi, mods = ~ Category-1,knha = TRUE, data = data_lnCVR2, method = "REML")
summary(metareg_lnCVR_Category2)
metareg_lnCVR_Category3 <- rma(yi = yi, vi = vi, mods = ~ Category-1,knha = TRUE, data = data_lnCVR3, method = "REML")
summary(metareg_lnCVR_Category3)



##############################################################################################3

################################################

###########################  Type moderate - Metaregression ###################################################

# Metarregressão para lnRR com Type como moderador
metareg_lnRR_Type1 <- rma(yi = yi, vi = vi, mods = ~ Type-1,knha = TRUE, data = data_lnRR1, method = "REML")
summary(metareg_lnRR_Type1)
metareg_lnRR_Type2 <- rma(yi = yi, vi = vi, mods = ~ Type-1,knha = TRUE, data = data_lnRR2, method = "REML")
summary(metareg_lnRR_Type2)
metareg_lnRR_Type3 <- rma(yi = yi, vi = vi, mods = ~ Type-1,knha = TRUE, data = data_lnRR3, method = "REML")
summary(metareg_lnRR_Type3)


# Metarregressão para lnCVR com Type como moderador
metareg_lnCVR_Type1 <- rma(yi = yi, vi = vi, mods = ~ Type-1,knha = TRUE, data = data_lnCVR1, method = "REML")
summary(metareg_lnCVR_Type1)
metareg_lnCVR_Type2 <- rma(yi = yi, vi = vi, mods = ~ Type-1,knha = TRUE, data = data_lnCVR2, method = "REML")
summary(metareg_lnCVR_Type2)
metareg_lnCVR_Type3 <- rma(yi = yi, vi = vi, mods = ~ Type-1,knha = TRUE, data = data_lnCVR3, method = "REML")
summary(metareg_lnCVR_Type3)




##############################################################################################3

################################################

###########################  Kingdom moderate - Metaregression ###################################################

# Metarregressão para lnRR com Kingdom como moderador
metareg_lnRR_Kingdom1 <- rma(yi = yi, vi = vi, mods = ~ Kingdom-1,knha = TRUE, data = data_lnRR1, method = "REML")
summary(metareg_lnRR_Kingdom1)
metareg_lnRR_Kingdom2 <- rma(yi = yi, vi = vi, mods = ~ Kingdom-1,knha = TRUE, data = data_lnRR2, method = "REML")
summary(metareg_lnRR_Kingdom2)
metareg_lnRR_Kingdom3 <- rma(yi = yi, vi = vi, mods = ~ Kingdom-1,knha = TRUE, data = data_lnRR3, method = "REML")
summary(metareg_lnRR_Kingdom3)


# Metarregressão para lnCVR com Kingdom como moderador
metareg_lnCVR_Kingdom1 <- rma(yi = yi, vi = vi, mods = ~ Kingdom-1,knha = TRUE, data = data_lnCVR1, method = "REML")
summary(metareg_lnCVR_Kingdom1)
metareg_lnCVR_Kingdom2 <- rma(yi = yi, vi = vi, mods = ~ Kingdom-1,knha = TRUE, data = data_lnCVR2, method = "REML")
summary(metareg_lnCVR_Kingdom2)
metareg_lnCVR_Kingdom3 <- rma(yi = yi, vi = vi, mods = ~ Kingdom-1,knha = TRUE, data = data_lnCVR3, method = "REML")
summary(metareg_lnCVR_Kingdom3)





##############################################################################################3

################################################

###########################  Biome moderate - Metaregression ###################################################


# Metarregressão para lnRR com Biomecomo moderador
metareg_lnRR_Biome1 <- rma(yi = yi, vi = vi, mods = ~ Biome-1,knha = TRUE, data = data_lnRR1, method = "REML")
summary(metareg_lnRR_Biome1)
metareg_lnRR_Biome2 <- rma(yi = yi, vi = vi, mods = ~ Biome-1,knha = TRUE, data = data_lnRR2, method = "REML")
summary(metareg_lnRR_Biome2)
metareg_lnRR_Biome3 <- rma(yi = yi, vi = vi, mods = ~ Biome-1,knha = TRUE, data = data_lnRR3, method = "REML")
summary(metareg_lnRR_Biome3)


# Metarregressão para lnCVR com Biomecomo moderador
metareg_lnCVR_Biome1 <- rma(yi = yi, vi = vi, mods = ~ Biome-1,knha = TRUE, data = data_lnCVR1, method = "REML")
summary(metareg_lnCVR_Biome1)
metareg_lnCVR_Biome2 <- rma(yi = yi, vi = vi, mods = ~ Biome-1,knha = TRUE, data = data_lnCVR2, method = "REML")
summary(metareg_lnCVR_Biome2)
metareg_lnCVR_Biome3 <- rma(yi = yi, vi = vi, mods = ~ Biome-1,knha = TRUE, data = data_lnCVR3, method = "REML")
summary(metareg_lnCVR_Biome3)














# Convertendo a coluna Ecosystem para fator

data_lnRR1$Stressor_Ecosystem <- interaction(data_lnRR1$Stressor,data_lnRR1$Ecosystem)
data_lnRR2$Stressor_Ecosystem <- interaction(data_lnRR2$Stressor,data_lnRR2$Ecosystem)
data_lnRR3$Stressor_Ecosystem <- interaction(data_lnRR3$Stressor,data_lnRR3$Ecosystem)

data_lnCVR1$Stressor_Ecosystem <- interaction(data_lnCVR1$Stressor,data_lnCVR1$Ecosystem)
data_lnCVR2$Stressor_Ecosystem <- interaction(data_lnCVR2$Stressor,data_lnCVR2$Ecosystem)
data_lnCVR3$Stressor_Ecosystem <- interaction(data_lnCVR3$Stressor,data_lnCVR3$Ecosystem)



# Metarregressão para lnRR com Stressor_Ecosystem como moderador
metareg_lnRR_Stressor_Ecosystem1 <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem-1,knha = TRUE, data = data_lnRR1, method = "REML")
summary(metareg_lnRR_Stressor_Ecosystem1)
metareg_lnRR_Stressor_Ecosystem2 <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem-1,knha = TRUE, data = data_lnRR2, method = "REML")
summary(metareg_lnRR_Stressor_Ecosystem2)
metareg_lnRR_Stressor_Ecosystem3 <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem-1,knha = TRUE, data = data_lnRR3, method = "REML")
summary(metareg_lnRR_Stressor_Ecosystem3)


# Metarregressão para lnCVR com Stressor_Ecosystem como moderador
metareg_lnCVR_Stressor_Ecosystem1 <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem-1,knha = TRUE, data = data_lnCVR1, method = "REML")
summary(metareg_lnCVR_Stressor_Ecosystem1)
metareg_lnCVR_Stressor_Ecosystem2 <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem-1,knha = TRUE, data = data_lnCVR2, method = "REML")
summary(metareg_lnCVR_Stressor_Ecosystem2)
metareg_lnCVR_Stressor_Ecosystem3 <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem-1,knha = TRUE, data = data_lnCVR3, method = "REML")
summary(metareg_lnCVR_Stressor_Ecosystem3)














# Carregar as bibliotecas necessárias
library(ggplot2)
library(reshape2)

# Criar o DataFrame
data <- data.frame(
  Moderador = c("Category", "Category", "Category", "Ecosystem", "Ecosystem", "Ecosystem", 
                "Kingdom", "Kingdom", "Kingdom", "Type", "Type", "Type"),
  Grupo = c("Fluctuation", "Fluctuation + Intensified Stressor", "Stressor", 
            "Fluctuation", "Fluctuation + Intensified Stressor", "Stressor",
            "Fluctuation", "Fluctuation + Intensified Stressor", "Stressor", 
            "Fluctuation", "Fluctuation + Intensified Stressor", "Stressor"),
  lnCVR = c(0.0622, 0.0162, 0.0001, 0.0009, 0.0530, 0.2894, 
            0.0003, 0.0001, 0.0008, 0.0019, 0.0209, 0.0388),
  lnRR = c(0.0001, 0.0060, 0.3474, 0.0135, 0.0001, 0.0135,
           0.0001, 0.0001, 0.0029, 0.0001, 0.4497, 0.0399)
)

# Visualizar o DataFrame
print(data)

# Reorganizar os dados para o heatmap (aqui, precisamos transformar para um formato largo)
heatmap_data <- melt(data, id.vars = c("Moderador", "Grupo"), variable.name = "Tipo", value.name = "Valor")

# Criar o heatmap
ggplot(heatmap_data, aes(x = Tipo, y = interaction(Moderador, Grupo))) +
  geom_tile(aes(fill = Valor), color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Valor") +
  theme_minimal() +
  labs(title = "Heatmap de lnCVR e lnRR",
       x = "Tipo",
       y = "Moderador e Grupo") +
  theme(axis.text.y = element_text(size = 8)) # Ajusta o tamanho do texto no eixo Y











###########################  Group moderate - Metaregression ###################################################

# Convertendo a coluna Group para fator

data_lnRR1$Group <- as.factor(data_lnRR1$Group)
data_lnRR2$Group <- as.factor(data_lnRR2$Group)
data_lnRR3$Group <- as.factor(data_lnRR3$Group)

data_lnCVR1$Group <- as.factor(data_lnCVR1$Group)
data_lnCVR2$Group <- as.factor(data_lnCVR2$Group)
data_lnCVR3$Group <- as.factor(data_lnCVR3$Group)



# Metarregressão para lnRR com Group como moderador
metareg_lnRR_Group1 <- rma(yi = yi, vi = vi, mods = ~ Group-1,knha = TRUE, data = data_lnRR1, method = "REML")
summary(metareg_lnRR_Group1)
metareg_lnRR_Group2 <- rma(yi = yi, vi = vi, mods = ~ Group-1,knha = TRUE, data = data_lnRR2, method = "REML")
summary(metareg_lnRR_Group2)
metareg_lnRR_Group3 <- rma(yi = yi, vi = vi, mods = ~ Group-1,knha = TRUE, data = data_lnRR3, method = "REML")
summary(metareg_lnRR_Group3)


# Metarregressão para lnCVR com Group como moderador
metareg_lnCVR_Group1 <- rma(yi = yi, vi = vi, mods = ~ Group-1,knha = TRUE, data = data_lnCVR1, method = "REML")
summary(metareg_lnCVR_Group1)
metareg_lnCVR_Group2 <- rma(yi = yi, vi = vi, mods = ~ Group-1,knha = TRUE, data = data_lnCVR2, method = "REML")
summary(metareg_lnCVR_Group2)
metareg_lnCVR_Group3 <- rma(yi = yi, vi = vi, mods = ~ Group-1,knha = TRUE, data = data_lnCVR3, method = "REML")
summary(metareg_lnCVR_Group3)





#######################

# Carregando pacotes necessários
library(metafor)
library(dplyr)
library(ggplot2)

### EXTRAINDO OS RESULTADOS DOS MODELOS DE META‐REGRESSÃO ###

# Para o modelo Group1 (ex.: Stressor)
sum1 <- summary(metareg_lnRR_Group1)
df1 <- data.frame(
  Group    = rownames(sum1$b),
  estimate = as.vector(sum1$b),
  ci.lb    = sum1$ci.lb,
  ci.ub    = sum1$ci.ub
) %>%
  mutate(GroupType = "Stressor")

# Para o modelo Group2 (ex.: Fluctuation)
sum2 <- summary(metareg_lnRR_Group2)
df2 <- data.frame(
  Group    = rownames(sum2$b),
  estimate = as.vector(sum2$b),
  ci.lb    = sum2$ci.lb,
  ci.ub    = sum2$ci.ub
) %>%
  mutate(GroupType = "Fluctuation")

# Para o modelo Group3 (ex.: Fluctuation + Stressor)
sum3 <- summary(metareg_lnRR_Group3)
df3 <- data.frame(
  Group    = rownames(sum3$b),
  estimate = as.vector(sum3$b),
  ci.lb    = sum3$ci.lb,
  ci.ub    = sum3$ci.ub
) %>%
  mutate(GroupType = "Fluctuation + Stressor")

### JUNTANDO OS RESULTADOS EM UM ÚNICO DATA FRAME ###
total_forest_data <- bind_rows(df1, df2, df3) %>%
  mutate(Group = gsub("Group", "", Group)) %>%
  arrange(estimate) %>%
  mutate(Group = factor(Group, levels = unique(Group)))

### CRIANDO O FOREST PLOT ###
# Define o deslocamento para evitar sobreposição dos pontos
dodge_width <- 0.7

ggplot(total_forest_data, aes(x = estimate, y = Group, color = GroupType, shape = GroupType)) +
  geom_point(position = position_dodge(width = dodge_width), size = 3) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), 
                 position = position_dodge(width = dodge_width), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_shape_manual(values = c("Stressor" = 17, 
                                "Fluctuation" = 16, 
                                "Fluctuation + Stressor" = 15)) +
  labs(
    title = NULL,
    x = "lnRR",
    y = NULL,
    color = "Tipo de Grupo",
    shape = "Tipo de Grupo"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.background = element_blank(), 
    plot.background = element_blank(),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    
  )


########################################################################


### EXTRAINDO OS RESULTADOS DOS MODELOS DE META‐REGRESSÃO ###

sum1 <- summary(metareg_lnCVR_Group1)
df1 <- data.frame(
  Group    = rownames(sum1$b),
  estimate = as.vector(sum1$b),
  ci.lb    = sum1$ci.lb,
  ci.ub    = sum1$ci.ub
) %>%
  mutate(GroupType = "Stressor")

# Para o modelo Group2 (ex.: Fluctuation)
sum2 <- summary(metareg_lnCVR_Group2)
df2 <- data.frame(
  Group    = rownames(sum2$b),
  estimate = as.vector(sum2$b),
  ci.lb    = sum2$ci.lb,
  ci.ub    = sum2$ci.ub
) %>%
  mutate(GroupType = "Fluctuation")

# Para o modelo Group3 (ex.: Fluctuation + Stressor)
sum3 <- summary(metareg_lnCVR_Group3)
df3 <- data.frame(
  Group    = rownames(sum3$b),
  estimate = as.vector(sum3$b),
  ci.lb    = sum3$ci.lb,
  ci.ub    = sum3$ci.ub
) %>%
  mutate(GroupType = "Fluctuation + Stressor")

### JUNTANDO OS RESULTADOS EM UM ÚNICO DATA FRAME ###
total_forest_data <- bind_rows(df1, df2, df3) %>%
  mutate(Group = gsub("Group", "", Group)) %>%
  arrange(estimate) %>%
  mutate(Group = factor(Group, levels = unique(Group)))

### CRIANDO O FOREST PLOT ###
# Define o deslocamento para evitar sobreposição dos pontos
dodge_width <- 0.7

ggplot(total_forest_data, aes(x = estimate, y = Group, color = GroupType, shape = GroupType)) +
  geom_point(position = position_dodge(width = dodge_width), size = 3) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), 
                 position = position_dodge(width = dodge_width), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_shape_manual(values = c("Stressor" = 17, 
                                "Fluctuation" = 16, 
                                "Fluctuation + Stressor" = 15)) +
  labs(
    title = NULL,
    x = "lnCVR",
    y = NULL,
    color = "Tipo de Grupo",
    shape = "Tipo de Grupo"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.background = element_blank(), 
    plot.background = element_blank(),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "none"
    
  )




########################################## category graphs #################################
###########################################################################################

ggplot(data_lnRR1, aes(x = yi, color = Category, fill = Category)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red"),
    guide = guide_legend(title = "Category")
  ) +
  scale_color_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red")
  ) +
  # Linha de referência para zero
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), 
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Behaviour
  geom_point(aes(x = 0.1650, y = -0.02, color = "Behaviour"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0858, xmax = 0.4157, y = -0.02, color = "Behaviour"),
                 height = 0.06) +
  # Ponto e barra de erro para Fitness
  geom_point(aes(x = -0.1220, y = -0.08, color = "Fitness"), size = 4) +
  geom_errorbarh(aes(xmin = -0.3357, xmax = 0.0918, y = -0.08, color = "Fitness"),
                 height = 0.05) +
  # Ponto e barra de erro para Metabolism
  geom_point(aes(x = 0.0064, y = -0.14, color = "Metabolism"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1187, xmax = 0.1314, y = -0.14, color = "Metabolism"),
                 height = 0.05) +
  # Ponto e barra de erro para Morphology
  geom_point(aes(x = 0.1167, y = -0.2, color = "Morphology"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0572, xmax = 0.2906, y = -0.20, color = "Morphology"),
                 height = 0.05) +
  # Ponto e barra de erro para Physiology
  geom_point(aes(x = 0.0927, y = -0.26, color = "Physiology"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0976, xmax = 0.2831, y = -0.26, color = "Physiology"),
                 height = 0.05) +
  labs(
    x = NULL, 
    y = NULL,
    color = "Category",
    fill  = "Category"
  ) +
  xlim(-2, 2) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8),
    legend.title  = element_text(size = 10),
    legend.text   = element_text(size = 8),
  )




ggplot(data_lnRR2, aes(x = yi, color = Category, fill = Category)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red"),
    guide = guide_legend(title = "Category")
  ) +
  scale_color_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Behaviour
  geom_point(aes(x = -0.0290, y = -0.05, color = "Behaviour"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1979, xmax = 0.1399, y = -0.05, color = "Behaviour"),
                 height = 0.1) +
  # Ponto e barra de erro para Fitness
  geom_point(aes(x = -0.1667, y = -0.17, color = "Fitness"), size = 4) +
  geom_errorbarh(aes(xmin = -0.2367, xmax = -0.0968, y = -0.17, color = "Fitness"),
                 height = 0.1) +
  # Ponto e barra de erro para Metabolism
  geom_point(aes(x = -0.0235, y = -0.29, color = "Metabolism"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1054, xmax = 0.0584, y = -0.29, color = "Metabolism"),
                 height = 0.1) +
  # Ponto e barra de erro para Morphology
  geom_point(aes(x = 0.0461, y = -0.43, color = "Morphology"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0373, xmax = 0.1295, y = -0.43, color = "Morphology"),
                 height = 0.1) +
  # Ponto e barra de erro para Physiology
  geom_point(aes(x = -0.0510, y = -0.60, color = "Physiology"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1163, xmax = 0.0143, y = -0.60, color = "Physiology"),
                 height = 0.1) +
  labs(
    x = NULL, 
    y = NULL,
    color = "Category",
    fill  = "Category"
  ) +
  xlim(-1, 1) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8),
    legend.title  = element_text(size = 10),
    legend.text   = element_text(size = 8),
    legend.position = "none"
  )


ggplot(data_lnRR3, aes(x = yi, color = Category, fill = Category)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red"),
    guide = guide_legend(title = "Category")
  ) +
  scale_color_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Behaviour
  geom_point(aes(x = 0.2019, y = -0.05, color = "Behaviour"), size = 4) +
  geom_errorbarh(aes(xmin = 0.0230, xmax = 0.3808, y = -0.05, color = "Behaviour"),
                 height = 0.1) +
  # Ponto e barra de erro para Fitness
  geom_point(aes(x = -0.1544, y = -0.15, color = "Fitness"), size = 4) +
  geom_errorbarh(aes(xmin = -0.3013, xmax = -0.0075, y = -0.15, color = "Fitness"),
                 height = 0.1) +
  # Ponto e barra de erro para Metabolism
  geom_point(aes(x = -0.1191, y = -0.25, color = "Metabolism"), size = 4) +
  geom_errorbarh(aes(xmin = -0.2580, xmax = 0.0197, y = -0.25, color = "Metabolism"),
                 height = 0.1) +
  # Ponto e barra de erro para Morphology
  geom_point(aes(x = 0.1514, y = -0.35, color = "Morphology"), size = 4) +
  geom_errorbarh(aes(xmin = 0.0093, xmax = 0.2934, y = -0.35, color = "Morphology"),
                 height = 0.1) +
  # Ponto e barra de erro para Physiology
  geom_point(aes(x = -0.0287, y = -0.45, color = "Physiology"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1449, xmax = 0.0874, y = -0.45, color = "Physiology"),
                 height = 0.1) +
  labs(
    x = NULL, 
    y = NULL,
    color = "Category",
    fill  = "Category"
  ) +
  xlim(-1.5, 1.5) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8),
    legend.title  = element_text(size = 10),
    legend.text   = element_text(size = 8),
    legend.position = "none"
  )


library(ggplot2)

ggplot(data_lnCVR1, aes(x = yi, color = Category, fill = Category)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red"),
    guide = guide_legend(title = "Category")
  ) +
  scale_color_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), 
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Behaviour
  geom_point(aes(x = -1.3956, y = -0.01, color = "Behaviour"), size = 4) +
  geom_errorbarh(aes(xmin = -2.8994, xmax = 0.1083, y = -0.01, color = "Behaviour"),
                 height = 0.02) +
  # Ponto e barra de erro para Fitness
  geom_point(aes(x = 0.7016, y = -0.03, color = "Fitness"), size = 4) +
  geom_errorbarh(aes(xmin = -0.5739, xmax = 1.9771, y = -0.03, color = "Fitness"),
                 height = 0.02) +
  # Ponto e barra de erro para Metabolism
  geom_point(aes(x = 0.8322, y = -0.05, color = "Metabolism"), size = 4) +
  geom_errorbarh(aes(xmin = 0.1109, xmax = 1.5534, y = -0.05, color = "Metabolism"),
                 height = 0.02) +
  # Ponto e barra de erro para Morphology
  geom_point(aes(x = 0.9196, y = -0.07, color = "Morphology"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1262, xmax = 1.9654, y = -0.07, color = "Morphology"),
                 height = 0.02) +
  # Ponto e barra de erro para Physiology
  geom_point(aes(x = -0.5444, y = -0.09, color = "Physiology"), size = 4) +
  geom_errorbarh(aes(xmin = -1.6905, xmax = 0.6016, y = -0.09, color = "Physiology"),
                 height = 0.02) +
  labs(
    x = NULL, 
    y = NULL,
    color = "Category",
    fill  = "Category"
  ) +
  xlim(-6, 5) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8),
    legend.title  = element_text(size = 10),
    legend.text   = element_text(size = 8),
    legend.position = "none"
  )
ggplot(data_lnCVR2, aes(x = yi, color = Category, fill = Category)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red"),
    guide = guide_legend(title = "Category")
  ) +
  scale_color_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Behaviour
  geom_point(aes(x = 0.0326, y = -0.02, color = "Behaviour"), size = 4) +
  geom_errorbarh(aes(xmin = -0.3690, xmax = 0.4342, y = -0.02, color = "Behaviour"),
                 height = 0.04) +
  # Ponto e barra de erro para Fitness
  geom_point(aes(x = 0.1293, y = -0.06, color = "Fitness"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0305, xmax = 0.2892, y = -0.06, color = "Fitness"),
                 height = 0.04) +
  # Ponto e barra de erro para Metabolism
  geom_point(aes(x = 0.1859, y = -0.1, color = "Metabolism"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0057, xmax = 0.3775, y = -0.1, color = "Metabolism"),
                 height = 0.04) +
  # Ponto e barra de erro para Morphology
  geom_point(aes(x = 0.0820, y = -0.14, color = "Morphology"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1189, xmax = 0.2829, y = -0.14, color = "Morphology"),
                 height = 0.04) +
  # Ponto e barra de erro para Physiology
  geom_point(aes(x = 0.1439, y = -0.18, color = "Physiology"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0024, xmax = 0.2902, y = -0.18, color = "Physiology"),
                 height = 0.04) +
  labs(
    x = NULL, 
    y = NULL,
    color = "Category",
    fill  = "Category"
  ) +
  xlim(-3, 3) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8),
    legend.title  = element_text(size = 10),
    legend.text   = element_text(size = 8),
    legend.position = "none"
  )


ggplot(data_lnCVR3, aes(x = yi, color = Category, fill = Category)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red"),
    guide = guide_legend(title = "Category")
  ) +
  scale_color_manual(
    values = c("Behaviour"   = "dodgerblue", 
               "Fitness"     = "forestgreen", 
               "Metabolism"  = "darkorange", 
               "Morphology"  = "purple", 
               "Physiology"  = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Behaviour
  geom_point(aes(x = -0.8390, y = -0.02, color = "Behaviour"), size = 4) +
  geom_errorbarh(aes(xmin = -1.5026, xmax = -0.1754, y = -0.02, color = "Behaviour"),
                 height = 0.05) +
  # Ponto e barra de erro para Fitness
  geom_point(aes(x = 0.2904, y = -0.06, color = "Fitness"), size = 4) +
  geom_errorbarh(aes(xmin = -0.2258, xmax = 0.8067, y = -0.06, color = "Fitness"),
                 height = 0.05) +
  # Ponto e barra de erro para Metabolism
  geom_point(aes(x = 0.3877, y = -0.1, color = "Metabolism"), size = 4) +
  geom_errorbarh(aes(xmin = -0.1309, xmax = 0.9062, y = -0.1, color = "Metabolism"),
                 height = 0.05) +
  # Ponto e barra de erro para Morphology
  geom_point(aes(x = 0.5680, y = -0.14, color = "Morphology"), size = 4) +
  geom_errorbarh(aes(xmin = 0.0455, xmax = 1.0906, y = -0.14, color = "Morphology"),
                 height = 0.05) +
  # Ponto e barra de erro para Physiology
  geom_point(aes(x = 0.0060, y = -0.18, color = "Physiology"), size = 4) +
  geom_errorbarh(aes(xmin = -0.4170, xmax = 0.4289, y = -0.18, color = "Physiology"),
                 height = 0.05) +
  labs(
    x = NULL, 
    y = NULL,
    color = "Category",
    fill  = "Category"
  ) +
  xlim(-5, 3) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8),
    legend.title  = element_text(size = 10),
    legend.text   = element_text(size = 8)
  )



################################# Type ###########################################
###################################################################################

ggplot(data_lnRR1, aes(x = yi, color = Type, fill = Type)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Community" = "blue", "Species" = "green"),
    guide = guide_legend(title = "Type")
  ) +
  scale_color_manual(
    values = c("Community" = "blue", "Species" = "green")
  ) +
  # Linha de referência para zero
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), 
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Community
  geom_point(aes(x = 0.3319, y = -0.03, color = "Community"), size = 4) +
  geom_errorbarh(aes(xmin = 0.0742, xmax = 0.5895, y = -0.03, color = "Community"),
                 height = 0.05) +
  # Ponto e barra de erro para Species
  geom_point(aes(x = 0.0128, y = -0.1, color = "Species"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0686, xmax = 0.0942, y = -0.1, color = "Species"),
                 height = 0.05) +
  labs(x = NULL, y = NULL, color = "Type", fill = "Type") +
  xlim(-1.5, 1.7) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8)
  )

ggplot(data_lnRR2, aes(x = yi, color = Type, fill = Type)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Community" = "blue", "Species" = "green"),
    guide = guide_legend(title = "Type")
  ) +
  scale_color_manual(
    values = c("Community" = "blue", "Species" = "green")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Community
  geom_point(aes(x = -0.3507, y = -0.07, color = "Community"), size = 4) +
  geom_errorbarh(aes(xmin = -0.4285, xmax = -0.2730, y = -0.07, color = "Community"),
                 height = 0.1) +
  # Ponto e barra de erro para Species
  geom_point(aes(x = 0.0194, y = -0.14, color = "Species"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0203, xmax = 0.0591, y = -0.14, color = "Species"),
                 height = 0.1) +
  labs(x = NULL, y = NULL, color = "Type", fill = "Type") +
  xlim(-1, 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8)
  )
ggplot(data_lnRR3, aes(x = yi, color = Type, fill = Type)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Community" = "blue", "Species" = "green"),
    guide = guide_legend(title = "Type")
  ) +
  scale_color_manual(
    values = c("Community" = "blue", "Species" = "green")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Community
  geom_point(aes(x = 0.0908, y = -0.03, color = "Community"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0754, xmax = 0.2569, y = -0.03, color = "Community"),
                 height = 0.07) +
  # Ponto e barra de erro para Species
  geom_point(aes(x = -0.0236, y = -0.1, color = "Species"), size = 4) +
  geom_errorbarh(aes(xmin = -0.0932, xmax = 0.0460, y = -0.1, color = "Species"),
                 height = 0.07) +
  labs(x = NULL, y = NULL, color = "Type", fill = "Type") +
  xlim(-1.2, 1.2) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8)
  )



ggplot(data_lnCVR1, aes(x = yi, color = Type, fill = Type)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Community" = "blue", "Species" = "green"),
    guide = guide_legend(title = "Type")
  ) +
  scale_color_manual(
    values = c("Community" = "blue", "Species" = "green")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf), 
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Community
  geom_point(aes(x = 1.8749, y = -0.02, color = "Community"), size = 6) +
  geom_errorbarh(aes(xmin = 0.3128, xmax = 3.4371, y = -0.02, color = "Community"),
                 height = 0.03) +
  # Ponto e barra de erro para Species
  geom_point(aes(x = 0.2493, y = -0.04, color = "Species"), size = 6) +
  geom_errorbarh(aes(xmin = -0.2413, xmax = 0.7399, y = -0.04, color = "Species"),
                 height = 0.03) +
  labs(x = NULL, y = NULL, color = "Type", fill = "Type") +
  xlim(-4, 4) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8)
  )


ggplot(data_lnCVR2, aes(x = yi, color = Type, fill = Type)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Community" = "blue", "Species" = "green"),
    guide = guide_legend(title = "Type")
  ) +
  scale_color_manual(
    values = c("Community" = "blue", "Species" = "green")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Community
  geom_point(aes(x = -0.0106, y = -0.02, color = "Community"), size = 5) +
  geom_errorbarh(aes(xmin = -0.1962, xmax = 0.1750, y = -0.02, color = "Community"),
                 height = 0.03) +
  # Ponto e barra de erro para Species
  geom_point(aes(x = 0.1684, y = -0.05, color = "Species"), size = 5) +
  geom_errorbarh(aes(xmin = 0.0754, xmax = 0.2614, y = -0.05, color = "Species"),
                 height = 0.03) +
  labs(x = NULL, y = NULL, color = "Type", fill = "Type") +
  xlim(-3.5, 3) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8)
  )


ggplot(data_lnCVR3, aes(x = yi, color = Type, fill = Type)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Community" = "blue", "Species" = "green"),
    guide = guide_legend(title = "Type")
  ) +
  scale_color_manual(
    values = c("Community" = "blue", "Species" = "green")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  # Ponto e barra de erro para Community
  geom_point(aes(x = 0.8526, y = -0.02, color = "Community"), size = 5) +
  geom_errorbarh(aes(xmin = 0.2521, xmax = 1.4530, y = -0.02, color = "Community"),
                 height = 0.04) +
  # Ponto e barra de erro para Species
  geom_point(aes(x = 0.0217, y = -0.04, color = "Species"), size = 5) +
  geom_errorbarh(aes(xmin = -0.2282, xmax = 0.2715, y = -0.04, color = "Species"),
                 height = 0.04) +
  labs(x = NULL, y = NULL, color = "Type", fill = "Type") +
  xlim(-2, 2) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 10, hjust = 0.5),
    axis.title.x  = element_text(size = 10, margin = margin(t = 10)),
    axis.text     = element_text(size = 8)
  )


################### kingdom graphs ############
ggplot(data_lnRR1, aes(x = yi, color = Kingdom, fill = Kingdom)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Animalia" = "orange", 
               "Chromista" = "blue", 
               "Mixed" = "purple", 
               "Plantae" = "red"),
    guide = guide_legend(title = "Kingdom")
  ) +
  scale_color_manual(
    values = c("Animalia" = "orange", 
               "Chromista" = "blue", 
               "Mixed" = "purple", 
               "Plantae" = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  geom_point(aes(x = 0.0051, y = -0.03, color = "Animalia"), size = 5) +
  geom_errorbarh(aes(xmin = -0.0842, xmax = 0.0945, y = -0.03, color = "Animalia"),
                 height = 0.04) +
  geom_point(aes(x = 0.2741, y = -0.07, color = "Chromista"), size = 5) +
  geom_errorbarh(aes(xmin = 0.0950, xmax = 0.4532, y = -0.07, color = "Chromista"),
                 height = 0.04) +
  geom_point(aes(x = 0.2928, y = -0.13, color = "Mixed"), size = 5) +
  geom_errorbarh(aes(xmin = -0.1026, xmax = 0.6882, y = -0.13, color = "Mixed"),
                 height = 0.04) +
  geom_point(aes(x = -0.3699, y = -0.15, color = "Plantae"), size = 5) +
  geom_errorbarh(aes(xmin = -0.6818, xmax = -0.0579, y = -0.15, color = "Plantae"),
                 height = 0.04) +
  labs(x = NULL, y = NULL, color = "Kingdom", fill = "Kingdom") +
  xlim(-2, 1.5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title   = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.text    = element_text(size = 8)
  )


ggplot(data_lnRR2, aes(x = yi, color = Kingdom, fill = Kingdom)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Animalia" = "orange", 
               "Bacteria" = "darkgreen", 
               "Chromista" = "blue", 
               "Fungi" = "purple", 
               "Mixed" = "pink", 
               "Plantae" = "red"),
    guide = guide_legend(title = "Kingdom")
  ) +
  scale_color_manual(
    values = c("Animalia" = "orange", 
               "Bacteria" = "darkgreen", 
               "Chromista" = "blue", 
               "Fungi" = "purple", 
               "Mixed" = "pink", 
               "Plantae" = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  geom_point(aes(x = 0.0151, y = -0.05, color = "Animalia"), size = 5) +
  geom_errorbarh(aes(xmin = -0.0338, xmax = 0.0639, y = -0.05, color = "Animalia"),
                 height = 0.08) +
  geom_point(aes(x = 0.2875, y = -0.12, color = "Bacteria"), size = 5) +
  geom_errorbarh(aes(xmin = -0.0233, xmax = 0.5982, y = -0.12, color = "Bacteria"),
                 height = 0.08) +
  geom_point(aes(x = -0.1824, y = -0.19, color = "Chromista"), size = 5) +
  geom_errorbarh(aes(xmin = -0.2451, xmax = -0.1198, y = -0.19, color = "Chromista"),
                 height = 0.08) +
  geom_point(aes(x = -0.4635, y = -0.26, color = "Fungi"), size = 5) +
  geom_errorbarh(aes(xmin = -1.0802, xmax = 0.1532, y = -0.26, color = "Fungi"),
                 height = 0.08) +
  geom_point(aes(x = -0.0973, y = -0.33, color = "Mixed"), size = 5) +
  geom_errorbarh(aes(xmin = -0.5524, xmax = 0.3577, y = -0.33, color = "Mixed"),
                 height = 0.08) +
  geom_point(aes(x = -0.0651, y = -0.450, color = "Plantae"), size = 5) +
  geom_errorbarh(aes(xmin = -0.1747, xmax = 0.0445, y = -0.450, color = "Plantae"),
                 height = 0.08) +
  labs(x = NULL, y = NULL, color = "Kingdom", fill = "Kingdom") +
  xlim(-1.5, 1.2) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title   = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.text    = element_text(size = 8)
  )



ggplot(data_lnRR3, aes(x = yi, color = Kingdom, fill = Kingdom)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Animalia" = "orange", 
               "Bacteria" = "darkgreen", 
               "Chromista" = "blue", 
               "Mixed" = "purple", 
               "Plantae" = "red"),
    guide = guide_legend(title = "Kingdom")
  ) +
  scale_color_manual(
    values = c("Animalia" = "orange", 
               "Bacteria" = "darkgreen", 
               "Chromista" = "blue", 
               "Mixed" = "purple", 
               "Plantae" = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  geom_point(aes(x = -0.0571, y = -0.03, color = "Animalia"), size = 5) +
  geom_errorbarh(aes(xmin = -0.1328, xmax = 0.0187, y = -0.03, color = "Animalia"),
                 height = 0.04) +
  geom_point(aes(x = -0.6256, y = -0.07, color = "Bacteria"), size = 5) +
  geom_errorbarh(aes(xmin = -1.1096, xmax = -0.1416, y = -0.07, color = "Bacteria"),
                 height = 0.04) +
  geom_point(aes(x = 0.1424, y = -0.12, color = "Chromista"), size = 5) +
  geom_errorbarh(aes(xmin = 0.0238, xmax = 0.2611, y = -0.12, color = "Chromista"),
                 height = 0.04) +
  geom_point(aes(x = 0.6470, y = -0.17, color = "Mixed"), size = 5) +
  geom_errorbarh(aes(xmin = 0.2526, xmax = 1.0413, y = -0.17, color = "Mixed"),
                 height = 0.04) +
  geom_point(aes(x = -0.4031, y = -0.22, color = "Plantae"), size = 5) +
  geom_errorbarh(aes(xmin = -0.7484, xmax = -0.0578, y = -0.22, color = "Plantae"),
                 height = 0.04) +
  labs(x = NULL, y = NULL, color = "Kingdom", fill = "Kingdom") +
  xlim(-1.5, 1.5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title   = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.text    = element_text(size = 8)
  )



ggplot(data_lnCVR1, aes(x = yi, color = Kingdom, fill = Kingdom)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Animalia" = "orange", 
               "Chromista" = "blue", 
               "Mixed" = "purple", 
               "Plantae" = "red"),
    guide = guide_legend(title = "Kingdom")
  ) +
  scale_color_manual(
    values = c("Animalia" = "orange", 
               "Chromista" = "blue", 
               "Mixed" = "purple", 
               "Plantae" = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  geom_point(aes(x = 0.2451, y = -0.02, color = "Animalia"), size = 5) +
  geom_errorbarh(aes(xmin = -0.2876, xmax = 0.7779, y = -0.02, color = "Animalia"),
                 height = 0.03) +
  geom_point(aes(x = -0.2905, y = -0.05, color = "Chromista"), size = 5) +
  geom_errorbarh(aes(xmin = -1.3813, xmax = 0.8003, y = -0.05, color = "Chromista"),
                 height = 0.03) +
  geom_point(aes(x = 4.8517, y = -0.08, color = "Mixed"), size = 5) +
  geom_errorbarh(aes(xmin = 2.4829, xmax = 7.2205, y = -0.08, color = "Mixed"),
                 height = 0.03) +
  geom_point(aes(x = 1.5075, y = -0.11, color = "Plantae"), size = 5) +
  geom_errorbarh(aes(xmin = -0.3807, xmax = 3.3958, y = -0.11, color = "Plantae"),
                 height = 0.03) +
  labs(x = NULL, y = NULL, color = "Kingdom", fill = "Kingdom") +
  xlim(-5, 7.5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title   = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.text    = element_text(size = 8)
  )


ggplot(data_lnCVR2, aes(x = yi, color = Kingdom, fill = Kingdom)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Animalia" = "orange", 
               "Bacteria" = "darkgreen", 
               "Chromista" = "blue", 
               "Fungi" = "purple", 
               "Mixed" = "pink", 
               "Plantae" = "red"),
    guide = guide_legend(title = "Kingdom")
  ) +
  scale_color_manual(
    values = c("Animalia" = "orange", 
               "Bacteria" = "darkgreen", 
               "Chromista" = "blue", 
               "Fungi" = "purple", 
               "Mixed" = "pink", 
               "Plantae" = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  geom_point(aes(x = 0.2357, y = -0.02, color = "Animalia"), size = 5) +
  geom_errorbarh(aes(xmin = 0.1223, xmax = 0.3491, y = -0.02, color = "Animalia"),
                 height = 0.05) +
  geom_point(aes(x = 0.6018, y = -0.06, color = "Bacteria"), size = 5) +
  geom_errorbarh(aes(xmin = -0.1141, xmax = 1.3177, y = -0.06, color = "Bacteria"),
                 height = 0.04) +
  geom_point(aes(x = -0.0673, y = -0.1, color = "Chromista"), size = 5) +
  geom_errorbarh(aes(xmin = -0.2106, xmax = 0.0761, y = -0.1, color = "Chromista"),
                 height = 0.05) +
  geom_point(aes(x = 1.3452, y = -0.14, color = "Fungi"), size = 5) +
  geom_errorbarh(aes(xmin = -0.1848, xmax = 2.8752, y = -0.14, color = "Fungi"),
                 height = 0.04) +
  geom_point(aes(x = 0.5499, y = -0.18, color = "Mixed"), size = 5) +
  geom_errorbarh(aes(xmin = -0.4737, xmax = 1.5734, y = -0.18, color = "Mixed"),
                 height = 0.04) +
  geom_point(aes(x = 0.1232, y = -0.22, color = "Plantae"), size = 5) +
  geom_errorbarh(aes(xmin = -0.1278, xmax = 0.3743, y = -0.22, color = "Plantae"),
                 height = 0.04) +
  labs(x = NULL, y = NULL, color = "Kingdom", fill = "Kingdom") +
  xlim(-7, 7) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title   = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.text    = element_text(size = 8)
  )


ggplot(data_lnCVR3, aes(x = yi, color = Kingdom, fill = Kingdom)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(
    values = c("Animalia" = "orange", 
               "Bacteria" = "darkgreen", 
               "Chromista" = "blue", 
               "Mixed" = "purple", 
               "Plantae" = "red"),
    guide = guide_legend(title = "Kingdom")
  ) +
  scale_color_manual(
    values = c("Animalia" = "orange", 
               "Bacteria" = "darkgreen", 
               "Chromista" = "blue", 
               "Mixed" = "purple", 
               "Plantae" = "red")
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = Inf),
               size = 0.3, color = "black") +
  geom_point(aes(x = -0.0474, y = -0.05, color = "Animalia"), size = 5) +
  geom_errorbarh(aes(xmin = -0.3191, xmax = 0.2243, y = -0.05, color = "Animalia"),
                 height = 0.04) +
  geom_point(aes(x = 0.1016, y = -0.12, color = "Bacteria"), size = 5) +
  geom_errorbarh(aes(xmin = -1.5248, xmax = 1.7280, y = -0.12, color = "Bacteria"),
                 height = 0.04) +
  geom_point(aes(x = 0.0424, y = -0.19, color = "Chromista"), size = 5) +
  geom_errorbarh(aes(xmin = -0.3700, xmax = 0.4547, y = -0.19, color = "Chromista"),
                 height = 0.04) +
  geom_point(aes(x = 4.5607, y = -0.26, color = "Mixed"), size = 5) +
  geom_errorbarh(aes(xmin = 3.1253, xmax = 5.9961, y = -0.26, color = "Mixed"),
                 height = 0.04) +
  geom_point(aes(x = 1.6690, y = -0.33, color = "Plantae"), size = 5) +
  geom_errorbarh(aes(xmin = 0.4715, xmax = 2.8666, y = -0.33, color = "Plantae"),
                 height = 0.04) +
  labs(x = NULL, y = NULL, color = "Kingdom", fill = "Kingdom") +
  xlim(-5, 6) +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.text    = element_text(size = 8)
  )







####################################################################################################################
###############################  Temperature Stressors ################################################################
##################################################################################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(metafor)
library(dplyr)
library(metafor)


data <- General_data_adjusted

######################### Calculo do lnRR global #################################

data_lnRR <- escalc(
  measure = "ROM", 
  m1i = me, sd1i = sde, n1i = ne, 
  m2i = mc, sd2i = sdc, n2i = nc, 
  data = data
)

# Meta-análise
overallresult_lnRR <- rma(yi, vi,knha = TRUE,data = data_lnRR)

# Resultado final
summary(overallresult_lnRR)



########################### Calculo do lnCVR global #####################################

# Determinar as funçoes de acordo com (Nikagawa et al. 2015)


Calc.lnCVR <- function(CMean, CSD, CN, EMean, ESD, EN) {
  ES <- log(ESD) - log(EMean) + 1 / (2 * (EN - 1)) - 
    (log(CSD) - log(CMean) + 1 / (2 * (CN - 1)))
  return(ES)
}


Calc.var.lnCVR <- function(CMean, CSD, CN, EMean, ESD, EN, Equal.E.C.Corr=T) {
  if(Equal.E.C.Corr == T) {
    mvcorr <- cor.test(log(c(CMean, EMean)), log(c(CSD, ESD)))$estimate
    S2 <- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 
      2 * mvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + 
      ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 
      2 * mvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))
  } else {
    Cmvcorr <- cor.test(log(CMean), log(CSD))$estimate
    Emvcorr <- cor.test(log(EMean), log(ESD))$estimate
    S2 <- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 
      2 * Cmvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + 
      ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 
      2 * Emvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))
  }
  return(S2)
}


# Aplicar as funções Calc.lnCVR e Calc.var.lnCVR aos dados
data_lnCVR <- data %>%
  mutate(yi = Calc.lnCVR(CMean = mc, CSD = sdc, CN = nc,
                         EMean = me, ESD = sde, EN = ne),
         vi = Calc.var.lnCVR(CMean = mc, CSD = sdc, CN = nc,
                             EMean = me, ESD = sde, EN = ne))


# Meta-análise com rma
overallresult_lnCVR <- rma(yi, vi,knha = TRUE, data = data_lnCVR)

# Visualizando os resultados
summary(overallresult_lnCVR)

#################################################################################################
###Stressor and Ecosystem 
################################################################################################


########## lnRR
data_lnRR$Stressor_Ecosystem <- interaction(data_lnRR$Stressor, data_lnRR$Ecosystem)
metareg_lnRR_Stressor_Ecosystem <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem -1,knha = TRUE, data = data_lnRR, method = "REML")
summary(metareg_lnRR_Stressor_Ecosystem)



########### lnCVR
data_lnCVR$Stressor_Ecosystem <- interaction(data_lnCVR$Stressor, data_lnCVR$Ecosystem)
metareg_lnCVR_Stressor_Ecosystem <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem -1,knha = TRUE, data = data_lnCVR, method = "REML")
summary(metareg_lnCVR_Stressor_Ecosystem)

# Extrair resultados da meta-regressão lnRR
lnRR_results_Stressor_Ecosystem <- data.frame(
  Stressor_Ecosystem = rownames(coef(summary(metareg_lnRR_Stressor_Ecosystem))),
  Estimate = coef(summary(metareg_lnRR_Stressor_Ecosystem))[, "estimate"],
  CI_LB = coef(summary(metareg_lnRR_Stressor_Ecosystem))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnRR_Stressor_Ecosystem))[, "ci.ub"]
)

# Extrair resultados da meta-regressão lnCVR
# Criar o dataframe lnCVR_results_Stressor_Ecosystem
lnCVR_results_Stressor_Ecosystem <- data.frame(
  Stressor_Ecosystem = rownames(coef(summary(metareg_lnCVR_Stressor_Ecosystem))),
  Estimate = coef(summary(metareg_lnCVR_Stressor_Ecosystem))[, "estimate"],
  CI_LB = coef(summary(metareg_lnCVR_Stressor_Ecosystem))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnCVR_Stressor_Ecosystem))[, "ci.ub"]
)

# Filtrar com correspondência parcial usando grepl
filtered_lnCVR_Stressor_Ecosystem <- lnCVR_results_Stressor_Ecosystem[
  grepl("W\\.|F_temp\\.|F_W\\.", lnCVR_results_Stressor_Ecosystem$Stressor_Ecosystem),]
filtered_lnCVR_Stressor_Ecosystem1 <- lnCVR_results_Stressor_Ecosystem[
  grepl("A\\.|F_pH\\.|F_A\\.", lnCVR_results_Stressor_Ecosystem$Stressor_Ecosystem),]
filtered_lnCVR_Stressor_Ecosystem2 <- lnCVR_results_Stressor_Ecosystem[
  grepl("F_hypoxia\\.|F_light\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnCVR_results_Stressor_Ecosystem$Stressor_Ecosystem),]


# Substituir o termo comum "Stressor_Ecosystem" por uma string vazia
filtered_lnCVR_Stressor_Ecosystem$Stressor_Ecosystem <- gsub( "Stressor_Ecosystem", "", filtered_lnCVR_Stressor_Ecosystem$Stressor_Ecosystem)
filtered_lnCVR_Stressor_Ecosystem1$Stressor_Ecosystem <- gsub( "Stressor_Ecosystem", "", filtered_lnCVR_Stressor_Ecosystem1$Stressor_Ecosystem)
filtered_lnCVR_Stressor_Ecosystem2$Stressor_Ecosystem <- gsub( "Stressor_Ecosystem", "", filtered_lnCVR_Stressor_Ecosystem2$Stressor_Ecosystem)

# Criar o forest plot com ggplot2

forest_plot_lnCVR_Ecosystem <- ggplot(data = filtered_lnCVR_Stressor_Ecosystem, aes(x = Stressor_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Ecosystem",
       y = "(lnCVR)") +
  theme_minimal()
print(forest_plot_lnCVR_Ecosystem)

forest_plot2 <- ggplot(data = filtered_lnCVR_Stressor_Ecosystem2, aes(x = Stressor_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Ecosystem",
       y = "(lnCVR)") +
  theme_minimal()
print(forest_plot2)


forest_plot1 <- ggplot(data = filtered_lnCVR_Stressor_Ecosystem1, aes(x = Stressor_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Ecosystem",
       y = "(lnCVR)") +
  theme_minimal()
print(forest_plot1)
##########################################################################################

# Filtrar com correspondência parcial usando grepl
filtered_lnRR_Stressor_Ecosystem <- lnRR_results_Stressor_Ecosystem[grepl("W\\.|F_temp\\.|F_W\\.", lnRR_results_Stressor_Ecosystem$Stressor_Ecosystem),]
filtered_lnRR_Stressor_Ecosystem1 <- lnRR_results_Stressor_Ecosystem[grepl("A\\.|F_pH\\.|F_A\\.", lnRR_results_Stressor_Ecosystem$Stressor_Ecosystem),]
filtered_lnRR_Stressor_Ecosystem2 <- lnRR_results_Stressor_Ecosystem[grepl("F_hypoxia\\.|F_light\\.|F_nut\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnRR_results_Stressor_Ecosystem$Stressor_Ecosystem),]


# Substituir o termo comum "Stressor_Ecosystem" por uma string vazia
filtered_lnRR_Stressor_Ecosystem$Stressor_Ecosystem <- gsub("Stressor_Ecosystem", "", filtered_lnRR_Stressor_Ecosystem$Stressor_Ecosystem)
filtered_lnRR_Stressor_Ecosystem1$Stressor_Ecosystem <- gsub("Stressor_Ecosystem", "", filtered_lnRR_Stressor_Ecosystem1$Stressor_Ecosystem)
filtered_lnRR_Stressor_Ecosystem2$Stressor_Ecosystem <- gsub("Stressor_Ecosystem", "", filtered_lnRR_Stressor_Ecosystem2$Stressor_Ecosystem)

# Criar o forest plot com ggplot2
forest_plot_lnRR <- ggplot(data = filtered_lnRR_Stressor_Ecosystem, aes(x = Stressor_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Ecosystem",
       y = "(lnRR)") +
  theme_minimal()
print(forest_plot_lnRR)

forest_plot_lnRR1 <- ggplot(data = filtered_lnRR_Stressor_Ecosystem1, aes(x = Stressor_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Ecosystem",
       y = "(lnRR)") +
  theme_minimal()
print(forest_plot_lnRR1)

forest_plot_lnRR2 <- ggplot(data = filtered_lnRR_Stressor_Ecosystem2, aes(x = Stressor_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Ecosystem",
       y = "(lnRR)") +
  theme_minimal()
print(forest_plot_lnRR2)





#################################################################################################
###Stressor and Kingdom 
################################################################################################


########## lnRR
data_lnRR$Stressor_Kingdom <- interaction(data_lnRR$Stressor, data_lnRR$Kingdom)
metareg_lnRR_Stressor_Kingdom <- rma(yi = yi, vi = vi, mods = ~ Stressor_Kingdom -1,knha = TRUE, data = data_lnRR, method = "REML")
summary(metareg_lnRR_Stressor_Kingdom)
########### lnCVR
data_lnCVR$Stressor_Kingdom <- interaction(data_lnCVR$Stressor, data_lnCVR$Kingdom)
metareg_lnCVR_Stressor_Kingdom <- rma(yi = yi, vi = vi, mods = ~ Stressor_Kingdom -1,knha = TRUE, data = data_lnCVR, method = "REML")
summary(metareg_lnCVR_Stressor_Kingdom)



# Extrair resultados da meta-regressão lnRR
lnRR_results_Stressor_Kingdom <- data.frame(
  Stressor_Kingdom = rownames(coef(summary(metareg_lnRR_Stressor_Kingdom))),
  Estimate = coef(summary(metareg_lnRR_Stressor_Kingdom))[, "estimate"],
  CI_LB = coef(summary(metareg_lnRR_Stressor_Kingdom))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnRR_Stressor_Kingdom))[, "ci.ub"])
# Extrair resultados da meta-regressão lnCVR
lnCVR_results_Stressor_Kingdom <- data.frame(
  Stressor_Kingdom = rownames(coef(summary(metareg_lnCVR_Stressor_Kingdom))),
  Estimate = coef(summary(metareg_lnCVR_Stressor_Kingdom))[, "estimate"],
  CI_LB = coef(summary(metareg_lnCVR_Stressor_Kingdom))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnCVR_Stressor_Kingdom))[, "ci.ub"])



filtered_lnRR_Stressor_Kingdom <- lnRR_results_Stressor_Kingdom[grepl("W\\.|F_temp\\.|F_W\\.", lnRR_results_Stressor_Kingdom$Stressor_Kingdom),]
filtered_lnRR_Stressor_Kingdom$Stressor_Kingdom <- gsub("Stressor_Kingdom", "", filtered_lnRR_Stressor_Kingdom$Stressor_Kingdom)
print(filtered_lnRR_Stressor_Kingdom)
filtered_lnCVR_Stressor_Kingdom <- lnCVR_results_Stressor_Kingdom[grepl("W\\.|F_temp\\.|F_W\\.", lnCVR_results_Stressor_Kingdom$Stressor_Kingdom),]
filtered_lnCVR_Stressor_Kingdom$Stressor_Kingdom <- gsub("Stressor_Kingdom", "", filtered_lnCVR_Stressor_Kingdom$Stressor_Kingdom)
print(filtered_lnCVR_Stressor_Kingdom)


filtered_lnRR_Stressor_Kingdom1 <- lnRR_results_Stressor_Kingdom[grepl("A\\.|F_pH\\.|F_A\\.", lnRR_results_Stressor_Kingdom$Stressor_Kingdom),]
filtered_lnRR_Stressor_Kingdom1$Stressor_Kingdom <- gsub("Stressor_Kingdom", "", filtered_lnRR_Stressor_Kingdom1$Stressor_Kingdom)
print(filtered_lnRR_Stressor_Kingdom1)
filtered_lnCVR_Stressor_Kingdom1 <- lnCVR_results_Stressor_Kingdom[grepl("A\\.|F_pH\\.|F_A\\.", lnCVR_results_Stressor_Kingdom$Stressor_Kingdom),]
filtered_lnCVR_Stressor_Kingdom1$Stressor_Kingdom <- gsub("Stressor_Kingdom", "", filtered_lnCVR_Stressor_Kingdom1$Stressor_Kingdom)
print(filtered_lnCVR_Stressor_Kingdom1)


filtered_lnRR_Stressor_Kingdom2 <- lnRR_results_Stressor_Kingdom[grepl("F_hypoxia\\.|F_light\\.|F_nut\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnRR_results_Stressor_Kingdom$Stressor_Kingdom),]
filtered_lnRR_Stressor_Kingdom2$Stressor_Kingdom <- gsub("Stressor_Kingdom", "", filtered_lnRR_Stressor_Kingdom2$Stressor_Kingdom)
print(filtered_lnRR_Stressor_Kingdom2)
filtered_lnCVR_Stressor_Kingdom2 <- lnCVR_results_Stressor_Kingdom[grepl("F_hypoxia\\.|F_light\\.|F_nut\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnCVR_results_Stressor_Kingdom$Stressor_Kingdom),]
filtered_lnCVR_Stressor_Kingdom2$Stressor_Kingdom <- gsub("Stressor_Kingdom", "", filtered_lnCVR_Stressor_Kingdom2$Stressor_Kingdom)
print(filtered_lnCVR_Stressor_Kingdom2)


forest_Kingdom_lnRR <- ggplot(data = filtered_lnRR_Stressor_Kingdom, aes(x = Stressor_Kingdom, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Kingdom_lnRR)



forest_Kingdom_lnCVR <- ggplot(data = filtered_lnCVR_Stressor_Kingdom, aes(x = Stressor_Kingdom, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom",
       y = "(lnCVR)") +
  theme_minimal()

# Exibir o gráfico
print(forest_Kingdom_lnCVR)

forest_Kingdom_lnRR2 <- ggplot(data = filtered_lnRR_Stressor_Kingdom2, aes(x = Stressor_Kingdom, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Kingdom_lnRR2)

forest_Kingdom_lnCVR2 <- ggplot(data = filtered_lnCVR_Stressor_Kingdom2, aes(x = Stressor_Kingdom, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom",
       y = "(lnCVR)") +
  theme_minimal()

# Exibir o gráfico
print(forest_Kingdom_lnCVR2)


#################################################################################################
###Stressor and Type 
################################################################################################


########## lnRR
data_lnRR$Stressor_Type <- interaction(data_lnRR$Stressor, data_lnRR$Type)
metareg_lnRR_Stressor_Type <- rma(yi = yi, vi = vi, mods = ~ Stressor_Type -1,knha = TRUE, data = data_lnRR, method = "REML")
summary(metareg_lnRR_Stressor_Type)
########### lnCVR
data_lnCVR$Stressor_Type <- interaction(data_lnCVR$Stressor, data_lnCVR$Type)
metareg_lnCVR_Stressor_Type <- rma(yi = yi, vi = vi, mods = ~ Stressor_Type -1,knha = TRUE, data = data_lnCVR, method = "REML")
summary(metareg_lnCVR_Stressor_Type)



# Extrair resultados da meta-regressão lnRR
lnRR_results_Stressor_Type <- data.frame(
  Stressor_Type = rownames(coef(summary(metareg_lnRR_Stressor_Type))),
  Estimate = coef(summary(metareg_lnRR_Stressor_Type))[, "estimate"],
  CI_LB = coef(summary(metareg_lnRR_Stressor_Type))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnRR_Stressor_Type))[, "ci.ub"])
# Extrair resultados da meta-regressão lnCVR
lnCVR_results_Stressor_Type <- data.frame(
  Stressor_Type = rownames(coef(summary(metareg_lnCVR_Stressor_Type))),
  Estimate = coef(summary(metareg_lnCVR_Stressor_Type))[, "estimate"],
  CI_LB = coef(summary(metareg_lnCVR_Stressor_Type))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnCVR_Stressor_Type))[, "ci.ub"])



filtered_lnRR_Stressor_Type <- lnRR_results_Stressor_Type[grepl("W\\.|F_temp\\.|F_W\\.", lnRR_results_Stressor_Type$Stressor_Type),]
filtered_lnRR_Stressor_Type$Stressor_Type <- gsub("Stressor_Type", "", filtered_lnRR_Stressor_Type$Stressor_Type)
print(filtered_lnRR_Stressor_Type)
filtered_lnCVR_Stressor_Type <- lnCVR_results_Stressor_Type[grepl("W\\.|F_temp\\.|F_W\\.", lnCVR_results_Stressor_Type$Stressor_Type),]
filtered_lnCVR_Stressor_Type$Stressor_Type <- gsub("Stressor_Type", "", filtered_lnCVR_Stressor_Type$Stressor_Type)
print(filtered_lnCVR_Stressor_Type)


filtered_lnRR_Stressor_Type1 <- lnRR_results_Stressor_Type[grepl("A\\.|F_pH\\.|F_A\\.", lnRR_results_Stressor_Type$Stressor_Type),]
filtered_lnRR_Stressor_Type1$Stressor_Type <- gsub("Stressor_Type", "", filtered_lnRR_Stressor_Type1$Stressor_Type)
print(filtered_lnRR_Stressor_Type1)
filtered_lnCVR_Stressor_Type1 <- lnCVR_results_Stressor_Type[grepl("A\\.|F_pH\\.|F_A\\.", lnCVR_results_Stressor_Type$Stressor_Type),]
filtered_lnCVR_Stressor_Type1$Stressor_Type <- gsub("Stressor_Type", "", filtered_lnCVR_Stressor_Type1$Stressor_Type)
print(filtered_lnCVR_Stressor_Type1)

filtered_lnRR_Stressor_Type2 <- lnRR_results_Stressor_Type[grepl("F_hypoxia\\.|F_light\\.|F_nut\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnRR_results_Stressor_Type$Stressor_Type),]
filtered_lnRR_Stressor_Type2$Stressor_Type <- gsub("Stressor_Type", "", filtered_lnRR_Stressor_Type2$Stressor_Type)
print(filtered_lnRR_Stressor_Type2)
filtered_lnCVR_Stressor_Type2 <- lnCVR_results_Stressor_Type[grepl("F_hypoxia\\.|F_light\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnCVR_results_Stressor_Type$Stressor_Type),]
filtered_lnCVR_Stressor_Type2$Stressor_Type <- gsub("Stressor_Type", "", filtered_lnCVR_Stressor_Type2$Stressor_Type)
print(filtered_lnCVR_Stressor_Type2)



forest_Type_lnRR <- ggplot(data = filtered_lnRR_Stressor_Type, aes(x = Stressor_Type, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Type",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Type_lnRR)

forest_Type_lnCVR <- ggplot(data = filtered_lnCVR_Stressor_Type, aes(x = Stressor_Type, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Type",
       y = "(lnCVR)") +
  theme_minimal()
print(forest_Type_lnCVR)

forest_Type_lnRR2 <- ggplot(data = filtered_lnRR_Stressor_Type2, aes(x = Stressor_Type, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Type",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Type_lnRR2)

forest_Type_lnCVR2 <- ggplot(data = filtered_lnCVR_Stressor_Type2, aes(x = Stressor_Type, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Type",
       y = "(lnCVR)") +
  theme_minimal()
print(forest_Type_lnCVR2)




#################################################################################################
###Stressor and Kingdom_Ecosystem 
################################################################################################


########## lnRR
data_lnRR$Stressor_Kingdom_Ecosystem <- interaction(data_lnRR$Stressor, data_lnRR$Kingdom, data_lnRR$Ecosystem)
metareg_lnRR_Stressor_Kingdom_Ecosystem <- rma(yi = yi, vi = vi, mods = ~ Stressor_Kingdom_Ecosystem -1,knha = TRUE, data = data_lnRR, method = "REML")
summary(metareg_lnRR_Stressor_Kingdom_Ecosystem)
########### lnCVR
data_lnCVR$Stressor_Kingdom_Ecosystem <- interaction(data_lnCVR$Stressor, data_lnCVR$Kingdom, data_lnCVR$Ecosystem)
metareg_lnCVR_Stressor_Kingdom_Ecosystem <- rma(yi = yi, vi = vi, mods = ~ Stressor_Kingdom_Ecosystem -1,knha = TRUE, data = data_lnCVR, method = "REML")
summary(metareg_lnCVR_Stressor_Kingdom_Ecosystem)



# Extrair resultados da meta-regressão lnRR
lnRR_results_Stressor_Kingdom_Ecosystem <- data.frame(
  Stressor_Kingdom_Ecosystem = rownames(coef(summary(metareg_lnRR_Stressor_Kingdom_Ecosystem))),
  Estimate = coef(summary(metareg_lnRR_Stressor_Kingdom_Ecosystem))[, "estimate"],
  CI_LB = coef(summary(metareg_lnRR_Stressor_Kingdom_Ecosystem))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnRR_Stressor_Kingdom_Ecosystem))[, "ci.ub"])
# Extrair resultados da meta-regressão lnCVR
lnCVR_results_Stressor_Kingdom_Ecosystem <- data.frame(
  Stressor_Kingdom_Ecosystem = rownames(coef(summary(metareg_lnCVR_Stressor_Kingdom_Ecosystem))),
  Estimate = coef(summary(metareg_lnCVR_Stressor_Kingdom_Ecosystem))[, "estimate"],
  CI_LB = coef(summary(metareg_lnCVR_Stressor_Kingdom_Ecosystem))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnCVR_Stressor_Kingdom_Ecosystem))[, "ci.ub"])



filtered_lnRR_Stressor_Kingdom_Ecosystem <- lnRR_results_Stressor_Kingdom_Ecosystem[grepl("W\\.|F_temp\\.|F_W\\.", lnRR_results_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem),]
filtered_lnRR_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem <- gsub("Stressor_Kingdom_Ecosystem", "", filtered_lnRR_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem)
print(filtered_lnRR_Stressor_Kingdom_Ecosystem)
filtered_lnCVR_Stressor_Kingdom_Ecosystem <- lnCVR_results_Stressor_Kingdom_Ecosystem[grepl("W\\.|F_temp\\.|F_W\\.", lnCVR_results_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem),]
filtered_lnCVR_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem <- gsub("Stressor_Kingdom_Ecosystem", "", filtered_lnCVR_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem)
print(filtered_lnCVR_Stressor_Kingdom_Ecosystem)

filtered_lnRR_Stressor_Kingdom_Ecosystem1 <- lnRR_results_Stressor_Kingdom_Ecosystem[grepl("A\\.|F_pH\\.|F_A\\.", lnRR_results_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem),]
filtered_lnRR_Stressor_Kingdom_Ecosystem1$Stressor_Kingdom_Ecosystem <- gsub("Stressor_Kingdom_Ecosystem", "", filtered_lnRR_Stressor_Kingdom_Ecosystem1$Stressor_Kingdom_Ecosystem)
print(filtered_lnRR_Stressor_Kingdom_Ecosystem1)
filtered_lnCVR_Stressor_Kingdom_Ecosystem1 <- lnCVR_results_Stressor_Kingdom_Ecosystem[grepl("A\\.|F_pH\\.|F_A\\.", lnCVR_results_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem),]
filtered_lnCVR_Stressor_Kingdom_Ecosystem1$Stressor_Kingdom_Ecosystem <- gsub("Stressor_Kingdom_Ecosystem", "", filtered_lnCVR_Stressor_Kingdom_Ecosystem1$Stressor_Kingdom_Ecosystem)
print(filtered_lnCVR_Stressor_Kingdom_Ecosystem1)


filtered_lnRR_Stressor_Kingdom_Ecosystem2 <- lnRR_results_Stressor_Kingdom_Ecosystem[grepl("F_hypoxia\\.|F_light\\.|F_nut\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnRR_results_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem),]
filtered_lnRR_Stressor_Kingdom_Ecosystem2$Stressor_Kingdom_Ecosystem <- gsub("Stressor_Kingdom_Ecosystem", "", filtered_lnRR_Stressor_Kingdom_Ecosystem2$Stressor_Kingdom_Ecosystem)
print(filtered_lnRR_Stressor_Kingdom_Ecosystem2)
filtered_lnCVR_Stressor_Kingdom_Ecosystem2 <- lnCVR_results_Stressor_Kingdom_Ecosystem[grepl("F_hypoxia\\.|F_light\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnCVR_results_Stressor_Kingdom_Ecosystem$Stressor_Kingdom_Ecosystem),]
filtered_lnCVR_Stressor_Kingdom_Ecosystem2$Stressor_Kingdom_Ecosystem <- gsub("Stressor_Kingdom_Ecosystem", "", filtered_lnCVR_Stressor_Kingdom_Ecosystem2$Stressor_Kingdom_Ecosystem)
print(filtered_lnCVR_Stressor_Kingdom_Ecosystem2)

forest_Kingdom_Ecosystem_lnRR <- ggplot(data = filtered_lnRR_Stressor_Kingdom_Ecosystem, aes(x = Stressor_Kingdom_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, lineKingdom_Ecosystem = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom_Ecosystem",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Kingdom_Ecosystem_lnRR)

forest_Kingdom_Ecosystem_lnCVR <- ggplot(data = filtered_lnCVR_Stressor_Kingdom_Ecosystem, aes(x = Stressor_Kingdom_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, lineKingdom_Ecosystem = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom_Ecosystem",
       y = "(lnCVR)") +
  theme_minimal()

# Exibir o gráfico
print(forest_Kingdom_Ecosystem_lnCVR)



forest_Kingdom_Ecosystem_lnRR2 <- ggplot(data = filtered_lnRR_Stressor_Kingdom_Ecosystem2, aes(x = Stressor_Kingdom_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, lineKingdom_Ecosystem = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom_Ecosystem",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Kingdom_Ecosystem_lnRR2)

forest_Kingdom_Ecosystem_lnCVR2 <- ggplot(data = filtered_lnCVR_Stressor_Kingdom_Ecosystem2, aes(x = Stressor_Kingdom_Ecosystem, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, lineKingdom_Ecosystem = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom_Ecosystem",
       y = "(lnCVR)") +
  theme_minimal()

# Exibir o gráfico
print(forest_Kingdom_Ecosystem_lnCVR2)




#################################################################################################
###Stressor and Kingdom_Type 
################################################################################################


########## lnRR
data_lnRR$Stressor_Kingdom_Type <- interaction(data_lnRR$Stressor, data_lnRR$Kingdom, data_lnRR$Type)
metareg_lnRR_Stressor_Kingdom_Type <- rma(yi = yi, vi = vi, mods = ~ Stressor_Kingdom_Type -1,knha = TRUE, data = data_lnRR, method = "REML")
summary(metareg_lnRR_Stressor_Kingdom_Type)
########### lnCVR
data_lnCVR$Stressor_Kingdom_Type <- interaction(data_lnCVR$Stressor, data_lnCVR$Kingdom, data_lnCVR$Type)
metareg_lnCVR_Stressor_Kingdom_Type <- rma(yi = yi, vi = vi, mods = ~ Stressor_Kingdom_Type -1,knha = TRUE, data = data_lnCVR, method = "REML")
summary(metareg_lnCVR_Stressor_Kingdom_Type)



# Extrair resultados da meta-regressão lnRR
lnRR_results_Stressor_Kingdom_Type <- data.frame(
  Stressor_Kingdom_Type = rownames(coef(summary(metareg_lnRR_Stressor_Kingdom_Type))),
  Estimate = coef(summary(metareg_lnRR_Stressor_Kingdom_Type))[, "estimate"],
  CI_LB = coef(summary(metareg_lnRR_Stressor_Kingdom_Type))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnRR_Stressor_Kingdom_Type))[, "ci.ub"])
# Extrair resultados da meta-regressão lnCVR
lnCVR_results_Stressor_Kingdom_Type <- data.frame(
  Stressor_Kingdom_Type = rownames(coef(summary(metareg_lnCVR_Stressor_Kingdom_Type))),
  Estimate = coef(summary(metareg_lnCVR_Stressor_Kingdom_Type))[, "estimate"],
  CI_LB = coef(summary(metareg_lnCVR_Stressor_Kingdom_Type))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnCVR_Stressor_Kingdom_Type))[, "ci.ub"])



filtered_lnRR_Stressor_Kingdom_Type <- lnRR_results_Stressor_Kingdom_Type[grepl("W\\.|F_temp\\.|F_W\\.", lnRR_results_Stressor_Kingdom_Type$Stressor_Kingdom_Type),]
filtered_lnRR_Stressor_Kingdom_Type$Stressor_Kingdom_Type <- gsub("Stressor_Kingdom_Type", "", filtered_lnRR_Stressor_Kingdom_Type$Stressor_Kingdom_Type)
print(filtered_lnRR_Stressor_Kingdom_Type)
filtered_lnCVR_Stressor_Kingdom_Type <- lnCVR_results_Stressor_Kingdom_Type[grepl("W\\.|F_temp\\.|F_W\\.", lnCVR_results_Stressor_Kingdom_Type$Stressor_Kingdom_Type),]
filtered_lnCVR_Stressor_Kingdom_Type$Stressor_Kingdom_Type <- gsub("Stressor_Kingdom_Type", "", filtered_lnCVR_Stressor_Kingdom_Type$Stressor_Kingdom_Type)
print(filtered_lnCVR_Stressor_Kingdom_Type)


filtered_lnRR_Stressor_Kingdom_Type1 <- lnRR_results_Stressor_Kingdom_Type[grepl("A\\.|F_pH\\.|F_A\\.", lnRR_results_Stressor_Kingdom_Type$Stressor_Kingdom_Type),]
filtered_lnRR_Stressor_Kingdom_Type1$Stressor_Kingdom_Type <- gsub("Stressor_Kingdom_Type", "", filtered_lnRR_Stressor_Kingdom_Type1$Stressor_Kingdom_Type)
print(filtered_lnRR_Stressor_Kingdom_Type1)
filtered_lnCVR_Stressor_Kingdom_Type1 <- lnCVR_results_Stressor_Kingdom_Type[grepl("A\\.|F_pH\\.|F_A\\.", lnCVR_results_Stressor_Kingdom_Type$Stressor_Kingdom_Type),]
filtered_lnCVR_Stressor_Kingdom_Type1$Stressor_Kingdom_Type <- gsub("Stressor_Kingdom_Type", "", filtered_lnCVR_Stressor_Kingdom_Type1$Stressor_Kingdom_Type)
print(filtered_lnCVR_Stressor_Kingdom_Type1)

filtered_lnRR_Stressor_Kingdom_Type2 <- lnRR_results_Stressor_Kingdom_Type[grepl("F_hypoxia\\.|F_light\\.|F_nut\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnRR_results_Stressor_Kingdom_Type$Stressor_Kingdom_Type),]
filtered_lnRR_Stressor_Kingdom_Type2$Stressor_Kingdom_Type <- gsub("Stressor_Kingdom_Type", "", filtered_lnRR_Stressor_Kingdom_Type2$Stressor_Kingdom_Type)
print(filtered_lnRR_Stressor_Kingdom_Type2)
filtered_lnCVR_Stressor_Kingdom_Type2 <- lnCVR_results_Stressor_Kingdom_Type[grepl("F_hypoxia\\.|F_light\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnCVR_results_Stressor_Kingdom_Type$Stressor_Kingdom_Type),]
filtered_lnCVR_Stressor_Kingdom_Type2$Stressor_Kingdom_Type <- gsub("Stressor_Kingdom_Type", "", filtered_lnCVR_Stressor_Kingdom_Type2$Stressor_Kingdom_Type)
print(filtered_lnCVR_Stressor_Kingdom_Type2)

forest_Kingdom_Type_lnRR <- ggplot(data = filtered_lnRR_Stressor_Kingdom_Type, aes(x = Stressor_Kingdom_Type, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, lineKingdom_Type = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom_Type",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Kingdom_Type_lnRR)

forest_Kingdom_Type_lnCVR <- ggplot(data = filtered_lnCVR_Stressor_Kingdom_Type, aes(x = Stressor_Kingdom_Type, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, lineKingdom_Type = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom_Type",
       y = "(lnCVR)") +
  theme_minimal()

# Exibir o gráfico
print(forest_Kingdom_Type_lnCVR)


forest_Kingdom_Type_lnRR2 <- ggplot(data = filtered_lnRR_Stressor_Kingdom_Type2, aes(x = Stressor_Kingdom_Type, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, lineKingdom_Type = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom_Type",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Kingdom_Type_lnRR2)

forest_Kingdom_Type_lnCVR2 <- ggplot(data = filtered_lnCVR_Stressor_Kingdom_Type2, aes(x = Stressor_Kingdom_Type, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, lineKingdom_Type = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Kingdom_Type",
       y = "(lnCVR)") +
  theme_minimal()
print(forest_Kingdom_Type_lnCVR2)



# Lista dos dataframes a serem combinados
dataframes_list <- list(
  filtered_lnCVR_Stressor_Kingdom_Type1,
  filtered_lnCVR_Stressor_Kingdom_Ecosystem1,
  filtered_lnCVR_Stressor_Kingdom1,
  filtered_lnCVR_Stressor_Ecosystem1,
  filtered_lnCVR_Stressor_Type1
)

# Padronizar os nomes das colunas
standardized_dataframes <- lapply(dataframes_list, function(df) {
  colnames(df)[1] <- "Standard_Column"  # Renomear a primeira coluna
  return(df)
})

# Combinar os dataframes
combined_lnCVR <- do.call(rbind, standardized_dataframes)

# Visualizar o resultado
print(combined_lnCVR)

# Carregar ggplot2
library(ggplot2)

# Criar o forest plot
forest_plot <- ggplot(data = combined_lnCVR, aes(x = Standard_Column, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e IC
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = NULL,
       y = "(lnCVR)") +
  theme_minimal()

# Exibir o gráfico
print(forest_plot)


##################################################################

# Lista dos dataframes a serem combinados
dataframes_list <- list(
  filtered_lnRR_Stressor_Kingdom_Type1,
  filtered_lnRR_Stressor_Kingdom_Ecosystem1,
  filtered_lnRR_Stressor_Kingdom1,
  filtered_lnRR_Stressor_Ecosystem1,
  filtered_lnRR_Stressor_Type1
)

# Padronizar os nomes das colunas
standardized_dataframes <- lapply(dataframes_list, function(df) {
  colnames(df)[1] <- "Standard_Column"  # Renomear a primeira coluna
  return(df)
})

# Combinar os dataframes
combined_lnRR <- do.call(rbind, standardized_dataframes)

# Visualizar o resultado
print(combined_lnRR)

# Carregar ggplot2
library(ggplot2)

# Criar o forest plot
forest_plot <- ggplot(data = combined_lnRR, aes(x = Standard_Column, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e IC
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = NULL,
       y = "(lnRR)") +
  theme_minimal()

# Exibir o gráfico
print(forest_plot)


#####################################################################################
####################################################################################




#################################################################################################
###Stressor and Category 
################################################################################################


########## lnRR
data_lnRR$Stressor_Category <- interaction(data_lnRR$Stressor, data_lnRR$Category)
metareg_lnRR_Stressor_Category <- rma(yi = yi, vi = vi, mods = ~ Stressor_Category -1,knha = TRUE, data = data_lnRR, method = "REML")
summary(metareg_lnRR_Stressor_Category)
########### lnCVR
data_lnCVR$Stressor_Category <- interaction(data_lnCVR$Stressor, data_lnCVR$Category)
metareg_lnCVR_Stressor_Category <- rma(yi = yi, vi = vi, mods = ~ Stressor_Category -1,knha = TRUE, data = data_lnCVR, method = "REML")
summary(metareg_lnCVR_Stressor_Category)



# Extrair resultados da meta-regressão lnRR
lnRR_results_Stressor_Category <- data.frame(
  Stressor_Category = rownames(coef(summary(metareg_lnRR_Stressor_Category))),
  Estimate = coef(summary(metareg_lnRR_Stressor_Category))[, "estimate"],
  CI_LB = coef(summary(metareg_lnRR_Stressor_Category))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnRR_Stressor_Category))[, "ci.ub"])
# Extrair resultados da meta-regressão lnCVR
lnCVR_results_Stressor_Category <- data.frame(
  Stressor_Category = rownames(coef(summary(metareg_lnCVR_Stressor_Category))),
  Estimate = coef(summary(metareg_lnCVR_Stressor_Category))[, "estimate"],
  CI_LB = coef(summary(metareg_lnCVR_Stressor_Category))[, "ci.lb"],
  CI_UB = coef(summary(metareg_lnCVR_Stressor_Category))[, "ci.ub"])



filtered_lnRR_Stressor_Category <- lnRR_results_Stressor_Category[grepl("W\\.|F_temp\\.|F_W\\.", lnRR_results_Stressor_Category$Stressor_Category),]
filtered_lnRR_Stressor_Category$Stressor_Category <- gsub("Stressor_Category", "", filtered_lnRR_Stressor_Category$Stressor_Category)
print(filtered_lnRR_Stressor_Category)
filtered_lnCVR_Stressor_Category <- lnCVR_results_Stressor_Category[grepl("W\\.|F_temp\\.|F_W\\.", lnCVR_results_Stressor_Category$Stressor_Category),]
filtered_lnCVR_Stressor_Category$Stressor_Category <- gsub("Stressor_Category", "", filtered_lnCVR_Stressor_Category$Stressor_Category)
print(filtered_lnCVR_Stressor_Category)


filtered_lnRR_Stressor_Category1 <- lnRR_results_Stressor_Category[grepl("A\\.|F_pH\\.|F_A\\.", lnRR_results_Stressor_Category$Stressor_Category),]
filtered_lnRR_Stressor_Category1$Stressor_Category <- gsub("Stressor_Category", "", filtered_lnRR_Stressor_Category1$Stressor_Category)
print(filtered_lnRR_Stressor_Category1)
filtered_lnCVR_Stressor_Category1 <- lnCVR_results_Stressor_Category[grepl("A\\.|F_pH\\.|F_A\\.", lnCVR_results_Stressor_Category$Stressor_Category),]
filtered_lnCVR_Stressor_Category1$Stressor_Category <- gsub("Stressor_Category", "", filtered_lnCVR_Stressor_Category1$Stressor_Category)
print(filtered_lnCVR_Stressor_Category1)


filtered_lnRR_Stressor_Category2 <- lnRR_results_Stressor_Category[grepl("F_hypoxia\\.|F_light\\.|F_nut\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnRR_results_Stressor_Category$Stressor_Category),]
filtered_lnRR_Stressor_Category2$Stressor_Category <- gsub("Stressor_Category", "", filtered_lnRR_Stressor_Category2$Stressor_Category)
print(filtered_lnRR_Stressor_Category2)
filtered_lnCVR_Stressor_Category2 <- lnCVR_results_Stressor_Category[grepl("F_hypoxia\\.|F_light\\.|F_nut\\.|F_water\\.|F_hyperoxia\\.|F_pCO2\\.", lnCVR_results_Stressor_Category$Stressor_Category),]
filtered_lnCVR_Stressor_Category2$Stressor_Category <- gsub("Stressor_Category", "", filtered_lnCVR_Stressor_Category2$Stressor_Category)
print(filtered_lnCVR_Stressor_Category2)


forest_Category_lnRR <- ggplot(data = filtered_lnRR_Stressor_Category, aes(x = Stressor_Category, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Category",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Category_lnRR)



forest_Category_lnCVR <- ggplot(data = filtered_lnCVR_Stressor_Category, aes(x = Stressor_Category, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Category",
       y = "(lnCVR)") +
  theme_minimal()

# Exibir o gráfico
print(forest_Category_lnCVR)

forest_Category_lnRR2 <- ggplot(data = filtered_lnRR_Stressor_Category2, aes(x = Stressor_Category, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Category",
       y = "(lnRR)") +
  theme_minimal()
print(forest_Category_lnRR2)

forest_Category_lnCVR2 <- ggplot(data = filtered_lnCVR_Stressor_Category2, aes(x = Stressor_Category, y = Estimate)) +
  geom_pointrange(aes(ymin = CI_LB, ymax = CI_UB), size = 0.5) +  # Pontos e ICs
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Linha de referência
  coord_flip() +  # Inverter os eixos
  labs(title = NULL,
       x = "Stressor/Category",
       y = "(lnCVR)") +
  theme_minimal()

# Exibir o gráfico
print(forest_Category_lnCVR2)









######################################## FIM ###################################################




# Filtrar temperatura na coluna Stressor para lnRR
data_lnRR_filtered <- data_lnRR %>%
  filter(Stressor %in% c("F_temp", "W", "F_W"))

# Filtrar temperatura na coluna Stressor para lnCVR
data_lnCVR_filtered <- data_lnCVR %>%
  filter(Stressor %in% c("F_temp", "W", "F_W"))

### Kingdom
data_lnRR_filtered$Stressor_Kingdom <- interaction(data_lnRR_filtered$Stressor, data_lnRR_filtered$Kingdom)
metareg_lnRR_Stressor_Kingdom_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Kingdom -1,knha = TRUE, data = data_lnRR_filtered, method = "REML")
summary(metareg_lnRR_Stressor_Kingdom_temperature)
data_lnCVR_filtered$Stressor_Kingdom <- interaction(data_lnCVR_filtered$Stressor, data_lnCVR_filtered$Kingdom)
metareg_lnCVR_Stressor_Kingdom_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Kingdom -1,knha = TRUE, data = data_lnCVR_filtered, method = "REML")
summary(metareg_lnCVR_Stressor_Kingdom_temperature)

### Ecosystem
data_lnRR_filtered$Stressor_Ecosystem <- interaction(data_lnRR_filtered$Stressor, data_lnRR_filtered$Ecosystem)
metareg_lnRR_Stressor_Ecosystem_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem -1,knha = TRUE, data = data_lnRR_filtered, method = "REML")
summary(metareg_lnRR_Stressor_Ecosystem_temperature)
data_lnCVR_filtered$Stressor_Ecosystem <- interaction(data_lnCVR_filtered$Stressor, data_lnCVR_filtered$Ecosystem)
metareg_lnCVR_Stressor_Ecosystem_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Ecosystem -1,knha = TRUE, data = data_lnCVR_filtered, method = "REML")
summary(metareg_lnCVR_Stressor_Ecosystem_temperature)

### Type
data_lnRR_filtered$Stressor_Type <- interaction(data_lnRR_filtered$Stressor, data_lnRR_filtered$Type)
metareg_lnRR_Stressor_Type_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Type -1,knha = TRUE, data = data_lnRR_filtered, method = "REML")
summary(metareg_lnRR_Stressor_Type_temperature)
data_lnCVR_filtered$Stressor_Type <- interaction(data_lnCVR_filtered$Stressor, data_lnCVR_filtered$Type)
metareg_lnCVR_Stressor_Type_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Type -1,knha = TRUE, data = data_lnCVR_filtered, method = "REML")
summary(metareg_lnCVR_Stressor_Type_temperature)

### Category
data_lnRR_filtered$Stressor_Category <- interaction(data_lnRR_filtered$Stressor, data_lnRR_filtered$Category)
metareg_lnRR_Stressor_Category_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Category -1,knha = TRUE, data = data_lnRR_filtered, method = "REML")
summary(metareg_lnRR_Stressor_Category_temperature)
data_lnCVR_filtered$Stressor_Category <- interaction(data_lnCVR_filtered$Stressor, data_lnCVR_filtered$Category)
metareg_lnCVR_Stressor_Category_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Category -1,knha = TRUE, data = data_lnCVR_filtered, method = "REML")
summary(metareg_lnCVR_Stressor_Category_temperature)

### Biome
data_lnRR_filtered$Stressor_Biome <- interaction(data_lnRR_filtered$Stressor, data_lnRR_filtered$Biome)
metareg_lnRR_Stressor_Biome_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Biome -1,knha = TRUE, data = data_lnRR_filtered, method = "REML")
summary(metareg_lnRR_Stressor_Biome_temperature)
data_lnCVR_filtered$Stressor_Biome <- interaction(data_lnCVR_filtered$Stressor, data_lnCVR_filtered$Biome)
metareg_lnCVR_Stressor_Biome_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Biome -1,knha = TRUE, data = data_lnCVR_filtered, method = "REML")
summary(metareg_lnCVR_Stressor_Biome_temperature)



####################################### triplo moderador    ########################


### Category_Kingdom
data_lnRR_filtered$Stressor_Category_Kingdom <- interaction(data_lnRR_filtered$Stressor, data_lnRR_filtered$Category, data_lnRR_filtered$Kingdom)
metareg_lnRR_Stressor_Category_Kingdom_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Category_Kingdom -1,knha = TRUE, data = data_lnRR_filtered, method = "REML")
summary(metareg_lnRR_Stressor_Category_Kingdom_temperature)
data_lnCVR_filtered$Stressor_Category_Kingdom <- interaction(data_lnCVR_filtered$Stressor, data_lnCVR_filtered$Category, data_lnCVR_filtered$Kingdom)
metareg_lnCVR_Stressor_Category_Kingdom_temperature <- rma(yi = yi, vi = vi, mods = ~ Stressor_Category_Kingdom -1,knha = TRUE, data = data_lnCVR_filtered, method = "REML")
summary(metareg_lnCVR_Stressor_Category_Kingdom_temperature)






################################# Sensitivity analisis ###########################################

# Análise de sensibilidade para lnRR
leave1out_lnRR <- leave1out(overallresult_lnRR)
print(leave1out_lnRR)

# Análise de sensibilidade para lnCVR
leave1out_lnCVR <- leave1out(overallresult_lnCVR)
print(leave1out_lnCVR)




#################### Gráfico de funnel plot para lnRR
funnel(overallresult_lnRR, main = NULL, xlab = "lnRR")
# Teste de Egger para lnRR
egger_test_lnRR <- regtest(overallresult_lnRR)
print(egger_test_lnRR)



# Gráfico de funnel plot para lnCVR
funnel(overallresult_lnCVR, main = NULL, xlab = "lnCVR")
# Teste de Egger para lnCVR
egger_test_lnCVR <- regtest(overallresult_lnCVR)
print(egger_test_lnCVR)




########################  Analise de subgrupos - classificação trófica ###################################

unique(data_lnRR_filtered$Species)
# Criando uma nova coluna com a classificação trófica
data_lnRR_filtered$Trophic_Mode <- ifelse(data_lnRR_filtered$Species %in% c(
  "Asterionella", "Staurastrum", "Scenedesmus", "Chlorella", "Kirchneriella", 
  "Microcystis aeruginosa", "Chlorella pyrenoidosa", "Cyclotella meneghiniana", 
  "Emiliania Huxleyi", "Phymatolithon lusitanicum", "Trichodesmium sp.", 
  "Thalassiosira pseudonana", "Phaeodactylum tricornutum & Thalassiosira pseudonana", 
  "Medicago sativa", "Stellaria media", 
  "Community (Synedra, Staurastrum, Chlorella)", "Community (Asterionella, Staurastrum, Kirchneriella)", 
  "Community (Synedra, Asterionella, Scenedesmus)", "Community (all species, 6)", 
  "Plankton community", "Natural plankton community", "Phytoplankton community", 
  "Cyanobacteria", "Cyanobacteria community", "Weeds community"), 
  "Autotroph", "Heterotroph")

# Contagem de observações por Trophic_Mode e Tipo de Estressor
table(data_lnRR_filtered$Trophic_Mode, data_lnRR_filtered$Stressor)
########## A maioria das observações no estudo são de heterótrofos 
## (799 observações, 68%), 
# enquanto os autótrofos representam 32% (377 observações).


# Criando uma nova coluna com a classificação trófica
data_lnCVR_filtered$Trophic_Mode <- ifelse(data_lnCVR_filtered$Species %in% c(
  "Asterionella", "Staurastrum", "Scenedesmus", "Chlorella", "Kirchneriella", 
  "Microcystis aeruginosa", "Chlorella pyrenoidosa", "Cyclotella meneghiniana", 
  "Emiliania Huxleyi", "Phymatolithon lusitanicum", "Trichodesmium sp.", 
  "Thalassiosira pseudonana", "Phaeodactylum tricornutum & Thalassiosira pseudonana", 
  "Medicago sativa", "Stellaria media", 
  "Community (Synedra, Staurastrum, Chlorella)", "Community (Asterionella, Staurastrum, Kirchneriella)", 
  "Community (Synedra, Asterionella, Scenedesmus)", "Community (all species, 6)", 
  "Plankton community", "Natural plankton community", "Phytoplankton community", 
  "Cyanobacteria", "Cyanobacteria community", "Weeds community"), 
  "Autotroph", "Heterotroph")



################ lnRR ############
data_lnRR_f_temp <- rma(yi = yi, vi = vi, 
                        mods = ~ Trophic_Mode -1, 
                        data = subset(data_lnRR_filtered, Stressor == "F_temp"),
                        method = "REML")
summary(data_lnRR_f_temp)

data_lnRR_F_W <- rma(yi = yi, vi = vi, 
                     mods = ~ Trophic_Mode -1, 
                     data = subset(data_lnRR_filtered, Stressor == "F_W"),
                     method = "REML")
summary(data_lnRR_F_W)

data_lnRR_W <- rma(yi = yi, vi = vi, 
                   mods = ~ Trophic_Mode -1, 
                   data = subset(data_lnRR_filtered, Stressor == "W"),
                   method = "REML")
summary(data_lnRR_W)


################ lnCVR ############
data_lnCVR_f_temp <- rma(yi = yi, vi = vi, 
                         mods = ~ Trophic_Mode -1, 
                         data = subset(data_lnCVR_filtered, Stressor == "F_temp"),
                         method = "REML")
summary(data_lnCVR_f_temp)

data_lnCVR_F_W <- rma(yi = yi, vi = vi, 
                      mods = ~ Trophic_Mode -1, 
                      data = subset(data_lnCVR_filtered, Stressor == "F_W"),
                      method = "REML")
summary(data_lnCVR_F_W)

data_lnCVR_W <- rma(yi = yi, vi = vi, 
                    mods = ~ Trophic_Mode -1, 
                    data = subset(data_lnCVR_filtered, Stressor == "W"),
                    method = "REML")
summary(data_lnCVR_W)





###########  graficos  #######


# Carregar pacotes
library(ggplot2)
library(ggdist)
library(dplyr)
library(tidyverse)

########### lnRR ############

# Criar dataframe com resultados da meta-análise para cada Stressor
data_plot_lnRR <- data.frame(
  Stressor = rep(c("F_temp", "F_W", "W"), each = 2),
  Trophic_Mode = rep(c("Autotroph", "Heterotroph"), 3),
  Effect_Size = c(data_lnRR_f_temp$b, data_lnRR_F_W$b, data_lnRR_W$b),
  CI_Lower = c(data_lnRR_f_temp$ci.lb, data_lnRR_F_W$ci.lb, data_lnRR_W$ci.lb),
  CI_Upper = c(data_lnRR_f_temp$ci.ub, data_lnRR_F_W$ci.ub, data_lnRR_W$ci.ub)
)

ggplot(data_lnRR_filtered, aes(x = Trophic_Mode, y = yi, fill = Trophic_Mode)) +
  # Adiciona distribuição de densidade por Stressor
  geom_violin(aes(color = Trophic_Mode), trim = TRUE, alpha = 0.3, draw_quantiles = c(0.5)) +
  # Adiciona pontos jitter para visualizar a dispersão
  geom_jitter(aes(color = Trophic_Mode), width = 0.1, alpha = 0.4, size = 2) +
  # Adiciona intervalos de confiança da meta-análise por Stressor
  geom_pointrange(data = data_plot_lnRR, aes(x = Trophic_Mode, y = Effect_Size, ymin = CI_Lower, ymax = CI_Upper), 
                  color = "black", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Separar por Stressor
  facet_wrap(~ Stressor, ncol = 1) +
  # Ajuste para cortar a metade esquerda do violin plot
  coord_flip() +
  ylim(-4, 3.2) +
  # Personalizações
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.text.y = element_blank())+
  labs(x = NULL, y = "                            Effect Size (lnRR)", title = NULL) +
  scale_fill_manual(values = c("Autotroph" = "#E69F00", "Heterotroph" = "#56B4E9")) +
  scale_color_manual(values = c("Autotroph" = "#E69F00", "Heterotroph" = "#56B4E9"))





########### lnCVR ############

# Criar dataframe com resultados da meta-análise para cada Stressor
data_plot_lnCVR <- data.frame(
  Stressor = rep(c("F_temp", "F_W", "W"), each = 2),
  Trophic_Mode = rep(c("Autotroph", "Heterotroph"), 3),
  Effect_Size = c(data_lnCVR_f_temp$b, data_lnCVR_F_W$b, data_lnCVR_W$b),
  CI_Lower = c(data_lnCVR_f_temp$ci.lb, data_lnCVR_F_W$ci.lb, data_lnCVR_W$ci.lb),
  CI_Upper = c(data_lnCVR_f_temp$ci.ub, data_lnCVR_F_W$ci.ub, data_lnCVR_W$ci.ub)
)

ggplot(data_lnCVR_filtered, aes(x = Trophic_Mode, y = yi, fill = Trophic_Mode)) +
  # Adiciona distribuição de densidade por Stressor
  geom_violin(aes(color = Trophic_Mode), trim = TRUE, alpha = 0.3, draw_quantiles = c(0.5)) +
  # Adiciona pontos jitter para visualizar a dispersão
  geom_jitter(aes(color = Trophic_Mode), width = 0.1, alpha = 0.4, size = 2) +
  # Adiciona intervalos de confiança da meta-análise por Stressor
  geom_pointrange(data = data_plot_lnCVR, aes(x = Trophic_Mode, y = Effect_Size, ymin = CI_Lower, ymax = CI_Upper), 
                  color = "black", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Separar por Stressor
  facet_wrap(~ Stressor, ncol = 1) +
  # Ajuste para cortar a metade esquerda do violin plot
  coord_flip() +
  ylim(-7.6, 7.6) +
  # Personalizações
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.text.y = element_blank())+
  labs(x = NULL, y = "Effect Size (lnCVR)", title = NULL) +
  scale_fill_manual(values = c("Autotroph" = "#E69F00", "Heterotroph" = "#56B4E9")) +
  scale_color_manual(values = c("Autotroph" = "#E69F00", "Heterotroph" = "#56B4E9"))

