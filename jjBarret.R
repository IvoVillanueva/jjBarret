library(tidyverse)
library(rvest)
library(janitor)
library(stringr)
library(extrafont)
library(cowplot)
library(ggtext)
library(ggimage)
theme_ivo <- function () { 
  theme_minimal(base_size=8, base_family="Chivo") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = '#f4f4f4', color = "#f4f4f4"),
      plot.caption = element_markdown(size = 6.5)
    )
}


# sacar datos de  basketaball-reference

df <- read_html("https://www.basketball-reference.com/leagues/NBA_2021_per_poss.html") %>% 
  html_node("table") %>% 
  html_table %>% 
  clean_names() %>% # Janitor library que transforma los nombres a unos mas accesibles
  filter(player != "Player" )

# Lo guardamos en csv

write.csv(df, "df_jjbarret.csv", row.names = FALSE)
jjbarret <- read.csv("df_jjbarret.csv", stringsAsFactors = FALSE)


# calculamos los datos ----------------------------------------------------



jjbarret <- jjbarret %>% 
          mutate(tm=ifelse(player=="R.J. Hampton"& tm=="DEN","NON",tm),
                 tm=ifelse(player=="R.J. Hampton"& tm=="ORL","NON",tm),
                 tm=ifelse(player=="R.J. Hampton"& tm=="TOT","ORL",tm)
            
          ) %>% 
          filter(age <= 20 & tm != "NON" & g > 12) %>% 
 
  select(player, Pts = pts, Ast=ast, Stl = stl, Blk = blk,Tov = tov,Trb = trb) %>% 
  arrange(desc(Pts)) %>% slice(1:20) %>% 
  pivot_longer(c(Pts, Trb, Ast,Tov, Stl, Blk ), names_to = "stats", values_to = "n")


# Creamos la tabla --------------------------------------------------------

jjbarret <- jjbarret %>% group_by(player, stats) %>% 
  ggplot(aes(x = fct_inorder(stats), y = n, fill = stats)) +
  
  geom_col() + 
  facet_wrap(~fct_inorder(player), scales = 'free_x', strip.position = 'bottom') +
  theme_ivo()+
  fishualize::scale_fill_fish(discrete = TRUE, option = "Trimma_lantana", alpha = .96)+ 
  geom_hline(yintercept = seq(1, 40, 1), color = "#f4f4f4", size = .4) +
  geom_hline(yintercept = 0, size = .68, color = 'black') +
  theme(legend.position = 'none', 
        axis.text.x = element_text(face = 'bold', margin = margin(t = -1.5)),
        panel.grid.major.x = element_blank(),
        strip.placement = 'outside',
        strip.text.x = element_text(vjust = 3.5),
        panel.spacing.x = unit(1, "lines"), 
        plot.title = element_text(face = 'bold', size = 15, hjust = 0.5), 
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.title.position = 'plot', 
        plot.margin = margin(10, 10, 20, 10))   +
  labs(x = "Leaderboard por 100 posesiones", 
       y = "",
       title = "Los mejores 20 de 20 por 100 posesiones",
       subtitle = paste0("Los mejores 20 jugadores de hasta 20 años | Updated ", format(Sys.Date(), "%d %B, %Y")),
       caption ="<br><br>**Datos**: *@bball_ref*  **Gráfico**: *Ivo Villanueva*") 
ggsave(
  "jjbarret .png", jjbarret 
  ,
  
  height = 7, width = 7, dpi = "retina"
)

