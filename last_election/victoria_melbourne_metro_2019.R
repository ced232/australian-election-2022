
# ----------
# Libraries
# ----------

library(dplyr)
library(ggplot2)
library(ggmap)
library(grid)
library(gridExtra)
library(sf)
library(tidyr)

# ----------
# Constants
# ----------

customPal <- c("#960000", "#b90000", "#dc0000", "#ff0000", "#ff4040", "#ff6464",
               "#ff8c8c", "#ffb4b4", "#ffffff", "#b4daff", "#8cc6ff", "#64b2ff",
               "#40a0ff", "#0080ff", "#006edc", "#005db9", "#004C96")

regional_electorates <- c("Ballarat", "Bendigo", "Casey", "Corangamite", "Corio",
                          "Gippsland", "Indi", "La Trobe", "Mallee", "Mcewen", 
                          "Monash", "Nicholls", "Wannon")

highlighted_electorate <- ""

highlighted_caption <- ""

# ----------
# Themes
# ----------

theme_cust <- function() {
    theme_minimal() %+replace%
    theme(
        plot.title = element_text(family="Avenir Black", 
                                  hjust=.5, 
                                  size=12),
        plot.subtitle = element_text(family="Avenir", 
                                     hjust=.5, 
                                     size=10),
        panel.grid.major = element_blank(),
        text = element_text(color="white", 
                            family="Avenir Black"),
        axis.text = element_blank(),
        legend.key.size = unit(.4, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        plot.background = element_rect(fill = "black", color = "black")
    )
}

# ----------
# Data import/cleaning
# ----------

gis_data <- read_sf("~/git/australian-election-2022/last_election/data/victoria_gis.geojson")

margin_data <- read.csv("~/git/australian-election-2022/last_election/data/victoria_margins.csv", stringsAsFactors = FALSE)

data = gis_data %>%
    rename(electorate = Elect_div) %>%
    left_join(., margin_data, by = "electorate") %>%
    mutate(post = ifelse(electorate %in% regional_electorates, NA, post)) %>%
    mutate(swing = ifelse(electorate %in% regional_electorates, NA, swing)) %>%
    mutate(highlighted = ifelse(electorate == highlighted_electorate, "yes", "no"))

# ----------
# Creating visuals
# ----------

margin_plot <- ggplot(data, aes(fill = post, color = highlighted, size = highlighted)) +
    geom_sf() +
    coord_sf(xlim = c(144.4, 145.55), ylim = c(-38.6, -37.47), expand = FALSE) +
    scale_color_manual(values = c("black", "yellow"), guide = "none") +
    scale_size_manual(values = c(0.2, 0.5), guide = "none") +
    scale_fill_gradientn(name = "Margin\n(2PP)", 
                         limits = c(-30, 30), 
                        breaks = c(-30, -20, -10, 0, 10, 20, 30),
                        labels = c("LIB +30",
                                  "LIB +20",
                                  "LIB +10",
                                  "even",
                                  "ALP +10",
                                  "ALP +20",
                                  "ALP +30"),
                        colours = rev(customPal),
                        na.value = "gray20") +
    theme_cust() +
    theme(legend.position = "left")

swing_plot <- ggplot(data, aes(fill = swing, color = highlighted, size = highlighted)) +
    geom_sf() +
    coord_sf(xlim = c(144.4, 145.55), ylim = c(-38.6, -37.47), expand = FALSE) +
    scale_color_manual(values = c("black", "yellow"), guide = "none") +
    scale_size_manual(values = c(0.2, 0.5), guide = "none") +
    scale_fill_gradientn(name = "Swing\n(2PP)", limits = c(-10, 10), 
                        breaks = c(-10, -5, 0, 5, 10),
                        labels = c("LIB +10",
                                  "LIB +5",
                                  "even",
                                  "ALP +5",
                                  "ALP +10"),
                        colours = rev(customPal),
                        na.value = "gray20") +
    theme_cust()

# create plot grid:

t1 <- textGrob("2019 Australian Federal Election", gp = gpar(fontsize = 16, font = 1, fontfamily = "Avenir Black", col="white"))
t2 <- textGrob("Victoria (Melbourne Metro)", gp = gpar(fontsize = 12, font = 1, fontfamily = "Avenir", col="white"))
t3 <- textGrob(highlighted_caption, gp = gpar(fontsize = 10, font = 1, fontfamily = "Avenir", col="yellow"))

lay <- rbind(c(NA,NA),
             c(1,1),
             c(2,2),
             c(3,4),
             c(5,5))

g <- grid.arrange(t1, t2, margin_plot, swing_plot, t3, layout_matrix = lay, heights = c(1,1,1,14,1))
g2 <- grobTree(rectGrob(gp = gpar(fill = "black", lwd = 0)), 
               arrangeGrob(g))

grid.draw(g2)
