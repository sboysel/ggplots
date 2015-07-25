## 02: Scrape Top AUR Packages
library(dplyr)
library(ggplot2)
library(rvest)

## Scrape
aur.html <- html("https://aur4.archlinux.org/packages/?O=0&SeB=nd&K=&outdated=&SB=v&SO=d&PP=250&do_Search=Go")
aur.tables.list <- aur.html %>%
  html_table(fill = TRUE)
aur <- aur.tables.list[[1]]

## Clean
aur <- tbl_df(aur)
names(aur) <- names(aur) %>%
  gsub(pattern = "[[:punct:]]", replacement = "", x = .) %>%
  toupper(.)

## Plot
aur %>%
  arrange(-VOTES) %>%
  head(15) %>%
  ggplot(aes(x = reorder(NAME, VOTES), y = VOTES)) +
    geom_bar(aes(fill = NAME), stat = "identity") +
    coord_flip() +
    theme(legend.position = "none") +
    labs(title = "Top Voted Packages in the\nArch User Repository",
         x = "Votes",
         y = "Package") +
    annotate("text",
             label = "Source:\nhttps://aur4.archlinux.org/packages/",
             x = 2,
             y = 2000,
             size = 3)

aur %>%
  group_by(MAINTAINER) %>%
  summarise(PKGS.IN.TOP.250 = n()) %>%
  arrange(-PKGS.IN.TOP.250) %>%
  head(10) %>%
  ggplot(aes(x = reorder(MAINTAINER, PKGS.IN.TOP.250), y = PKGS.IN.TOP.250)) +
    geom_bar(aes(fill = factor(MAINTAINER)), stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Maintainers with Packages in Top 250 of\nArch User Repository",
       x = "Maintainer",
       y = "Packages") +
  annotate("text",
           label = "Source:\nhttps://aur4.archlinux.org/packages/",
           x = 2,
           y = 12,
           size = 3)
