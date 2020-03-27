library(magrittr)
library(rnaturalearth)
library(rnaturalearthdata)
# pak::pkg_install("dmi3kno/bunny")
library(bunny)
library(magick)
library(emojifont)
library(fontawesome)
library(svglite)
library(ggplot2)

mfblue <- rgb(10, 115, 189, maxColorValue = 255)

cz <- ne_countries(scale = "medium", returnclass = "sf", country = "Czech Republic")
plot(cz, max.plot = 1)

# https://pkgdown.r-lib.org/reference/build_favicon.html
# https://pkgdown.r-lib.org/reference/build_home.html
# https://www.ddrive.no/post/making-hex-and-twittercard-with-bunny-and-magick/

hex_border <- image_canvas_hexborder(border_color = "grey", border_size = 2)
hex_canvas <- image_canvas_hex(border_color = "grey", border_size = 5, fill_color = "white")
hex_canvas

icon <- fontawesome::fa("cash-register")
writeLines(icon, here::here("prep/icon.svg"))
icon <- image_read_svg(here::here("prep/icon.svg"), width = 400) %>%
  image_colorize(100, "white") %>%
  image_convert(colorspace = "sRGB")
icon

ggplot() +
  geom_sf(data = cz, colour = NA, fill = mfblue) +
  theme_void()
ggsave("prep/cz_for_hex.svg", width = 12, height = 10, units = "cm",
       bg = "transparent")

cz_for_hex <- image_read_svg("prep/cz_for_hex.svg", width = 1400) %>%
  image_convert(colorspace = "sRGB")
cz_for_hex
img_hex <- hex_canvas %>%
  bunny::image_compose(cz_for_hex, gravity = "north", offset = "+0+220") %>%
  bunny::image_compose(icon, gravity = "north", offset = "+0+600") %>%
  image_annotate("statnipokladna", size = 250, gravity = "south", location = "+0+520",
                 font = "Teuton Normal", color = mfblue) %>%
  image_annotate("petrbouchal.gihub.io/statnipokladna", size = 50, gravity = "south",
                 location = "+350+270",
                 degrees = 330, font = "sans", color = "grey")
img_hex

img_hex %>%
  image_convert("png", colorspace = "sRGB") %>%
  image_write("prep/logo.png")
img_hex %>%
  image_scale("300x300")

img_hex %>%
  image_convert("png", colorspace = "sRGB") %>%
  image_scale("1200x1200") %>%
  image_write(here::here("prep", "logo_hex_print.png"), density = 600)

img_hex %>%
  image_convert(format = "png", colorspace = "cmyk", matte = T) %>%
  image_scale("1200x1200") %>%
  image_write(here::here("prep", "logo_hex_print.png"), density = 1200, format = "png")

img_hex %>%
  image_scale("200x200") %>%
  image_write(here::here("logo.png"), density = 600)

img_hex_for_pkgdown <- img_hex %>%
  image_scale("480x556") %>%
  image_write(here::here("prep/logo.png"), density = 600, quality = 100)

img_hex_gh <- img_hex %>%
  image_scale("400x400")

gh_logo <- bunny::github %>%
  image_scale("40x40") %>%
  image_colorize(70, "grey")

gh <- image_canvas_ghcard("black") %>%
  image_compose(img_hex_gh, gravity = "East", offset = "+80+0") %>%
  image_annotate("Czech public finance data for R", gravity = "West", location = "+80-30",
                 color = "white", size = 40, font = "Fira Sans") %>%
  image_compose(gh_logo, gravity = "West", offset = "+110+45") %>%
  image_annotate("petrbouchal/statnipokladna", gravity = "West", location = "+160+45",
                 size = 35, font="Fira Sans", color = "grey") %>%
  image_border_ghcard("grey")

gh

gh %>%
  image_write(here::here("prep", "statnipokladna_ghcard.png"))
