theme_set(theme_bw(base_size = 15, base_family = "serif"))

theme_update(
  axis.text = element_text(colour="black"),
  strip.text = element_text(face = "bold"),
  strip.background = element_rect(fill="white")
)

# Create the colours
mpidr_grey <- rgb(200, 200, 200, maxColorValue = 254)
mpidr_blue <- rgb(14, 123, 172, maxColorValue = 254)
mpidr_green <- rgb(6,110,110, maxColorValue = 254)
mpidr_red <- rgb(180, 55, 70, maxColorValue = 254)
mpidr_orange <- rgb(239, 125, 0, maxColorValue = 254)

# Create the colour palette
mpidr_colour_pallete <- unlist(mget(ls(pattern = "mpidr_[a-z]+")))
