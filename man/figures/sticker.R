
# Shout out to https://github.com/stevecondylios/rawr/blob/master/man/figures/sticker.R
# https://github.com/Bioconductor/BiocStickers/tree/devel/xcms


# RStudio: https://github.com/rstudio/hex-stickers
# hexbin: http://hexb.in/
# hexbin github: https://github.com/maxogden/hexbin
# make stickers with: https://github.com/GuangchuangYu/hexSticker


# Note the hexbin github readme says to use
# 181x209 as a png (preview on mac can easily resize)
# and also provide an svg
# I used this to convert png to svg: https://www.aconvert.com/image/png-to-svg/


# https://coolors.co/193763-f4f4f4-00173d-ff5a5f-c81d25


#install.packages("hexSticker")
library(hexSticker)

# generate hexagonal shapes
library(ggplot2)
set.seed(33)
dat = data.frame(x=runif(5000, 0,10), y=runif(5000,0,10))

p = ggplot(dat, aes(x,y)) +
  geom_hex(colour="black", fill = "transparent", bins=20,linewidth=0.1, alpha=0.1) +
  guides(fill="none") +
  scale_y_continuous(limits=c(-0.4,10.6)) +
  scale_x_continuous(limits=c(-0.4,10.6)) +
  theme(axis.text=element_blank(),
        axis.title=element_blank()
        )
p <- p + coord_equal(ratio=1) + theme_void() + theme_transparent()

sticker(p,
        package="pxR2",

        p_size=40,
        p_x = 1,
        p_y = 1.25,
        #p_family = "Arial",
        s_x=0.9,
        s_y=.6,
        s_width=3.4,
        s_height=3.4,
        p_color = "#00173D",
        #p_color = "#F4F4F4",
        h_color = "#00173D",
        h_fill = "#C81D25",
        spotlight = T,
        white_around_sticker = TRUE,

        filename="man/figures/pxR2_original.png") |> plot()

usethis::use_logo(img = "man/figures/pxR2_original.png")

# ### Convert to svg
# # From: https://stackoverflow.com/questions/61300636/convert-png-to-svg-in-r
# library(magick)
# my_image <- image_read("man/figures/rawr.png")
# my_svg <- image_convert(my_image, format="svg")
# image_write(my_svg, "man/figures/rawr.svg")
#
