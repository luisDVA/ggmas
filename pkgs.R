##%######################################################%##
#                                                          #
####            Tutorial Latinr  04-11'2021'            ####
####              Luis D. Verde Arregoitia              ####
#                                                          #
##%######################################################%##

library(dplyr)
library(ggplot2)
library(patchwork)
library(ggh4x)
library(gghighlight)
library(ggalt)
library(ggridges)
library(ggforce)
library(ggnewscale)
library(scico)
library(ggrepel)
library(ggeasy)
library(ggfx)


# ggforce -----------------------------------------------------------------

# agrupar puntos
ggplot(data=mpg,aes(cty,hwy))+
  geom_point()


ggplot(data=mpg,aes(cty,hwy,color=class))+
  geom_point()

ggplot(data=mpg,aes(cty,hwy,color=class))+
  geom_point()+
  geom_mark_rect()

ggplot(data=mpg,aes(cty,hwy,color=class))+
  geom_point()+
  geom_mark_rect(aes(label=class))

# sina
ggplot(data=mpg,aes(x=drv,y=cty))+geom_boxplot()
ggplot(data=mpg,aes(x=drv,y=cty))+geom_jitter()

ggplot(data=mpg,aes(x=drv,y=cty))+geom_sina()
ggplot(data=mpg,aes(x=drv,y=cty))+geom_sina(maxwidth=0.2)


# ggalt -------------------------------------------------------------------

ggplot(data=mpg,aes(x=cty))+
  geom_histogram()
ggplot(data=mpg,aes(x=cty))+
  geom_density()
ggplot(data=mpg,aes(x=cty))+
  geom_bkde()
ggplot(data=mpg,aes(x=cty))+
  geom_bkde()

require(MASS)
crab.pca <- prcomp(crabs[,4:8],retx=TRUE)
crabs$PC1 <- crab.pca$x[,1]
crabs$PC2 <- crab.pca$x[,2]
crabs$PC3 <- crab.pca$x[,3]
crabs

ggplot(data=crabs,aes(PC1,PC3,shape=sp,color=sp))+
  geom_point()+
  geom_encircle()

ggplot(data=crabs,aes(PC1,PC3,shape=sp,color=sp))+
  geom_point()+
  geom_encircle()

ggplot(data=crabs,aes(PC1,PC3,shape=sp,color=sp))+
  geom_point()+
  geom_encircle(expand=0.01)

# splines
cachorros <- read.csv("https://raw.githubusercontent.com/luisDVA/canes/master/datos/crecimiento_canes.csv")
ggplot(cachorros,aes(edad,peso,
                          group=factor(ID_can),
                          color=factor(ID_can)))+
  geom_point()

cachorros_samp <- cachorros %>%
    group_by(ID_can) %>% sample_n(10) %>% arrange(edad)

ggplot(cachorros_samp,aes(edad,peso,
                          group=factor(ID_can),
                          color=factor(ID_can)
                          ))+
  geom_point()

ggplot(cachorros_samp,aes(edad,peso,
                          group=factor(ID_can),
                          color=factor(ID_can)))+
  geom_point()+
  geom_line()

ggplot(cachorros_samp,aes(edad,peso,
                          group=factor(ID_can),
                          color=factor(ID_can)))+
  geom_point()+
  geom_smooth(se = FALSE)


ggplot(cachorros_samp,aes(edad,peso,
                          group=factor(ID_can),
                          color=factor(ID_can)))+
  geom_point()+
  geom_xspline(spline_shape = 0.8)


# paletas
roedores <- msleep %>% filter(order=="Rodentia")

ggplot(roedores,aes(x=name,y=sleep_total))+
  geom_point()

ggplot(roedores,aes(x=name,y=sleep_total))+
  geom_point()+coord_flip()

ggplot(roedores,aes(x=name,y=sleep_total))+
  geom_lollipop()


ggplot(roedores,aes(x=name,y=sleep_total))+
  geom_lollipop()+coord_flip()

# ggridges ----------------------------------------------------------------

midwest

ggplot(midwest,aes(x=log(poptotal),y=state))+
  geom_density_ridges()

ggplot(midwest,aes(x=log(poptotal),y=state))+
  geom_density_ridges(scale=1)

ggplot(midwest,aes(x=log(poptotal),y=state))+
 geom_density_ridges(stat="binline",binwidth=0.5,)

ggplot(midwest,aes(x=log(poptotal),y=state))+
  geom_density_ridges(stat="binline",binwidth=0.5,scale=1)

ggplot(midwest,aes(x=log(poptotal),y=state))+
  geom_density_ridges(stat="binline",binwidth=0.5,scale=0.5)


# patchwork ---------------------------------------------------------------

fig1 <- ggplot(roedores,aes(x=name,y=sleep_total))+
  geom_point()

fig2 <- ggplot(roedores,aes(x=name,y=sleep_total))+
  geom_point()+coord_flip()

fig3 <- ggplot(roedores,aes(x=name,y=sleep_total))+
  geom_lollipop()


fig1+fig2+fig3
fig1+fig2+fig3+plot_layout(ncol=1)


# ggh4x -------------------------------------------------------------------

ggplot(mpg, aes(displ, cty)) + geom_point()

ggplot(mpg, aes(displ, cty)) + geom_point()+
  facet_grid(~drv)

mpg %>% mutate(tracc=case_when(drv=="4"~"integral",
                               TRUE~'sencilla')) %>%
  ggplot(aes(displ, cty)) + geom_point()+
  facet_nested(~tracc+drv)


mpg %>% mutate(tracc=case_when(drv=="4"~"integral",
                               TRUE~'sencilla')) %>%
  ggplot(aes(displ, cty)) + geom_point()+
  facet_nested(~tracc+drv)+
  force_panelsizes(cols = c(2, 1, 1), respect = TRUE)

# ggnewscale --------------------------------------------------------------
dat1 <-
tibble(x=rpois(200,4),
       y=rnorm(200,5,2),
       z=rnorm(200,5,2))

dat2 <-
  tibble(x=rpois(100,1),
         y=rnorm(100,5,2),
         w=rnorm(100,5,2))

ggplot(dat1,aes(x,y,color=z))+
   geom_point(size=5)+
  scale_color_scico(palette = "bamako")+
  geom_point(data=dat2,aes(x,y,color=w),size=3,pch=17)


ggplot()+
  geom_point(data=dat1,aes(x,y,color=z),size=2)+
  scale_color_scico(palette = "bamako")+
  geom_point(data=dat2,aes(x,y,color=w),size=3,pch=17)+
  scale_color_scico(palette = "davos")

ggplot()+
  geom_point(data=dat1,aes(x,y,color=z),size=2)+
  scale_color_scico(palette = "bamako")+
  new_scale_color()+
  geom_point(data=dat2,aes(x,y,color=w),size=3,pch=17)+
  scale_color_scico(palette = "davos")


# ggrepel -----------------------------------------------------------------
msleep40 <- msleep %>% sample_n(40)

ggplot(msleep40,aes(x=sleep_total,y=sleep_rem))+
  geom_point()

ggplot(msleep40,aes(x=sleep_total,y=sleep_rem,label=name))+
  geom_point()

ggplot(msleep40,aes(x=sleep_total,y=sleep_rem,label=name))+
  geom_point()+geom_text()

ggplot(msleep40,aes(x=sleep_total,y=sleep_rem,label=name))+
  geom_point()+geom_text_repel()

ggplot(msleep40,aes(x=sleep_total,y=sleep_rem,label=name))+
  geom_point()+geom_text_repel(min.segment.length = 0)

ggplot(msleep40,aes(x=sleep_total,y=sleep_rem,label=name))+
  geom_point()+geom_label_repel(min.segment.length = 0.2,
                                segment.curvature=1)

# gghighlight -------------------------------------------------------------

cachorros$ID <- as.character(cachorros$ID_can)
cachorros <- as_tibble(cachorros)

ggplot(cachorros)+
  geom_point(aes(edad,peso,
                 color=ID))

ggplot(cachorros)+
  geom_point(aes(edad,peso,
                 color=ID))+
  gghighlight(peso>35)



ggplot(mpg, aes(displ, cty)) +
  geom_point()

ggplot(mpg, aes(displ, cty, color=drv)) +
  geom_point()

ggplot(mpg, aes(displ, cty,color=drv)) + geom_point()+
  facet_grid(~drv)

ggplot(mpg, aes(displ, cty,color=drv)) +
  geom_point()+
  gghighlight()+
  facet_grid(~drv)

# ggeasy ------------------------------------------------------------------

ggplot(mpg, aes(displ, cty, color=drv)) +
  geom_point()

ggplot(mpg, aes(displ, cty, color=drv)) +
  geom_point()+easy_all_text_size(20)

ggplot(mpg, aes(displ, cty, color=drv)) +
  geom_point()+easy_all_text_size(20,teach = TRUE)

ggplot(mpg, aes(displ, cty, color=drv)) +
  geom_point()+
  easy_legend_at("left", teach = TRUE)+
  easy_rotate_x_labels(teach = TRUE)

# ggfx --------------------------------------------------------------------

ggplot(mpg, aes(displ, cty, color=drv)) +
  geom_point()+easy_all_text_size(20)


ggplot(mpg, aes(displ, cty, fill=drv)) +
  with_outer_glow(geom_point(color="black",pch=21,size=4),colour = "#ccff33",sigma = 5)+
  easy_all_text_size(20)+theme_dark()

ggplot(mpg, aes(displ, cty, fill=drv)) +
  with_outer_glow(geom_point(color="black",pch=21,size=4),colour = "#ccff33",sigma = 5)+
  easy_all_text_size(20)+theme_dark()+
  theme(legend.background = with_shadow(element_rect(fill = "#EBEBFF"), sigma = 3))
