##-------------------------------------------------------------------------
## R code for graphics in Riichi Book
## Author: Daina Chiba
## Last modified: 2017-04-01
##-------------------------------------------------------------------------

library(foreign)
library(ggplot2)

old <- theme_set(theme_bw(base_family="HiraKakuProN-W3"))

# data
dat <- read.dta("ratestats2015-12.dta")
dat $ rank <- ordered(dat $ rank, levels = 
    c("新人", "９級", "８級", "７級", "６級", "５級", "４級", "３級", 
      "２級", "１級", "初段", "二段", "三段", "四段", "五段", "六段", 
      "七段", "八段", "九段", "十段", "天鳳"))

colnames(dat)[colnames(dat)=="deal_in"] <- "deal-in"

dat.1 <- dat[c("rank", "win")]
dat.2 <- dat[c("rank", "deal-in")]
dat.3 <- dat[c("rank", "call")]
dat.4 <- dat[c("rank", "riichi")]

names(dat.1)[names(dat.1)=="win"] <- "rate"
dat.1 $ stats <- "win"
dat.1 $ graph <- "Win / Deal-in"
names(dat.2)[names(dat.2)=="deal-in"] <- "rate"
dat.2 $ stats <- "deal-in"
dat.2 $ graph <- "Win / Deal-in"
names(dat.3)[names(dat.3)=="call"] <- "rate"
dat.3 $ stats <- "call"
dat.3 $ graph <- "Call / Riichi"
names(dat.4)[names(dat.4)=="riichi"] <- "rate"
dat.4 $ stats <- "riichi"
dat.4 $ graph <- "Call / Riichi"

dat.wf <- rbind(dat.1, dat.2)
dat.cr <- rbind(dat.3, dat.4)

dat.wf $ rank <- ordered(dat.wf $ rank, levels = 
    c("新人", "９級", "８級", "７級", "６級", "５級", "４級", "３級", 
      "２級", "１級", "初段", "二段", "三段", "四段", "五段", "六段", 
      "七段", "八段", "九段", "十段", "天鳳"))
dat.cr $ rank <- ordered(dat.cr $ rank, levels = 
    c("新人", "９級", "８級", "７級", "６級", "５級", "４級", "３級", 
      "２級", "１級", "初段", "二段", "三段", "四段", "五段", "六段", 
      "七段", "八段", "九段", "十段", "天鳳"))


cbgFillPalette <- scale_colour_manual(values=c("#ca0020", "#0571b0"))
# red <- "#ca0020"; blue <- "#0571b0"; green <- "#4daf4a"

# Graphical summary (overall)
g <- ggplot(data = dat.wf, 
  aes(x = rank, y = rate, group = stats, shape = stats, colour = stats))
g <- g + geom_line(aes(linetype = stats), size = 1.5)
g <- g + geom_point(size=3, fill="white") + expand_limits(y=c(.1, .25)) + 
  scale_shape_manual(name = "stats", values=c(22,21)) + 
  scale_linetype_discrete(name="stats") + xlab("Rank") + ylab("rate")
g <- g + theme(legend.position=c(.9, .5)) + cbgFillPalette + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + 
  theme(axis.text.y = element_text(angle = 90, hjust = .5))
g <- g + scale_y_continuous(name="")
g


## Graph 1: win vs deal-in rates

quartz(type="pdf", file="stats_wd.pdf", width = 4.8, height = 4.2)
theme_set(theme_bw(base_family="HiraKakuProN-W3"))
g <- ggplot(data = dat.wf, 
  aes(x = rank, y = rate, group = stats, shape = stats, colour = stats))
g <- g + geom_line(aes(linetype = stats), size = 1)
g <- g + geom_point(size=2.5, fill="white") + expand_limits(y=c(.1, .25)) + 
  scale_shape_manual(name = "stats", values=c(22,21)) + 
  scale_linetype_discrete(name="stats") + xlab("Player rank") + ylab("Average rates")
g <- g + theme(legend.position=c(.85, .5)) + cbgFillPalette + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + 
  theme(axis.text.y = element_text(angle = 90, hjust = .5))
g
dev.off()

quartz(type="pdf", file="stats_cr.pdf", width = 4.8, height = 4.2)
theme_set(theme_bw(base_family="HiraKakuProN-W3"))
g <- ggplot(data = dat.cr, 
  aes(x = rank, y = rate, group = stats, shape = stats, colour = stats))
g <- g + geom_line(aes(linetype = stats), size = 1)
g <- g + geom_point(size=2.5, fill="white")  + 
  scale_shape_manual(name = "stats", values=c(22,21)) + 
  scale_linetype_discrete(name="stats") + xlab("Player rank") + ylab(" ")
g <- g + theme(legend.position=c(.9, .5)) + cbgFillPalette + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + 
  theme(axis.text.y = element_text(angle = 90, hjust = .5))
g
dev.off()


theme_set(theme_gray(base_family="")) # Set the default font back to the original
