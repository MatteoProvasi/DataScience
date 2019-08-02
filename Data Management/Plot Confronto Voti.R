library(ggplot2)
library(gridExtra)

match = read.csv('C:/path/MatchingHotel.csv', sep=';', header=TRUE)
match$Stars = as.factor(match$Stars)
match$Diff = (match$Score.Booking-match$Score.Trivago)
match$RevSum = (match$Number.of.Reviews+match$Review.Count)

ggplot(match, aes(ID.Booking, Diff, colour = Stars, size=abs(RevSum))) + 
  geom_point(alpha=0.6) +
  ggtitle('Differenze di voto medio fra Booking') +
  xlab("Hotel") +
  labs(size = "Totale recensioni" ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_discrete(breaks = seq(1, length(match), 100)) +
  scale_y_continuous(breaks = seq(-3, 3, 1)) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"))

cor(match$Diff, match$RevSum)
  
quantile(match$Diff, probs=c(1, 10,20,30,40,50,60,70,80,90, 99)/100)
range03 = nrow(match) - (sum(match$Diff>0.3) + sum(match$Diff<(-0.3)))

(range03/nrow(match))*100
table(round(match$Diff, 1))

star1 <- match[ which(match$Stars==1),]
star2 <- match[ which(match$Stars==2),]
star3 <- match[ which(match$Stars==3),]
star4 <- match[ which(match$Stars==4),]
star5 <- match[ which(match$Stars==5),]

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

win.graph()
#Plots
#####
plot1 = ggplot(star1, aes(ID.Booking, Diff, size=abs(RevSum))) + 
  geom_point(alpha=0.6, color=gg_color_hue(5)[1]) +
  ggtitle('Hotel 1 stella - Differenze di voto medio fra Booking e Trivago') +
  xlab("Hotel") +
  ylab("Differenza") + 
  labs(size = "Totale recensioni" ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_discrete(breaks = seq(1, length(match), 100)) +
  scale_y_continuous(breaks = seq(-3, 3, 1), limits = c(min(match$Diff), max(match$Diff))) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"))

plot2 = ggplot(star2, aes(ID.Booking, Diff, size=abs(RevSum))) + 
  geom_point(alpha=0.6, color=gg_color_hue(5)[2]) +
  ggtitle('Hotel 2 stelle - Differenze di voto medio fra Booking e Trivago') +
  xlab("Hotel") +
  ylab("Differenza") +  
  labs(size = "Totale recensioni" ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_discrete(breaks = seq(1, length(match), 100)) +
  scale_y_continuous(breaks = seq(-3, 3, 1), limits = c(min(match$Diff), max(match$Diff))) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black")) + 
  theme(legend.text=element_text(size=8))

plot3 = ggplot(star3, aes(ID.Booking, Diff, size=abs(RevSum))) + 
  geom_point(alpha=0.6, color=gg_color_hue(5)[3]) +
  ggtitle('Hotel 3 stelle - Differenze di voto medio fra Booking e Trivago') +
  xlab("Hotel") +
  ylab("Differenza") + 
  labs(size = "Totale recensioni" ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_discrete(breaks = seq(1, length(match), 100)) +
  scale_y_continuous(breaks = seq(-3, 3, 1), limits = c(min(match$Diff), max(match$Diff))) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black")) + 
  theme(legend.text=element_text(size=8))

plot4 = ggplot(star4, aes(ID.Booking, Diff, size=abs(RevSum))) + 
  geom_point(alpha=0.6, color=gg_color_hue(5)[4]) +
  ggtitle('Hotel 4 stelle - Differenze di voto medio fra Booking e Trivago') +
  xlab("Hotel") +
  ylab("Differenza") + 
  labs(size = "Totale recensioni" ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_discrete(breaks = seq(1, length(match), 100)) +
  scale_y_continuous(breaks = seq(-3, 3, 1), limits = c(min(match$Diff), max(match$Diff))) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black")) + 
  theme(legend.text=element_text(size=8))

plot5 = ggplot(star5, aes(ID.Booking, Diff, size=abs(RevSum))) + 
  geom_point(alpha=0.6, color=gg_color_hue(5)[5]) +
  ggtitle('Hotel 5 stelle - Differenze di voto medio fra Booking e Trivago') +
  xlab("Hotel") +
  ylab("Differenza") + 
  labs(size = "Totale recensioni" ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_discrete(breaks = seq(1, length(match), 100)) +
  scale_y_continuous(breaks = seq(-3, 3, 1), limits = c(min(match$Diff), max(match$Diff))) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black")) + 
  theme(legend.text=element_text(size=8))

plot6 = ggplot(match, aes(ID.Booking, Diff, color=Stars, size=abs(RevSum))) + 
  geom_point(alpha=0.6) + 
  ggtitle('Differenze di voto medio fra Booking e Trivago') +
  xlab("Hotel") +
  ylab("Differenza") + 
  labs(size = "Totale recensioni" ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_discrete(breaks = seq(1, length(match), 100)) +
  scale_y_continuous(breaks = seq(-3, 3, 1), limits = c(min(match$Diff), max(match$Diff))) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"))

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)
####