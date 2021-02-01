ger_basic = c(36.44, 37.94, 35.8125, 36.5, 36.63, 38.19)
ger_ae1 = c(41.5, 39.69, 39.5625, 40.875, 41.75, 41.75)
ger_ae2 = c(30.5,29.56,30.9375,32.625,34.75,30.81)
ger_ae3 = c(40.56,38.25,41.8125,40,40.13,41.31)
ger_cc1 = c(38,36.75,39.6875,38.8125,39.81,40.06)
ger_cc2 = c(37.94,38.94,40.5,40.3125,38.25,40.81)
ger_cc3 = c(38.69,36.56,38.25,38.5,37.38,40.31)

ger = c(ger_basic, ger_ae1, ger_ae2, ger_ae3, ger_cc1, ger_cc2, ger_cc3)
ger_labels = c("ger_basic", "ger_ae1", "ger_ae2", "ger_ae3", "ger_cc1", "ger_cc2", "ger_cc3")
boxplot(ger_basic, ger_ae1, ger_ae2, ger_ae3, ger_cc1, ger_cc2, ger_cc3, 
        main="Our results of hare population after 20 ticks",
        names=ger_labels, xlab="Scenario", ylab="Hares per km^2")

paper_means = c(8.2, 12.7, 6.9, 12.8, 9.1, 10.3, 12.5)

own_means = c(mean(ger_basic), mean(ger_ae1), mean(ger_ae2), mean(ger_ae3),
              mean(ger_cc1), mean(ger_cc2), mean(ger_cc3))
indexes_own_means = own_means / own_means[1]
indexes_paper_means = paper_means / paper_means[1]
plot(indexes_paper_means, main="Relativ change of hare population", col="blue",
     ylab="Relative change (1.0 = basic scenario)", xlab="Scenario",type="l",
     ylim = c(0.8,2), xaxt = "n")
axis(1, at=1:length(ger_labels), labels=ger_labels)
lines(indexes_own_means, col="green")
legend(1,2,legend=c("Paper results", "Our results"), col=c("blue", "green"), lty=1, cex=0.8)


