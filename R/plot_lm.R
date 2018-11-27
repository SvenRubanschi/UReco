colvec = c("orange", "red2","forestgreen")

x1 = log10(repro_1$PS)
y1 = repro_1$RP*100
y1 <- y1[-5]
x1 <- x1[-5]

plot(y1~x1,type="n", xlab = "XXX", ylab = "YYY", ylim = c(1,100), las=1)
m1 = lm(y1~x1)
wx = par("usr")[1:2]
new.x = seq(wx[1],wx[2],len=100)
pred = predict(m1, new=data.frame(x1=new.x), interval="conf")
polygon(c(new.x,rev(new.x)),c(pred[,"lwr"],rev(pred[,"upr"])),border=NA,col="gray65")
lines(new.x,pred[,"fit"],lwd=2)
lines(new.x,pred[,"lwr"],lty=3)
lines(new.x,pred[,"upr"],lty=3)
points(x1,y1,pch=16, col= colvec[repro_1$Group])
legend("topright", legend = c("Obs 1","Obs 2", "Obs 3"), col=colvec, cex = 1.2, pch = 16, horiz = T)
box()
summary(m1)