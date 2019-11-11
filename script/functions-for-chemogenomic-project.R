## Ez a file tartalmazza a felhasznalt segedfuggvenyeket.
## Ezeket atraktam kulon fileba, hogy �gy atlathatobb legyen a kod
##
## Ezt a filet nem kell kulon lefuttatni. Aminek szuksege van ra, az behivatjkozza. 


# fuggveny, ami megbecsli az adatok eloszlasanak a modusat
# "azt a pontot, ahol a legmagasabb a histogram"
estimate_mode <- function(x) {
	x<-x[is.finite(x)]
	d <- density(x)
	d$x[which.max(d$y)]
}


# A fuggvenyt arra talaltam ki, hogy a tablazat oszlopait ertelmes sorrendbe rendezze.
# az orig.names-be kell tenni egy string listat, pl. names(df1)-et
# A key.list parameterbe egy stringekbol, illetve regexp pattern-ekbol allo vektor vagy lista kerul.
# A retun value ugyanazt tartalmazza mint az orig.names, csak atrendezve.
# Legelore veszi azokat, amik a key.list-ben az elso helyen vannak, aztan amik a masodik helyen vannak, stb.  
my.order<-function(orig.names, key.list)
{
#	orig.names=n2;
#	key.list=c("background", "t0", "t1")
#	pattern="background"
	
	reordered.names=c();
	for(pattern in key.list){
#		print(pattern)
		idx=grepl(pattern, orig.names)
		reordered.names=c(reordered.names,orig.names[idx])
		orig.names=orig.names[!idx]
	}
	
	reordered.names=c(reordered.names,orig.names)
	return(reordered.names)
}




#Ez a fuggveny egy lm() fuggveny altal megadott linearis model egyenesere tud rairni szoveget
# ugy, hogy kiszamolja a megfelelo szoget es y koordinatat. (Az x koordinatat meg kell neki adni)
#
# tex.on.line(lm1=lm1,x=5.3,labels="svalamiss")
tex.on.line<-function(lm1, x, ...){

	#x=4.5
	uy <- diff(grconvertY(1:2,"user","inches"))
	ux <- diff(grconvertX(1:2,"user","inches"))
	asp<-	uy/ux
	angle=180/pi*atan(lm1$coefficients[2]*asp)
	text(x=x, y=lm1$coefficients[1]+lm1$coefficients[2]*x, srt=angle, adj=c(0.5,0) , ...)
}


# text kiir�sa abrara, ugy hogy kap egy kis arnyekot, igy jobban olvashato
shadowtext <- function(x, y=NULL, labels, col='white', bg='black',
		theta= seq(pi/4, 2*pi, length.out=8), r=0.1, ... ) {
	
	xy <- xy.coords(x,y)
	xo <- r*strwidth('x')
	yo <- r*strheight('x')
	
	for (i in theta) {
		text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
	}
	text(xy$x, xy$y, labels, col=col, ... )
}





# ez egy olyan fuggveny, ami a treshold folott logaritmus, alatta linearis, 
# es az illeszkedesi pontban pont folytonos es derivalhato
log10LinearHybrid<-function(x, treshold=1e-4){
#x= rnorm(15)
#treshold=0.1
	
	idx<-(x>treshold)
	
	x[idx]<-log10(x[idx])
	
	x[!idx]<-( x[!idx]/treshold + log(treshold)-1)/ log(10)
	
	return(x)
}

#inverz fuggvenye a log10LinearHybrid()-nek
exp10LinearHybrid<-function(y, treshold=1e-4){
#x= rnorm(15)
#treshold=0.1
	treshold2<-log10(treshold)
	
	idx<-(y>treshold2)
	
	y[idx]<-10^(y[idx])
	
	# y= a*x+b
	# y= 1/(treshold*log(10)) * x + (log(treshold)-1)/ log(10)
	#
	#inverz:
	# x= (y-b)/a
	# x= (y - (log(treshold)-1)/ log(10))*treshold*log(10)
	# x= ( y*log(10)  - (log(treshold)-1) ) * treshold
	
	y[!idx]<- ( y[!idx]*log(10) - (log(treshold)-1) ) * treshold
	
	
	return(y)
}
#
#
#treshold=1e-5
#x=seq(from=-5, to=30, length.out=100	 )
#y=exp10LinearHybrid(log10LinearHybrid(x,treshold), treshold )
#
#sum(x!=y)
#plot(x,y, cex=0.2)
#plot(x,log10LinearHybrid(x,treshold), cex=0.2)
#exp10LinearHybrid(-4.3)



# a log10LinearHibrid fuggvenyleg tovabbonyolitasa.
# a treshold-hal nagyobb szamok eseten sima log10()
# a [-treshold,+treshold] intervallumon linearis, ugy hogy folytonosan es derivalhatoan csatlakozik a log10()-hez
# -treshold alatt pedig log10(-x) alak� �gy eltolva lefele, hogy folytonosan es diferencialhatoan csatlakozzon a linearis reszhez.
#
# nincs az a nyavaly�ja, hogy �r�lt nagy negat�v sz�mokat kapunk negat�v �rt�kekre.
myLog10LinearHybrid<-function(x, treshold=10^-4.5)
{
	#value.at.0<-(log(treshold)-1)/ log(10)
	value.at.minus.treshold <- (log(treshold)-2)/ log(10)
	value.at.plus.treshold <-  log10(treshold)

	idx1 <- (x> treshold)
	idx2 <- (x< -treshold)
	idx3 <- !(idx1 | idx2)

	x[idx1]<- log10(x[idx1])
	x[idx2]<- value.at.minus.treshold+value.at.plus.treshold- log10(-x[idx2])

	x[idx3]<-( x[idx3]/treshold + log(treshold)-1)/ log(10)

	return(x)
}


#############

myExp10LinearHybrid<-function(y, treshold=10^-4.5){

	value.at.minus.treshold <- (log(treshold)-2)/ log(10)
	value.at.plus.treshold <-  log10(treshold)


	idx1 <- (y> value.at.plus.treshold)
	idx2 <- (y< value.at.minus.treshold)
	idx3 <- !(idx1 | idx2)

	y[idx1]<- 10^(y[idx1])
	y[idx2]<- -10^((value.at.minus.treshold+value.at.plus.treshold)-y[idx2])


	# y= a*x+b
	# y= 1/(treshold*log(10)) * x + (log(treshold)-1)/ log(10)
	#
	#inverz:
	# x= (y-b)/a
	# x= (y - (log(treshold)-1)/ log(10))*treshold*log(10)
	# x= ( y*log(10)  - (log(treshold)-1) ) * treshold

	y[idx3]<- ( y[idx3]*log(10) - (log(treshold)-1) ) * treshold


	return(y)
}


