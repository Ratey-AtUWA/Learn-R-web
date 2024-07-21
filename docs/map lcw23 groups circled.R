# lcext <- st_as_sf(data.frame(x=c(383900,384860), y=c(6461250,6462350)),
#                   coords = c("x","y"), crs=st_crs(32750))
# lcUTM <- get_tiles(lcext, provider="Thunderforest.Outdoors",
#                    apikey="03b0375e8e7c47d0a03018a9001de439",
#                    crop=TRUE, zoom=16)
par(oma=c(0,0,0,0), mgp=c(1.7,0.3,0), font.lab=2, tcl=-0.2)
plot_tiles(lcUTM, axes=T, adjust = F, mar=c(3,3,0.5,0.5))
# axis(1);axis(2);box()
mtext("Easting",1,1.7,font=2,cex=1.2)
mtext("Northing",2,1.7,font=2,cex=1.2)
with(lcw23, points(Easting.GPS, Northing.GPS, pch=19, cex=3, 
                   col=scico(16,pal="hawaii",alpha=0.2)[Group]))
with(lcw23, text(Easting.GPS, Northing.GPS, labels=Group, cex=0.85, 
                 col=scico(16,pal="hawaii")[Group], font=2))
addnortharrow(pos="topright")
addscalebar(plotepsg=32750, pos="topleft", htin=0.16, 
            label.cex=1.2, widthhint=0.3, padin=c(.2,.2))
mtext("WGS84/UTM Zone 50S (EPSG 32750)",3,-1,cex=0.7, adj=0.04)
lines(c(384300,384650,NA,384200,384600),
      c(6461640,6461700,NA,6461850,6461850), 
      col="forestgreen", lty=2)
with(lcDrainsMin, lines(E,N, col="steelblue", lwd=4))
shadowtext(c(384200, 384700, 384400, 384470, 384600),
     c(6462050,6461605,6461360,6461240,6461240), 
     labels=paste("Drain",c(1,2,3,4,5)),
     pos=c(3,3,2,2,4), col="steelblue", cex=1.6,bg="white")