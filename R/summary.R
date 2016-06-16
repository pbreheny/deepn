summary.psm.deepn <- function(object, sort=FALSE, outfile) {
  Vector <- deepn:::RPM(object$Data)$Vector
  Bait <- deepn:::RPM(object$Data)$Bait
  pm <- object$pm
  ps <- object$ps
  Gene <- rownames(pm)
  Z <- pm/ps

  # Raw PPMs
  Base <- apply(cbind(Vector[,1,], Bait[,1,]), 1, mean)
  Vec <- apply(Vector[,2,], 1, mean)
  Enr1 <- log2((Bait[,2,1]+0.05)/(Vec+0.05))
  Enr2 <- log2((Bait[,2,1]+0.05)/(Vec+0.05))

  Tab <- data.frame(
    Gene = Gene,
    Base = Base,
    Vec = Vec,
    Bait1 = Bait[,2,1],
    Bait2 = Bait[,2,2],
    Enr1 = Enr1,
    Enr2 = Enr2,
    AdjEnr1 = pm[,1]/log(2),
    AdjEnr2 = pm[,2]/log(2),
    p1 = pnorm(Z[,1]),
    p2 = pnorm(Z[,2]),
    p3 = pnorm(-Z[,3]),
    p4 = pnorm(Z[,3]),
    pb1 = pnorm(Z[,1])*pnorm(-Z[,3]),
    pb2 = pnorm(Z[,2])*pnorm(Z[,3])
  )
  m <- ncol(Tab)
  if (sort==1) {
    Tab <- Tab[order(-Tab[,m-1]),]
  } else if (sort==2) {
    Tab <- Tab[order(-Tab[,m]),]
  }
  if (!missing(outfile)) {
    write.csv(Tab, file=outfile, quote=FALSE, row.names=FALSE)
  } else {
    return(Tab[,-1])
  }
}
