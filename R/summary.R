summary.psm.deepn <- function(object, sort=FALSE, outfile, ...) {
  Vector <- RPM(object$Data)$Vector
  Bait <- RPM(object$Data)$Bait
  multi <- object$Data$multiBait
  Gene <- dimnames(Vector)[[1]]
  pm <- object$pm
  ps <- object$ps
  Z <- pm/ps

  # Raw PPMs
  B <- cbind(Vector[,1,], if (multi) Bait[,1,] else Bait[,1])
  Base <- apply(B, 1, mean)
  Vec <- apply(Vector[,2,], 1, mean)
  if (multi) {
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
      pBait1_Vec = pnorm(Z[,1]),
      pBait2_Vec = pnorm(Z[,2]),
      pBait1_Bait2 = pnorm(-Z[,3]),
      pBait2_Bait1 = pnorm(Z[,3]),
      pBait1 = pnorm(Z[,1])*pnorm(-Z[,3]),
      pBait2 = pnorm(Z[,2])*pnorm(Z[,3])
    )
  } else {
    Enr <- log2((Bait[,2]+0.05)/(Vec+0.05))
    Tab <- data.frame(
      Gene = Gene,
      Base = Base,
      Vec = Vec,
      Bait = Bait[,1],
      Enr = Enr,
      AdjEnr = pm/log(2),
      p = pnorm(Z)
    )
  }
  m <- ncol(Tab)
  if (multi) {
    if (sort==1) {
      Tab <- Tab[order(-Tab[,m-1]),]
    } else if (sort==2) {
      Tab <- Tab[order(-Tab[,m]),]
    }
  } else {
    if (sort) Tab <- Tab[order(-Tab[,m]),]
  }
  if (!missing(outfile)) {
    write.csv(Tab, file=outfile, quote=FALSE, row.names=FALSE)
  } else {
    return(Tab[,-1])
  }
}
