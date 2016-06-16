summary.psm.deepn <- function(object, outfile) {
  pm <- object$pm
  ps <- object$ps

  load(psmfile)
  pmd <- pm$delta
  psd <- ps$delta
  load(vpsmfile)
  pm$delta <- pmd
  ps$delta <- psd
  Gene <- rownames(pm$delta)
  Z <- (pm$delta / ps$delta)
  load(datfile)
  load(vdatfile)

  # Raw PPMs
  vBase <- apply(Vector[,1,], 2, ppm)
  bBase <- apply(Data[,1,], 2, ppm)
  Base <- apply(cbind(vBase, bBase), 1, mean)
  vSel <- apply(Vector[,2,], 2, ppm)
  Vec <- apply(vSel, 1, mean)
  bSel <- apply(Data[,2,], 2, ppm)
  Enr1 <- log2((bSel[,1]+0.05)/(Vec+0.05))
  Enr2 <- log2((bSel[,2]+0.05)/(Vec+0.05))

  Tab <- data.frame(Base = Base[Gene],
                    Vec = Vec[Gene],
                    Bait1 = bSel[Gene,1],
                    Bait2 = bSel[Gene,2],
                    Enr1 = Enr1[Gene],
                    Enr2 = Enr2[Gene],
                    sEnr1 = pm$delta[,1]/log(2),
                    sEnr2 = pm$delta[,2]/log(2),
                    p1 = pnorm(Z[,1]),
                    p2 = pnorm(Z[,2]),
                    p3 = pnorm(Z[,3]),
                    p4 = pnorm(-Z[,3]),
                    pb1 = pnorm(Z[,1])*(1-pnorm(Z[,3])),
                    pb2 = apply(1-pnorm(Z[,2:3], lower=FALSE), 1, prod))
  names(Tab)[3:4] <- colnames(bSel)
  names(Tab)[5:6] <- paste0("Enr(", id, ")")
  names(Tab)[7:8] <- paste0("sEnr(", id, ")")
  names(Tab)[9:10] <- paste0("p(", id, " > Vec)")
  names(Tab)[11] <- paste0("p(", id[2], " > ", id[1], ")")
  names(Tab)[12] <- paste0("p(", id[1], " > ", id[2], ")")
  names(Tab)[13:14] <- paste0("p(", id, ")")
  Tab <- Tab[order(-Tab[,14]),]
  print(htmlTable(Tab, class="'sortable ctable'", digits=c(1,1,1,1,2,2,2,2,3,3,3,3,3,3)),
        name=name)
}
ppm <- function(x) {
  x / sum(x) * 1e6
}
