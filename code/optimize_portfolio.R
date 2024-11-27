# Optimize Portfolio
optimize_portfolio <- function(Sigma, Mu, constraints) {
    solve.QP(
        Dmat = Sigma,
        dvec = Mu,
        Amat = constraints$Amat,
        bvec = constraints$bvec,
        meq = 1
    )$solution
}