# Setup Constraints
setup_constraints <- function(Sigma, Mu, bond_limit = 0.25, equity_limit = 0.60, single_limit = 0.40) {
    n_assets <- length(Mu)

    Amat <- rbind(
        rep(1, n_assets),
        diag(n_assets),
        -diag(n_assets)
    )

    bvec <- c(1, rep(0, n_assets), rep(-single_limit, n_assets))

    bond_exposure <- if_else(colnames(Sigma) %in% "credit", 1, 0)
    equity_exposure <- if_else(colnames(Sigma) %in% "equity", 1, 0)

    Amat <- rbind(Amat, bond_exposure, equity_exposure)
    bvec <- c(bvec, bond_limit, equity_limit)

    list(Amat = Amat, bvec = bvec)
}