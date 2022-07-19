#' Calculate sex meta-analysis data
#'
#' @param data high-burden country data
#' @import data.table
#' @importFrom metafor rma rma.mv ranef transf.ilogit
#' @importFrom stats fitted predict
#'

calculate_sex_meta_data <- function(data = clean$hbc) {
        data <- data.table::as.data.table(data)

        M <- dcast(data, iso3 + year + age_group ~ sex,
                value.var = "cases"
        )
        M[, tot := f + m]

        ## add logit props & variance for ML model
        M[, c("yi", "vi") := metafor::escalc(
                measure = "PLO",
                xi = m,
                ni = tot
        )]

        ## naive meta-analysis
        res <- metafor::rma(
                measure = "PLO", xi = m, ni = tot,
                data = M[age_group == "0-4 years"]
        )

        print(res, digits = 3)
        predict(res,
                transf = transf.ilogit,
                digits = 3
        )

        ## years within countries
        res.ML <- rma.mv(
                yi = yi, V = vi,
                slab = iso3,
                data = M[age_group == "0-4 years"],
                random = ~ 1 | iso3 / year,
                method = "REML"
        )

        predict(res.ML,
                transf = transf.ilogit,
                digits = 3
        )

        ## check extraction:
        tmp <- ranef(res.ML)
        F <- tmp$iso3
        F <- data.table::as.data.table(F)
        F$iso3 <- rownames(ranef(res.ML)$iso3)
        fitted(res.ML)

        F[, c("mid", "lo", "hi") := .(
                transf.ilogit(intrcpt + fitted(res.ML)[1:30]),
                transf.ilogit(pi.lb + fitted(res.ML)[1:30]),
                transf.ilogit(pi.ub + fitted(res.ML)[1:30])
        )]
        GS <- M[age_group == "0-4 years",
                .(mid2 = sum(m, na.rm = TRUE) / sum(tot, na.rm = TRUE)),
                by = iso3
        ]

        F <- merge(F, GS, by = "iso3")

        ## loop over for output
        agz <- c("0-4 years", "5-14 years", "15+ years")
        fans <- ans <- list()
        for (ag in agz) {
                res.ML <- rma.mv(
                        yi = yi,
                        V = vi,
                        slab = iso3,
                        data = M[age_group == ag],
                        random = ~ 1 | iso3 / year,
                        method = "REML"
                )

                tmp <- as.data.table(
                        predict(res.ML,
                                transf = transf.ilogit,
                                digits = 3
                        )
                )

                tmp[, age_group := ag]
                ans[[ag]] <- tmp
                ## fits/predicts
                F <- ranef(res.ML)$iso3
                F <- as.data.table(F)
                F$iso3 <- rownames(ranef(res.ML)$iso3)
                F[, c("mid", "lo", "hi") := .(
                        transf.ilogit(intrcpt +
                                fitted(res.ML)[1:nrow(F)]),
                        transf.ilogit(pi.lb +
                                fitted(res.ML)[1:nrow(F)]),
                        transf.ilogit(pi.ub +
                                fitted(res.ML)[1:nrow(F)])
                )]
                F[, age_group := ag]
                fans[[ag]] <- F
        }
        ans <- rbindlist(ans)
        fans <- rbindlist(fans)

        # fwrite(ans,file=here('plots/sexmetaresults.csv'))

        M[, c("mid", "lo", "hi") := .(m / tot, NA_real_, NA_real_)]
        fans$age_group <- factor(fans$age_group,
                levels = c("0-4 years", "5-14 years", "15+ years"),
                ordered = TRUE
        )
        M$age_group <- factor(M$age_group,
                levels = c("0-4 years", "5-14 years", "15+ years"),
                ordered = TRUE
        )
        M$iso3 <- factor(M$iso3)
        fans$iso3 <- factor(fans$iso3)

        return(list(
                ans = ans,
                fans = fans,
                M = M
        ))
}