plot_KM <- function(surv, lp){# ECB_bcam_fnl$offset <- 0,

# browser()
# ggsurvplot(fit = survfit(Surv(time, event = status)~1, data =surv))
  # ggsurvplot(fit = survfit(surv~1))
pred_dat <- data.frame(surv,
                               insample_pred = lp %>% as.vector) %>%
  mutate(
    insample_class = cut(insample_pred,
                         breaks = c(-Inf, median(lp), Inf),
                         labels = c("Low", "High"))
  )
plot( survfit(Surv(time, event = status)~insample_class, data = pred_dat), ylim = c(0.7,1), col = c(3,4), mark.time = TRUE)
lines(survfit(Surv(time, event = status)~1, data = pred_dat), mark.time = TRUE, col = 2, conf.int = FALSE)
}
