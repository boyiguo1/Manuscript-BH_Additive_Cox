plot_KM <- function(){# ECB_bcam_fnl$offset <- 0,
ECB_bcam_insample_pred = predict(ECB_bcam_fnl, newx = ECB_dsn_mat %>% as.matrix(), type = "link", newoffset = 0)
ggsurvplot(fit = survfit(Surv(time, event = status)~1, data =ECB_outcome))

ECB_bcam_pred_dat = data.frame(ECB_outcome,
                               insample_pred = ECB_bcam_insample_pred %>% as.vector) %>%
  mutate(
    insample_class = cut(insample_pred,
                         breaks = c(-Inf, median(ECB_bcam_insample_pred), Inf),
                         labels = c("Low", "High"))
  )
plot( survfit(Surv(time, event = status)~insample_class, data =ECB_bcam_pred_dat), ylim = c(0.7,1), mark.time = TRUE)
lines(survfit(Surv(time, event = status)~1, data =ECB_bcam_pred_dat), mark.time = TRUE, col = 2, conf.int = FALSE)
}
