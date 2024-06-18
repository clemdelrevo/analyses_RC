butterfly_modelisation <- function(pit_may, fish_may, invert_may) {
  
  targets::tar_load(pit_may)
  targets::tar_load(fish_may)
  targets::tar_load(invert_may)
  
  final_data <- merge(pit_may, fish_may, by = c("site", "annee", "transect", "reef_type"))
  final_data$HC <- (final_data$HC * 40) / 100
  final_data$SC <- (final_data$SC * 40) / 100
  plot(butterflyfish ~ HC, data = final_data)
  plot(butterflyfish ~ SC, data = final_data)
  boxplot(butterflyfish ~ reef_type, data = final_data)
  
  model <- glm(butterflyfish ~ HC + SC + reef_type, data = final_data, family = poisson)
  summary(model)
  disp <- glm(butterflyfish ~ HC + SC + reef_type, data = final_data, family = quasipoisson)
  summary(disp)
  
  stats::drop1(disp, test = "F")
  model2 <- glm(butterflyfish ~ SC + reef_type, data = final_data, family = quasipoisson)
  summary(model2)
  
  final_data <- final_data|>
    dplyr::select()
  d <- with(final_data, expand.grid(SC = seq(0, 4, by = 1),
                   reef_type = levels(as.factor(final_data$reef_type))))
  predict <- predict.glm(model2, newdata = d, type = "response", se.fit = TRUE)
  d$butterflyfish <- predict$fit
  d$semax <- d$butterflyfish + predict$se.fit * 1.96
  d$semin <- d$butterflyfish - predict$se.fit * 1.96
  
  
  ggplot2::ggplot() +
    ggplot2::geom_line(data = d, ggplot2::aes(x = SC, y = butterflyfish, color = reef_type)) +
    ggplot2::geom_point(data = final_data, ggplot2::aes(x = SC, y = butterflyfish, color = reef_type))
  
  
  
}
