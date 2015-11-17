pred_r_squared <- function(linear.model) {
    lm.anova <- anova(linear.model)
    tss <- sum(lm.anova$"Sum Sq")
    # predictive R^2
    pred.r.squared <- 1 - PRESS(linear.model)/(tss)
    return(pred.r.squared)
}

PRESS <- function(linear.model) {
    pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
    PRESS <- sum(pr^2)
    return(PRESS)
}
