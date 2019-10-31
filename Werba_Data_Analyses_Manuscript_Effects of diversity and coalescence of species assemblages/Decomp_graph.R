source("Ecosystem_Function.R")
source("Graphing_Set_up.R")

#need a dataframe for prediction lines, yes it weird to do richness with fractions but i want a smooth line

newdat <- expand.grid(
  m_rich = seq(200,1000,1) - 200,
  z_rich = median(dat_gather_decomp$z_rich),
  Leaf_Type = unique(dat_gather_decomp2$Leaf_Type),
  Dispersal = unique(dat_gather_decomp2$Dispersal),
  #Salinity_Measured = unique(dat_gather_decomp2$Salinity),
  Salinity_Measured = mean(unique(dat_gather_decomp2$Salinity)),
  weight_change = 0
)

newdat$weight_change <- predict(rich_decomp,newdata = newdat, type = "response")
#newdat$weight_change <- linkinv(predict(rich_decomp,newdata=newdat))
#newdat$weight_change <- plogis(predict(rich_decomp,newdata=newdat))


easyPredCI <- function(model,newdata,alpha=0.05) {
  ## baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  ## fixed-effects model matrix for new data
  X <- model.matrix(formula(model,fixed.only=TRUE)[-2],
                    newdata)
  beta <- fixef(model) ## fixed-effects coefficients
  V <- vcov(model)     ## variance-covariance matrix of beta, remove phi estimate
  pred.se <- sqrt(diag(X %*% V %*% t(X))) ## std errors of predictions
  ## inverse-link (logistic) function: could also use plogis()
  linkinv <- model@resp$family$linkinv
  ## construct 95% Normal CIs on the link scale and
  ##  transform back to the response (probability) scale:
  crit <- -qnorm(alpha/2)
  linkinv(cbind(lwr=pred0-crit*pred.se,
                upr=pred0+crit*pred.se))
}

#cpred1.CI <- easyPredCI(rich_decomp,newdat)

#pred0 <- predict(rich_decomp,newdata=newdat)
#X <- model.matrix(formula(rich_decomp),newdat)
#V <-vcov(rich_decomp)[-10,-10]
#pred.se <- sqrt(diag(X %*% V %*% t(X))) 

#linkinv <- rich_decomp$link$mean$linkinv

#newdat$lower <- linkinv(pred0-1.96*pred.se)
#newdat$upper <- linkinv(pred0+1.96*pred.se)

#newdat_check <- newdat %>% 
  #group_by(Leaf_Type, m_rich) %>%
  #summarise(mean_change = mean(weight_change))

#ggplot(newdat_check, aes(m_rich, mean_change)) + geom_point(aes(colour = Leaf_Type))
  

leaf_type_names <- c("diff_maple" ="Maple", "diff_phrag" = "Phragmites","diff_spar" = "Spartina")


## change name of salinity in newdat for legend

newdat$Salinity_Measured <- 5

dec_g <- ggplot(dat_gather_decomp2, aes(m_rich, weight_change)) + 
  geom_point(aes(color=as.factor(Salinity), shape = as.factor(Dispersal)),size = 3)

dec_g1 <- dec_g +
  #geom_ribbon(data = newdat, aes(ymin=lower,ymax=upper, 
                                 #fill = as.factor(Dispersal)), alpha =0.3)+ 
  geom_line(data = newdat, aes(m_rich,weight_change, 
                               linetype = as.factor(Dispersal)), size = 1)

(dec_g3 <- dec_g1 + facet_grid(Leaf_Type ~ ., labeller = as_labeller(leaf_type_names)) + 
    scale_color_brewer(type = "seq",palette = "Dark2") +
    ylab("Proportion Remaining in Dry Weight (g)") + xlab("Observed Microbial Richness") + labs(color = "Salinity")+
    scale_linetype_manual(name = "Mixing Treatment",values = c(1,2),
                          breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")) +
    scale_shape_manual(name = "Mixing Treatment",values = c(16,17), 
                       breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment")
  
  ) #+
    

#scale_fill_manual(name = "Dispersal Prediction Lines",
                      #values = c("lightsteelblue4","lightsteelblue1"), 
                      #breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")))




#alternate graph across zooplankton richness-- for supplement
newdat1 <- expand.grid(
  m_richness = seq(306,987,20),
  z_richness = seq(1,6,0.5),
  Leaf_Type = unique(dat_gather_decomp_ns$Leaf_Type),
  Dispersal = unique(dat_gather_decomp_ns$Dispersal),
  Salinity = unique(dat_gather_decomp_ns$Salinity),
  weight_change = 0
)
newdat1$weight_change <- exp(predict(rich_decomp,newdata = newdat1))


dec_g <- ggplot(dat_gather_decomp_ns, aes(z_richness, weight_change)) + 
  geom_point(aes(color=as.factor(Salinity)),size = 2)

dec_g1 <- dec_g + geom_line(data = newdat1
                            , aes(z_richness,weight_change,  color = as.factor(Salinity)
                                  , linetype = as.factor(Dispersal)), size = 1)

(dec_g3 <- dec_g1 + facet_grid(Leaf_Type ~ ., labeller = as_labeller(leaf_type_names)) + 
    scale_color_brewer(type = "seq",palette = "Dark2") +
    ylab("Proportional Change in Dry Weight") + xlab("Zooplankton Richness") + labs(color = "Salinity")+
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2),
                          breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")))
