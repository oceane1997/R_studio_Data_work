
################################################################################
###################################R PROGRAMMING 101##############################################
#######################STATISTICS : regression lineaire simple




#women dataset

head(women)

ggplot(women, aes(x = height, y= weight)) +
  geom_point(size = 3, alpha = .5 )+
  geom_smooth(method = lm, se = F)+
  theme_bw()+
  labs(title = "Weight explained by height in women",
       x = "Height or explanatory variable",
       y = "Weight or dependant variable")


#create a model----

model1 <- lm(weight ~ height , data = women)

summary(model1)

                    #RESULT:

              #Residual standard error: 1.525 on 13 degrees of freedom
               #Multiple R-squared:  0.991,	Adjusted R-squared:  0.9903 #look only the first value since it's a simple regression with one independant variable. 0.991 => 99,1% of the change in weight can be explained by a change in height #interessant pour evaluer la force explicative de notre modele lorsqu'on retire ou ajoute des variables 
                #F-statistic:  1433 on 1 and 13 DF,  p-value: 1.091e-14 #le pvalue significatif veut dire qu'au moins une variable de notre modele explique ou influe sur notre variable d'interet
                   #The F-statistic is a measure of the overall ability of the explanatory variables to explain. (or predict) the outcome variable’s values.


#predictive modeling----

#predict the weight of a woman, given a certain height

new_data <- data.frame(height = 68)

#use the predict fonction
predict(model1, new_data)

#same with multiple data

new_data1 <- data.frame(height = c(68, 71, 74))

round(predict(model1, new_data1))


###so 2 things : to what extinct one variable (the height in our example) explains the outcome (the weight of a woman) and
## How to predict the outcome 


########################################################################################
#######################STATISTICS : Multiple linear regression 

head(trees)

ggplot(trees, aes(Girth,Volume, color = Height)) +
  geom_point()+
  geom_smooth(method = lm, se = F)+
  theme_bw()+
  labs(title = "Tree volume explained by girth and height")



#Create a model by adding a numerical variable (Height)

lm(Volume ~ Girth + Height, data = trees) |> summary()

#Create a model by adding a categorical variable (verify the interaction )

head(mtcars)

mtcars$am1 <- as.character(mtcars$am)

## Recoding mtcars$am1 into mtcars$am1_rec
mtcars$am1 <- mtcars$am1 %>%
  fct_recode(
    "automatic" = "0",
    "manual" = "1"
  )



ggplot(mtcars, aes(wt, mpg, color = am1)) +
  geom_point()+
  geom_smooth(method = lm, se = F)+
  theme_bw()+
  labs(title = "Fuel efficiency explained by weight of cars and transmission",
       x = "Weight of cars",
       y = "Fuel efficiency",
       color = "Transmission")

#les smooth line pour les modalites de transmissions ne sont pas paralleles donc l'evolution de fuel effyciency en fonction du poids de la voiture depend du type de voiture. automatique ou manuel
#d'où la notion d'interaction

lm(mpg ~ wt + am1, data = mtcars) |> summary()

lm(mpg ~ wt * am1, data = mtcars) |> summary()


#Assumptions ---- 
# when we make linear regression should be validated 

#assumption 1 : there's a linear relationship between the explanatory variable and the outcome variable. # the next 3 are about residual
     #residals are the distance between the actual values and the fitted or predicted values

#assumption 2 : residuals are normally distributed


#assumption 3 : residuals are homoscadastic : the variance between the residuals as you go across the fitted values is evenly distributed: no pattern


#assumption 4 : residuals are independent : the error that we see at a point of the plot is not influenced by an error at another point of the plot 

                                 #####how to check those assumptions


##ASSUMPTION 1 : LINEAR RELATIONSHIP BETWEEN EXPOSURE AND OUTCOME----

#visual verification (a plot to visualize the outcome by the explanatory variable 
     #or a fitted value/ residuals : we expect the dots to be distrobuted randomly on the plot without any pattern)

#statistical test can be done too especially if plot shows some pattern : 
                   #The HARVEY COLLEAR TEST 
         #: L'hypothese nulle ~ il existe une relation lineaire : we are then looking for a high p value




##ASSUMPTION 2 : RESIDUALS NORMALLY DISTRIBUTED----

#visual verification (histogram of the residuals, QQplot) subjective visual clue

##statistical test can be done too : 
                   # The SHAPIRO WILK NORMALITY TEST
         #: L'hypothese nulle ~ Les residus sont normaement distribués : we are then looking for a high p value




##ASSUMPTION 3 and 4 : HOMOSCADISTIC AND INDEPENDANT RESIDUALS----

#visual verification (residuals vs fitted plot) : no pattern 

##Statistical test for homoscadistic 

       # BREUSCH PAGAN TEST bptest(model) # again high pvalue or
       # COOK weisberg TEST


##Statistical test for independent residuals

      #DURBIN WATSON TEST  durbinWatsonTest(model) # if the DW Statistic is close to 2 then there is independance


#COLINEARITY----

#when we have more than 1 explanatory variable and they are correlated with each other ~ same story being told by more than one variable 
#consider removing one of them and based on our knowledge on the subject and undertanding of the data


########################################################################################
#######################STATISTICS : how to select variables 














