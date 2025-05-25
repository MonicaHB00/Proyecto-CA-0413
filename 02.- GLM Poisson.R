rm(list=ls(all=TRUE))

file_ = "C:\\Users\\cegan\\OneDrive\\Mis Documentos\\Profesional\\Actuaria\\Cursos de Edgar\\Actuary Hunters\\GLM\\Curso Frecuencia\\CarDataRespaldo\\car.csv"

datos = read.csv(file_,header=TRUE)

head(datos)
dim(datos)
summary(datos)

table(datos$numclaims)
mean(datos$numclaims)
mean(datos$exposure)

sum(datos$numclaims)/sum(datos$exposure)

f      = "numclaims ~ I(sqrt(exposure))+veh_body + veh_age + agecat"
modelo = glm(formula=f, family = poisson(link = "log"), data=datos )
modelo
summary(modelo)
#confint(modelo)
AIC(modelo)
as.numeric(logLik(modelo))
modelo$deviance
################################


sum(datos$numclaims)
sum(modelo$fitted.values)

modelo$deviance
modelo$df.residual
nrow(datos)-length(coef(modelo))

modelo$deviance/modelo$df.residual                # KPI<1
modelo$deviance/qchisq(.95,modelo$df.residual)    # KPI<1

modelo
datos[100,]
modelo$linear.predictors[100];exp(modelo$linear.predictors[100])
modelo$fitted.values[100]


-2.96863+2.51157*sqrt(0.2299795)+-0.99490+-0.06351*2+-0.09030*1
exp(-2.976397)

sum(as.numeric(modelo$fitted.values)-exp(as.numeric(modelo$linear.predictors)))


datos$lambda= as.numeric(modelo$fitted.values)
sum(datos$numclaims)
sum(datos$lambda)

head(datos)

plot(datos$exposure,datos$lambda,col=datos$veh_age)

datos

write.csv(datos,"ModeloPoisson_00.csv")


#####################

f      = "numclaims ~ I(sqrt(exposure))+veh_body + veh_age + agecat"

eval_modelo <-function(f,datos)
{
	print(f)
	modelo      = glm(formula=f, family = poisson(link = "log"), data=datos )
	resumen     = data.frame(
	formula     = as.character(modelo$formula),
	AIC         = AIC(modelo),
	logLik      = as.numeric(logLik(modelo)),
	Dev         = modelo$deviance
					)
	resumen
}

eval_modelo(f,datos)

f=list()

f[[1]]=  "clm ~ I(exposure)"
f[[2]]=  "clm ~ I(exposure)+I(exposure^2)"
f[[3]]=  "clm ~ I(exposure)+I(exposure^2)+I(exposure^3)"
f[[4]]=  "clm ~ I(sqrt(exposure))"
f[[5]]=  "clm ~ I(exposure)+I(sqrt(exposure))"
f[[6]]=  "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))"
f[[7]]=  "clm ~ I(exposure)+I(exposure^2)+I(exposure^3)+I(sqrt(exposure))"
f[[8]]=  "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body"
f[[9]]=  "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_age"
f[[10]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+gender"
f[[11]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+area"
f[[12]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+agecat"
f[[13]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + veh_age"
f[[14]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + gender"
f[[15]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + area"
f[[16]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + agecat"
f[[17]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_age + gender"
f[[18]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_age + area"
f[[19]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_age + agecat"
f[[20]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+gender + area"
f[[21]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+gender + agecat"
f[[22]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+area + agecat"
f[[23]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + veh_age + gender"
f[[24]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + veh_age + area"
f[[25]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + veh_age + agecat"
f[[26]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + gender + area"
f[[27]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + gender + agecat"
f[[28]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + area + agecat"
f[[29]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_age + gender + area"
f[[30]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_age + gender + agecat"
f[[31]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_age + area + agecat"
f[[32]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+gender + area + agecat"
f[[33]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + veh_age + gender + area"
f[[34]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + veh_age + gender + agecat"
f[[35]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + veh_age + area + agecat"
f[[36]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + gender + area + agecat"
f[[37]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_age + gender + area + agecat"
f[[38]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + veh_age + gender + area + agecat"
f[[39]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + as.factor(veh_age) + gender + area + agecat"
f[[40]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body +           veh_age  + gender + area + as.factor(agecat)"
f[[41]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + as.factor(veh_age) + gender + area + as.factor(agecat)"
f[[42]]= "clm ~                           I(sqrt(exposure))+veh_body + area + as.factor(agecat):as.factor(veh_age)"
f[[43]]= "clm ~                           I(sqrt(exposure))+veh_body +        as.factor(agecat):as.factor(veh_age)"
f[[44]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_age + area + agecat + I(agecat*veh_age)"
f[[45]]= "clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + I(gender*area) + agecat"


Resumen = data.frame()


for(i in 1:length(f))
{
	Resumen = rbind(Resumen,eval_modelo(f[[i]],datos))
}

Resumen

write.csv(Resumen,"ResumenModelosFullPoisson.csv")


f="clm ~ I(exposure)+I(exposure^2)+I(sqrt(exposure))+veh_body + as.factor(veh_age) + gender + area + as.factor(agecat)"
modelo = glm(formula=f, family = poisson(link = "log"), data=datos )
modelo
summary(modelo)
#confint(modelo)
AIC(modelo)
as.numeric(logLik(modelo))
modelo$deviance


names(modelo)

as.numeric(modelo$coefficients)

tabla.coeff = data.frame(coeff=names(modelo$coefficients),val=as.numeric(modelo$coefficients))
write.csv(tabla.coeff,"tabla_coeff_Poissson.csv")


datos[500,]
modelo$linear.predictors[500]
exp(modelo$linear.predictors[500])
modelo$fitted.values[500]