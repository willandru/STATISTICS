#2.9 
      difference.means <- 2.35
      T.Stat <- 2.01
      standar.error <- difference.means/T.Stat
      
      cuantil <- qt(0.9, df=18)
      
      lim_inf <- difference.means - cuantil*standar.error
      lim_sup <- difference.means + cuantil*standar.error
      cbind(lim_inf, lim_sup)

#2.20   Vida útil de las bebidas

        vida.util <- c(180, 138, 124, 163, 124, 159, 106, 134, 115, 139)

        #INTERVALO DE CONFIANZA PARA LA MEDIA CON VARIANZA DESCONOCIDA
        t.test(vida.util, conf.level= 0.99)


#2.22 IC 95% PARA LA MEDIA CON VARIANZA DESCONOCIDA
        
        tiempo.reparacion <- c(159, 280, 101, 212, 224, 379, 179, 264, 222, 362
                               , 168, 250, 149, 260, 485, 170)
        t.test(tiempo.reparacion)

#2.30  IC 95% PARA LA DIFERENCIA DE MEDIAS 
        T.10 <- c(1,2,1,3,5,1,5,2,3,5,3,6,5,3,2,1,6,8,2,3)
        T.20 <- c(7,6,8,9,5,5,9,7,5,4,8,6,6,8,4,5,6,8,7,7)
        
        var.test(T.10, T.20)
          #Como incluye al 1, no hay diferencia significativa entre las varianzas
        
        #CON VARIANZAS DESCONOCIDAS E IGUALES
        t.test(T.10, T.20, var.equal = TRUE)
#2.31 IC 95% PARA LA VARIANZA
        
        observations <- c(5.34,6.65,4.76,5.98,7.25,6.0,7.55,5.54,5.62,6.21,5.97,
                          7.35,5.44,4.39,4.98,5.25,6.35,4.61,6.0,5.32)
        lim_inf <- (length(observations)-1)*var(observations)/qchisq(0.975,length(observations)-1)
        lim_sup <- (length(observations)-1)*var(observations)/qchisq(0.025,length(observations)-1)
        cbind(lim_inf,lim_sup)

#2.32 95% PARA LA DIFERENCIA DE MEDIAS 
        
        inst1 <- c(0.265,0.265,0.266,0.267,0.267,0.265,0.267,0.267,0.265,0.268,0.268,0.265)
        inst2 <- c(0.264,0.265,0.264,0.266,0.267,0.268,0.264,0.265,0.265,0.267,0.268,0.269)
        var.test(inst1,inst2)
        #VARIANZAS DESCONOCIDAS PERO IGUALES
        t.test(inst1, inst2, var.equal = TRUE)
        
#2.33  95% PARA LA DIFERENCIA DE MEDIAS
        first.birth <- c(6.08,6.22,7.99,7.44,6.48,7.99,6.32,7.60,6.03,7.52)
        second.birth<- c(5.73,5.8,8.42,6.84,6.43,8.76,6.32,7.62,6.59,7.67)
        var.test(first.birth,second.birth)
        #VARIANZAS DESCONOCIDAS PERO IGUALES
        t.test(first.birth, second.birth, var.equal = TRUE)
        
        