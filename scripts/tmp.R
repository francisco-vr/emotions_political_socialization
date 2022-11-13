## by grouping treatments - Social ties ##
E3EGAngry <-ggplot(data = df, aes(x = factor(E3TTie), y = E3Angry, color = factor(E3TTie))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Strong Tie \n Misinfo","Strong tie \n Arg. validated", "Weak Tie \n Misinfo","Weak Tie \n Validated"))+
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.2, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Fear # 

E3EGFear <-ggplot(data = df, aes(x = factor(E3TTie), y = E3Fear, color = factor(E3TTie))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Strong Tie \n Misinfo","Strong tie \n Arg. validated", "Weak Tie \n Misinfo","Weak Tie \n Validated"))+
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Joy # 

E3EGJoy <-ggplot(data = df, aes(x = factor(E3TTie), y = E3Joy, color = factor(E3TTie))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Strong Tie \n Misinfo","Strong tie \n Arg. validated", "Weak Tie \n Misinfo","Weak Tie \n Validated"))+
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Sadness # 

E3EGSad <-ggplot(data = df, aes(x = factor(E3TTie), y = E3Sad, color = factor(E3TTie))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Strong Tie \n Misinfo","Strong tie \n Arg. validated", "Weak Tie \n Misinfo","Weak Tie \n Validated"))+
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3, method = "wilcox.test") +
  stat_compare_means(label.y = 105, method = "wilcox.test")

# Mix and create plot #

PlotE3GE <-((E3EGSad / E3EGFear) | (E3EGJoy / E3EGAngry))
PlotE3GE<-PlotE3GE + plot_annotation(title = 'Experimento N°2: Nivel de emociones según condiciones experimentales, \n Agrupadas por tipo de Lazo social',
                                     theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3GE, filename = "Results/Plots/E3_emocion-tie.png",
       dpi = 400, width = 14, height = 11)

## by grouping treatments - Type of Arguent  ##

E3EAAngry <-ggplot(data = df, aes(x = factor(E3TArg), y = E3Angry, color = factor(E3TArg))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.2, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Fear # 

E3EAFear <-ggplot(data = df, aes(x = factor(E3TArg), y = E3Fear, color = factor(E3TArg))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Joy # 

E3EAJoy <-ggplot(data = df, aes(x = factor(E3TArg), y = E3Joy, color = factor(E3TArg))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Sadness # 

E3EASad <-ggplot(data = df, aes(x = factor(E3TArg), y = E3Sad, color = factor(E3TArg))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3, method = "wilcox.test") +
  stat_compare_means(label.y = 105, method = "wilcox.test")

# Mix and create plot #

PlotE3AE <-((E3EASad / E3EAFear) | (E3EAJoy / E3EAAngry))
PlotE3AE<-PlotE3AE + plot_annotation(title = 'Experimento N°2: Nivel de emociones según condiciones experimentales, \n Agrupadas por tipo de argumentación',
                                     theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3AE, filename = "Results/Plots/E3_emocion-argument.png",
       dpi = 400, width = 14, height = 11)


## Emotions by digital citizenship and Echo Chamber membership ##

# Anger #

E3BAngryDigi <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Angry, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por nivel de Ciudadanía Digital",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~DigitIndex, nrow = 1, labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                       '1'="Alta Ciudadanía Digital")))


E3BAngryHomo <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Angry, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c('Mantener lazos', 'Romper lazos')) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por pertenencia a Cámaras de eco",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE3Angry <-(E3BAngryDigi / E3BAngryHomo)
PlotE3Angry<-PlotE3Angry + plot_annotation(title = 'Experimento N°2: Relación entre nivel de ira y desición de mantener o romper lazos sociales \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                           theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3Angry, filename = "Results/Plots/E3AngrySub.png",
       dpi = 400, width = 14, height = 11)


# Fear # 

E3BFearDigi <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Fear, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por niveles de ciudadanía digital",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 



E3BFearHomo <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Fear, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por pertenencia a Cámaras de eco",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE3Fear <-(E3BFearDigi / E3BFearHomo)
PlotE3Fear<-PlotE3Fear + plot_annotation(title = 'Experimento N°2: Distribución de temor según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                         theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3Fear, filename = "Results/Plots/E3FearSub.png",
       dpi = 400, width = 14, height = 11)

# Sadness #

E3BSadDigi <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Sad, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por niveles de ciudadanía digital",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 



E3BSadHomo <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Sad, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por pertenencia a Cámaras de eco",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE3Sad <-(E3BSadDigi / E3BSadHomo)
PlotE3Sad <-PlotE3Sad + plot_annotation(title = 'Experimento N°2: Distribución de tristeza según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                        theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3Sad, filename = "Results/Plots/E3SadSub.png",
       dpi = 400, width = 14, height = 11)


# Joy  # 

E3BJoyDigi <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Joy, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 


E3BJoyHomo <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Joy, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE3Joy <-(E3BJoyDigi / E3BJoyHomo)
PlotE3Joy <-PlotE3Joy + plot_annotation(title = 'Experimento N°2: Distribución de felicidad según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                        theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3Joy, filename = "Results/Plots/E3JoySub.png",
       dpi = 400, width = 14, height = 11)
