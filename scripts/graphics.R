########################################################################################################
### Make Graphics                                                                                   ####          
### September 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la        ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################


df <-readRDS("data/Final Data/DF-final.RDS")


#####################
#### Experiment 1 ###
#####################


### Maintain/Broke ties general ###

plotdata <- df %>%
  dplyr::group_by(E3Treat, E3) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(pct = n/sum(n),
                lbl = scales::percent(pct))
plotdata

ordered(df$E3Treat, levels = c("Amigo-Validado", "Amigo-Misinfo","Conocido-Misinfo","Conocido-validado"))

PlotE1 <-ggplot(plotdata, 
       aes(x = E3Treat,
           y = pct,
           fill = as.character(E3))) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_discrete(labels = c("Keeping following \n on social media","Unfollow"))+
  scale_x_discrete(labels = c("Strong Tie - Misinfo","Strong tie - Arg. validated", "Weak Tie - Misinfo","Weak Tie - Validated"))+
  geom_text(aes(label = lbl), 
            size = 5, 
            position = position_stack(vjust = 0.5)) +
  labs(y = "Percentage",
       fill = "Behavior",
       x = "Experimental conditions",
       title= "Experiment 1: Willigness to unfollow friends in Social Media") +
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())+
  coord_flip()

PlotE1

ggsave(PlotE1, filename = "Results/Plots/PlotE1General.png",
       dpi = 400, width = 15, height = 9)

 #By echo chamber membership - high #


plotdataEco1 <- df %>%
  filter(HomoIndex=="1")%>%
  dplyr::group_by(E3Treat, E3) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(pct = n/sum(n),
                lbl = scales::percent(pct))
plotdataEco1

ordered(df$E3Treat, levels = c("Amigo-Validado", "Amigo-Misinfo","Conocido-Misinfo","Conocido-validado"))


E1EcoHigh <-ggplot(plotdataEco1, 
                aes(x = E3Treat,
                    y = pct,
                    fill = as.character(E3))) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_discrete(labels = c("Keeping following \n on social media","Unfollow"))+
  scale_x_discrete(labels = c("Strong Tie - Misinfo","Strong tie - Arg. validated", "Weak Tie - Misinfo","Weak Tie - Validated"))+
  geom_text(aes(label = lbl), 
            size = 5, 
            position = position_stack(vjust = 0.5)) +
  labs(y = "Percentage",
       fill = "Behavior",
       x = "Experimental Conditions",
       title = "Experiment 1: Willigness to unfollow friends in Social Media, \n subset by High Echo Chamber membership") +
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())+
  coord_flip()

ggsave(E1EcoHigh, filename = "Results/Plots/PLotE1EcoHigh.png",
       dpi = 400, width = 15, height = 9)


# Mantain or borke ties by echo chamber membership - Low #

plotdataEco0 <- df %>%
  filter(HomoIndex=="0")%>%
  dplyr::group_by(E3Treat, E3) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(pct = n/sum(n),
                lbl = scales::percent(pct))
plotdataEco0

ordered(df$E3Treat, levels = c("Amigo-Validado", "Amigo-Misinfo","Conocido-Misinfo","Conocido-validado"))

E1EcoLow <-ggplot(plotdataEco0, 
                aes(x = E3Treat,
                    y = pct,
                    fill = as.character(E3))) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_discrete(labels = c("Keeping following \n on social media","Unfollow"))+
  scale_x_discrete(labels = c("Strong Tie - Misinfo","Strong tie - Arg. validated", "Weak Tie - Misinfo","Weak Tie - Validated"))+
  geom_text(aes(label = lbl), 
            size = 5, 
            position = position_stack(vjust = 0.5)) +
  labs(y = "Percentage",
       fill = "Behavior",
       x = "Experimental conditions",
       title = "Experiment 1: Willigness to unfollow friends in Social Media \n Subset by low Echo Chamber membership") +
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())+
  coord_flip()

ggsave(E1EcoLow, filename = "Results/Plots/PlotE1EcoLow.png",
       dpi = 400, width = 15, height = 9)



# maintain or broke ties, by digital citizenship - HIGH #

plotdataDigi1 <- df %>%
  filter(DigitIndex=="1")%>%
  dplyr::group_by(E3Treat, E3) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(pct = n/sum(n),
                lbl = scales::percent(pct))
plotdataDigi1

ordered(df$E3Treat, levels = c("Amigo-Validado", "Amigo-Misinfo","Conocido-Misinfo","Conocido-validado"))

E1DigitHigh <-ggplot(plotdataDigi1, 
                    aes(x = E3Treat,
                        y = pct,
                        fill = as.character(E3))) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_discrete(labels = c("Keeping following \n on social media","Unfollow"))+
  scale_x_discrete(labels = c("Strong Tie - Misinfo","Strong tie - Arg. validated", "Weak Tie - Misinfo","Weak Tie - Validated"))+
  geom_text(aes(label = lbl), 
            size = 5, 
            position = position_stack(vjust = 0.5)) +
  labs(y = "Percentage",
       fill = "Behavior",
       x = "Experimental conditions",
       title = "Experiment 1: Willigness to unfollow friends in Social Media \n Subset by high Digital Citizenship levels") +
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())+
  coord_flip()

ggsave(E1DigitHigh, filename = "Results/Plots/PlotE1DigitHigh.png",
       dpi = 400, width = 15, height = 12)

## Maintain or broke ties by digital citizenship - low #

plotdataDigi2 <- df %>%
  filter(DigitIndex=="0")%>%
  dplyr::group_by(E3Treat, E3) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(pct = n/sum(n),
                lbl = scales::percent(pct))
plotdataDigi2

ordered(df$E3Treat, levels = c("Amigo-Validado", "Amigo-Misinfo","Conocido-Misinfo","Conocido-validado"))

PlotE3Digi2 <-ggplot(plotdataDigi2, 
                     aes(x = E3Treat,
                         y = pct,
                         fill = as.character(E3))) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_discrete(labels = c("Keeping following \n on social media","Unfollow"))+
  scale_x_discrete(labels = c("Strong Tie - Misinfo","Strong tie - Arg. validated", "Weak Tie - Misinfo","Weak Tie - Validated"))+
  geom_text(aes(label = lbl), 
            size = 5, 
            position = position_stack(vjust = 0.5)) +
  labs(y = "Percentage",
       fill = "Behavior",
       x = "Experimental conditions",
       title = "Experiment 1: Willigness to unfollow friends in Social Media \n Subset by low Digital Citizenship levels") +
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())+
  coord_flip()

ggsave(PlotE3Digi2, filename = "Results/Plots/PlotE1DigitLow.png",
       dpi = 400, width = 15, height = 12)



##broke ties  by emotions ##
compE3 <-list(c("0","1"))

E3BAngry <-ggplot(data = df, aes(x = factor(E3), y = E3Angry, color = factor(E3))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Keeping following \n on social media", "Unfollow")) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=8,  hjust = 1.5, vjust = -1.5) +
  labs(title = "Angry",
       x = "", y = "Anger levels",
       tile = "Anger") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"),
        axis.text = element_text(size = 15),
        axis.title= element_text(size=15),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()) +
  stat_compare_means(comparisons = compE3, label = "p.signif", label.y = 105)+
  guides(color="none")
  


E3BJoy <-ggplot(data = df, aes(x = factor(E3), y = E3Joy, color = factor(E3))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Keeping following \n on social media", "Unfollow")) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=8, hjust = 1.5, vjust = -1.5) +
  labs(title = "Happiness",
       x = "", y = "Happiness levels") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        axis.text = element_text(size = 15),
        axis.title= element_text(size=15),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()) +
  stat_compare_means(comparisons = compE3, label = "p.signif", label.y = 105)+
  guides(color="none")



E3BSad <-ggplot(data = df, aes(x = factor(E3), y = E3Sad, color = factor(E3))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Keeping following \n on social media", "Unfollow")) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=8, hjust = 1.4, vjust= -0.7) +
  labs(title = "Sadness",
       x = "", y = "Sadness levels") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"),
        axis.text = element_text(size = 15),
        axis.title= element_text(size=15),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()) +
  stat_compare_means(comparisons = compE3, label = "p.signif", label.y = 105)+
  guides(color="none")



E3BFear <-ggplot(data = df, aes(x = factor(E3), y = E3Fear, color = factor(E3))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Keeping following \n on social media", "Unfollow")) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=8, hjust= 1.5, vjust = -1) +
  labs(title = "Fear",
       x = "", y = "Fear levels") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"),
        axis.text = element_text(size = 15),
        axis.title= element_text(size=15),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()) +
  stat_compare_means(comparisons = compE3, label.y = 105, label = "p.signif")+
  guides(color="none")



PlotE3B <-((E3BSad / E3BFear) | (E3BJoy / E3BAngry))
PlotE3B <-PlotE3B + plot_annotation(title = 'Experiment 1.2: Un follow to social media, by emotional intensity',
                                    caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001",
                                    theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3B, filename = "Results/Plots/Experimento3-broke-emotions.png",
       dpi = 600, width = 15, height = 11)


## Emotions by treatment ##

comp <-list(c("Amigo-Misinfo","Amigo-validado"), c("Conocido-Misinfo", "Conocido-validado"),
              c("Amigo-Misinfo", "Conocido-Misinfo"), c("Amigo-validado", "Conocido-validado"))

# Anger #

E3EAngry <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Angry, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Strong Tie \n Misinfo","Strong tie \n Arg. validated", "Weak Tie \n Misinfo","Weak Tie \n Validated"))+
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.2, vjust = -1.5) +
  labs(x = "", y = "Anger levels",
       title= "Anger") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"),
        axis.text = element_text(size=15),
        axis.title= element_text(size=15),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()) +
  stat_compare_means(comparisons = comp, label = "p.signif") +
  stat_compare_means(label.y = 145)+
  guides(color="none")


# Fear # 

E3EFear <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Fear, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Strong Tie \n Misinfo","Strong tie \n Arg. validated", "Weak Tie \n Misinfo","Weak Tie \n Validated"))+
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.3, vjust = -1) +
  labs(title = "Fear",
       x = "", y = "Fear levels") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"),
        axis.text = element_text(size=14),
        axis.title= element_text(size=15),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()) +
  stat_compare_means(comparisons = comp, label = "p.signif") +
  stat_compare_means(label.y = 145)+
  guides(color="none")


# Joy # 

E3EJoy <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Joy, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Strong Tie \n Misinfo","Strong tie \n Arg. validated", "Weak Tie \n Misinfo","Weak Tie \n Validated"))+
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  labs(title = "Happiness",
       x = "", y = "Hapiness levels") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"),
        axis.text = element_text(size=14),
        axis.title= element_text(size=15),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()) +
  stat_compare_means(comparisons = comp, label = "p.signif") +
  stat_compare_means(label.y = 145)+
  guides(color="none")


# Sadness # 

E3ESad <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Sad, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("Strong Tie \n Misinfo","Strong tie \n Arg. validated", "Weak Tie \n Misinfo","Weak Tie \n Validated"))+
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  labs(title = "Sadness",
       x = "", y = "Sadness levels") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"),
        axis.text = element_text(size = 14),
        axis.title= element_text(size=15),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()) +
  stat_compare_means(comparisons = comp,label = "p.signif") +
  stat_compare_means(label.y = 145)+
  guides(color="none")

# Mix and create plot #

PlotE3E <-((E3ESad / E3EFear) | (E3EJoy / E3EAngry))
PlotE3E<-PlotE3E + plot_annotation(title = 'Experiment 1.1 - Emotional intensity by experimental conditions',
                                   caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001",
                                   theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3E, filename = "Results/Plots/E3_emocion.png",
       dpi = 400, width = 14, height = 11)



#####################
#### Experiment 2###
#####################

compE2 <-list(c("Control","Afin"), c("Control", "Opuesto"), c("Afin", "Opuesto"))

E2general <-df%>%
  dplyr::filter(!is.na(E1Treat))%>%
  ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.05) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  labs(title = "Experiment 2: anger levels by social proximity, \n according to political affinity",
       x = "Treatment", y = "Anger levels (Max 7)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  geom_text(data = data.frame(x = 1.99323459079714, y = 7.06367300320391, 
                              label = "Wilcoxon test"), mapping = aes(x = x, y = y, label = label), 
            size = 4.23, vjust = 0L, inherit.aes = FALSE)+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, vjust = -1.5) +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 9) +
  theme(axis.text = element_text(size = 16),
        axis.title=element_text(size=18,face="bold"))+
  guides(fill="none")

ggsave(E2general, filename = "Results/Plots/Plot2General.png",
       dpi = 600, width = 8, height = 10)


E2Homo <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  geom_text(data = data.frame(x = 1.97856251472404, y = 6.99398493549631, 
                              label = "Wilcoxon Test", HomoIndex = 0L),
            mapping = aes(x = x,y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 1.98265356948427, y = 6.96350427825525, 
                              label = "Wilcoxon Test", HomoIndex = 1L),
            mapping = aes(x = x,y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  labs(title = "Anger levels subset by Eco Chamber membership",
       y = "Anger levels (Max 7)", x = "Experimental Conditions",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") + 
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Low Eco Chamber membership",
                                                                     '1'="High Eco Chamber membership"))) +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 9.2) +
  theme(axis.text = element_text(size = 16),
        axis.title=element_text(size=18,face="bold"))+
  guides(fill="none")

ggsave(E2Homo, filename = "Results/Plots/Plot2Homo.png",
       dpi = 600, width = 15, height = 10)


E2Digit <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=3.5, vjust = -2) +
  geom_text(data = data.frame(x = 1.98815731442137, y = 7.00733768984006, 
                              label = "Wilcoxon test", DigitIndex = 1L),
            mapping = aes(x = x,y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 1.98226559070716, y = 7.00733768984006, 
                              label = "Wilcoxon test", DigitIndex = 0L),
            mapping = aes(x = x, y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 9.2) +
  labs(title = "Anger levels subset by Digital Citizenship levels",
       y = "Anger levels (Max 7)", x = "Experimental conditions",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  facet_wrap(~DigitIndex, nrow = 1, labeller = labeller(DigitIndex = c('0'="Low Digital citizenship",
                                                                       '1'="Hight Digital Citizenship")))+
  theme(axis.text = element_text(size = 16),
        axis.title=element_text(size=18,face="bold"))+
  guides(fill="none")

ggsave(E2Digit, filename = "Results/Plots/E2Digit.png",
       dpi = 400, width = 15, height = 10)

