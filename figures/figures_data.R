# This scripts contains....

library(ggplot2)
library(reshape2)

# Figure 1 (a)

ggplot(data = webscrapped_data_experiments1and2, aes(x=year)) + geom_bar(stat="count", fill ="#4196B6") +
  labs(x = "Years", y = "Number of surveys") +
  stat_count(geom = "text", aes(label = ..count..), position=position_stack(vjust=0.5), color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# Figure 1(b)

frequencies <- as.data.frame(table(webscrapped_data_experiments1and2$country))
colnames(frequencies) <- c("country", "n_surveys")

number_surveys <- merge(frequencies, webscrapped_data_experiments1and2[,c("country","region")], by.x = "country")
colnames(number_surveys) <- c("Country","Number of Surveys", "World Region")
number_surveys$`Number of Surveys` <- factor(number_surveys$`Number of Surveys`)

ggplot(data = number_surveys, aes(x=`Number of Surveys`, fill = `World Region`)) +
  geom_bar(width = 0.5) + theme_minimal() + scale_size_continuous(range = c(1, 18)) +
  theme(legend.position = c(.8, .6) ,
        axis.text.x = element_text(angle = 0, vjust = 1.5, hjust=1)) + ylab("Absolute Frequency") +
  scale_fill_manual(breaks = c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", "Middle East & North Africa","South Asia","Sub-Saharan Africa"),
                    values=c("#B2DCEB", "#68B5D2", "#4196B6","#545387","#8470A1","#A68FC0"))
