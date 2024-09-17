#file_path <- paste(getwd(),"/results/EXPERIMENT1_reps98_MPI_dataset1", sep="")
#load_results <- readRDS(file_path)

plot.data <- do.call("rbind",results)
#plot.data$country <- as.factor(stringr::str_extract(rownames(plot.data), "[^.]+"))
#plot.data$year <- as.factor(stringr::str_extract(rownames(plot.data), "\\b\\w+$"))
plot.data <- reshape2::melt(plot.data)
plot.data$variable <- as.factor(plot.data$variable)

library(ggplot2)
ggplot(data = plot.data, aes(x = value, color = variable)) +
  geom_density() + theme_minimal() + geom_histogram(freq)

ggplot(plot.data, aes(x = value, color = variable)) +
  geom_histogram(aes(y = ..density..),
                 colour = "gray", fill = "white", position = "identity") +
  geom_density() + theme_minimal()


ggplot(plot.data, aes(x = value, fill=variable))+
  geom_histogram(aes(y=..density..), color='gray50',
                 alpha=0.2, binwidth=0.25, position = "identity")+
  geom_density(alpha=0.2)
