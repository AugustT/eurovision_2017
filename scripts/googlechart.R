# visualisation

data <- read.csv('bias_data.csv')

head(data)

to.c <- 'Italy'

selected.data <- data[data$to.country == to.c,]

selected.data$confidence <- ifelse(selected.data$estimate > 0, 'low', 'high')

# origin.row <- selected.data[1,] 
# origin.row$voter <- to.c
# origin.row$estimate <- 'NA'
# origin.row$confidence <- 'NA'
# 
# selected.data <- rbind(selected.data, origin.row)

confidence <- function(x){
  if(x <= 1) ret <- 'low'
  if(x < 0.2) ret <- 'medium'
  if(x < 0.05) ret <- 'high'
  return(ret)
}


names(selected.data)[4] <- 'Bias'
  
selected.data$hover_text <- paste('Confidence:', sapply(selected.data$p_value, FUN = confidence))

require(googleVis)

Geo <- gvisGeoChart(selected.data,
                    locationvar = "voter",
                    hovervar = 'hover_text',
                    colorvar = 'Bias',
                    options = list(projection = "kavrayskiy-vii",
                                   region = 150,
                                   colorAxis="{values:[-5, 0, 5],
                                               colors:[\'red', \'white', \'green\']}",
                                   legend = 'none',
                                   height = 800,
                                   width = 800))
plot(Geo)

sig_data <- data[data$significant,]

biased_voters <- sort(tapply(sig_data$estimate, sig_data$voter,  FUN = function(x) sum(abs(x))), decreasing = TRUE)

recieved_bias <- sort(tapply(sig_data$estimate, sig_data$to.country,  sum), decreasing = TRUE)

Geo <- gvisGeoChart(data.frame(country = names(biased_voters),
                               Bias = as.numeric(biased_voters)),
                    locationvar = "country",
                    colorvar = 'Bias',
                    options = list(projection = "kavrayskiy-vii",
                                   region = 150,
                                   # colorAxis="{values:[-5, 0, 5],
                                   # colors:[\'red', \'white', \'green\']}",
                                   legend = 'none',
                                   height = 800,
                                   width = 800))
plot(Geo)


Geo <- gvisGeoChart(data.frame(country = names(recieved_bias),
                               Bias = log(as.numeric(recieved_bias))),
                    locationvar = "country",
                    colorvar = 'Bias',
                    options = list(projection = "kavrayskiy-vii",
                                   region = 150,
                                   # colorAxis="{values:[-5, 0, 5],
                                   # colors:[\'red', \'white', \'green\']}",
                                   legend = 'none',
                                   height = 800,
                                   width = 800))
plot(Geo)

