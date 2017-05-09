# Load in teh data
raw_data <- read.csv('data/raw_data.csv', stringsAsFactors = FALSE)
nrow(raw_data)

# We dont need the Jury columns
raw_data <- raw_data[,-c(3:7)]

# Get rid of NAs
raw_data[raw_data == '#N/A'] <- NA
raw_data <- na.omit(raw_data)
nrow(raw_data)

# Format columns
raw_data$Jury.Points <- as.numeric(raw_data$Jury.Points)
raw_data$Televote.Points <- as.numeric(raw_data$Televote.Points)

# Calculate the overall judges scores
# per country per contest
Judge_totals <- tapply(X = raw_data$Jury.Points, INDEX = list(raw_data$To.country, raw_data$Competition), sum)
Judge_ranks <- Judge_totals

for(i in 1:ncol(Judge_totals)){
  
  cJ <- Judge_totals[,i][!is.na(Judge_totals[,i])]
  cJ <- max(cJ) - cJ
  Judge_ranks[,i][!is.na(Judge_ranks[,i])] <- rank(cJ)
  
}

rank(Judge_totals[,1]-max(Judge_totals[,1]), na.last = NA)


# Add these totals back into the raw_data
for(i in 1:nrow(raw_data)){
  raw_data$Judge_total[i] <- Judge_totals[raw_data$To.country[i], raw_data$Competition[i]]
  raw_data$Judge_total_rank[i] <- Judge_ranks[raw_data$To.country[i], raw_data$Competition[i]]
}

# Add a difference column
raw_data$rank_diff <- raw_data$Judge_total_rank - raw_data$Televote.Rank

head(raw_data)

######################################
## Do the public agree with judges? ##
######################################

cor_data <- data.frame(voter = unique(raw_data$From.country),
                       correlation = NA,
                       significant = NA,
                       p_value = NA)

for(voter in unique(raw_data$From.country)){

  voter_data <- raw_data[raw_data$From.country == voter, ]
  cor_sum <- cor.test(voter_data$Judge_total_rank, voter_data$Televote.Rank)
  cor_data[cor_data$voter == voter, 'correlation'] <- cor_sum$estimate
  cor_data[cor_data$voter == voter, 'significant'] <- cor_sum$p.value < 0.05
  cor_data[cor_data$voter == voter, 'p_value'] <- cor_sum$p.value
  rm(list = 'cor_sum')  
}

plot_cor <- function(voter){
  
  voter_data <- raw_data[raw_data$From.country == voter, ]
  plot(x = voter_data$Judge_total_rank,
       y = voter_data$Televote.Rank,
       xlab =  'Judge vote',
       ylab = 'Public vote',
       main = voter)
  
}

plot_cor('Australia')
plot_cor('Montenegro')
plot_cor('Croatia')
plot_cor('Bosnia & Herzegovina')
plot_cor('F.Y.R. Macedonia')

# In general the public and judges dont really agree
# and for some countries there is no correllation at all
write.csv(x = cor_data, file = 'output/judge_public_correlations.csv')

##################################################################
## Do countries show bias in how they vote for other countries? ##
##################################################################

country_bias <- function(voter){

  voter_data <- raw_data[raw_data$From.country == voter, ]
  vote_counts <- table(voter_data$To.country)
  
  # Only model for countries that have 3 or more voting occassions
  good_c <- names(vote_counts[vote_counts >= 3])
  if(length(good_c) < 2) return(NULL)
  m2 <- glm(rank_diff ~ To.country + -1, data = voter_data[voter_data$To.country %in% good_c, ])
  coefs <- summary(m2)$coefficients  
  
  out <- data.frame(voter = voter,
             to.country = gsub('To.country','',row.names(coefs)),
             estimate = coefs[,'Estimate'],
             p_value = coefs[,'Pr(>|t|)'],
             significant = coefs[,'Pr(>|t|)'] <= 0.05)
  
  rm(list = c('m2', 'coefs'))
  return(out)
}

# Model rank difference against country
bias_data <- do.call(rbind, sapply(unique(raw_data$From.country), FUN = country_bias))
row.names(bias_data) <- 1:nrow(bias_data)
write.csv(bias_data, 'bias_data.csv')


# Which country is most liked?
# use only the significant results
sig_bias <- bias_data[bias_data$significant,]
sort(tapply(sig_bias$estimate, sig_bias$to.country, FUN = sum))
