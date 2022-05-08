SentKeywords <- function(DATASET, textcol, keywords, emot = FALSE) {

	keywords.lem <- textstem::lemmatize_strings({{keywords}}) 
	DF.keywords <- subset({{DATASET}},word_count>1)
	DF.keywords[,paste0("KEYWORD. "{{keywords}})] <- NA

	for (i in seq_len(length(keywords}}))) { 
		DF. keywords[, ncol(DF. keywords)-length({{keywords}}) + 1] <- grepl(paste0("\\b", keywords.lem[i],"\\b"), textstem:: lemmatize_strings(DF. keywords %>% pull({{textcol}})),fixed = FALSE, ignore.case = TRUE)
	}

	if (emot == FALSE) {
		new.word <- vector(length = length({{keywords}})+1)
		new.Sent <- vector(length = length({{keywords}})+1) 
		new.AVGsd <- vector(length = length({{keywords}})+1)
		new.Sent.se <- vector(length = length({{keywords}})+1)
		new.n <- vector(length = length({{keywords}})+1)
		
		new.word[1] <- ".OVERALL"
		new.Sent[1] <- mean(DF.keywords$ave_sentiment, na.rm = TRUE)
		new.AVGsd[1] <- mean(DF.keywords$sd_sentiment, na.rm = TRUE)
		new.Sent.se[1]<- sd(DF.keywords$ave_sentiment, na.rm-TRUE)/sqrt(length(DF.keywords$ave_sentiment)) 
		new.n[1] <- length(DF.keywords$ave_sentiment)


		for (i in seq_len(length({{keywords}}))) { 
			index <- (ncol(DF.keywords) - length({{keywords}}) + i)
			new.word[i+1] <- {{keywords}}[i]
			new.Sent[i+1] <- mean(subset(DF.keywords$ave_sentiment, DF.keywords[, index]==TRUE), na.rm=TRUE)
			new.AVGsd[i+1]<- mean(subset(DF.keywords$sd_sentiment, DF.keywords[, index]==TRUE), na.rm-TRUE)
			new.Sent.se[i+1]<-sd(subset(DF.keywords$ave_sentiment, DF.keywords[, index]==TRUE), na.rm=TRUE) / sqrt(length(subset(DF.keywords$ave_sentiment, DF.keywords[index]==TRUE))) 
			new.n[i+1] <- length(subset(DF.keywords$ave_sentiment, DF.keywords[, index]==TRUE)) 
		}

		SentTable <- data.frame(Keyword = new.word,
								Occurrences = new.n,
								ave_sentiment = new.sent, 
								se_sentiment = new.Sent.se)

	return(SentTable)
	}


	if (emot == TRUE) { 
		DF.Emot <- DF.keywords %>% 
			select(ID, 
					{{textcol}}, 
					word_count, 
					ave_scrap_sentiment = ave_sentiment, 
					starts with("ave_emotion"), 
					starts with("KEYWORD")) %>%  
			tidyr::pivot_longer(ave_scrap_sentiment:ave_emotion_trust,
								names_to = c("scrap","emotion"), 
								names_pattern = ("([a-z]"_[a-z]")_([a-z]*)"),
								values_to = "Meanscore")


		Emot.table <- DF.Emot %>% group_by(emotion) %>%
			Summarise(occurrences = length(Meanscore), 
			ave = mean(Meanscore, na.rm = TRUE),
			se = sd(Meanscore,na.rm = TRUE) / sqrt(length(Meanscore))) %>%
			mutate(Keyword = ".OVERALL")



		for (i in keywords) { 
			Emot.table <- DF.Emot %>%
				filter(word_count>1 & `!!`(rlang::sym(paste0('KEYWORD,',i))) == TRUE) %>%
				group_by(emotion) %>% 
				summarise(Occurrences = length(Meanscore),
							ave = mean(Meanscore,na.rm-TRUE), 
							se = sd(Meanscore, na.rm = TRUE)/sqrt(length(Meanscore))) %>% 
				mutate(Keyword = i) %>%
				rbind(Emot.table)
		}

		EmotTable <- as.data.frame(Emot.table %>% 
									pivot_wider(names_from = emotion, values from c(ave, se))) %>% 
									relocate(Keyword, Occurrences, ave_sentiment, se_sentiment) 
	return(left_join(data.frame(Keyword = append(c('.OVERALL'),{{keywords}})), EmotTable, by = "Keyword"))
}
