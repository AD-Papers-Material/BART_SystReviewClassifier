lemmatize <- function(text.vec, dict = lexicon::hash_lemmas,
											separator = '_tagseparator_') {

	dict <- setNames(lexicon::hash_lemmas$lemma, lexicon::hash_lemmas$token)

	terms <- paste(text.vec, separator, collapse = ' ')
	terms <- gsub(sprintf(' *%s$', separator), '', terms, perl = T) %>%
		str_split('\\b') %>% unlist
	terms <- terms[!(terms %in% c('', ' '))]

	terms.lower <- tolower(terms)

	output <- ifelse(
		terms.lower %in% names(dict),
		dict[terms.lower], terms
	) %>%
		paste(collapse = ' ') %>%
		str_split(sprintf(' *%s *', separator)) %>% unlist

	output <- gsub('\\s+', ' ', output, perl = T)
	output <- gsub('^\\s+|\\s+$', '', output, perl = T)

	replace(output, output == 'NA', NA)


	# terms <- paste(text.vec, separator, collapse = ' ') %>%
	# 	str_remove(sprintf(' *%s$', separator)) %>%
	# 	str_split('\\b') %>% unlist %>%
	# 	str_subset('^ ?$', negate = T)
	#
	# terms.lower <- str_to_lower(terms)
	#
	# output <- ifelse(
	# 	terms.lower %in% names(dict),
	# 	dict[terms.lower], terms
	# ) %>%
	# 	paste(collapse = ' ') %>%
	# 	str_split(sprintf(' *%s *', separator)) %>% unlist %>%
	# 	str_squish()
	#
	# replace(output, output == 'NA', NA)
}


tokenize_text <- function(corpus) {

	message('- tokenizing text...')
	#tictoc::tic()

	stopwords <- stopwords("english")

	#  tictoc::tic()
	# corpus <- corpus %>% str_to_lower %>% # low case
	# 	str_replace_all('-', '_') %>%
	# 	removeWords(stopwords("english")) %>% # remove stopwords
	# 	str_replace_all(c(
	# 		"\'(s|re|t|d)?\\b" = '',
	# 		'_' = ' ',
	# 		'[^\\w\\d\\s]+' = ' ' # remove non letters/numbers/spaces
	# 	)) %>%
	# 	lemmatize() # lemmatize
	#  tictoc::toc()

	tictoc::tic()
	corpus <- tolower(corpus)
	corpus <- gsub('-', '_', corpus, fixed = T)
	corpus <- removeWords(corpus, stopwords("english"))
	corpus <- gsub("\'(s|re|t|d)?\\b", '', corpus, perl = T)
	corpus <- gsub('_',' ', corpus, fixed = T)
	corpus <- gsub('[^\\w\\d\\s]+', ' ', corpus, perl = T)
	corpus <- lemmatize(corpus)

	tictoc::toc()

	corpus
}

tokenize_authors <- function(corpus) {

	message('- tokenizing authors')
	tictoc::tic()

	ids = 1:length(corpus)

	with.comma <- str_detect(corpus, ',')

	corpus <- corpus %>% str_squish()

	output <- mclapply(1:length(corpus), function(i) {
		if (is.na(with.comma[i])) NA # No authors listed
		else if (with.comma[i] == TRUE) { # Pubmed or WOS style author list
			corpus[i] %>%
				str_remove_all('[^\\w ,;]') %>%
				str_replace_all('(?<=,)[ \\-\\w]+?(?:(?=;)|$)', function(x) {
					paste0(str_extract_all(x, '\\b\\w')[[1]], collapse = '')
				}) %>% str_replace_all(',', '_') %>% str_remove_all(' +')
		} else { # IEEE style author list
			corpus[i] %>%
				str_remove_all('[^\\w\\.;]') %>% # remove non letters and other characters
				str_replace_all('[^;]+(?:(?=;)|$)', function(x) { # extract names between ;
					str_replace(x, '([\\w \\.]+)\\.([\\w ]+)', '\\2_\\1') #use the rightmost dot to separate first and last names
				}) %>% str_remove_all('\\.')
		}
	}) %>% unlist %>%
		str_replace_all('; *', ' ')

	tictoc::toc()

	output
}

tokenize_keywords <- function(keywords) {
	keywords %>%
		str_to_lower() %>%
		str_replace_all(c(
			'\\s*;\\s*' = ';',
			'[^;\\w]+' = '_',
			';' = ' '))
}


tokenize_MESH <- function(mesh) {

	message('- tokenizing Mesh terms')
	tictoc::tic()

	output <- mesh %>% str_replace_all(c(' *; *' = ';', '[\\(\\)]' = '', '[ ,\\-]+' = '_', '&' = 'and')) %>%
		str_replace_all('(?:(?<=;)|^)[^;]+/[^;]+(?:(?=;)|$)', function(x) {
			x <- str_split(x, '/')[[1]]
			paste(c(x[1], paste(x[1],x[-1], sep = '.sh.')), collapse = ';')
		}) %>% str_replace_all(';', ' ') %>% str_squish()

	tictoc::toc()

	output
}


text_to_DTM <- function(corpus, min.freq = 20, ids = 1:length(corpus),
												freq.subset.ids = ids,
												included.pos = c('Noun', 'Verb', 'Adjective'),
												tokenize.fun = tokenize_text, add.ngrams = T,
												aggr.synonyms = T, n.gram.thresh = .5,
												syn.thresh = .9, label = 'TERM__', na.as.missing = T) {

	raw.corpus <- corpus
	order.ids <- 1:length(corpus)
	names(ids) <- order.ids

	if (is.na(min.freq)) stop('"min.freq" is NA.')

	if (!is.null(tokenize.fun)) {
		corpus <- tokenize.fun(corpus)
	}

	splitted.corpus <- corpus %>% str_split(' +')

	if (length(splitted.corpus) != length(ids)) stop('Number of documents and ids are different')

	excluded.pos <- lexicon::hash_grady_pos %>%
		mutate(pos = str_remove_all(pos, ' \\(.*')) %>%
		filter(!(pos %in% included.pos) & !(word %in% word[pos %in% included.pos])) # Keep terms that are ONLY associated to non relevant parts of speech

	message('- to long format...')
	tictoc::tic()

	corpus <- data.frame(
		term =  splitted.corpus %>% unlist,
		val = 1,
		ID = rep(order.ids, splitted.corpus %>% sapply(length))
	) %>% na.omit() %>% distinct() %>%
		mutate(
			val = replace(val, str_detect(term, '\\*'), 2),
			term = str_remove(term, '\\*')
		) %>%
		filter(!(term %in% excluded.pos$word))
	tictoc::toc()

	message('- removing rare terms...')
	frequent_terms <- corpus %>%
		filter(ID %in% order.ids[ids %in% freq.subset.ids]) %>%
		count(term, name = 'Freq') %>% # count term frequency, but only in relevant IDs
		filter(Freq >= min.freq) %>% pull(term) # create frequent terms list

	corpus <- corpus %>% filter(term %in% frequent_terms) %>% # filter out unfrequent terms
		arrange(ID, term, desc(val)) %>%
		distinct(ID, term, .keep_all = T) # Remove duplicate terms keeping the first of each occurrence (useful for Mesh data)

	message('- to wide format...')
	tictoc::tic()
	DTM <- tidyr::pivot_wider(corpus, id_cols = ID, names_from = term,
														names_prefix = label, values_from = val,
														values_fill = 0)

	tictoc::toc()

	if (add.ngrams) {
		message('- find non consecutive ngram...')
		tictoc::tic()

		DTM <- DTM.add_ngrams(DTM, min.sim = n.gram.thresh)

		tictoc::toc()
	}

	if (aggr.synonyms) {

		message('- find synonyms...')
		tictoc::tic()

		DTM <- DTM.aggr_synonyms(DTM, min.sim = syn.thresh)

		tictoc::toc()
	}

	# The synonyms creation procedure can create very long names
	DTM <- DTM %>% setNames(str_sub(colnames(DTM), 1, 10000))

	message('- managing missings...')

	if (nrow(DTM) < length(raw.corpus)) { # Add documents with no content, ie. NAs
		missing_docs <- setdiff(order.ids, DTM$ID)

		DTM <- bind_rows(
			DTM,
			DTM[rep(1, length(missing_docs)),] %>% # add the missing documents using the first DTM row as template
				mutate(ID = missing_docs, across(c(-ID), ~ 0)) # set the term score to zero for all added documents
		) %>% arrange(ID)
	}

	if (na.as.missing) { # Put NA to terms for documents with no content, otherwise leave zero
		DTM <- DTM %>% mutate(across(-ID, ~ replace(.x, is.na(raw.corpus), NA)))
	}

	DTM %>% mutate(ID = ids[ID])
}

DTM.add_ngrams <- function(DTM, min.sim = .5) {
	mat <- as.matrix(DTM[,-1])

	mat.sparse <- as(mat, "dgCMatrix") # Using sparse matrices

	TTM <- (t(mat.sparse) %*% mat.sparse)/sqrt(tcrossprod(colSums(mat^2, na.rm = T))) # Cosine similarity

	ngram.cliques <- graph_from_adjacency_matrix(as.matrix(TTM) >= min.sim, mode = 'undirected', diag = F) %>% # From TTM to undirected network
		max_cliques(min = 2) %>% lapply(names) # Extracting cliques

	if (length(ngram.cliques) > 0) {
		new_cols <- lapply(ngram.cliques, function(clique) { # For each clique
			new_col <- data.frame(
				as.numeric(DTM[, clique] %>% rowSums() == length(clique)) # TRUE if all words are present in the doc
			)

			colnames(new_col) <- paste0(str_extract(clique[1], '\\w+__'), str_remove(clique, '\\w+__') %>% paste(collapse = '._.'))

			new_col
		}) %>% bind_cols()

		DTM <- DTM %>% bind_cols(new_cols) # Add joined terms
	}
}

DTM.aggr_synonyms <- function(DTM, min.sim = .9) {

	mat <- as.matrix(DTM[,-1])

	mat.sparse <- as(mat, "dgCMatrix") # Using sparse matrices

	TTM <- (t(mat.sparse) %*% mat.sparse)/sqrt(tcrossprod(colSums(mat^2, na.rm = T))) # Cosine similarity

	syn.components <- graph_from_adjacency_matrix(as.matrix(TTM) >= min.sim, mode = 'undirected', diag = F) %>% # From TTM to undirected network
		components() # Extracting connected subgraphs

	syn.components <- lapply(which(syn.components$csize > 1), function(i) {
		names(syn.components$membership)[syn.components$membership == i]
	})

	if (length(syn.components) > 0) {
		new_cols <- lapply(syn.components, function(component) { # For each component
			new_col <- data.frame(
				as.numeric(DTM[, component] %>% rowSums() > 0) # TRUE if at least one word is in the doc
			)

			colnames(new_col) <- paste0(str_extract(component[1], '\\w+__'), str_remove(component, '\\w+__') %>% paste(collapse = '.'))

			new_col
		}) %>% bind_cols()

		DTM <- DTM %>% select(-all_of(syn.components %>% unlist)) %>% # Remove single terms
			bind_cols(new_cols) # Add joined terms
	} else DTM
}
