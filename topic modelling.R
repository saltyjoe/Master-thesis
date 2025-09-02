# Enhanced SEC 10-K LDA Topic Modeling Analysis
# This script performs LDA topic modeling on SEC 10-K filings, extracts document-topic distributions,
# and analyzes topic distribution across different 10-K sections

# Install and load necessary packages
if (!require("pacman")) install.packages("pacman")

# Load required packages
pacman::p_load(
  tm,           # Text mining
  topicmodels,  # LDA models
  dplyr,        # Data processing
  tidytext,     # Text data processing
  stringr,      # String manipulation
  readtext,     # Reading text files
  ggplot2,      # Visualization
  tidyr,        # Data tidying
  scales,       # Formatting
  gridExtra,    # Multi-plot layout
  reshape2,     # For data reshaping
  stopwords,    # Stopwords
  slam,         # Sparse matrices
  parallel,     # Parallel processing
  foreach,      # Parallel processing
  doParallel,   # Parallel processing
  Matrix        # Matrix operations
)

# Try to install textmineR if not available
if (!require("textmineR")) {
  install.packages("textmineR", dependencies = TRUE)
  library(textmineR)
}



# 1. Parameter Setup ------------------------------------------------------
# Paths and parameters
data_dir <- "E:/me/RSM/thesis/data/textdata" # Replace with your text files directory
stopwords_file <- "E:/me/RSM/thesis/data/stopwords.txt" # Replace with your stopwords file
output_dir <- "E:/me/RSM/thesis/results" # Output directory

# Create output directory (if it doesn't exist)
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Set LDA parameters
num_topics <- 10    # Initial number of topics
min_word_length <- 4 # Minimum word length
min_doc_freq <- 2    # Minimum document frequency
max_doc_freq <- 0.5  # Maximum document frequency (percentage)

# 2. File information extraction --------------------------------------------
get_file_info <- function(filepath) {
  # Extract information from filename
  filename <- basename(filepath)
  
  # Process filename, format example: 20230203_10-K_edgar_data_37996_0000037996-23-000012
  # Extract date and CIK
  date_match <- str_extract(filename, "^\\d{8}")
  date <- if(!is.na(date_match)) date_match else "Unknown"
  
  # Extract year from date
  year <- if(date != "Unknown") substr(date, 1, 4) else "Unknown"
  
  # Extract CIK number - from the part after edgar_data_
  cik_match <- str_extract(filename, "edgar_data_(\\d+)")
  cik <- if(!is.na(cik_match)) {
    str_extract(cik_match, "\\d+")
  } else {
    # Try to extract CIK from end of filename
    alt_cik <- str_extract(filename, "\\d{10}-\\d{2}-\\d{6}$")
    if(!is.na(alt_cik)) {
      str_extract(alt_cik, "^\\d+")
    } else {
      "Unknown"
    }
  }
  
  return(list(
    filepath = filepath,
    date = date,
    year = year,
    cik = cik
  ))
}

# In step 3. ENHANCED Text Preprocessing Function, add stemming and better filtering:

preprocess_text <- function(text, stopwords_set, min_length = 4) {
  # Convert text to lowercase
  text <- tolower(text)
  
  # Break text into paragraphs and lines for filtering
  paragraphs <- unlist(strsplit(text, "\\n\\s*\\n"))
  
  # Process each paragraph
  processed_paragraphs <- sapply(paragraphs, function(para) {
    # Character restrictions: delete paragraphs with fewer than 80 characters
    if (nchar(para) < 80) {
      return("")  # Exclude short paragraphs (Blankespoore, 2016)
    }
    
    # Count non-alphabetic characters
    total_chars <- nchar(para)
    non_alpha_count <- nchar(gsub("[a-zA-Z]", "", para))
    
    # Remove paragraphs with more than 50% non-alphabetic characters (following Li, 2008)
    if (non_alpha_count / total_chars > 0.5) {
      return("")
    }
    
    # Remove URLs and emails
    para <- stringr::str_replace_all(para, "http\\S+|www\\S+|\\S+@\\S+", "")
    
    # Remove Roman numerals (common in 10-K section headers)
    para <- stringr::str_replace_all(para, "\\b[ivxlcdm]{1,7}\\b", "")
    
    # Remove section numbers (like 1.2.3, 1.2, etc.)
    para <- stringr::str_replace_all(para, "\\b\\d+(\\.\\d+)*\\b", "")
    
    # Remove symbols and punctuation
    para <- stringr::str_replace_all(para, "[^a-zA-Z\\s]", " ")
    
    # Remove extra whitespace
    para <- stringr::str_replace_all(para, "\\s+", " ")
    para <- stringr::str_trim(para)
    
    return(para)
  })
  
  # Combine non-empty processed paragraphs
  text <- paste(processed_paragraphs[processed_paragraphs != ""], collapse = " ")
  
  # Tokenize
  words <- unlist(strsplit(text, "\\s+"))
  
  # Remove stopwords and short words
  words <- words[!words %in% stopwords_set & nchar(words) >= min_length]
  
  # Use a simpler stemming approach with common word endings replacement
  # This is much faster than full stemming + POS tagging
  words <- gsub("(s|es|ed|ing|ly)$", "", words)
  
  # Remove common adjectives and adverbs directly
  common_modifiers <- c(
    "very", "extremely", "really", "quite", "rather", "somewhat", 
    "slightly", "pretty", "fairly", "absolutely", "completely",
    "totally", "entirely", "fully", "highly", "especially", 
    "particularly", "specifically", "notably", "generally",
    "primarily", "mainly", "mostly", "largely", "widely", 
    "greatly", "significantly", "substantially", "considerable",
    "considerable", "remarkable", "extraordinary", "exceptional",
    "outstanding", "excellent", "superior", "tremendous", "extensive",
    "big", "small", "large", "huge", "tiny", "enormous", "massive",
    "giant", "little", "minor", "major", "significant", "important"
  )
  
  # Remove financial specific terms that don't contribute to topics
  financial_noise <- c(
    # From your provided list
    "company", "will", "value", "information", "years", "upon", "company", 
    "fiscal", "rate", "based", "report", "sales", "management", "services", 
    "form", "costs", "related", "tax", "ended", "certain", "market", "credit", 
    "products", "amount", "period", "net", "including", "operations", "securities", 
    "cash", "time", "statements", "income", "section", "common", "assets", "shares", 
    "business", "plan", "year", "date", "interest", "december", "agreement", 
    "stock", "may", "financial", "million", "shall",
    
    # Additional financial terms
    "corporation", "inc", "quarter", "annual", "report", "statement",
    "billion", "dollars", "january", "february", "march", "april", 
    "may", "june", "july", "august", "september", "october", 
    "november", "december", "pursuant", "herein", "thereof", 
    "thereto", "hereby", "hereof",
    
    # Brand names and common corporate terms
    "toyota", "baby", "corp", "llc", "ltd"
  )
  
  # Apply the same stemming to financial_noise for consistent comparison
  financial_noise <- gsub("(s|es|ed|ing|ly)$", "", financial_noise)
  
  # Combined list of words to remove
  words_to_remove <- c(common_modifiers, financial_noise)
  
  # Remove financial noise terms and modifiers
  words <- words[!words %in% words_to_remove]
  
  # Return cleaned text
  return(paste(words, collapse = " "))
}

# 4. NEW: Function to Identify 10-K Item Sections ----------------------------
# Fixed identify_10k_sections function
identify_10k_sections <- function(text) {
  # Ensure text is a single string (atomic character)
  if (!is.character(text) || length(text) > 1) {
    text <- paste(text, collapse = " ")
  }
  
  # Check if text is valid after conversion
  if (!is.character(text) || length(text) == 0) {
    return(list())  # Return empty list if invalid
  }
  
  # Define regular expressions for identifying section headers
  # Looking for patterns like "Item 1.", "ITEM 1.", "Item 1:", etc.
  section_patterns <- c(
    "item\\s+1[^0-9a-zA-Z]",    # Item 1
    "item\\s+1a[^0-9a-zA-Z]",   # Item 1A
    "item\\s+1b[^0-9a-zA-Z]",   # Item 1B
    "item\\s+2[^0-9a-zA-Z]",    # Item 2
    "item\\s+3[^0-9a-zA-Z]",    # Item 3
    "item\\s+4[^0-9a-zA-Z]",    # Item 4
    "item\\s+5[^0-9a-zA-Z]",    # Item 5
    "item\\s+6[^0-9a-zA-Z]",    # Item 6
    "item\\s+7[^0-9a-zA-Z]",    # Item 7
    "item\\s+7a[^0-9a-zA-Z]",   # Item 7A
    "item\\s+8[^0-9a-zA-Z]",    # Item 8
    "item\\s+9[^0-9a-zA-Z]",    # Item 9
    "item\\s+9a[^0-9a-zA-Z]",   # Item 9A
    "item\\s+9b[^0-9a-zA-Z]",   # Item 9B
    "item\\s+10[^0-9a-zA-Z]",   # Item 10
    "item\\s+11[^0-9a-zA-Z]",   # Item 11
    "item\\s+12[^0-9a-zA-Z]",   # Item 12
    "item\\s+13[^0-9a-zA-Z]",   # Item 13
    "item\\s+14[^0-9a-zA-Z]",   # Item 14
    "item\\s+15[^0-9a-zA-Z]"    # Item 15
  )
  
  # Standard 10-K section names for reference
  section_names <- c(
    "Business",                                 # Item 1
    "Risk Factors",                             # Item 1A
    "Unresolved Staff Comments",                # Item 1B
    "Properties",                               # Item 2
    "Legal Proceedings",                        # Item 3
    "Mine Safety Disclosures",                  # Item 4
    "Market Information and Dividends",         # Item 5
    "Selected Financial Data",                  # Item 6
    "Management's Discussion and Analysis",     # Item 7
    "Quantitative and Qualitative Disclosures", # Item 7A
    "Financial Statements",                     # Item 8
    "Changes in Accounting Disagreements",      # Item 9
    "Controls and Procedures",                  # Item 9A
    "Other Information",                        # Item 9B
    "Directors and Executive Officers",         # Item 10
    "Executive Compensation",                   # Item 11
    "Security Ownership",                       # Item 12
    "Related Transactions",                     # Item 13
    "Principal Accountant Fees",                # Item 14
    "Exhibits and Financial Statement Schedules" # Item 15
  )
  
  # Convert text to lowercase for case-insensitive matching
  lower_text <- tolower(text)
  
  # Find all section markers
  section_locations <- list()
  for (i in 1:length(section_patterns)) {
    pattern <- section_patterns[i]
    # Find all matches
    matches <- gregexpr(pattern, lower_text, perl = TRUE)
    
    if (length(matches) > 0 && matches[[1]][1] != -1) {  # If found matches
      positions <- c()
      for (pos in matches[[1]]) {
        # Extract a small context around the match to check if it's a real section header
        start_context <- max(1, pos - 50)
        end_context <- min(nchar(lower_text), pos + 50)
        context <- substr(lower_text, start_context, end_context)
        
        # Only include if it looks like a real section header
        if (grepl("\\bitem\\s+[0-9ab]+\\s*[.:]", context)) {
          positions <- c(positions, pos)
        }
      }
      
      if (length(positions) > 0) {
        section_id <- ifelse(
          grepl("a$", section_patterns[i]), 
          sub("item\\\\s\\+([0-9]+)a.*", "\\1A", section_patterns[i]),
          sub("item\\\\s\\+([0-9]+).*", "\\1", section_patterns[i])
        )
        section_locations[[section_id]] <- positions
      }
    }
  }
  
  # If no sections found, return empty list
  if (length(section_locations) == 0) {
    return(list())
  }
  
  # Process the sections
  # Sort sections by position
  all_positions <- unlist(section_locations)
  if (length(all_positions) == 0) {
    return(list())
  }
  
  names(all_positions) <- rep(names(section_locations), 
                              sapply(section_locations, length))
  all_positions <- sort(all_positions)
  
  # Find the longest sequence of correctly ordered sections
  unique_sections <- unique(names(all_positions))
  last_positions <- sapply(unique_sections, function(sec) {
    pos <- all_positions[names(all_positions) == sec]
    max(pos)
  })
  
  # Sort by position
  last_positions <- sort(last_positions)
  
  # Create section boundaries
  section_boundaries <- c(last_positions, nchar(text))
  
  # Extract section content
  section_content <- list()
  for (i in 1:length(last_positions)) {
    section <- names(last_positions)[i]
    start_pos <- last_positions[i]
    
    # Find end position (start of next section)
    if (i < length(last_positions)) {
      end_pos <- last_positions[i+1] - 1
    } else {
      end_pos <- nchar(text)
    }
    
    # Extract content
    content <- substr(text, start_pos, end_pos)
    
    # Add minimum word counts for specific sections
    word_count <- length(unlist(strsplit(content, "\\s+")))
    
    # Apply minimum word thresholds for specific sections
    if ((section %in% c("1", "7", "8") && word_count >= 50) || 
        (section %in% c("10", "11", "12") && word_count >= 20) ||
        !(section %in% c("1", "7", "8", "10", "11", "12"))) {
      
      # Map section number to name for easier reference
      section_num <- as.numeric(gsub("[^0-9]", "", section))
      section_suffix <- gsub("[0-9]", "", section)
      
      if (!is.na(section_num) && section_num <= length(section_names)) {
        section_name <- section_names[section_num]
        if (section_suffix == "A") {
          section_name <- paste(section_name, "(A)")
        } else if (section_suffix == "B") {
          section_name <- paste(section_name, "(B)")
        }
      } else {
        section_name <- paste("Item", section)
      }
      
      section_content[[section_name]] <- content
    }
  }
  
  return(section_content)
}

# 5. Load Stopwords ----------------------------------------------------
cat("Loading stopwords...\n")
# Load standard stopwords
standard_stopwords <- stopwords::stopwords("en")

# Load custom stopwords (if file exists)
custom_stopwords <- c()
if (file.exists(stopwords_file)) {
  custom_stopwords <- tolower(readLines(stopwords_file, warn = FALSE))
}

# Additional financial-specific stopwords
additional_stopwords <- c(
  # Common financial terms that don't contribute to meaningful topics
  "accordance", "aggregate", "approximately", "basis", "certain", "common", 
  "consolidated", "expenses", "filing", "following", "form", "generally", 
  "information", "interest", "liabilities", "management", "may", "net", 
  "note", "operations", "period", "portion", "previously", "primarily", 
  "principal", "related", "respectively", "revenue", "securities", "share", 
  "significant", "substantially", "total", "various", 
  
  # Numbers as words and time references
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  "first", "second", "third", "fourth", "quarter", "annual", "annually",
  
  # Business document terms
  "appendix", "exhibit", "index", "section", "table", "page", "paragraph",
  
  # Very common verbs in financial documents
  "see", "refer", "include", "including", "included", "regarding", "based", 
  "described", "discussed", "disclosed", "determined", "provided", "issued"
)

# Merge all stopwords
all_stopwords <- unique(c(standard_stopwords, custom_stopwords, additional_stopwords))
cat(sprintf("Total of %d stopwords loaded\n", length(all_stopwords)))


# 6. Load and Preprocess Documents ----------------------------------------------
cat("Loading and preprocessing documents...\n")
# Get all txt files
all_files <- list.files(data_dir, pattern = "\\.txt$", full.names = TRUE)

# For testing purposes, use only a limited number of files
sample_size <- max(100, length(all_files))  # Adjust sample size
cat(sprintf("Total files: %d, Will use %d files for analysis\n", length(all_files), sample_size))
sample_files <- sample(all_files, sample_size)

# Store file information and preprocessed text
doc_data <- data.frame(
  filepath = character(),
  date = character(),
  year = character(),
  cik = character(),
  doc_id = integer(),
  stringsAsFactors = FALSE
)

# NEW: Store section information
section_data <- data.frame(
  doc_id = integer(),
  section_name = character(),
  section_text = character(),
  stringsAsFactors = FALSE
)

corpus_texts <- character()
doc_ids <- integer()

# Process each file
for (i in 1:length(sample_files)) {
  file_path <- sample_files[i]
  # Extract file information
  file_info <- get_file_info(file_path)
  
  # Read text content
  tryCatch({
    # Try to safely read the file with better error handling
    text_content <- tryCatch({
      lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
      paste(lines, collapse = " ")
    }, error = function(e) {
      # Try alternative approach if the first method fails
      cat(sprintf("Initial read failed for %s, trying alternative method: %s\n", file_path, e$message))
      con <- file(file_path, "rb")
      raw_content <- readBin(con, "raw", file.info(file_path)$size)
      close(con)
      rawToChar(raw_content)
    })
    
    # Check if text_content is valid
    if (!is.character(text_content) || length(text_content) == 0) {
      cat(sprintf("Skipping %s - invalid content type\n", file_path))
      next
    }
    
    # NEW: Identify 10-K sections (before preprocessing)
    sections <- tryCatch({
      identify_10k_sections(text_content)
    }, error = function(e) {
      cat(sprintf("Section identification failed for %s: %s\n", file_path, e$message))
      list() # Return empty list if section identification fails
    })
    
    # Preprocess whole document text
    processed_text <- tryCatch({
      preprocess_text(text_content, all_stopwords, min_length = min_word_length)
    }, error = function(e) {
      cat(sprintf("Text preprocessing failed for %s: %s\n", file_path, e$message))
      "" # Return empty string if preprocessing fails
    })
    
    # Skip if processed text is too short
    if (nchar(processed_text) < 100) {
      cat(sprintf("Skipping %s - too short after preprocessing\n", file_path))
      next
    }
    
    # Add file information to dataframe
    doc_data <- rbind(doc_data, data.frame(
      filepath = file_info$filepath,
      date = file_info$date,
      year = file_info$year,
      cik = file_info$cik,
      doc_id = i,
      stringsAsFactors = FALSE
    ))
    
    # NEW: Process and store each section separately
    if (length(sections) > 0) {
      for (section_name in names(sections)) {
        section_text <- sections[[section_name]]
        
        # Add error handling for section preprocessing
        processed_section <- tryCatch({
          preprocess_text(section_text, all_stopwords, min_length = min_word_length)
        }, error = function(e) {
          cat(sprintf("Failed to preprocess section %s in document %s: %s\n", 
                      section_name, file_path, e$message))
          ""  # Return empty string on error
        })
        
        # Only include sections with sufficient content after preprocessing
        if (nchar(processed_section) >= 100) {
          # Add to section data with error handling
          tryCatch({
            section_data <- rbind(section_data, data.frame(
              doc_id = i,
              section_name = section_name,
              section_text = processed_section,
              stringsAsFactors = FALSE
            ))
          }, error = function(e) {
            cat(sprintf("Failed to add section %s to data frame: %s\n", 
                        section_name, e$message))
          })
        }
      }
    }
    
    # Add to corpus
    corpus_texts <- c(corpus_texts, processed_text)
    doc_ids <- c(doc_ids, i)
    
  }, error = function(e) {
    cat(sprintf("Error processing file %s: %s\n", file_path, e$message))
  })
}

cat(sprintf("Successfully loaded and preprocessed %d documents\n", length(corpus_texts)))
cat(sprintf("Identified %d sections across all documents\n", nrow(section_data)))


# 假设你有一个列表保存了所有处理过的文本
saveRDS(processed_text, file = file.path(output_dir, "processed_text.rds"))
write.csv(section_data, file.path(output_dir, "section_data.csv"), row.names = TRUE)
write.csv(doc_data, file.path(output_dir, "doc_data.csv"), row.names = TRUE)

# 7. Create Document-Term Matrix --------------------------------------------
cat("Creating document-term matrix...\n")

# Create corpus
corpus <- Corpus(VectorSource(corpus_texts))

# Create Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Remove low-frequency and high-frequency terms
rowTotals <- row_sums(dtm)
dtm <- dtm[rowTotals > 0, ]  # Remove empty documents

dtm_sparse <- removeSparseTerms(dtm, 1 - min_doc_freq / length(corpus_texts))
dtm_filtered <- dtm_sparse[, col_sums(dtm_sparse) <= max_doc_freq * nrow(dtm_sparse)]

cat(sprintf("DTM dimensions: %d documents x %d terms\n", nrow(dtm_filtered), ncol(dtm_filtered)))

# Convert DTM to a format compatible with textmineR
dtm_matrix <- as.matrix(dtm_filtered)  # Convert DTM to a regular matrix
dtm_final <- Matrix(dtm_matrix, sparse = TRUE)  # Convert to sparse matrix format (dgCMatrix)

print(1 - min_doc_freq / length(corpus_texts))
print(max_doc_freq * nrow(dtm_sparse))

dtm_filtered <- readRDS(file.path(output_dir, "dtm_filtered.rds"))
dtm_final <- readRDS(file.path(output_dir, "dtm_final.rds"))

# 8. Find Optimal Number of Topics -----------------------------------------
find_optimal_topics <- function(dtm, topic_range = seq(150, 170, by = 10)) {
  cat("Finding optimal number of topics...\n")
  
  set.seed(1234)
  
  # Step 1: Train/Test Split
  n_docs <- nrow(dtm)
  train_idx <- sample(1:n_docs, round(0.75 * n_docs))
  
  train_dtm <- dtm[train_idx, ]
  valid_dtm <- dtm[-train_idx, ]
  
  # Step 2: Perplexity via Train/Valid
  cat("Calculating perplexity for each candidate k...\n")
  perplexity_results <- matrix(0, nrow = length(topic_range), ncol = 2)
  colnames(perplexity_results) <- c("k", "perplexity")
  
  for (i in 1:length(topic_range)) {
    k <- topic_range[i]
    cat(sprintf("Testing k = %d\n", k))
    
    tryCatch({
      lda_model <- LDA(train_dtm, k = k, method = "Gibbs", 
                       control = list(seed = 1234, burnin = 250, iter = 500))
      perp <- perplexity(lda_model, newdata = valid_dtm)
      perplexity_results[i, ] <- c(k, perp)
      cat(sprintf("  k = %d, perplexity = %.2f\n", k, perp))
    }, error = function(e) {
      cat(sprintf("  Error for k = %d: %s\n", k, e$message))
      perplexity_results[i, ] <- c(k, NA)
    })
  }
  
  perplexity_df <- as.data.frame(perplexity_results)
  perplexity_df <- perplexity_df[!is.na(perplexity_df$perplexity), ]
  
  # Plot Perplexity
  if(nrow(perplexity_df) > 0) {
    p1 <- ggplot(perplexity_df, aes(x = k, y = perplexity)) +
      geom_point() +
      geom_line() +
      labs(title = "Perplexity vs Number of Topics", x = "Number of Topics", y = "Perplexity") +
      theme_minimal()
    
    print(p1)
    
    if(exists("output_dir")) {
      ggsave(file.path(output_dir, "topic_perplexity.png"), p1, width = 8, height = 6)
    }
  }
  
  # Step 3: Coherence calculation
  coherence_values <- c()
  
  # Check if textmineR is available and calculate coherence
  if (requireNamespace("textmineR", quietly = TRUE)) {
    for (k in topic_range) {
      cat(sprintf("Calculating coherence for %d topics...\n", k))
      
      tryCatch({
        lda_model <- LDA(dtm, k = k, method = "Gibbs",
                         control = list(seed = 1234, burnin = 250, iter = 500))
        
        # Extract beta matrix and set correct row/column names
        beta <- exp(lda_model@beta)
        rownames(beta) <- paste0("topic_", 1:k)
        colnames(beta) <- lda_model@terms
        
        # Calculate coherence
        tc <- textmineR::CalcProbCoherence(
          phi = beta,
          dtm = dtm_final, 
          M = 10
        )
        
        coherence_values <- c(coherence_values, mean(tc))
        cat(sprintf("  k = %d, coherence = %.4f\n", k, mean(tc)))
      }, error = function(e) {
        cat(sprintf("  Error calculating coherence for k = %d: %s\n", k, e$message))
        coherence_values <- c(coherence_values, NA)
      })
    }
    
    coherence_df <- data.frame(k = topic_range, coherence = coherence_values)
    coherence_df <- coherence_df[!is.na(coherence_df$coherence), ]
    
    # Plot Coherence
    if(nrow(coherence_df) > 0) {
      p2 <- ggplot(coherence_df, aes(x = k, y = coherence)) +
        geom_line() +
        geom_point() +
        labs(title = "Topic Coherence by Number of Topics",
             x = "Number of Topics",
             y = "Average Coherence") +
        theme_minimal()
      
      print(p2)
      
      if(exists("output_dir")) {
        ggsave(file.path(output_dir, "topic_coherence.png"), p2, width = 8, height = 6)
        write.csv(coherence_df, file.path(output_dir, "topic_coherence.csv"), row.names = FALSE)
      }
    }
  }
  
  # Find best k based on metrics
  best_k <- NA
  if(nrow(perplexity_df) > 0) {
    best_k_perplexity <- perplexity_df$k[which.min(perplexity_df$perplexity)]
    cat(sprintf("Best k based on perplexity: %d\n", best_k_perplexity))
    best_k <- best_k_perplexity
  }
  
  if(exists("coherence_df") && nrow(coherence_df) > 0) {
    best_k_coherence <- coherence_df$k[which.max(coherence_df$coherence)]
    cat(sprintf("Best k based on coherence: %d\n", best_k_coherence))
    
    # Prefer coherence over perplexity if available
    if(!is.na(best_k_coherence)) best_k <- best_k_coherence
  }
  
  return(best_k)
}

# Run topic optimization if we have enough documents
optimal_k <- NA
if (nrow(dtm_filtered) >= 20) {
  optimal_k <- find_optimal_topics(dtm_filtered)
  if (!is.na(optimal_k)) {
    num_topics <- optimal_k
    cat(sprintf("Using optimal number of topics: %d\n", num_topics))
  } else {
    cat(sprintf("Could not determine optimal topics, using default: %d\n", num_topics))
  }
} else {
  cat("Not enough documents, using default number of topics:", num_topics, "\n")
}

num_topics <- 100


# 9. Train LDA Model --------------------------------------------------
cat(sprintf("Training LDA model with %d topics...\n", num_topics))
lda_model <- LDA(
  dtm_filtered, 
  k = 100,
  method = "Gibbs",
  control = list(
    seed = 42,
    iter = 1000,    # Number of iterations
    verbose = 100   # Show info every 100 iterations
  )
)
cat("LDA model training complete\n")

# 10. Extract Topic Keywords -----------------------------------------------
cat("Extracting topic keywords...\n")
# Get top 20 keywords for each topic
topic_terms <- terms(lda_model, 30)

# Convert results to dataframe
topic_keywords <- data.frame(topic_terms)
colnames(topic_keywords) <- paste0("Topic_", 1:num_topics)

# Save topic keywords
write.csv(topic_keywords, file.path(output_dir, "topic_keywords_100.csv"), row.names = TRUE)

# 11. Extract Document-Topic Distribution Matrix ----------------------------
cat("Extracting document-topic distribution...\n")
# Get document-topic probabilities
doc_topics <- lda_model@gamma

# Create dataframe with document-topic distribution
doc_topic_df <- as.data.frame(doc_topics)
colnames(doc_topic_df) <- paste0("Topic_", 1:num_topics)

# Add document identifiers
doc_topic_df$doc_id <- doc_ids

# Merge with document metadata
topic_distribution <- merge(doc_data, doc_topic_df, by = "doc_id")

topic_distribution$cik_padded <- str_pad(topic_distribution$cik, width = 10, side = "left", pad = "0")
topic_distribution$uniqueid <- str_c(topic_distribution$year, topic_distribution$cik_padded, sep = "-")

# Save document-topic distribution matrix
write.csv(topic_distribution, file.path(output_dir, "document_topic_distribution.csv"), row.names = FALSE)


# 12. Section-Level Analysis for Predictive Topics -------------------------------------
cat("Performing section-level analysis for predictive topics...\n")

if (nrow(section_data) > 0) {
  # Step 1: Create section corpus and DTM using the SAME terms as the main model
  cat("Creating section corpus...\n")
  section_corpus <- Corpus(VectorSource(section_data$section_text))
  
  # Create a DTM using the same terms as the main model
  cat("Creating section DTM with main model terms...\n")
  section_dtm <- DocumentTermMatrix(section_corpus, control = list(
    dictionary = Terms(dtm_filtered)
  ))
  
  # Remove empty rows
  row_sums <- row_sums(section_dtm)
  valid_rows <- row_sums > 0
  
  # Check if we have sufficient data
  if (sum(valid_rows) > 0) {
    section_dtm_valid <- section_dtm[valid_rows, ]
    section_data_valid <- section_data[valid_rows, ]
    
    cat(sprintf("Valid sections for analysis: %d\n", nrow(section_dtm_valid)))
    
    # Step 2: For each section, calculate the average term frequency
    # This will allow us to match sections to topics without running inference
    
    # Create a matrix to store average term frequency by section
    unique_sections <- unique(section_data_valid$section_name)
    section_term_avg <- matrix(0, nrow = length(unique_sections), ncol = ncol(section_dtm_valid))
    colnames(section_term_avg) <- colnames(section_dtm_valid)
    rownames(section_term_avg) <- unique_sections
    
    # Calculate average term frequency for each section
    for (i in 1:length(unique_sections)) {
      section_name <- unique_sections[i]
      section_indices <- which(section_data_valid$section_name == section_name)
      
      if (length(section_indices) > 0) {
        section_subset <- section_dtm_valid[section_indices, ]
        # Convert to matrix and calculate average term frequency
        section_matrix <- as.matrix(section_subset)
        section_term_avg[i, ] <- colMeans(section_matrix)
      }
    }
    
    # Step 3: For each topic in the main model, calculate its similarity to each section
    # Extract the term-topic probability matrix (beta) from the LDA model
    topic_term_matrix <- exp(lda_model@beta)
    
    # Create a matrix to store topic-section similarity
    topic_section_similarity <- matrix(0, nrow = num_topics, ncol = length(unique_sections))
    colnames(topic_section_similarity) <- unique_sections
    rownames(topic_section_similarity) <- paste0("Topic_", 1:num_topics)
    
    # Calculate cosine similarity between each topic and each section
    for (topic_idx in 1:num_topics) {
      topic_vector <- topic_term_matrix[topic_idx, ]
      
      for (section_idx in 1:length(unique_sections)) {
        section_vector <- section_term_avg[section_idx, ]
        
        # Calculate cosine similarity
        similarity <- sum(topic_vector * section_vector) / 
          (sqrt(sum(topic_vector^2)) * sqrt(sum(section_vector^2)))
        
        # Handle NaN (when a vector is all zeros)
        if (is.nan(similarity)) {
          similarity <- 0
        }
        
        topic_section_similarity[topic_idx, section_idx] <- similarity
      }
    }
    
    # Save the topic-section similarity matrix
    topic_section_df <- as.data.frame(topic_section_similarity)
    topic_section_df$Topic <- paste0("Topic_", 1:num_topics)
    write.csv(topic_section_df, file.path(output_dir, "topic_section_similarity_1.csv"), row.names = FALSE)
    
    # Step 4: For each topic, identify the most similar sections
    topic_top_sections <- data.frame(
      Topic = paste0("Topic_", 1:num_topics),
      TopKeywords = apply(topic_terms, 2, function(x) paste(x[1:5], collapse = ", ")),
      TopSection1 = character(num_topics),
      TopSection2 = character(num_topics),
      TopSection3 = character(num_topics),
      stringsAsFactors = FALSE
    )
    
    for (topic_idx in 1:num_topics) {
      similarities <- topic_section_similarity[topic_idx, ]
      top_indices <- order(similarities, decreasing = TRUE)[1:3]
      top_sections <- unique_sections[top_indices]
      top_similarities <- similarities[top_indices]
      
      # Format as "Section (similarity)"
      top_sections_with_sim <- paste0(top_sections, " (", round(top_similarities, 3), ")")
      
      topic_top_sections$TopSection1[topic_idx] <- top_sections_with_sim[1]
      if (length(top_sections_with_sim) >= 2) {
        topic_top_sections$TopSection2[topic_idx] <- top_sections_with_sim[2]
      }
      if (length(top_sections_with_sim) >= 3) {
        topic_top_sections$TopSection3[topic_idx] <- top_sections_with_sim[3]
      }
    }
    
    # Save the topic-top sections data
    write.csv(topic_top_sections, file.path(output_dir, "topic_top_sections.csv"), row.names = FALSE)
    
    # Step 5: For each section, identify the most similar topics
    section_top_topics <- data.frame(
      Section = unique_sections,
      TopTopic1 = character(length(unique_sections)),
      TopTopic2 = character(length(unique_sections)),
      TopTopic3 = character(length(unique_sections)),
      stringsAsFactors = FALSE
    )
    
    for (section_idx in 1:length(unique_sections)) {
      section <- unique_sections[section_idx]
      similarities <- topic_section_similarity[, section_idx]
      top_indices <- order(similarities, decreasing = TRUE)[1:3]
      top_topics <- paste0("Topic_", top_indices)
      top_similarities <- similarities[top_indices]
      
      # Get keywords for these topics
      top_keywords <- sapply(top_indices, function(idx) {
        paste(topic_terms[1:5, idx], collapse = ", ")
      })
      
      # Format as "Topic_X (keywords) [similarity]"
      top_with_keywords <- paste0(top_topics, " (", top_keywords, ") [", round(top_similarities, 3), "]")
      
      section_top_topics$TopTopic1[section_idx] <- top_with_keywords[1]
      if (length(top_with_keywords) >= 2) {
        section_top_topics$TopTopic2[section_idx] <- top_with_keywords[2]
      }
      if (length(top_with_keywords) >= 3) {
        section_top_topics$TopTopic3[section_idx] <- top_with_keywords[3]
      }
    }
    
    # Save the section-top topics data
    write.csv(section_top_topics, file.path(output_dir, "section_top_topics.csv"), row.names = FALSE)
    
    cat("Section-level analysis for predictive topics complete!\n")
  } else {
    cat("No valid sections found after filtering. Skipping section-level analysis.\n")
  }
} else {
  cat("No section data available. Skipping section-level analysis.\n")
}

# 13. Prepare for Compustat Integration -------------------------------------
cat("Preparing data for Compustat integration...\n")

# Create a simplified dataframe with CIK, year, and topic distributions
compustat_ready_df <- topic_distribution %>%
  select(cik, year, starts_with("Topic_"))

# Save the Compustat-ready dataframe
write.csv(compustat_ready_df, file.path(output_dir, "compustat_ready_topics.csv"), row.names = FALSE)

# 14. NEW: Paragraph-Level Analysis for Representative Paragraphs -------------
cat("Performing paragraph-level analysis to find representative paragraphs...\n")

# Initialize a list to store representative paragraphs for each topic
representative_paragraphs <- list()

# Function to break text into paragraphs
extract_paragraphs <- function(text) {
  # Split by paragraph breaks
  paragraphs <- unlist(strsplit(text, "\\n\\s*\\n"))
  
  # Remove very short paragraphs
  paragraphs <- paragraphs[nchar(paragraphs) >= 80]
  
  # Return only first 1000 paragraphs if there are too many
  if (length(paragraphs) > 1000) {
    paragraphs <- paragraphs[1:1000]
  }
  
  return(paragraphs)
}

# Check if we have enough documents
if (length(corpus_texts) >= 10) {
  # Get all paragraphs from the corpus
  all_paragraphs <- list()
  para_doc_mapping <- list()
  
  # For a subset of documents (to keep computation manageable)
  max_docs <- min(50, length(corpus_texts))
  cat(sprintf("Extracting paragraphs from %d documents...\n", max_docs))
  
  for (i in 1:max_docs) {
    doc_paragraphs <- extract_paragraphs(corpus_texts[i])
    
    # Store original paragraphs
    all_paragraphs <- c(all_paragraphs, doc_paragraphs)
    
    # Remember which document each paragraph came from
    para_doc_mapping <- c(para_doc_mapping, rep(doc_ids[i], length(doc_paragraphs)))
  }
  
  # Preprocess paragraphs for topic modeling
  cat(sprintf("Preprocessing %d paragraphs...\n", length(all_paragraphs)))
  processed_paragraphs <- sapply(all_paragraphs, function(para) {
    tryCatch({
      preprocess_text(para, all_stopwords, min_length = min_word_length)
    }, error = function(e) {
      ""  # Return empty string on error
    })
  })
  
  # Filter out empty or very short paragraphs
  valid_indices <- which(nchar(processed_paragraphs) >= 50)
  processed_paragraphs <- processed_paragraphs[valid_indices]
  original_paragraphs <- all_paragraphs[valid_indices]
  para_doc_mapping <- para_doc_mapping[valid_indices]
  
  # Check if we have enough paragraphs to proceed
  if (length(processed_paragraphs) >= 100) {
    # Create corpus for paragraphs
    para_corpus <- Corpus(VectorSource(processed_paragraphs))
    
    # Create DTM with the same terms as the main DTM
    para_dtm <- DocumentTermMatrix(para_corpus, control = list(
      dictionary = Terms(dtm_filtered)
    ))
    
    # Ensure the DTM is not empty
    if (nrow(para_dtm) > 0 && ncol(para_dtm) > 0) {
      # Get topic distributions for each paragraph
      cat("Inferring topics for paragraphs...\n")
      para_topic_dist <- posterior(lda_model, newdata = para_dtm)$topics
      
      # Find dominant topic for each paragraph
      dominant_topics <- apply(para_topic_dist, 1, which.max)
      
      # For each topic, find all paragraphs where that topic is dominant
      for (topic in 1:num_topics) {
        topic_paragraphs <- which(dominant_topics == topic)
        
        # If we have at least 100 paragraphs for this topic
        if (length(topic_paragraphs) >= 100) {
          # Get paragraph indices for this topic
          topic_para_indices <- topic_paragraphs
          
          # Get only the middle tercile by length to avoid outliers
          topic_para_lengths <- nchar(original_paragraphs[topic_para_indices])
          length_sorted_indices <- topic_para_indices[order(topic_para_lengths)]
          
          # Get middle third by length
          start_idx <- floor(length(length_sorted_indices) / 3)
          end_idx <- floor(2 * length(length_sorted_indices) / 3)
          
          middle_tercile <- length_sorted_indices[start_idx:end_idx]
          
          # If we still have enough paragraphs
          if (length(middle_tercile) >= 30) {
            # Compute pairwise cosine similarity between all paragraphs
            # (Using a sample of at most 100 paragraphs to keep computation manageable)
            sample_size <- min(100, length(middle_tercile))
            sample_indices <- sample(middle_tercile, sample_size)
            
            # Create a DTM for just these paragraphs
            sample_dtm <- para_dtm[sample_indices, ]
            
            # Convert to matrix for similarity calculation
            sample_matrix <- as.matrix(sample_dtm)
            
            # Calculate cosine similarity
            similarity_matrix <- matrix(0, nrow = sample_size, ncol = sample_size)
            for (i in 1:sample_size) {
              for (j in 1:sample_size) {
                # Skip if either vector is all zeros
                if (sum(sample_matrix[i, ]) == 0 || sum(sample_matrix[j, ]) == 0) {
                  similarity_matrix[i, j] <- 0
                } else {
                  # Calculate cosine similarity
                  similarity_matrix[i, j] <- sum(sample_matrix[i, ] * sample_matrix[j, ]) / 
                    (sqrt(sum(sample_matrix[i, ]^2)) * sqrt(sum(sample_matrix[j, ]^2)))
                }
              }
            }
            
            # Find paragraph with highest average similarity
            avg_similarity <- rowMeans(similarity_matrix)
            most_representative_idx <- sample_indices[which.max(avg_similarity)]
            
            # Store the original paragraph and its metadata
            representative_paragraphs[[paste0("Topic_", topic)]] <- list(
              paragraph = original_paragraphs[most_representative_idx],
              doc_id = para_doc_mapping[most_representative_idx],
              avg_similarity = max(avg_similarity, na.rm = TRUE)
            )
          }
        }
      }
      
      # Prepare output for representative paragraphs
      if (length(representative_paragraphs) > 0) {
        # Create a data frame with topic, keywords, and representative paragraph
        rep_para_df <- data.frame(
          topic = character(),
          keywords = character(),
          representative_paragraph = character(),
          doc_id = integer(),
          avg_similarity = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (topic_name in names(representative_paragraphs)) {
          topic_idx <- as.integer(gsub("Topic_", "", topic_name))
          
          # Get top keywords for this topic
          topic_keywords <- paste(topic_terms[1:10, topic_idx], collapse = ", ")
          
          # Add to data frame
          rep_para_df <- rbind(rep_para_df, data.frame(
            topic = topic_name,
            keywords = topic_keywords,
            representative_paragraph = representative_paragraphs[[topic_name]]$paragraph,
            doc_id = representative_paragraphs[[topic_name]]$doc_id,
            avg_similarity = representative_paragraphs[[topic_name]]$avg_similarity,
            stringsAsFactors = FALSE
          ))
        }
        
        # Save representative paragraphs
        write.csv(rep_para_df, file.path(output_dir, "representative_paragraphs.csv"), row.names = FALSE)
        cat(sprintf("Found representative paragraphs for %d topics\n", nrow(rep_para_df)))
      } else {
        cat("No representative paragraphs found.\n")
      }
    } else {
      cat("Paragraph DTM is empty. Skipping representative paragraph analysis.\n")
    }
  } else {
    cat("Not enough valid paragraphs for analysis. Skipping representative paragraph analysis.\n")
  }
} else {
  cat("Not enough documents for paragraph-level analysis. Skipping.\n")
}

cat("Analysis complete. Results are saved in the output directory.\n")