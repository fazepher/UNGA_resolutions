library(tidyverse)
library(magrittr)
library(here)
library(quanteda)
library(quanteda.textstats)
library(parallel)
library(topicmodels)

load(here("resol_data","resol_text_df.RData"))

#### Corpus ####

fit_topic_model <- function(model, k, 
                            data = resol_text_df, 
                            text_field = "Resol_Text_WPOP", docid_field = "ID_FAZH",
                            remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, 
                            en_stopwords = stopwords("en",source = "marimo"), 
                            coll_size = 2:5, coll_min_count = 15,
                            unga_stopwords = unga_stopwords,
                            remove_1_2l_words = TRUE, 
                            min_term_freq = 15, 
                            seed = 51295, ...){
  
  corpus_unga <- data %>% 
    corpus(text_field = text_field, docid_field = docid_field)
  
  tokenized_unga <- corpus_unga %>% 
    tokens(remove_punct = remove_punct, 
           remove_symbols = remove_symbols, 
           remove_numbers = remove_numbers, 
           padding = T) %>% 
    tokens_remove(pattern = en_stopwords, padding = TRUE) 
  
  if(remove_1_2l_words){
    tokenized_unga <- tokenized_unga %>% 
      tokens_keep(min_nchar = 3)
  }
  
  collocations_nested <- tokenized_unga %>% 
    textstat_collocations(size = coll_size, min_count = coll_min_count)
  
  tokenized_unga_col <- tokenized_unga %>% 
    tokens_compound(pattern = collocations_nested[collocations_nested$z > 3])
  
  tm_unga <- tokenized_unga_col %>% 
    tokens_remove(c("","_______________",
                    "including","resolution","implementation",
                    "united_nations","secretary-general","general_assembly")) %>% 
    dfm() %>%
    dfm_trim(min_termfreq = min_term_freq) %>% 
    convert(to = "topicmodels")
  
  set.seed(seed)
  if(model == "LDA"){
    print("Model: LDA")
    model_unga <- mclapply(k, LDA, x = tm_unga, ...)
  } else{
    print("Model: CTM")
    model_unga <- mclapply(k, CTM, x = tm_unga, ...)
  }
  
  return(list(fitted_model = model_unga, call = sys.call()))
  
}

# tokenized_unga_col %>% 
#   dfm() %>% 
#   textstat_frequency(n = 50) %>% 
#   filter(!feature %in% c("","_______________")) %>%  
#   mutate(feature = reorder(feature,frequency)) %>% 
#   ggplot(aes(x=frequency,y=feature)) + geom_point() + scale_x_log10()



unga_stopwords <- c("","_______________",
                    "including","resolution","implementation",
                    "united_nations","secretary-general","general_assembly")


#### Hyperparameter tunning LDA ####

# 1
times <- Sys.time()
print(Sys.time())
lda_unga <- fit_topic_model(model = "LDA", k = 2:16, 
                            remove_1_2l_words = TRUE, 
                            coll_min_count = 10, min_term_freq = 20, 
                            control = list(seed = 51295),
                            mc.cores = 15, mc.set.seed = FALSE)
save(lda_unga,file=here("topic_models","LDA_1.RData"))

# 2
times <- c(time,Sys.time())
print(Sys.time())
lda_unga <- fit_topic_model(model = "LDA", k = 2:16, 
                            remove_1_2l_words = FALSE, 
                            coll_min_count = 10, min_term_freq = 20, 
                            control = list(seed = 51295),
                            mc.cores = 15, mc.set.seed = FALSE)
save(lda_unga,file=here("topic_models","LDA_2.RData"))

# 3
times <- c(time,Sys.time())
print(Sys.time())
lda_unga <- fit_topic_model(model = "LDA", k = 2:16, 
                            remove_1_2l_words = TRUE, 
                            coll_min_count = 50, min_term_freq = 50, 
                            control = list(seed = 51295),
                            mc.cores = 15, mc.set.seed = FALSE)
save(lda_unga,file=here("topic_models","LDA_3.RData"))

# 4
times <- c(time,Sys.time())
print(Sys.time())
lda_unga <- fit_topic_model(model = "LDA", k = 2:16, 
                            remove_1_2l_words = FALSE, 
                            coll_min_count = 50, min_term_freq = 50, 
                            control = list(seed = 51295),
                            mc.cores = 15, mc.set.seed = FALSE)
save(lda_unga,file=here("topic_models","LDA_4.RData"))

# 5
times <- c(time,Sys.time())
print(Sys.time())
lda_unga <- fit_topic_model(model = "LDA", k = 2:16, 
                            remove_1_2l_words = TRUE, 
                            coll_min_count = 100, min_term_freq = 75, 
                            control = list(seed = 51295),
                            mc.cores = 15, mc.set.seed = FALSE)
save(lda_unga,file=here("topic_models","LDA_5.RData"))

# 6
times <- c(time,Sys.time())
print(Sys.time())
lda_unga <- fit_topic_model(model = "LDA", k = 2:16, 
                            remove_1_2l_words = FALSE, 
                            coll_min_count = 100, min_term_freq = 75, 
                            control = list(seed = 51295),
                            mc.cores = 15, mc.set.seed = FALSE)
save(lda_unga,file=here("topic_models","LDA_6.RData"))

#### Hyperparameter tunning CTM ####

# 1
times <- c(time,Sys.time())
print(Sys.time())
ctm_unga <- fit_topic_model(model = "CTM", k = 2:16, 
                            remove_1_2l_words = TRUE, 
                            coll_min_count = 10, min_term_freq = 20, 
                            control = list(seed = 51295, 
                                           cg = list(iter.max = 750, tol = 10^-5)),
                            mc.cores = 15, mc.set.seed = FALSE)
save(ctm_unga,file=here("topic_models","CTM_1.RData"))

# 2
times <- c(time,Sys.time())
print(Sys.time())
ctm_unga <- fit_topic_model(model = "CTM", k = 2:16, 
                            remove_1_2l_words = FALSE, 
                            coll_min_count = 10, min_term_freq = 20, 
                            control = list(seed = 51295, 
                                           cg = list(iter.max = 750, tol = 10^-5)),
                            mc.cores = 15, mc.set.seed = FALSE)
save(ctm_unga,file=here("topic_models","CTM_2.RData"))

# 3
times <- c(time,Sys.time())
print(Sys.time())
ctm_unga <- fit_topic_model(model = "CTM", k = 2:16, 
                            remove_1_2l_words = TRUE, 
                            coll_min_count = 50, min_term_freq = 50, 
                            control = list(seed = 51295, 
                                           cg = list(iter.max = 750, tol = 10^-5)),
                            mc.cores = 15, mc.set.seed = FALSE)
save(ctm_unga,file=here("topic_models","CTM_3.RData"))

# 4
times <- c(time,Sys.time())
print(Sys.time())
ctm_unga <- fit_topic_model(model = "CTM", k = 2:16, 
                            remove_1_2l_words = FALSE, 
                            coll_min_count = 50, min_term_freq = 50, 
                            control = list(seed = 51295, 
                                           cg = list(iter.max = 750, tol = 10^-5)),
                            mc.cores = 15, mc.set.seed = FALSE)
save(ctm_unga,file=here("topic_models","CTM_4.RData"))

# 5
times <- c(time,Sys.time())
print(Sys.time())
ctm_unga <- fit_topic_model(model = "CTM", k = 2:16, 
                            remove_1_2l_words = TRUE, 
                            coll_min_count = 100, min_term_freq = 75, 
                            control = list(seed = 51295, 
                                           cg = list(iter.max = 750, tol = 10^-5)),
                            mc.cores = 15, mc.set.seed = FALSE)
save(ctm_unga,file=here("topic_models","CTM_5.RData"))

# 6
times <- c(time,Sys.time())
print(Sys.time())
ctm_unga <- fit_topic_model(model = "CTM", k = 2:16, 
                            remove_1_2l_words = FALSE, 
                            coll_min_count = 100, min_term_freq = 75, 
                            control = list(seed = 51295, 
                                           cg = list(iter.max = 750, tol = 10^-5)),
                            mc.cores = 15, mc.set.seed = FALSE)
save(ctm_unga,file=here("topic_models","CTM_6.RData"))

