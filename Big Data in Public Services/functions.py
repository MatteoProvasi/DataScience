
# Funzioni per aprire i file
def get_articles(path, files):
    df = pd.DataFrame()
    chars = ["'", ']', '[']
    
    for name in tqdm(files): # for each file in folder
        ids = []
        keyw = []
        
        with open(name, encoding="UTF-8") as f:
            reader = pd.read_csv(f, delimiter=",")
            ids = [reader["articleID"].iloc[i] for i in range(len(reader))]
            temp_keyw = [reader["keywords"].iloc[i] for i in range(len(reader))]
            
            for i in range(len(temp_keyw)):
                for char in chars:
                    temp_keyw[i] = temp_keyw[i].strip(char)
                                                      
            keyw = [temp_keyw[i] for i in range(len(temp_keyw))]
            

        data = pd.DataFrame({"artID":ids, "Keywords":keyw})
        df = df.append(data)
        f.close()

    return df
def get_comments(path, files):
    df = pd.DataFrame()
    
    for name in tqdm(files):
        com = []
        ids = []
        with open(name, encoding="UTF-8") as f:
            reader = pd.read_csv(f, delimiter=",")
            com = [reader["commentBody"].iloc[i] for i in range(len(reader)) if reader["commentBody"].iloc[i]]
            ids = [reader["articleID"].iloc[i] for i in range(len(reader))]
            
        data = pd.DataFrame({"comID":ids, "Comments":com})
        df = df.append(data)
        f.close()
    
    return df
	
# Funzioni per l'LDA (da aggiornare)
def prepare_lda(kw_list):
    kw_list = list(kw_list) # create a list from the 
    base_keywords = []
    
    """
    To use LDA we need a list of keywords for each observation.
    Keywords are into a list-like format but they are strings! So we need to create a keyword list.
    These lines do:
    For every observation in keywords (list of keywords for each comments)
        * Split the list into separate keywords
        * For each keyword remove spaces and apexes
        * If the keyword's length is 2 or more, it is a valid keyword
        * Create a list of these valid keywords at observation level and append it to a new list
    """
    for row in kw_list:
        base_keywords.append([keyw.strip(' ').strip("'") for keyw in row.split("'") if len(keyw) > 2])
    
    dictionary = corpora.Dictionary(base_keywords) # dictionaty first
    corpus = [dictionary.doc2bow(topic) for topic in base_keywords] # BOW then
    
    return (dictionary, corpus, base_keywords)

def lda_to_topic(dictionary, model, base_keywords, topics):

    diz_assignment = {} # dictinary in order to get assignment only once for each set of keywords
    topic_assignment = [] # list of the topic assignment to each comment

    for valid_key in tqdm(base_keywords):
        if str(valid_key) in diz_assignment.keys():
            topic_assignment.append(diz_assignment[str(valid_key)])
            
        else:
            topic_index = []
            topic_prob = []

			# https://github.com/RaRe-Technologies/gensim/issues/2439
			# it might happen that function does not work, one solution is to upgrade gensim library
            new_bow = dictionary.doc2bow(valid_key)
            assignment = ldamodel.get_document_topics(new_bow) # topic for every observation


            for i in range(len(assignment)): # extract the index and the prob
                topic_index.append(assignment[i][0])
                topic_prob.append(assignment[i][1])

            """
            Create a dataset with two columns: topic and probability
            Sort the df by decreasing probability
            Take the best topic 
            """
            df_topic = pd.DataFrame({"Topic":topic_index, "Prob":topic_prob})
            df_topic = df_topic.sort_values(by=["Prob"], ascending=False)
            best_topic = df_topic["Topic"].iloc[0]

            topic_assignment.append(topics[best_topic]) # use the topic assignment id to get the topic name
            diz_assignment[str(valid_key)] = topics[best_topic] # update dictionary
        
    return (topic_assignment)
def lda_assign(filename, dataset, ldamodel):
    try:
        datar = pd.read_csv(filename, sep=";", encoding="UTF-8")
        datar.drop("Unnamed: 0", axis=1, inplace=True)
		
        return (datar)
        
    except FileNotFoundError:
        print("File not found, running 'lda_to_topic' function to assign topics")
		
		# Create dictionary and corpus for LDA
        (key_dictionary, key_corpus, base_keywords) = prepare_lda(data["Keywords"])
        topics = ["US_Elections_2016", "Environment", "International", "US_Politics", "Guns", "Ethics", "Laws", "(Social)_Media", "News"]
        topic_assignment = lda_to_topic(key_dictionary, ldamodel, base_keywords, topics)
        
        dataset.drop("Keywords", axis=1, inplace=True)
        dataset["Keywords"] = topic_assignment
        dataset.to_csv(filename, sep=";", encoding="UTF-8")
    
    return(dataset)

	# Funzioni di pre-processing
def remove_short(df, colname, given_len):
    index_remove_list = []
    
    for i in tqdm(range(len(df))):
        com_list = (str(df[colname].iloc[i])).split(" ")
        
        if len(com_list) < given_len:
            index_remove_list.append(i)
    
    df = df.drop(df.index[index_remove_list], axis=0)   
    return df	
def pre_processing(texts: list, stopwords=0):
    texts = [re.sub(r'http\S+', '', str(line)) for line in texts]
    texts = [re.sub('<br>', '', str(line)) for line in texts]
    texts = [re.sub('</br>', '', str(line)) for line in texts]
    my_filter = [
        lambda x: x.lower(),
        gensim.parsing.strip_punctuation,
        gensim.parsing.strip_multiple_whitespaces,
        gensim.parsing.strip_numeric,
        gensim.parsing.strip_tags]
    if stopwords == 1:
        my_filter.append(gensim.parsing.remove_stopwords)
        my_filter.append(gensim.parsing.strip_short)
    
    texts = [preprocess_string(x, filters=my_filter) for x in tqdm(texts[0:len(texts)])]
    texts = [" ".join((x)) for x in texts if type(x)==list]
    
    return texts	
def lemmatization(df_column):
    pos_list = ["v", "n", "a"]
    lemmatizer = WordNetLemmatizer()
    for pos in tqdm(pos_list):
        df_column = df_column.apply(lambda x: " ".join([lemmatizer.lemmatize(word, pos) for word in x.split()]))
        
    return df_column

# Funzioni per i modelli
def sampling(original_df, num_topics):
    new_df = pd.DataFrame()
    
    key_freq = Counter(list(original_df["Keywords"])).most_common(num_topics)    
    mean_key = int(np.mean([key_freq[i][1] for i in range(len(key_freq))]))

    for i in range(len(key_freq)):
        temp = original_df.loc[original_df["Keywords"] == key_freq[i][0]]
        
        if len(temp) > mean_key:
            temp = temp.sample(mean_key)
        else:
            temp = temp.sample(mean_key, replace=True)
        
        new_df = new_df.append(temp)
    
    return new_df
def train_model(classifier, feature_vector_train, label, feature_vector_valid, method):
    # New colormap
    norm = matplotlib.colors.Normalize(-1,1)
    colors = [[norm(-1.0), "lightyellow"],
              [norm(-0.95), "antiquewhite"],
              [norm(-0.8), "wheat"],
              [norm(-0.3), "navajowhite"],
              [norm( 0.3), "orange"],
              [norm( 0.8), "tomato"],
              [norm( 0.9), "red"],
              [norm( 1.0), "darkred"]]

    cmap = matplotlib.colors.LinearSegmentedColormap.from_list("", colors)


    classifier.fit(feature_vector_train, label) # train classificator
    
    predictions = classifier.predict(feature_vector_valid) # class classification on test set
    print(classification_report(y_test, predictions)) # classification report
    
    cm = confusion_matrix(predictions, y_test) # confusion matrix
    print(cm)
    cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
    
    fig = plt.figure(figsize = (8, 8))
    ax = fig.add_subplot(111)
    cax = ax.matshow(cm, cmap=cmap)
    fig.colorbar(cax)

    plt.title('Confusion matrix of ' + method, pad=40)
    ax.set_xticklabels([''] + topic_name, rotation=35)
    ax.set_yticklabels([''] + topic_name)
    plt.xlabel('Predicted')
    plt.ylabel('True')
    plt.show()
    
    return metrics.accuracy_score(predictions, y_test)	
def tf_idf(train, test, freq_list, ngram_list, model, params):
    for gram in ngram_list:
        for freq in freq_list:
            tfidf_vect = TfidfVectorizer(min_df=freq, ngram_range=gram)
            X_train_tfidf = tfidf_vect.fit_transform(train)
            X_test_tfidf =  tfidf_vect.transform(test)
            print("Min freq %s \t n_gram range %s \t %s" %(freq, gram, X_train_tfidf.shape))
            
            model_clf = GridSearchCV(model, params, cv=2, n_jobs=1, verbose=0)
            model_clf.fit(X_train_tfidf, y_train)

            # Best paramete set
            print("Best parameters found:\n", model_clf.best_params_)
            print("Accuracy: " + str((model_clf.best_score_)*100) + " % \n")
            
            accuracy = train_model(model_clf, X_train_tfidf, y_train, X_test_tfidf, "MultinomialNB")
            print(accuracy, "\n")

# Funzioni per la ricerca della nuova frase
def viterbi(sentence):
    viterbi, backpointer = ([] for i in range(2))
    first_viterbi, first_backpointer = ({} for i in range(2))
    
    for tag in distinct_tags:
        if tag == "START": continue
        first_viterbi[tag] = cpd_tags["START"].prob(tag) * cpd_tagwords[tag].prob(sentence[0])
        first_backpointer[tag] = "START"
        
    viterbi.append(first_viterbi)
    backpointer.append(first_backpointer)
    currbest = max(first_viterbi.keys(), key = lambda tag: first_viterbi[tag])
    
    
    for wordindex in range(1, len(sentence)):
        this_viterbi, this_backpointer = ({} for i in range(2))
        prev_viterbi = viterbi[-1]
        
		# I look for a X tag and a specific word, i try to find the previous tag Y such as Y X is the most probable
        for tag in distinct_tags:
            if tag == "START": continue
            best_previous = max(prev_viterbi.keys(),
                                key = lambda prevtag: \
            prev_viterbi[prevtag] * cpd_tags[prevtag].prob(tag) * cpd_tagwords[tag].prob(sentence[wordindex]))

            this_viterbi[tag] = prev_viterbi[best_previous] * \
            cpd_tags[best_previous].prob(tag) * cpd_tagwords[tag].prob(sentence[wordindex])
            this_backpointer[tag] = best_previous

        currbest = max(this_viterbi.keys(), key = lambda tag: this_viterbi[tag])
        viterbi.append(this_viterbi)
        backpointer.append(this_backpointer)
    
    
	# Probability for each tag to have an END as the following tag.
    prev_viterbi = viterbi[-1]
    best_previous = max(prev_viterbi.keys(),
                        key = lambda prevtag: prev_viterbi[prevtag] * cpd_tags[prevtag].prob("END"))

    prob_tagsequence = prev_viterbi[best_previous] * cpd_tags[best_previous].prob("END")

    # best tagsequence: we store this in reverse for now, will invert later
    best_tagsequence = ["END", best_previous]
    # invert the list of backpointers
    backpointer.reverse()

    current_best_tag = best_previous
    for bp in backpointer:
        best_tagsequence.append(bp[current_best_tag])
        current_best_tag = bp[current_best_tag]

    best_tagsequence.reverse()
    
    #print(" ".join(best_tagsequence[1:-1]))
    return(" ".join(best_tagsequence[1:-1]))
	
def category(model, sentence):
    """
    Given a sentence this function estimate the probability of belonging to one class.
    It return a dataframe with classes and estimated probabilities.
    """
    
    # https://datascienceplus.com/multi-class-text-classification-with-scikit-learn/
    if type(sentence) == list:
         sentence = " ".join(sentence)
    predictions = (model.predict_proba(count_vect.transform([sentence])))
    ref_classes = (model.classes_).tolist()
    predictions = [value for lista in predictions.tolist() for value in lista]
    
    df_prob_class =  pd.DataFrame({'Pred': predictions, 'class': ref_classes})
    df_prob_class = df_prob_class.sort_values(["Pred"], ascending=False)
    df_prob_class = df_prob_class.reset_index().drop("index", axis=1)
    
    print(tabulate(df_prob_class, headers="keys", tablefmt="psql", showindex=False))
    
    return(df_prob_class)

def random_select(length, comm_list):
    pos = np.random.choice(len(comm_list))
    sentence = (comm_list[pos:pos+length])
    sentence = [el for el in list(sentence) if len(el) > 0]
    
    temp_sent = pre_processing(sentence, stopwords=1)
    temp_sent = " ".join(temp_sent)
    
    temp_df = pd.DataFrame([temp_sent], columns=["Sentence"])
    
    temp_sent = lemmatization(temp_df["Sentence"])
    
    temp_sent = [el for el in list(temp_sent) if len(el) > 0]
    temp_sent = ' '.join(temp_sent)
    
    return(sentence, temp_sent)

def follow(topic_df, sentence, end, n_match):
    data = pd.DataFrame()
    afinn = Afinn()
    follow_freq = {}
    
    while len(follow_freq) < n_match: # I need at least a given number of following set of words
        follow_freq = {}
        string_sentence = " ".join(sentence[-end:])
        
        for comment in topic_df["Comments"]:
            if string_sentence in comment: # I only need comment with the sentence 
                try:
                    comment_list = comment.split(string_sentence)
                except ValueError:
                    print("No more words can be added for the given sentence!")
                    break

                for part in comment_list[1:]:
                    part = part.split(" ")
                    if len(part) > end: # I need at least end following words (the first element is a blank space)
                        part = part[1:]
                        following = ""

                        for i in range(end):
                            following = following + part[i] + " "
                        
                        following = following[:len(following)-1] # remove the last ""
                        
                        if following in follow_freq:
                            follow_freq[following] += 1
                        else:
                            follow_freq[following] = 1

        if len(follow_freq) < n_match:
            end -= 1
    
    follow_freq = Counter(follow_freq).most_common()
    
    # Check if null sentiment
    max_sent = 0
    for cont in range(len(follow_freq)):
        temp_follow = follow_freq[cont][0].split(" ")
        for i in range(len(temp_follow)):
            test_sent = afinn.score(temp_follow[i])
            if test_sent > max_sent:
                max_sent = test_sent
    
    if max_sent == 0:
        print("Warning! Adding word with a null sentiment")
        
    return(follow_freq, end, max_sent)  

def words_dataframe(topic_df, sentence, alpha, method, step, n_match, end_seq):
    if step == 0:
        end = len(sentence)
    else:
        end = end_seq
        
    status = "not ok"
    
    while status == "not ok":
        (follow_freq, end, max_sent) = follow(topic_df, sentence, end, n_match)
        
        if method == 1: # If what I care most is the sentence's meaning
            if len(follow_freq) > 0:
                status = "ok"
        if method == 0: # If what I care most is the sentence's sentiment
            if max_sent == 0:
                if end > 2:
                    end -= 1
                else:
                    print("No more words can be added for the given sentence!")
                    break
            else:
                status = "ok"
              
            
    """
    Follow freq is a tuple with "list of words", "freq".
    I create as many lists as the number of words in "list of words" which is exactly the end value
    I must have two more lists: one for store the freq count and one for the sentiment
    Words in the "list of words" which also appears at the end of the sentence are not considered
    With what I found, I create a new index value which is a combination between the freq count and the sentiment
    """        
    lists = [[] for _ in range(end+2)]
    big_list = [el for el in lists]
    
    follow_count = []
    afinn_score = []
    for cont in range(len(follow_freq)):
        temp_follow = follow_freq[cont][0].split(" ")
        sent_score = 0
        for i in range(len(temp_follow)):
            if temp_follow[i] not in sentence[-end:]: # I try not to add a word which is already at the end of the sentence
                try:
                    big_list[i].append(temp_follow[i])
                except IndexError:
                    break
                sent_score = sent_score + afinn.score(temp_follow[i])
            else:
                big_list[i].append(" ")
                sent_score += 0
                
        big_list[end].append(follow_freq[cont][1])
        big_list[end+1].append(sent_score)
        try:
            if len(big_list[end]) != len(big_list[i]): # It happens when a word in also at the end of the sentence
                del (big_list[end][-1], big_list[end+1][-1]) 
        except IndexError:
            pass

    for i in range(len(big_list[end])):
        if type(big_list[end][i]) == str:
            big_list[end][i] = 0
        if type(big_list[end+1][i]) == str:
            big_list[end+1][i] = 0

    (max_count, max_sent) = (max(big_list[end]), max(big_list[end+1]))
    count_stand = [big_list[end][cont]/max_count for cont in range(len(big_list[end]))]
    
    if max_sent == 0:
        max_sent = 0.01
    
    sent_stand = [big_list[end+1][cont]/max_sent for cont in range(len(big_list[end+1]))]
    best_pick = [(count_stand[cont]*(sent_stand[cont] + alpha)) for cont in range(len(big_list[end]))]
            
    # Creating names from the df
    variables = ["Follow" + str(i+1) for i in range(end)]
    new_vars = ["Count", "Sentiment", "Count_stand", "Sent_stand", "Best_pick"]
    for var in new_vars:
        variables.append(var)
    
    big_list.append(count_stand)
    big_list.append(sent_stand)
    big_list.append(best_pick)
    diz = {}
    for i in range(len(big_list)):
        diz[variables[i]] = big_list[i]
    

    datt = pd.DataFrame(diz, columns=diz.keys())
    
    return(datt, end, max_sent)  
	
def final_function(df, model, length, comment_list, alpha, end_seq, n_match, method):
    (sentence, prep_sentence) = random_select(length, comment_list)
    
    threshold = int(input("Type a number as the sentiment upper bound "))
    print("Sentiment threshold:", str(threshold))
    
    sentiment = 0
    step = 0
    
    # predict the class
    data_class = category(model, prep_sentence)
    print("{", sentence, "}\t predicted class:", data_class["class"].iloc[0])
    globals()["data_%s" %data_class["class"].iloc[0]] = df.loc[df["Keywords"] == data_class["class"].iloc[0]]
    
    
    while abs(sentiment) < abs(threshold):
        print("{", " ".join(sentence), "}\t sentiment score:", afinn.score(" ".join(sentence)), "\n")
        
        # find the most common following words
        (data, end, max_sent) = words_dataframe(globals()["data_%s" %data_class["class"].iloc[0]], 
                                                      sentence, alpha=alpha, n_match=n_match, step=step, method=method,
                                                      end_seq=end_seq)
        data = data.reset_index(drop=True)
        #data.drop("index",axis=1,inplace=True)
        
        
        if threshold > 0:
            data = data[data["Best_pick"] > 0]
            data = data.sort_values(by=['Best_pick'], ascending=False)
        else:
            data = data[data["Best_pick"] < 0]
            data = data.sort_values(by=['Best_pick'], ascending=True)
        
        
        # Tag sequence
        tag_seq_list = []
        tag_seq = ""
        for n in range(len(data)):
            for i in range(end):
                try:
                    tag_seq = tag_seq + (list(word_tags[data["Follow" + str(i+1)].iloc[n]]))[0] + " "
                except IndexError:
                    tag_seq = tag_seq + "None" + " "
            tag_seq = tag_seq[:-1]
            tag_seq_list.append(tag_seq)
        data["Tags"] = tag_seq_list
        
        best_tag_seq = viterbi(sentence[-end:])
        try:
            data_tag = data.loc[data["Tags"] == best_tag_seq]
        except TypeError:
            pass
        if len(data_tag) > 0:
            data = data_tag
        
        
        
        # Update sentence
        sentence = " ".join(sentence)
        for i in range(end):
            if data["Follow" + str(i+1)].iloc[0] != "":
                sentence = sentence + " " + data["Follow" + str(i+1)].iloc[0]
        print(sentence)
                
        sentence = sentence.split(" ")
        sentiment = afinn.score(" ".join(sentence))
        step += 1
        
    print("New sentence: {", " ".join(sentence), "}\t score:", afinn.score(" ".join(sentence)))
    return(sentence)