# Librerie

# File reading and data handling
import csv
import glob
import matplotlib.colors
import matplotlib.pyplot as plt # version 2.2 or newer to run "train_model" function properly
import numpy as np
import os
import random
import pandas as pd
import warnings
from collections import Counter
from tabulate import tabulate
from tqdm import tqdm


# Pre-processing
import gensim
import re
from gensim.parsing.preprocessing import preprocess_string
from nltk.stem import WordNetLemmatizer


# Models
import multiprocessing
from afinn import Afinn
from gensim import corpora
from gensim.models import Word2Vec
from sklearn import metrics
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_extraction.text import TfidfTransformer, TfidfVectorizer, CountVectorizer
from sklearn.metrics import classification_report, confusion_matrix
from sklearn.model_selection import cross_val_predict, cross_val_score, GridSearchCV, train_test_split 
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import MultinomialNB
from sklearn.neural_network import MLPClassifier
from sklearn.svm import SVC

# Final step
from collections import defaultdict, Counter
from nltk.corpus import brown
import nltk

warnings.filterwarnings(action="ignore")