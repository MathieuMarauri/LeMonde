## Topic modeling on news articles

A topic modeling method, namely [LDA](https://endymecy.gitbooks.io/spark-ml-source-analysis/content/%E8%81%9A%E7%B1%BB/LDA/docs/Latent%20Dirichlet%20Allocation.pdf), is performed on news articles.

The articles are extracted from the news paper website of [LeMonde](http://www.lemonde.fr/).

### Extraction

The articles are automatically extracted using the **rvest** package. The source code can be found [here](https://github.com/MathieuMarauri/LeMonde/blob/master/code/1%20-%20scrapping.R). Helper functions are defined [here](https://github.com/MathieuMarauri/LeMonde/blob/master/code/functions/1%20-%20scrapping%20functions.R)

Scrapping is done by topic so that articles are pre-classified. Every articles from every categories are extracted. The title, the author, the text, the timestamp, the topic, the subtopic and the url are retrieved for each article.

A coprus of 2404 articles with metadata is created.

### Topic modeling

Topic modeling is the process of extracting hidden topics from a corpus of documents. The goal of this projet is to apply LDA method on the news articel to see how well it perfoms. Before any modeling it is necessary to clean the corpus, namely to remove stopwords. Stopwords are words that do not carry information. Several methods are compared.

#### Pre-processing

Before applying the LDA on the corpus, it must be pre-processed. The method is supposed to give better results on a clean corpus. 

First the corpus is cleaned ([tokenized](https://nlp.stanford.edu/IR-book/html/htmledition/tokenization-1.html) and [lemmatized](https://nlp.stanford.edu/IR-book/html/htmledition/stemming-and-lemmatization-1.html)) using [*treetagger*](http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/). 

Then stopwords are removed using 5 diferent methods (the last one being a combination of the others). These methods are compared regarding the data sparsity of the document term matrix and the number of words removed by the method. The baseline will be no removal at all. The methods are: bag of words, part-of-speech, TF1, IDF, TF-IDF and a mix of all the methods. 

At the end of this step document term matrices are obtained, one for each stopwords removal method pus one for the baseline. 

#### LDA

LDA is performed on the different document term matrices. 