## Topic modeling on news articles

A topic modeling method, namely [LDA](https://endymecy.gitbooks.io/spark-ml-source-analysis/content/%E8%81%9A%E7%B1%BB/LDA/docs/Latent%20Dirichlet%20Allocation.pdf), is performed on news articles.

The articles are extracted from the news paper website of [LeMonde](http://www.lemonde.fr/).

### Extraction

The articles are automatically extracted using the **rvest** package. The source code can be found [here](https://github.com/MathieuMarauri/LeMonde/blob/master/code/1%20-%20scrapping.R). 

A coprus of more than 2000 articles is created. Metadata is also extracted.

### Topic modeling

The corpus is cleaned ([tokenized](https://nlp.stanford.edu/IR-book/html/htmledition/tokenization-1.html) and [lemmatized](https://nlp.stanford.edu/IR-book/html/htmledition/stemming-and-lemmatization-1.html)) using [*treetagger*](http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/). Stopwords are then removed using some of the methods described in this [paper](http://www.lrec-conf.org/proceedings/lrec2014/pdf/292_Paper.pdf). 

LDA is perfored on the cleaned corpus and the obtained topics are analyzed and compare to the categories used to classifiy the articles on the website. 