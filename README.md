

# LeMonde topic modeling
 
## Introduction
  
  This project's objective is to apply a topic modeling method on a corpus of documents. The goal is to test the Latent Dirichlet Method (LDA) and to see how it performs on a corpus pre-processed with different stopwords removal methods. The corpus is built using news articles from [lemonde](http://www.lemonde.fr/), a french newspaper. The goal is to analyze the results of the LDA model with regard to the website categories in which the articles were. The LDA model needs an apriori, user defined, number of topics as input. Knowing the category in which the article was on the website is a start point to decide the number of topics to ask to the model and also an indicator of performance of the model: are the articles most associated with the topic corresponding to their category on the website. The entire process -scrapping, pre-processing and modelization- is done in R. 


## Creation of the corpus

The corpus is created using news articles from the website of a french newspaper. The articles are scrapped using the `rvest` package. The content of the article is extracted, along with the title, the author, the date of publication, the category on the website in which the article is and the subactegory found in the url. The resulting corpus contains 2404 articles published between the 11th of July and the 13th of October of 2017. There were extracted the 13th of October. 


```r
install.packages('rvest')
library('rvest')
```

Other packages are used in the scrapping process. 


```r
library('data.table') # to manipulate table 
library('stringi') # to manipulate strings
```

### Extraction process

The goal is to create a corpus of articles on which a LDA model can be performed. Article category is necessary information, articles are extracted by category. The process to create the corpus with metadata was constructed with trial an errors looking at the structure of the website and based on what was needed for this project.

#### Categories name

The first step is to extract the categories name so that the extraction can then be performed by category. Indeed articles from a given category are listed in an url of the form _http://www.lemonde.fr/politique/1.html_ (first 20 articles of the political category). The categories extraced are: international, politique, societe, economie, culture, idees, planete, spot and sciences. 

Extracting category names is done using the following code:


```r
topic_names <- read_html('http://www.lemonde.fr/') %>% 
html_nodes("#navigation-generale li") %>% 
html_attr("class") %>% 
unique() %>% 
stri_replace_all_fixed(pattern = ' ', replacement = '')
topic_names <- topic_names[2:10] 
```

First the web page is read with `read_html`, then, looking at the source code of the web page, the css identifiers of the categories names are used in `html_nodes`. Here we select everything that is inside a _li_ tag with a parent of id _navigation-generale_. The _class_ attribute is extracted using `html_attr`. 

#### Article url

Before extracting the content of an article it is necessary to have the url of that article. Using the url associated with a given category, we loop through the 20 first pages and extract all the articles url. The timestamp of publication on the website is also retrieves at this step. 

Extracting the urls of the articles of a webpage is done using the following.


```r
url_path <- 'http://www.lemonde.fr/politique/1.html'
url_path <- read_html(url_path)
url <- html_nodes(url_path, ".grid_12.alpha.enrichi .grid_11.conteneur_fleuve.omega h3 a") %>% 
html_attr(name = 'href')
```

The attribute _href_ is extracted for all elements having the stucture described in `html_nodes`. This piece of code in incorporated in a lager function that extract the urls of all articles of a category along with the timestamp of publication. 

#### Article content

The content of an article is the central information of the corpus, after extracting the url it is possible to have the content of the article. Doing so require to find the correct css structure leading to the content for every articles. Some trials and errors are needed at this step. The solution found is the following:


```r
articleUrl <- read_html(articleUrl)
first_par <- html_nodes(articleUrl, "div[itemprop=articleBody] .taille_courante") %>% html_text() %>% paste(collapse = ' ')
other_par <- html_nodes(articleUrl, "div[itemprop=articleBody] p:not(.lire)") %>% html_text() %>% paste(collapse = ' ')
text <- paste(first_par, other_par, sep = '\n')
```

Two differents css structures are used to extract the entire content of the text of an article. Once again this code is incorporated in a function to automate the process.

#### Article author

When it is available, the author of the article is also extracted from the webpage. 


```r
author <- html_nodes(articleUrl, ".signature_article span") %>% html_text()
```

#### The corpus

The resulting corpus contains the article content and some metadata. It has 2404 rows (articles) and 10 columns. For each article in the corpus we have the following information:

- id: unique integer to identify the article,
- tittle: the title of the article as a string,
- text: the content of the article as a string,
- topic: the category in which the article was found, can contain several categories since the same article can be present in several categories on the website,
- subtopic: the subcategory of the article as stated in the url after the category, can contain several subcategory, the last one being the most precise one,
- date: the date of publication of the article on the website,
- time: the time of publication of the article on the website,
- author: the author of the article, can be missing,
- url: the complete url of the article,
- date_creation: the timestamp of the extraction of the information on the article.

The text and the subcategory are the two variables that will be used. The other features may be of use for other studies. 

The complete code to get the corpus can be found [here](https://github.com/MathieuMarauri/LeMonde/blob/master/code/1%20-%20scrapping.R). 


## Cleaning the corpus

The corpus created in the previous section contains raw text data. NLP tasks should be done on pre-processed text. Raw text yields poor results because it usually contains lots of non-informative words. Those words are called stopwords. LDA tries to find latent topic hidden in a corpus of text, it basically tries to find groups of words that appears in the same document and not in others. It can be easily understood that english words like `a`, `the` or `are` will not help the model find good topics. Hence it is necessary to properly clean the corpus from all these stopwords. Several methods can be used to remove stopwords, 5 different methods are presented here and tested on the corpus. Besides, a mix of several methods is presented as a 6th method. These approaches are compared with respect to the diminution of the features space (the number of words) and th reduction of the sparsity of the document term matrix. The performance of the LDA model based on the different cleaned corpus is also taken inton account to compare the stopwords removal methods. Such a comparison is a good way to discover other approach than the classic bag-of-words one and to see how it can impact resuld of LDA modeling.

The methods exposed here are:

- bag-of-words: a list of stopwords is defined ad hoc and used to clean the corpus.
- pos tags: words with certain part-of-speech are removed. 
- TF1: words with term frequency equal to 1 are removed.
- IDF: words with IDF equal to 0 or to the max possible value are removed. 
- TF-IDF: words with low term-frequency-inverse-term-frequency are removed.
- mix: part-of-speech and IDF.


### Pre-processing

Before any stopwords removal approach is applied to the corpus, it is tokenized, lemmatized and part-of-speech tags are added to each words. Treetagger (available [here](http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/)) is used here to annotate the corpus. It can be used to tag several languages including English and French.  

#### Tokenization

Tokenization is a necessary step in any tasks involving text data. It means dividing sentences into a vector of tokens. Usually a token is a word, it can also be a punctuation mark. It is a document unit. Tokenization is mainly performed to divide a document into vectors of tokens representing one unit but it can also be n-grams or groups of n units. 

The following sentence, `A short sentence to tokenize`, will become this vector `A, short, sentence, to, tokenize` after being tokenized. If bigrams are used then the vector is `A short, short sentence, sentence to, to tokenize`.

**_How does tokenization work?_**

The goal is to split a document into tokens. To do so heuristics are used. Tokens are split on all non alpha-numeric character (,, ;, :, -, whitespace, ...) and sentences on sentence-ending-characters (., !, ?, ...). Special rules are added to treat cases like _aren't_, _I've_, ... Specific rules differ from one language to the other. Lists of known abbreviations can be pre-defined to avoid splitting _d'abord_ (first) into _de, abord_ for example. 

After being tokenized the corpus can be transformed to a document-term-matrix. Documents are on the rows, tokens (or terms) on the columns and each cell represents the number of times the term appears in the document. Words constitute the features of the dataset. 

This newly created dataset has to be cleaned as it would be before any modeling is done on more usual data. Here cleaning means reducing the feature space by grouping tokens that are derive from the same one and removing non-informative words. The first part can be achieved with lemmatization, the second with stopwords removal methods. 

The words _word, token, term and feature_ can be used to represent the same idea. 

#### Lemmatization

Lemmatization is the process of reducing a word to its lemma, its dictionary form. The lemma is the canonical form of a word. Different inflections of the verb _to have_ may be  _having, has, haven_ is _have_. Lemmatisation reduces the feature space by reducing inflections of the same words to its dictionary form. 

**_How does lemmatization work?_ **

Lemmatization relies on part-of-speech tagging to properly reduce a word inflection to its dictionary form. It will not treat a word the same it treats a verb. For example the word _meeting_ in _I have a meeting tomorrow_ will be kept as is whereas in _we are meeting him tomorrow_ it will be reduce to _meet_. The part-of-speech tagging is done using decidin trees to estimate the transition probability in the Markov model. The paper explaining the construction of such a tagger can be found [here](http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/tree-tagger1.pdf). The training data is obtained from the Penn-Teebank corpus.  

The complete list of tags in the treetagger is displayed in the following table.

<div style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height:400px; overflow-x: scroll; width:300px; "><table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead><tr>
<th style="text-align:left;"> Tag </th>
   <th style="text-align:left;"> Meaning </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;"> ADJ </td>
   <td style="text-align:left;"> adjective </td>
  </tr>
<tr>
<td style="text-align:left;"> ADV </td>
   <td style="text-align:left;"> adverb </td>
  </tr>
<tr>
<td style="text-align:left;"> DET:ART </td>
   <td style="text-align:left;"> article </td>
  </tr>
<tr>
<td style="text-align:left;"> DET:POS </td>
   <td style="text-align:left;"> possessive pronoun </td>
  </tr>
<tr>
<td style="text-align:left;"> INT </td>
   <td style="text-align:left;"> interjection </td>
  </tr>
<tr>
<td style="text-align:left;"> KON </td>
   <td style="text-align:left;"> conjunction </td>
  </tr>
<tr>
<td style="text-align:left;"> NAM </td>
   <td style="text-align:left;"> proper name </td>
  </tr>
<tr>
<td style="text-align:left;"> NOM </td>
   <td style="text-align:left;"> noun </td>
  </tr>
<tr>
<td style="text-align:left;"> NUM </td>
   <td style="text-align:left;"> numeral </td>
  </tr>
<tr>
<td style="text-align:left;"> PRO </td>
   <td style="text-align:left;"> pronoun </td>
  </tr>
<tr>
<td style="text-align:left;"> PRO:DEM </td>
   <td style="text-align:left;"> demonstrative pronoun </td>
  </tr>
<tr>
<td style="text-align:left;"> PRO:IND </td>
   <td style="text-align:left;"> indefinite pronoun </td>
  </tr>
<tr>
<td style="text-align:left;"> PRO:PER </td>
   <td style="text-align:left;"> personal pronoun </td>
  </tr>
<tr>
<td style="text-align:left;"> PRO:POS </td>
   <td style="text-align:left;"> possessive pronoun </td>
  </tr>
<tr>
<td style="text-align:left;"> PRO:REL </td>
   <td style="text-align:left;"> relative pronoun </td>
  </tr>
<tr>
<td style="text-align:left;"> PRP </td>
   <td style="text-align:left;"> preposition </td>
  </tr>
<tr>
<td style="text-align:left;"> PRP:det </td>
   <td style="text-align:left;"> preposition plus article </td>
  </tr>
<tr>
<td style="text-align:left;"> PUN </td>
   <td style="text-align:left;"> punctuation </td>
  </tr>
<tr>
<td style="text-align:left;"> PUN:cit </td>
   <td style="text-align:left;"> punctuation citation </td>
  </tr>
<tr>
<td style="text-align:left;"> SENT </td>
   <td style="text-align:left;"> sentence tag </td>
  </tr>
<tr>
<td style="text-align:left;"> SYM </td>
   <td style="text-align:left;"> symbol </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:cond </td>
   <td style="text-align:left;"> verb conditional </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:futu </td>
   <td style="text-align:left;"> verb futur </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:impe </td>
   <td style="text-align:left;"> verb imperative </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:impf </td>
   <td style="text-align:left;"> verb imperfect </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:infi </td>
   <td style="text-align:left;"> verb infinitive </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:pper </td>
   <td style="text-align:left;"> verb past participle </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:ppre </td>
   <td style="text-align:left;"> verb present participle </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:pres </td>
   <td style="text-align:left;"> verb present </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:simp </td>
   <td style="text-align:left;"> verb simple past </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:subi </td>
   <td style="text-align:left;"> verb subjunctive imperfect </td>
  </tr>
<tr>
<td style="text-align:left;"> VER:subp </td>
   <td style="text-align:left;"> verb subjunctive present </td>
  </tr>
</tbody>
</table></div>

Knowing the part-of-speech of the word to lemmatize, the algorithm uses a dictionary look-up table and some heuristics to find the lemma.

#### The code

The entire pre-procesing is done in one call to treetagger. It tokenizes then lemmatize and tag the corpus. The call has been encapsulated in a function for convenience mainly to have a proper structure in the output. Treetagger is installed as a dependency and it is called using a shell command. The resulting text file is imported into R and returned as a table. 


```r
#' This function lemmatizes a text using treetagger.
#' 
#' @param text a raw character vector of length one.
#' @param id the id of the text in the corpus
#' @param filter boolean, should some pos be filtered out
#' 
#' @return a table with id, lemma and count
#' 
lemmatize <- function(text, id, filter = TRUE) {
  path_to_treetag <- 'C:/TreeTagger'
  writeLines(text = text, con = file.path(path_to_treetag, 'article_to_tag.txt'), useBytes = TRUE)
  shell('cd /d C:/TreeTagger & tag-french article_to_tag.txt tag_results.txt')
  tag_results <- fread(input = file.path(path_to_treetag, 'tag_results.txt'), header = FALSE, encoding = 'UTF-8', quote = "")
  names(tag_results) <- c('token', 'pos', 'lemma')
  file.remove(file.path(path_to_treetag, 'article_to_tag.txt'), file.path(path_to_treetag, 'tag_results.txt'))
  if(filter) {
    tag_results <- tag_results[pos %in% c('ADJ', 'ADV', 'NOM', 'NAM') | stringi::stri_detect_regex(str = pos, pattern = '^VER')]
  }
  tag_results <- cbind(id = id, tag_results)
  return(tag_results)
}
```

This function is used document wise without filtering out any words. An id is given to each document to track which document the words belongs to. 

### Stopwords removal

Words left in the dataset are now cleaned and tagged with their part-of-speech. Non-informative words need to be removed. As explained earlier 6 different methods are tested. They are describe in the subsequent sections.

#### Bag-of-words

Non-informative words are removed using a predefined list of stopwords. The list is created ad hoc, independantly of the context of the study. The list can be found [here](https://github.com/MathieuMarauri/LeMonde/blob/master/data/dtm/stopwords_fr.txt). Some words were added after some test on the data, mostly the most frequent words that carried no useful meaning. This method is pretty straighforward and only requires a predefined list of words. It is easy to use and can be applied evenly on all datasets. On the other hand finding such a list can be difficult in languages other than english and constructing one yourself is a pain. Besides, it does not depend on the corpus studied. 

This method leads to less than 1% of words removed. 

#### POS tags

The part-of-speech tags are used to remove some words. We consider that only adjectives, adverbs, nouns and proper nouns can carry information. More precisely we consider that other tags cannot carry any information and are therefore removed. 

This method leads to a reduction of 1% of the feature space. 

#### TF1

Term-frequency is the number of times a term appears in the corpus. We consider here words appearing only once in the corpus as being non-informative. Indeed as the objective is to find hidden topics in the corpus, words appearing only once will not help in finding groups of co-occuring words in documents. Such a word can carry a lot of information in itself but since it will not contribute in revealing a topic it is safe to remove it. The threshold of 1 is used because it is to justify and leads to a great reduction of the feature space. 

The choice of using TF1 can be visualy justify with an elbow like method. 

<img src="README_files/figure-html/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

TF1 clearly is ahead in terms of number of words. TF2 may also be used with the same reasons used for TF1. 
