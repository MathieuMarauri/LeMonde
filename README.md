# LeMonde topic modeling

--- 

## Introduction

This project's objective is to apply a topic modeling method on a corpus of documents. The goal is to test the Latent Dirichlet Method (LDA) and to see how it performs on a corpus pre-processed with different stopwords removal methods. The corpus is built using news articles from [lemonde](http://www.lemonde.fr/), a french newspaper. The goal is to analyze the results of the LDA model with regard to the website categories in which the articles were. The LDA model needs an apriori, user defined, number of topics as input. Knowing the category in which the article was on the website is a start point to decide the number of topics to ask to the model and also an indicator of performance of the model: are the articles most associated with the topic corresponding to their category on the website. The entire process -scrapping, pre-processing and modelization- is done in R. 

--- 

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

--- 
