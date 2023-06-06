# Seeing is Believing: The Presence and Impact of Ambient Sexism Toward Collegiate Women in STEM
 Code to run and analyze data for Seeing is Believing project

E.R. Silver<sup>1</sup>, Linnea C. Ng<sup>2</sup>, Abby Corrington<sup>3</sup>, Mikki Hebl<sup>1</sup>, & Janet Braam<sup>4</sup>

1. Department of Psychological Sciences, Rice University
2. Department of Psychology, Lawrence University
3. Department of Management, Providence College School of Business
4. Department of BioSciences, Rice University

## Study 1

The first study in this code repository analyzes a dataset of codes from images taken in the hallways of an elite STEM university in the US. The dataset (`data/buildings/cleaned_building_data_parsed.csv`) contains only resolved codes. There is also a codebook for determining STEM and non-STEM buildings, saved as `data/buildings/building_recode.csv`.

The R Script to reproduce our analysis is `analyze_building_data.R`.

## Study 2

The second study in this code repository scrapes STEM research news articles from the top five US universities as of 2019. The python (version 3.8) code requires the following packages: `BeautifulSoup`, `pandas`, `requests`, and `numpy`. Note that we have redacted full article texts and links to images embedded in articles to ensure compliance with each university's copyrights and terms of use. 

* `data_io.py` tells python where to save scraped data. 
* `utils.py` contains helper functions to scrape, format, and save article data.
* `Get article links.ipynb` scrapes relevant university news websites websites to collect and save lists of links to STEM news articles. Note that running this script may yield different results from those published or may throw errors if the website layout has changed since this code was developed. Link datasets are saved as `data/articles/UNIVERSITY_links.csv`. 
* `Get article data.ipynb` scrapes data from each link collected in the previous script. This includes article text data and any images embedded in a given article. To toggle off collection of images (as doing so requires a large amount of storage), set the global variable `SAVE_PICS` to `False` in the `utils.py` script. Note that running this script may yield different results from those published or may throw errors if the website layout has changed since this code was developed. It may also result in different articles being saved as we randomly selected 100 articles from the year 2019. To scrape article data from the same links as those published originally, set the `direct_rep` to `True`. 
  * This notebook yields long datasets for each article, such that each unique image embedded in an article is a new row (e.g., if an article has 4 images, the same `article_id` will appear on four separate rows). For clarity, the article information only included in the first occurrence of a given `article_id`. Datasets are stored as `data/articles/UNIVERSITY_data_2019_formatted.xlsx`. 
* `News articles cleaning.ipynb` processes the scraped datasets _after_ they have been manually coded by research assistants for specific text features. These datasets are saved as `data/articles/UNIVERSITY_data_2019_formatted_coded.xlsx`. This notebook collapses across duplicate `article_id` values to produce an analysis-ready dataset. The analysis-ready dataset is saved as `data/articles/article_data_cleaned.csv`.
* `analyze_articles.R` reproduces the statistics reported in our manuscript. 


## Study 3

The final study is an experimental study. To protect participants' privacy and confidentiality, we have made a redacted version of the full dataset available to other researchers upon request to Elisabeth Silver (email: elisabeth.silver[at]rice.edu). However, we have provided a cleaned version of the dataset to facilitate replication of our analyses. The only script that requires the redacted dataset is `clean_experimental.R`.

Note that many of the test statistic formatting operations are accomplished through a custom package that is still in development called `statstring`. The package can be installed via Github using the R package `devtools` as follows: `devtools::install_github("silverer/statstring")`. Alternatively, you can remove all instances of `format_anova_string` and calls to objects produced by this function in `SIB Formatted Outputs.Rmd`. 

* `clean_experimental.R` cleans a redacted dataset and prepares it for analysis. It excludes participants based on several criteria (see publication for details). It uses the redacted data files `data/experiment/experiment_de_id_data_1.csv` and `data/experiment/de_id_text_data.csv`. This script also produces a smaller clean dataset called `data/experiment/outcomes_experimental_data_clean_v1.csv`. This is the dataset required to run all subsequent analysis scripts.
* `unstd_printout_backend.R` uses `data/experiment/outcomes_experimental_data_clean_v1.csv` to generate outputs containing zero-order correlations, plots, ANOVAs, pairwise comparisons, and means by experimental condition and participant gender. 
* `Unstandardized SIB Formatted Outputs.Rmd` sources `unstd_printout_backend.R` and, when knit to a Word document, produces a file with formatted test statistics, mean differences, effect sizes, plots, etc. that can be easily copied and pasted into a manuscript file. 


