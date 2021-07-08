# Seeing is Believing: The Presence and Impact of Ambient Sexism Toward Collegiate Women in STEM
 Code to run and analyze data for Seeing is Believing project

E.R. Silver<sup>1</sup>, Linnea C. Ng<sup>1</sup>, Abby Corrington<sup>2</sup>, Mikki Hebl<sup>1</sup>, & Janet Braam<sup>3</sup>

1. Department of Psychological Sciences, Rice University
2. Department of Management, Providence College School of Business
3. Department of BioSciences, Rice University

## Study 1

The first study in this code repository analyzes a dataset of codes from images taken in the hallways of an elite STEM university in the US. The dataset (`data/buildings/cleaned_building_data_parsed.csv`) contains only resolved codes. There is also a codebook for determining STEM and non-STEM buildings, saved as `data/buildings/building_recode.csv`.

The R Script to reproduce our analysis is `analyze_building_data.R`.

## Study 2

The second study in this code repository scrapes STEM research news articles from the top five US universities as of 2019. The python (version 3.8) code requires the following packages: `BeautifulSoup`, `pandas`, `requests`, and `numpy`.

* `data_io.py` tells python where to save scraped data. 
* `utils.py` contains helper functions to scrape, format, and save article data.
* `Get article links.ipynb` scrapes relevant university news websites websites to collect and save lists of links to STEM news articles. Note that running this script may yield different results from those published or may throw errors if the website layout has changed since this code was developed. Link datasets are saved as `data/articles/UNIVERSITY_links.csv`. 
* `Get article data.ipynb` scrapes data from each link collected in the previous script. This includes article text data and any images embedded in a given article. To toggle off collection of images (as doing so requires a large amount of storage), set the global variable `SAVE_PICS` to `False` in the `utils.py` script. Note that running this script may yield different results from those published or may throw errors if the website layout has changed since this code was developed. It may also result in different articles being saved as we randomly selected 100 articles from the year 2019. To scrape article data from the same links as those published originally, set the `direct_rep` to `True`. 
  * This notebook yields long datasets for each article, such that each unique image embedded in an article is a new row (e.g., if an article has 4 images, the same `article_id` will appear on four separate rows). For clarity, the article text data is only included in the first occurrence of a given `article_id`. Datasets are stored as `data/articles/UNIVERSITY_data_2019_formatted.xlsx`. 
* `News articles cleaning.ipynb` processes the scraped datasets _after_ they have been manually coded by research assistants for specific text features. These datasets are saved as `data/articles/UNIVERSITY_data_2019_formatted_coded.xlsx`. This notebook collapses across duplicate `article_id` values to produce an analysis-ready dataset. The analysis-ready dataset is saved as `data/articles/article_data_cleaned.csv`.
* `analyze_articles.R` reproduces the statistics reported in our manuscript. 


## Study 3

The final study is an experimental study. To protect participants' privacy, we have removed all variables that may directly or indirectly reveal participants' identities. This includes most free response items (other than anagram responses), specific dates and times, specific college majors, detailed race and ethnicity data, etc. Because many of the scripts source other scripts in this analysis, the only scripts that must be run are `save_anova_outputs.R` and `SIB Formatted Outputs.Rmd`.

* `clean_experimental.R` cleans the redacted dataset and prepares it for analysis. It excludes participants based on several criteria (see publication for details). It uses the redacted data file `data/experiment/experiment_de_id_data.csv`. This script does not need to be run, as it is sourced by `zscore_descriptive_stats.R` automatically. 
* `zscore_descriptive_stats.R` uses the clean dataset produced by the cleaning script to generate outputs containing zero-order correlations, scale reliability, and means by experimental condition and participant gender. 
* `save_anova_outputs.R` sources `zscore_descriptive_stats.R` and uses the dataset produced by the script to run and save the results of 3-way ANOVAs for the primary outcomes. This script also runs and produces results of 4-way ANOVAs to include in supplementary materials. The results of these tests are saved in the `output/` folder. This script creates a smaller dataset (`data/experiment/outcomes_experimental_data_clean.csv`) that can be piped into subsequent scripts for easy formatting and copying and pasting into a manuscript file. 
* `printout_backend.R` reads in the `data/experiment/outcomes_experimental_data_clean.csv` data file and runs 3-way ANOVAs, prepares plots, calculates FDR-adjusted planned pairwise comparisons, and produces formatted test statistics.
* `SIB Formatted Outputs.Rmd` sources `printout_backend.R` and, when knit to a Word document, produces a file with formatted test statistics, mean differences, effect sizes, plots, etc. that can be easily copied and pasted into a manuscript file. 


