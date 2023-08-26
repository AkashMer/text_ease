
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **Text-Ease**

<!-- badges: start -->
<!-- badges: end -->

**Text-Ease**, an app which predicts the next word in a sentence and
provide the user with 3 possible options to choose from.

### **Link to the app** - [Text-Ease](http://akashmer.shinyapps.io/text_ease)

### **Link to the Slide-Deck** - [Text-Ease Slide Deck](https://rpubs.com/akashmer/text_ease_slide_deck)

Data was made available as part of JHU Data Science Capstone Course in
collaboration with Microsoft *Swiftkey* and can be downloaded from
[here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

# **References<sup>[1](#ref-Data_Source)–[6](#ref-jurafsky2000speech)</sup>**

<div id="refs" class="references csl-bib-body">

<div id="ref-Data_Source" class="csl-entry">

<span class="csl-left-margin">1.
</span><span class="csl-right-inline">Leek J, Peng RD, Caffo B, Cross S,
SwiftKey. Data science capstone course.[
https://www.coursera.org/learn/data-science-project/supplement/4phKX/about-the-corpora
](
https://www.coursera.org/learn/data-science-project/supplement/4phKX/about-the-corpora
)</span>

</div>

<div id="ref-Silge_2017" class="csl-entry">

<span class="csl-left-margin">2.
</span><span class="csl-right-inline">Silge J, Robinson D. *Text Mining
with r*. O’Reilly Media; 2017.</span>

</div>

<div id="ref-Hvitfeldt_2021" class="csl-entry">

<span class="csl-left-margin">3.
</span><span class="csl-right-inline">Hvitfeldt E, Silge J.
Tokenization. In: *Supervised Machine Learning for Text Analysis in r*.
Chapman; Hall/CRC; 2021:9-36.
doi:[10.1201/9781003093459-3](https://doi.org/10.1201/9781003093459-3)</span>

</div>

<div id="ref-Manning_2008" class="csl-entry">

<span class="csl-left-margin">4.
</span><span class="csl-right-inline">Manning CD, Raghavan P, Schütze H.
*Introduction to Information Retrieval*. Cambridge University Press;
2008.
doi:[10.1017/cbo9780511809071](https://doi.org/10.1017/cbo9780511809071)</span>

</div>

<div id="ref-Chen_1999" class="csl-entry">

<span class="csl-left-margin">5.
</span><span class="csl-right-inline">Chen SF, Goodman J. An empirical
study of smoothing techniques for language modeling. *Computer Speech &
Language*. 1999;13(4):359-393.
doi:[10.1006/csla.1999.0128](https://doi.org/10.1006/csla.1999.0128)</span>

</div>

<div id="ref-jurafsky2000speech" class="csl-entry">

<span class="csl-left-margin">6.
</span><span class="csl-right-inline">Jurafsky D, Martin JH. *Speech &
Language Processing*. Pearson Education India; 2000.</span>

</div>

</div>

## **Directories and Files in the repository**

- **R** : A sub-directory to store all the raw R scripts and R markdown
  files used to create the report
  - **download.R** : File to define function fro downloading the data
  - **eda.R** : File meant for the author, contains all the rough
    exploratory data analysis done
  - **kgrams_model.R** : Very slow model built using `kgrams` R package
  - **milestone_report.Rmd** : Raw Rmarkdown file for the Milestone
    report describing data cleaning steps and exploratory data analysis
  - **mkn_model.R** : **Code which was used behind the final model used
    in the app**
  - **quiz1.R** : Code used to answer 1st week quiz questions
  - **tidyData.R** : Preliminary data cleaning steps
- **data** : A sub-directory to store the raw data used for the project.
- **results** : A sub-directory to store the Milestone Report as an HTML
  file. A markdown version of the file and a figures sub-directory
  containing all the figures used in the report in PNG format can also
  be found here. HTML version of the Slide-Deck is also stored here
