# c2z 0.1.1

#### Critical

* Added an internal function `FixCreators` to correct Zotero items where the
creators is list by lastName but with an empty firstName rather than using name
only. The error caused an crash in `ZoteroIndex`.

* Fixed `Zotero`not being able to use an empty API key to access public 
libraries

#### Minor

* Added examples and a vignette prior to CRAN submission.

* Made it optional to include items from `ZoteroLibrary`.

* Added an internal function `ErrorCode` to provide information on JSON response
codes.

* Made some adjustments to `ZoteroGet` with an option to define results name 
(e.g., as collections or items). 

* Made some adjustments to the noise levels of the `silent` argument, being 
somewhat inconsistent between the different functions. Still not happy with
the amount of feedback for recursive operations.

* Fixed about a million typos. 

# c2z 0.1.0
* Initial launch with the following features:
    + Add, edit, copy, and delete (nested) Zotero collections.
    + Add, edit, copy, and delete Zotero items, including attachments.
    + Export Zotero items in R as BibLaTeX (and other formats).
    + (Batch) import common references from Cristin.
        - Currently supported formats: books (e.g., monographs, anthologies), 
        book chapters, journal articles, presentations (e.g., lectures), and 
        opinions pieces. 
    + (Batch) import references from ISBN and DOI.
        - Currently supported formats: books, book chapters, conference papers, 
        journal articles 
    + (Batch) import Norwegian white papers (i.e., Meld. St., St.meld.) and 
    official Norwegian reports (i.e., NOU).
    + Batch import R packages from CRAN.
    + Search CrossRef, automatically and manually, by author(s), title, and year.
    + Augment Cristin references through ISBN, DOI, or CrossRef search.
* Added https://oeysan.github.io/c2z/ for documentation 
* Added a `NEWS.md` file to track changes to the package.
* Added a `TODO.md` file to track future work on the package.
* Added a `README.md` file as an introduction to the package.
* Added a `CONDUCT.md` because people should be nice.
* Added a `badge.webp` just because
