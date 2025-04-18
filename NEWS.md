# c2z (development version)

#### Major

* `Cristin` can now use the `futureverse` package (with `progressr`) for 
parallel processing. This can greatly reduce runtime. 

#### Breaking

* Moved `CristinMonthly` and `CristinUnits`, including several internal 
functions (e.g., `CristinMail` and `CristinWeb` `Dict`) to `c2z4uni` to keep
maintenance simpler.

#### Minor

* Fixed a bug in `CristinSupported` where presentationType were always
set to English regardless of requested language.

* Added the internal function `ApaTitle` to helpe with producing titles
in Title Case when the input is all CAPITAL LETTERS.

* Added the options ot use `post.token`, Zotero-Write-Token (TRUE) or 
If-Unmodified-Since-Version (FALSE), in `ZoteroPost`

* Made `zotero.types` globally available.

* Slimmed down `ZoteroWrangler`, in preperation for adding the new Cris/NVA.
Added several functions to aid the process of enhancing metadata using 
`ZoteroEnhancer` and `ProcessData` for parallel processing. 

* Fixed a bug in `ZoteroIndex` where items use shortTitle and not title.

* Fixed some changes in `ZoteroGov` due to changes in the API of 
regjeringen.no

* Fixed a bug in internal function `CleanText` that failed to remove 
dots. 

* Fixed a bug in `ZoteroDoi` where a regex pattern caused error in some DOis.

* Fixed a bug in `ZoteroGet` where Zotero now enforces a limit of 25 items per
query for bibliography and citation data.

* Fixed a bug in `CristinWrangler` where an empty creators field caused an error
when deciding to use external data or Cristin data.

* Added internal function `CrossrefRetracted` to check whether CrossRef 
(Retraction Watch) has marked a publication/DOI as retracted in api.labs. If 
true [RETRACTED] will be added as a prefix to the title of the publication.
 
* Added internal functions `UnescapeHtml` and `HtmlCollapse` to improve fetching
abstracts from XML.

* Added internal function `SemanticScholar` to provide better abstracts for 
ZoteroDoi (if using `use.semantic` is set to TRUE). 

* Added a internal function `InsertUpdate` to update/insert zotero items by key.

* Fixed minor bug in `CristinWrangler` with empty external.creators  

* Fixed a bug in `ZoteroDelete` where the function failed to get the latest
version of the library.

* Exported the previously internal `ZoteroFormat`. It can now check the 
structure of a provided tibble/dataframe using the `check.structure` argument. 

* Fixed an error in `DoiCrossref` where sub.type was length > 1 and type 
(e.g., "preprint") was not succesfully extracted with rvest.

* Made `CristinWrangler` somewhat less cumbersome.

* Made a small adjustment to `ZoteroIsbn` where some edited books contained
both 100 and 700 fields leading to an error where first editor was also
listed as first author rather than only listing editors.

* Added the option to filter out non-nvi publication in `Cristin`, using the 
logical argument `nvi`, default FALSE.

* Improved speed of `ZoteroDelete`.

* Moved `Cls` out of `ZoteroExport` for more universal handling.

* Fixed a bug in `ZoteroExport` where `ZoteroGET` reported wrong number of 
results.

* Added a `full.update` argument (default: TRUE) to `CristinMonthly`, if set
to false the function will only create bibliography for new items.

* Fixed a bug in `ZoteroLibrary` where function did not return NULL when 
searching for non-existing collection keys.

* Fixed a bug in `ZoteroGET` where using item/collection keys would only result
in a maximum result of 100 items/collections.

* Creted an internal function `Dict` to handle language output, for the moment
English and Norwegian. 

* Minor adjustments to `CristinMonthly` and the internal functions 
`CristinMail` and `CristinWeb`.

* Removed limit from Zotero API when requsted format is versions or keys.

# c2z 0.2.0

#### Feature

* Added the function `Cristinunits` to create a tibble with information about
(nested) units in Cristin (e.g., A University -> Faculties -> Departments -> 
Groups). The tibble can than be used to extract data for each unit from Cristin.

* Added the function `CristinMonthly` to create a per month Zotero collections
for units defined in `Cristinunits`. It may use the internal functions 
`CristinMail` and `CristinWeb` to create a newsletter using HTML for email and 
web, respectively. 

#### Critical

* Fixed a bug in `ZoteroFormat` where `parentCollection` key, in some special 
cases, were identified as logical rather than character (Zotero uses a 
combination of logical and character in this field). The error caused an error
in combining some collections.

* Fixed a bug in `ZoteroWrangler` where book sections with empty creators in 
main book caused a NA error.

#### Major

* Made a major revision of `ZoteroLibray`, making it less recursive and bloated.

#### Moderate

* Made handling of API status codes consistent throughout the package. Removed 
the now somewhat redundant `debug` checks.

#### Minor

* Fixed a bug in `CristinWrangler` where successfully identified duplicates with
modified content in Cristin failed to update key, version and collections, thus
creating new copies rather than updating existing items.

* Removed hyphens from ISBN when importing from `Cristin` to simplify filtering.

* Fixed a bug in `CristinWrangler` where `remove.na` was set to TRUE rather
than forcing item-type to book (`part_of`) for book chapters.

* Fixed an issue in `ZoteroLibrary` where zero collections were reported when
finding a specific collection key. 

* Fixed an issue in `ZoteroFormat` where tags could be listed as either a 
data.frame or list

* Fixed an issue in `CristinWrangler` where `creatorType` of parent items are
listed as authors when they are (most probably) editors.

* Fixed an issue in `DoiCrossref` where first and last names sometimes are 
switched due to an alt-name field in the XML.

* Fixed some issues with badges in `README`.

* Made some adjustments to `ZoteroGet`. `bibliography` now contains, in 
addition to `bib`, `bib.body` and `bib.item` separating style form the 
reference.

* Created an internal function called `ZoteroId` to extract ids from extra 
field.

* Added some internal functions to handle dates: `Months` to display 
(abbreviated) month names in Norwegian and English, `ChangeDate` to add/subtract
date from date (e.g., days, weeks, months), `FloorDate` to set the first day of
the month, and `CeilingDate` so set the last day of the month.

* Added a flowchart, why not.

# c2z 0.1.4

* Resubmission to CRAN: Part Deux

#### Moderate

* Moved `CristinSupported` from `CristinWrangler` to `Cristin` to better manage
filtering of items, and to reduce the number of lines (666) in `CristinWrangler`
to a more manageable function (522).

#### Minor

* Moved option to include HTML bibliography from `ZoteroExport` to 
`ZoteroLibrary` to avoid any redundant API calls.

* Omitted the redundant "in R" at the end of title in `DESCRIPTION`.

* Made examples in exported functions executable. All functions, except `Zotero` 
and `CristinSupported`, are set within `\donttest`, as the functions, either 
directly or indirectly calls various API's, and may violate CRAN's 5 second limit.

# c2z 0.1.3

* Resubmission to CRAN 

#### Critical

* Fixed an error in `ZoteroDoi` that did not account for spaces within the DOI
itself (e.g., `10.3390/ foods12061167`)

#### Moderate

* Made `ZoteroGet` split up `itemKey` and `collectionKey` in lists of keys based
on the `limit` argument, as Zotero API does not limit using multiple keys in the 
same fashion as ordinary query (i.e., based on `start` and `limit` intervals).

* Made retrieving bibliography and citation fields from Zotero more elegant.

#### Minor

* Fixed broken URLs.

* Added single quotes to (software) names in the `Description` field in 
`DESCRIPTION`.

* Separated counting items (`n.items`) and attachments (`n.attachments`).  

* Fixed another million typos. 

# c2z 0.1.2

* Submission to CRAN

# c2z 0.1.1

#### Critical

* Added an internal function `FixCreators` to correct Zotero items where the
creators is listed by `lastName` but with an empty `firstName` rather than using 
`name` only. The error caused an crash in `ZoteroIndex`.

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
