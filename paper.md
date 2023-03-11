---
title: 'Cristin to Zotero: a Reference Manager in R'
tags:
  - R
  - reference manager
  - Zotero
  - Cristin
  - DOI
  - ISBN
  - BibLaTeX
  - document generation
authors:
  - name: Øystein Olav Skaar
    orcid: 0000-0001-5605-3492
    affiliation: "1, 2"
    corresponding: true
affiliations:
 - name: Department of Educational Studies in Teacher Education, Inland University, Norway
   index: 1
 - name: Department of Education, University of Bergen, Norway
   index: 2
date: 11 March 2023
bibliography: paper.bib
---

# Summary

The observant reader has already identified the brilliant word play on Psalm
72:8 (King James Version): "He shall have dominion also from **sea to sea**, and
from the river unto the ends of the earth". *`c2z`* aims at obtaining total
dominion over Cristin ["Current Research Information 
SysTem in Norway", @cristin] and Zotero [@zotero]. The package 
enables manipulating Zotero libraries using *R* [@rteam]. 
Import, in batch, references from Cristin, regjeringen.no, CRAN, ISBN 
(currently, Alma and Library of Congress), and DOI (currently, CrossRef and 
DataCite) to a Zotero library. Add, edit, copy, or delete items, including 
attachments and collections, and export references to BibLaTeX [@biblatex] and 
other formats directly in *R*.

For further documentation see the *`c2z`* [website](https://oeysan.github.io/c2z/).

# Statement of need

Prior to *`c2z`* there exists no *R* package that can fully manipulate Zotero 
libraries. @refmanager has created an excellent package to, among multiple 
functionalities, import references from Zotero. However, *`RefManageR`* lacks the 
ability to write to the Zotero library. Two, seemingly abandoned, specialized 
packages [@zoterro; @zoteror] aim at more control of the Zotero 
library. Unfortunately, the packages are currently not working properly. 
Thus, *`c2z`*  is at present the only fully fledged *R* package to natively 
work with Zotero in *R*.

Furthermore, for Norwegian researchers, students, and others, *`c2z`* provides a
much-needed tool to access, and export in batch, the vast amount (*n* = 1,978,305,
at the time of writing) of references stored in Cristin, regardless of 
programming platform.

# Acknowledgements

Henrik Karlstrøm @rcristin for his work on *`rcristin`*. 

# References
