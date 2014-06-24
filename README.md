# interpret-sheet: a SWI-prolog library for interpreting ODF spreadsheets
interpret sheet is a SWI-Prolog  library  for analyzing  and interpreting 
ODF  spreadsheets. It uses plsheet library to convert the spreadsheet in 
to a Prolog fact base which includes cell contents, cell types, and cell location.
interpret-sheet matches cell text labels with concepts and corresponding ancestors
from a domain vocabulary. It uses the block segmentation from plsheet as a context 
to select the most suitable concepts and ancestors.

The file sheet-concepts.pl contains sample toplevel  code   that  can be used as a
starting point to use the library.


## Processing MicroSoft Excel files
This library only processes ODF (Open Document Format) files. ODS is the
ODF sub-format for spreadsheets. 


## Home
PlSheet is hosted on GitHub at https://github.com/Data2Semantics/interpret-sheet


## Dependencies
interpret-sheet uses SWI-prolog libraries:  
plsheet
available on github https://github.com/Data2Semantics/plsheet

semweb/sparql_client
semweb/rdf_db
semweb/rdf_litindex
aggregate
all available from http://www.swi-prolog.org

interpret-sheet uses domain vocabulary AGROVOC, which is available
from ftp://ftp.fao.org/gi/gil/gilws/aims/kos/agrovoc_formats/current/agrovoc.skos.xml.en.zip


## Acknowledgements
This library was developed in the context  of COMMIT/, and in particular
the Data2Semantics project thereof.


[commit.png;height="80pt"](http://www.commit-nl.nl/)
[data2semantics.png;height="80pt",align="right"](http://www.data2semantics.org/)






















