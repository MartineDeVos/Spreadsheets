# Agrovoc

From ftp://ftp.fao.org/gi/gil/gilws/aims/kos/agrovoc_formats/current/agrovoc.skos.xml.en.zip

# Installation

  - Install SWI-Prolog 7.1.10 or later
  - Install ODF sheet library (previously known as plsheet):

    ==
    % swipl
    ?- pack_install(odf_sheet).
    ==

  - See usage by running (assumes swipl 7.1.10 is in $PATH)

    ==
    ./sheet-concepts.pl --help
    ==
