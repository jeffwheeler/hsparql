module DBPedia where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

main = do (Just s) <- query "http://dbpedia.org/sparql" simple
          putStrLn . take 500 . show $ s
          -- putStrLn $ createQuery tricky -- or just print the query

simple :: Query [Variable]
simple = do
    resource <- prefix (iriRef "http://dbpedia.org/resource/")
    dbpprop  <- prefix (iriRef "http://dbpedia.org/property/")
    foaf     <- prefix (iriRef "http://xmlns.com/foaf/0.1/")

    x    <- var
    name <- var
    page <- var

    triple x (dbpprop .:. "genre") (resource .:. "Web_browser")

    triple x (foaf .:. "name") name
    triple x (foaf .:. "page") page

    return [name, page]

tricky :: Query [Variable]
tricky = do
    resource <- prefix (iriRef "http://dbpedia.org/resource/")
    dbpprop  <- prefix (iriRef "http://dbpedia.org/property/")
    foaf     <- prefix (iriRef "http://xmlns.com/foaf/0.1/")
    owl      <- prefix (iriRef "http://www.w3.org/2002/07/owl#")
    rdfs     <- prefix (iriRef "http://www.w3.org/2000/01/rdf-schema#")

    x     <- var
    name  <- var
    fbase <- var
    page  <- var

    -- Identify
    triple x (dbpprop .:. "genre") (resource .:. "Web_browser")

    -- Query
    triple x (foaf .:. "name") name
    optional $ do triple x (owl .:. "sameAs") fbase
                  filterExpr $ regex fbase "freebase"
    filterExpr $ notExpr $ bound fbase

    triple x (foaf .:. "page") page

    distinct

    orderNext name
    orderNextDesc fbase

    return [x, name, page, fbase]

frenchFilms :: Query [Variable]
frenchFilms = do
    skos <- prefix (iriRef "http://www.w3.org/2004/02/skos/core#")
    film <- var
    triple film (skos .:. "subject") (iriRef "http://dbpedia.org/resource/Category:French_films")
    return [film]

fps :: Query [Variable]
fps = do
    property  <- var
    hasValue  <- var
    isValueOf <- var

    union (triple (iriRef "http://dbpedia.org/resource/Category:First-person_shooters") property hasValue)
          (triple isValueOf property (iriRef "http://dbpedia.org/resource/Category:First-person_shooters"))

    return [isValueOf]

berliners :: Query [Variable]
berliners = do
    xsd  <- prefix (iriRef "http://www.w3.org/2001/XMLSchema#")
    prop <- prefix (iriRef "http://dbpedia.org/property/")
    dbo  <- prefix (iriRef "http://dbpedia.org/ontology/")
    foaf <- prefix (iriRef "http://xmlns.com/foaf/0.1/")
    resc <- prefix (iriRef "http://dbpedia.org/resource/")

    name     <- var
    birth    <- var
    death    <- var
    person   <- var
    knownfor <- var

    triple person (prop .:. "birthPlace") (resc .:. "Berlin")
    triple person (dbo  .:. "birthdate")  birth
    triple person (foaf .:. "name")       name
    triple person (dbo  .:. "deathdate")  death

    filterExpr $ birth .<. ("1900-01-01", (xsd .:. "date"))

    optional $ triple person (prop .:. "KnownFor") knownfor

    return [name, birth, death, person, knownfor]
