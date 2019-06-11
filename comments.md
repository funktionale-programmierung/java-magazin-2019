# Kommentare

## Architektur

Den Begriff "Softwarearchitektur" kann man auf zweierlei Art lesen
1. als Herangehensweise zur systematischen Konstruktion von großen Programmsystemen oder
2. als Konstruktionsschema für bestimmte Anwendungen.

Unsere Bedeutung ist 1. Aber ist das nicht eher Entwurf/Design und die Architektur ist das Ergebnis?

## Leseransprache

Wir könnten den Haskell-geneigten Lesern als Übungsaufgabe den Entwurf neuer Strategien vorschlagen.

## data deriving

Muss man das deriving im Artikel zeigen?

## Set API

Vielleicht in einem Float?

## unveränderlich vs immutable

Immutable ist durchaus ein Begriff, der den Jung-Entwicklern bekannt ist.

## Notwenig?

Das `deck` wird von Haskell folgendermaßen ausgedruckt:

```
[Card {suit = Spades, rank = Numeric 2},
 Card {suit = Hearts, rank = Numeric 2}, 
 <50 weitere Karten>]
```


## Kartenspiel modellieren

Der Abschnitt ist zu lang und könnte unterteilt werden.

## in der conclusion?

(In der
funktionalen Programmierung stehen auch andere leistungsfähige
Patterns zur Verfügung wie fs2/conduit FIXME, deren Erläuterung aber mehr
Vorlauf erfordern würde, als hier Platz zur Verfügung steht.)
