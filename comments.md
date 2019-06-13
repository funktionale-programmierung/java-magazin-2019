# Kommentare

## DONE ein anderes Bild

Ich hab pptx mal ein alternatives Bild skizziert. Leider sind meine pp
Skills nicht so gut, aber vielleicht kannst du trotzdem was erkennen.

## DONE Architektur

Den Begriff "Softwarearchitektur" kann man auf zweierlei Art lesen
1. als Herangehensweise zur systematischen Konstruktion von großen Programmsystemen oder
2. als Konstruktionsschema für bestimmte Anwendungen.

Unsere Bedeutung ist 1. Aber ist das nicht eher Entwurf/Design und die Architektur ist das Ergebnis?

## NOT DONE Leseransprache

Wir könnten den Haskell-geneigten Lesern als Übungsaufgabe den Entwurf neuer Strategien vorschlagen.

## DONE data deriving

Muss man das deriving im Artikel zeigen?

## Set API

Vielleicht in einem Float?

## unveränderlich vs immutable

Immutable ist durchaus ein Begriff, der den Jung-Entwicklern bekannt ist.

## DONE Notwendig?

Das `deck` wird von Haskell folgendermaßen ausgedruckt:

```
[Card {suit = Spades, rank = Numeric 2},
 Card {suit = Hearts, rank = Numeric 2}, 
 <50 weitere Karten>]
```

## replaced


Für die Abstraktionsfähigkeit in der funktionalen Sprache ist dies nur
ein winziges Beispiel.  Sie potenziert aber ihre Fähigkeit im
Zusammenhang mit den reinen Funktionen:

## NOT DONE Bemerkung zum Ende vom "Modellieren"

Manche Aspekte der Softwareentwicklung sind sprachübergreifend und
gelten sowohl für objektorientierte als auch für funktionale
Programmierung.  Ein Beispiel ist die Kapselung von
Implementierungsdetails, zum Beispiel durch die Verwendung von
abstrakten Datentypen. In funktionalen Sprachen ist die Gefahr eines
Abstraktionsbruchs vielleicht sogar noch größer aufgrund des
allgegenwärtigen Pattern-Matching. (Tritt bisher auch noch nicht im
Code auf.)

## Kartenspiel modellieren

Der Abschnitt ist zu lang und könnte unterteilt werden.

## in der conclusion?

(In der
funktionalen Programmierung stehen auch andere leistungsfähige
Patterns zur Verfügung wie fs2/conduit FIXME, deren Erläuterung aber mehr
Vorlauf erfordern würde, als hier Platz zur Verfügung steht.)

## DONE GameEvent

Anstelle von `HandsDealt (Map PlayerName Hand)` könnte man auch einen
Event `HandDealt PlayerName Hand` vorsehen, der jedem Spieler einzeln
seine `Hand` mitteilt. Das liefert einen Vorgeschmack von
Event-Routing.

DONE in dem Sinne, dass da eine Fußnote steht.

## DONE processGameCommand, processGameEvent

Ich fände es sinnvoll, die Argumente zu vertauschen, so dass jeweils
das `GameCommand` bzw das `GameEvent` der erste Parameter ist. Passt
dann auch genau zur State-Monade.

## NOT DONE processGameCommand, processGameEvent

Beim `processGameCommand` ist auch komisch, dass der
`processGameEvent` sofort ausgeführt wird, während der Rest der Welt
verzögert informiert wird. Logisch wäre es, wenn der
`processGameEvent` genauso am Event-Strom lauschen müsste wie alle
anderen Komponenten des Spiels. 
