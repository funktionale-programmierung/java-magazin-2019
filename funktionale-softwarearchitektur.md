# Funktionale Softwarearchitektur

#### Michael Sperber, Active Group GmbH; Peter Thiemann, Universität Freiburg

## Abstract

Nachhaltige Softwarearchitektur ist eine Herausvorderung: Mit der
Größe nimmt in vielen klassich objektorientierten Softwareprojekten
auch die Komplexität zu.  Mit viel Disziplin und regelmäßigen
Refaktorisierungen lässt sich das Problem eine Weile in Schach halten,
aber die wechselseitigen Abhängigkeiten und komplexen Abläufe von
Zustandsveränderungen nehmen mit der Zeit trotzdem zu.
Die funktionale Softwarearchitektur geht an die Strukturierung großer
Systeme anders heran als objektorientierte Ansätze und vermeidet so
viele Quellen von Komplexität und Wechselwirkungen im System.

## Aspekte funktionaler Softwarearchitektur

Erst einmal zum Begriff: "Funktionale Softwarearchitektur" heißt
Softwarearchitektur mit den Mitteln der funktionalen Programmierung,
also nicht "Softwarearchitektur, die funktioniert".  (Obwohl das
natürlich auch der Fall ist.)  Funktionale Softwarearchitektur
zeichnet sich aus durch folgende Aspekte:

* Statt des Objekts mit gekapseltem Zustand ist die fundamentale
  Einheit funktionaler Architektur die *Funktion*, die auf
  *unveränderlichen Daten* arbeitet.

* Die mächtigen Typsysteme funktionaler Sprachen (ob statisch oder
  dynamisch) erlauben ein von den *Typen* aus getriebenes,
  systematisches Design von Datenmodellen und Funktionen.

* In funktionaler Architekur entstehen statt starrer hierarchischen
  Strukturen flexible sogenannte *Kombinatormodelle* und in die
  funktionale Programmiersprache *eingebettete domänenspezifische
  Sprachen*.

Das ist also ein ziemlich weites Feld (und die Liste ist noch nicht
einmal vollständig), und jedes dieser Aspekte füllt ganze Bücher.  Wir
konzentrieren uns in diesem Artikel darum auf den ersten Punkt, also
den Umgang mit Funktionen und unverändlichen Daten.  Dabei werden wir
auch die Rolle von Typen beleuchten.

## Funktionale Programmiersprachen

Funktionale Softwarearchitektur ist in (fast) jeder Programmiersprache
möglich, aber so richtig Freude macht das nur mit einer funktionalen
Sprache wie Haskell, OCaml, Clojure, Scala, Elixir, Erlang, F# oder
Swift.  Funktionale Softwarearchitektur wird außerdem in der Regel als
Code ausgedrückt, also nicht in Form von Diagrammen.  Entsprechend
benutzen wir für die Beispiele in diesem Artikel die funktionale
Sprache Haskell [^1], die besonders kurze und elegante Programme
ermöglicht.  Keine Sorge: Wir erläutern den Code, so dass er auch ohne
Vorkenntnisse in Haskell lesbar ist.  Wer aber selbst in Haskell
programmieren will, sollte sich zunächst noch eine Einführung in
funktionale Programmierung [^2] und ein Buch zu Haskell [^3] zu Gemüte
führen.

## Überblick

Wir erklären die funktionale Softwarearchitektur anhand des
Kartenspiel *Hearts* [^4], von dem wir nur die wichtigsten Teile
umsetzen:

Hearts wird mit vier Spielerinnen gespielt.  In jeder Runde eröffnet
eine Spielerin, in dem sie eine Karte ausspielt. (Am Anfang die
Spielerin muss das die Kreuz Zwei sein.)  Die nächste Spielerin muss
dann, wenn möglich, eine Karte mit der gleichen Farbe wie die
Eröffnungskarte ausspielen.  Haben alle Spielerinnen eine Karte
ausgespielt, muss die Spielerin den Stich einziehen, deren Karte die
gleiche Farbe wie die Eröffnungskarte hat sowie den höchsten Wert.
Ziel ist, mit den eingezogenen Karten einen möglichst geringen
Punktestand zu erreichen.

FIXME

Architekturüberblick, Events, Commands

## Programmieren mit unveränderlichen Daten

FIXME Einleitung

### Kartenspiel modellieren

Wir fangen mit der Modellierung der Karten an.  Die folgende
Definition eines Datentyps legt fest, dass ein Karte eine Farbe
("suit") und und Wert ("rank") hat:

``` haskell
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq, Ord)
```

Die Zeile `deriving (Show, Eq, Ord)` sorgt dafür, dass Haskell
automatisch Funktionen generiert, um einen Wert auszudrucken (`Show` -
analog zum Beispiel zu `toString` in Java), auf Gleichheit zu testen
(`Eq` - analog zu `equals`) und mit "größer als" und "kleiner als" zu
vergleichen (`Ord` - analog zu `compareTo`).

Für die Definition werden noch Definitionen von `Suit` und `Rank`
benötigt:

``` haskell
data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Show, Eq, Ord)
  
data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)
```

Hier handelt es sich um Aufzählungen - das `|` steht für "Oder",
entsprechend steht dort: Ein `Suit` ist `Diamonds` oder `Clubs` oder
`Spades` oder `Hearts`.  Bei `Rank` ist es ähnlich - zusätzlich hat
eine der Alternativen, `Numeric`, ein Feld vom Typ `Integer`, das den
Wert einer Zahlenspielkarte angibt.  Bei `Rank` sind die Alternativen
schon so angeordnet, dass der Vergleich aus `Ord` den Wert der
Spielkarten korrekt abbildet.
  
Hier ist die Definition der Kreuz Zwei auf Basis dieser
Datentypdefinition in Form einer Gleichung:

``` haskell
twoOfClubs = Card Clubs (Numeric 2)
```

Das Beispiel zeigt, dass `Card` als Konstruktorfunktion agiert.
Außerdem auffällig: In Haskell werden Funktionsaufrufe ohne Klammern
und Komma geschrieben, Klammern dienen nur zum Gruppieren.

FIXME: Selektoren erläutern

Für Hearts wird ein kompletter Satz Karten benötigt.  Der wird durch
folgende Definitionen generiert, die jeweils eine Liste aller Farben,
eine Liste aller Werte und schließlich daraus eine Liste aller Karten
(also aller Kombinationen aus Farben und Werten).

``` haskell
allSuits :: [Suit]
allSuits = [Spades, Hearts, Diamonds, Clubs]

allRanks :: [Rank]
allRanks = [Numeric i | i <- [2..10]] ++ [Jack, Queen, King, Ace]

deck :: [Card]
deck = [Card suit rank | rank <- allRanks, suit <- allSuits]
```

Jede Definition wird von einer *Typdeklaration* begleitet.  `allSuits
:: [Suit]` bedeutet, dass `allSuits` eine *Liste* (die eckigen
Klammern) von Farben ist.  Die Definitionen für `allRanks` benutzen
sogenannte Comprehension-Syntax, um effizient platzsparend Werte und
schließlich alle Karten aufzuzählen.  Das `deck` wird von Haskell
folgendermaßen ausgedruckt:

```
[Card {suit = Spades, rank = Numeric 2},
 Card {suit = Hearts, rank = Numeric 2}, 
 <50 weitere Karten>]
```

Für die Umsetzung von Hearts müssen die Karten repräsentiert werden,
die eine Spielerin auf der Hand hat - die ist als Menge von Karten
repräsentiert:

``` haskell
type Hand = Set Card
```

Wir nehmen an, dass das Standard-Modul `Set`, das hier benutzt wird,
explizit importiert wurde.  Dort steht `type` und nicht `data`, weil
kein neuer Typ definiert wurde sondern nur ein Typsynonym.

Einige Hilfsdefinitionen erleichtern den Umgang mit dem Typ `Hand`.
Zunächst die Funktion `isHandEmpty` - der Typ `Hand -> Bool` bedeutet
"Funktion, die eine Hand als Eingabe nimmt und ein `Bool` als Ausgabe
liefert:

``` haskell
isHandEmpty :: Hand -> Bool
isHandEmpty hand = Set.null hand
```

Die nächste Funktion `containsCard` ist zweistellig und prüft mit
Hilfe der Library-Funktion `Set.member`, ob eine
gegebene Karte zu einer Hand gehört:

``` haskell
containsCard :: Card -> Hand -> Bool
containsCard card hand = Set.member card hand
```

Die merkwürdige Typsystem `Card -> Hand -> Bool` wird erst deutlich,
wenn sie korrekt geklammert wird, nämlich von rechts: `Card -> (Hand
-> Bool)`  Das bedeutet, dass die Funktion zunächst eine Karte
akzeptiert und dann *eine Funktion liefert*, die ihrerseits eine Hand
akzeptiert und dann einen booleschen Wert zurückliefert.  Streng
genommen kennt Haskell also nur einstellige Funktionen und "simuliert"
höherstellige Funktionen durch diese Technik.

Die nächste Funktion schließlich zeigt endlich beispielhaft, wie der
Umgang mit unveränderlichen Daten funktioniert - `removeCard` entfernt
eine Karte aus einer Hand:

``` haskell
removeCard :: Card -> Hand -> Hand
removeCard card hand = Set.delete card hand
```

In typischem Java hätte diese Methode eine Signatur wie `void
removeCard(Card card)` und würde den Zustand des `Hand`-Objekts
verändern.  Nicht so in der funktionalen Programmierung, wo
`removeCard` eine neue Hand liefert.  Nach:

``` haskell
hand2 = removeCard card hand1
```



FIXME: Typsignatur


### Spiel-Logik

## Zustand modellieren

### FIXME: github-Repo

### Michael Sperber

FIXME

### Peter Thiemann

FIXME


## Literatur

[^1]: haskell.org

[^2]: Schreibe Dein Programm!

[^3]: Graham Hutton: 

[^4]: Hearts auf Wikipedia
