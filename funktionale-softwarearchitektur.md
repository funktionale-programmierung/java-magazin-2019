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

Der Code zu diesem Artikel ist in einem Github-Repositorium abrufbar:

[`https://github.com/funktionale-programmierung/hearts/`](https://github.com/funktionale-programmierung/hearts/)

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

![Abbildung 1: Modellierung des Spielablaufs](gameplay.pdf)

Wir implementieren das Kartenspiel auf der Basis von *domain events*.
Abbildung 1 zeigt den Ablauf: Jede Spielerin nimmt Events entgegen,
die den bisherigen Spielverlauf repräsentieren, und generiert dafür
Commands, die Spielzüge repräsentieren.  Die "Gameplay"-Komponente
nimmt die Commands entgegen, überprüft sie auf Korrektheit (War die
Spielerin überhauptd ran? War der Spielzug regelkonform?) und
generiert ihrerseits daraus wieder Events.

Diese Architektur ist ein klassisches taktisches Pattern aus dem
*Domain-Driven Design*[^5].  Während das Pattern das gleiche ist wie
in "objektorientiertem DDD", unterscheidet sich die Umsetzung gerade
in der Verwendung von unverändlichen Daten und Funktionen. (In der
funktionalen Programmierung stehen auch andere leistungsfähige
Patterns zur Verfügung wie fs2/conduit FIXME, deren Erläuterung aber mehr
Vorlauf erfordern würde, als hier Platz zur Verfügung steht.)

Wir stellen die Kommunikation zwischen den einzelnen Komponenten der
Architektur direkt mit Funktionsaufrufen her, aber andere
Mechanismen - nebenläufige Prozesse oder Mikroservices - sind
natürlich auch möglich.

## Programmieren mit unveränderlichen Daten

Eine Vorbemerkung: "Unverändlicher Daten" bedeutet, dass Objekte nicht
verändert werden - es gibt keine Zuweisungen, die Attribute von
Objekten verändern können.  Wenn Veränderung modelliert werden soll,
so generiert ein funktionales Programm in der Regel neue Objekte.  Das
mag erstmal als reine Einschränkung erscheinen, bietet aber enorme
Vorteile:

* Es gibt niemals Probleme mit interferierenden Veränderungen des
  Zustands durch die Aufrufe von Methoden oder nebenläufige Prozesse.
  
* Es gibt keine inkosistenten, "Zwischenzustände" dadurch, dass das
  Programm erst das eine Feld, dann das nächste etc. setzt
  
* Das Programm kann problemlos durch ein Gedächtnis erweitert werden,
  das zum Beispiel zum vorigen Spielständen zurückkehrt, wenn eine
  Spielerin ihren Zug zurücknimmt.

## Kartenspiel modellieren

Jetzt geht es aber mit der konkreten Modellierung los.  Wir fangen den
Spielkarten an.  Die folgende
Definition eines Datentyps legt fest, dass ein Karte eine Farbe
("suit") und und Wert ("rank") hat:

```haskell
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

```haskell
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

```haskell
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

```haskell
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

```haskell
type Hand = Set Card
```

Wir nehmen an, dass das Standard-Modul `Set`, das hier benutzt wird,
explizit importiert wurde.  Dort steht `type` und nicht `data`, weil
kein neuer Typ definiert wurde sondern nur ein Typsynonym.

Einige Hilfsdefinitionen erleichtern den Umgang mit dem Typ `Hand`.
Zunächst die Funktion `isHandEmpty` - der Typ `Hand -> Bool` bedeutet
"Funktion, die eine Hand als Eingabe nimmt und ein `Bool` als Ausgabe
liefert:

```haskell
isHandEmpty :: Hand -> Bool
isHandEmpty hand = Set.null hand
```

Die nächste Funktion `containsCard` ist zweistellig und prüft mit
Hilfe der Library-Funktion `Set.member`, ob eine
gegebene Karte zu einer Hand gehört:

```haskell
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

Die nächste Funktion schließlich zeigt beispielhaft, wie der Umgang
mit unveränderlichen Daten funktioniert - `removeCard` entfernt eine
Karte aus einer Hand:

```haskell
removeCard :: Card -> Hand -> Hand
removeCard card hand = Set.delete card hand
```

In typischem Java hätte diese Methode eine Signatur wie `void
removeCard(Card card)` und würde den Zustand des `Hand`-Objekts
verändern.  Nicht so in der funktionalen Programmierung, wo
`removeCard` eine neue Hand liefert und die alte Hand unverändert
lässt.  Nach:

```haskell
hand2 = removeCard card hand1
```

ist `hand1` immer noch die "alte" Hand und `hand2` die neue.

Das ist in Haskell nicht nur eine Konvention: Eine Funktion *kann* gar
nicht einfach so Objekte "verändern", es handelt es sich im Sprech der
funktionalen Programmierung immer um eine "reine" oder "pure"
Funktion.  Diese rein funktionale Programmierung macht die Typsignatur
enorm nützlich, weil sie wirklich alles aufführt, was in die Funktion
hineingeht und wieder hinausgeht: Es gibt keine versteckten
Abhängigkeiten zu globalem Zustand und alle Ausgaben stehen hinter dem
rechten Pfeil der Signatur.

Da Hearts ein sogenanntes "Stichspiel" ist, hat der Code einen Typ für
den Stich, auf englisch "Trick".  Dieser muss mitführen, wer welche
Karte ausgespielt hat, um nach einer Runde zu entscheiden, wer den
Stich einziehen muss:

```haskell
type PlayerName = String

type Trick = [(PlayerName, Card)]
```

Die Typdefinition von `Trick` besagt, dass ein Stich eine Liste (die
eckigen Klammern) von Zwei-Tupeln (die runden Klammern innendrin) ist.
Listen werden in funktionalen Sprachen "von hinten nach vorn"
aufgebaut, die zuletzt ausgespielte Karte ist also vorn.

Wenn der Stich eingezogen wird, zählen nur noch die Karten, nicht
mehr, von wem sie stammen.  Dafür ist folgende Funktion nützlich:

```haskell
cardsOfTrick :: Trick -> [Card]
cardsOfTrick trick = map snd trick
```

Was die Funktion macht, ist wieder aus der Typsignatur ersichtlich.
Interessant ist hier aber auch die Implementierung, weil sie die
eingebaute *Higher-Order-Funktion* `map` bemüht, deren Typsignatur so
aussieht:

```haskell
map :: [a] -> (a -> b) -> [b]
```

Das `a` und das `b` sind *Typvariablen*, in Java-Sprech Generics.
Ausgesprochen steht dort: `map` akzeptiert eine Liste von `a`s sowie
eine Funktion, die aus einem `a` ein `b` macht und liefert eine Liste
von `b`s.  Bei `cardsOfTrick` ist `a` das Zwei-Tupel `(PlayerName,
Card)` und `b` ist `Card`.  Die Funktion `snd` extrahiert aus dem
Zwei-Tupel die zweite Komponente und deshalb folgenden Typ:

```haskell
snd :: (a, b) -> b
```

Solche generischen Funktionen gibt es (inzwischen) auch in Java, aber
in funktionalen Sprachen kommen sie viel häufiger zur Anwendung.

Funktionen wie `map` sind ein wichtiger Aspekt funktionaler
Architektur: Diese macht nicht an der konkreten Modellierung von
fachlichem Wissen halt, sondern erlaubt den Entwicklerinnen,
Abstraktionen zu bilden, die das fachliche Wissen verallgemeinern.

Für die Abstraktionsfähigkeit in der funktionalen Sprache ist dies nur
ein winziges Beispiel.  Sie potenziert aber ihre Fähigkeit im
Zusammenhang mit den reinen Funktionen: Weil Funktionen nie etwas
"verstecktes" machen, können sie bedenkenlos zu immer größeren
Gebilden zusammengestöpselt werden.  In OO-Sprachen wächst mit der
Größe das Risiko, dass versteckte Effekte unerwünschte
Wechselwirkungen haben.  Deshalb sind Disziplin und eigene
architektonische Patterns notwendig, um die resultierende Komplexität
in den Griff zu bekommen.  In der funktionalen Programmierung ist das
nicht so, und entsprechend ist das "Programmieren im Großen" dem
"Programmieren im Kleinen" ziemlich ähnlich.

## Spiel-Logik

Als nächstes ist der architektonische Mittelbau der Architektur an der
Reihe, die Spiellogik.  Ein Event-Storming produziert folgende
Event-Klassen:

* Karten wurden ausgeteilt.
* Eine neue Spielerin ist an der Reihe.
* Eine Spielerin hat eine bestimmte Karte aufgenommen.
* Eine Spielerin hat den Stich aufgenommen.
* Eine Spielerin hat versucht, eine unzulässige Karte auszuspielen.
* Das Spiel ist vorbei.

Diese Klassen lassen sich direkt in eine Typdefinition übersetzen:

```haskell
data GameEvent =
    HandsDealt (Map PlayerName Hand)
  | PlayerTurn PlayerName
  | CardPlayed PlayerName Card
  | TrickTaken PlayerName Trick
  | IllegalMove PlayerName
  | GameOver
  deriving Show
```

Der senkrechte Strich `|` zwischen den Klassen bedeutet "oder".  Das
`HandsDealt`-Event trägt eine "Map" zwischen Spielernamen und ihren
Karten mit sich.  Ein Verlauf des Spiels kann immer aus dessen Folge
von Events rekonstruiert werden.

Es gibt nur zwei Klassen von Commands:

```haskell
data GameCommand =
    DealHands (Map PlayerName Hand)
  | PlayCard PlayerName Card
  deriving Show
```

Die erste Klasse ist das direkte Pendant zu `HandsDealt`; sie 
setzt das Spiel zu Beginn in Gang.  Die zweite repräsentiert den
Versuch einer Spielerin, eine bestimmte Karte auszuspielen.

Die Spielregeln werden durch die Verarbeitung von Commands zu Events
implementiert.  Die Regeln beziehen sich ständig auf den *Zustand* des
Spiels: welche Karten zulässig ausgespielt werden können, welche
Spielerin als nächstes dran ist etc.  Von diesem Zustand wird
folgendes verlangt:

* Wer sind die Spieler und in welcher Reihenfolge spielen sie?
* Was hat jede Spielerin auf der Hand?
* Welche Karten hat jede Spielerin eingezogen?
* Was liegt auf dem Stich?

Das alles wird durch eine weitere Record-Definition repräsentiert:

```haskell
data GameState =
  GameState 
  { gameStatePlayers :: [PlayerName],
    gameStateHands   :: PlayerHands,
    gameStateStacks  :: PlayerStacks,
    gameStateTrick   :: Trick
  }
  deriving Show
```

Die Liste im Feld `gameStatePlayers` wird dabei immer so umsortiert,
dass die mächste Spielerin vorn steht.  Für die beiden Felder
`gameStateHands` und `gameStateStacks` müssen jeweils Karten *pro
Spieler* vorgehalten werden, darum sind die dazugehörigen Typen
Syonyme für Maps:

```haskell
type PlayerStacks = Map PlayerName (Set Card)
type PlayerHands  = Map PlayerName Hand
```

Auch bei der Umsetzung der Spielregeln macht sich die funktionale
Architektur bemerkbar:  Während des Spiels wird der Zustand nicht
verändert, sondern es wird für jede Änderung ein neuer Zustand
erzeugt.  Die zentrale Funktion für die Verarbeitung eines Commands
hat deswegen folgende Signatur:

```haskell
processGameCommand :: GameState -> GameCommand -> (GameState, [GameEvent])
```

Mit anderen Worten (Repräsentation des) Zustand vorher rein, Command
rein, Tupel aus (Repräsentation des) neuem Zustand und Liste
resultierender Events raus.  Hier ist die Implementierung der
Gleichung für das `DealHands`-Command:
  
```haskell
processGameCommand state (DealHands hands) =
  let event = HandsDealt hands
  in (processGameEvent state event, [event])
```

Da dieser Befehl von der "Spielleitung" kommt, führt er immer zu einem
`HandsDealt`-Event.  Der Effekt des Events auf den Zustand wird durch
die Funktion `processGameEvent` berechnet, deren Definition aus
Platzgründen fehlt, deren Arbeitsweise sich wieder gut an der
Typsignatur ablesen lässt:

```haskell
processGameEvent :: GameState -> GameEvent -> GameState
```

Die Kernlogik ist in der Gleichung für den `PlayCard`-Command. Diese
verlässt sich auf Hilfsfunktionen `playValid` (die einen Spielzug auf
Korrektheit überprüft), `whoTakesTrick` (die berechnet, wer den Stich
einziehen muss) und `gameOver` (die feststellt, ob das Spiel vorbei
ist).  Der Code führt eine Reihe von Fallunterscheidungen in Form von
`if ... then ... else` durch und bindet lokale Variablen (insbesondere
Events `event1`, `event2` etc. und Zwischenzustände `state1`, `state2`
mit `let`:

```haskell
processGameCommand state (PlayCard player card) =
  if playValid state player card
  then
    let event1 = CardPlayed player card
        state1 = processGameEvent state event1
    in  if turnOver state1 then
          let trick = gameStateTrick state1
              trickTaker = whoTakesTrick trick
              event2 = TrickTaken trickTaker trick
              state2 = processGameEvent state event2
              event3 = if gameOver state
                       then GameOver
                       else PlayerTurn trickTaker
              state3 = processGameEvent state event3
          in (state3, [event1, event2, event3])
        else
          let event2 = PlayerTurn (nextPlayer state1)
              state2 = processGameEvent state event2
          in (state2, [event1, event2])
  else
    (state, [IllegalMove player, PlayerTurn player])
```

Deutlich zu sehen ist, dass niemals der Zustand verändert wird und
stattdessen die Zwischenzustände alles separate, voneinander
unabhängige Objekte sind.

## Zustand verwalten


## Michael Sperber

[`michael.sperber@active-group.de`](mailto:michael.sperber@active-group.de)

Michael Sperber ist Geschäftsführer der Active Group GmbH.  Er wendet
seit über 25 Jahren funktionale Programmierung in Forschung, Lehre und
industrieller Entwicklung an.  Er ist Mitbegründer des Blogs
[`funktionale-programmierung.de`](https://funktionale-programmierung.de/)
und Mitorganisator der jährlichen Entwicklerkonferenz
[`BOB`](https://bobkonf.de/).

## Peter Thiemann

[`thiemann@informatik.uni-freiburg.de`](mailto:thiemann@informatik.uni-freiburg.de)

Peter Thiemann ist Professor für Informatik an der
Universität Freiburg und leitet dort den Arbeitsbereich
Programmiersprachen.  Er ist einer der führenden
Experten zur funktionalen Programmierung, der partiellen
Auswertung, domänenspezifischen Sprachen und zahlreichen
anderen Themen der Softwaretechnik.
Seine aktuelle Forschung beschäftigt sich mit statischen und
dynamischen Analysemethoden für JavaScript.

## Literatur

[^1]: haskell.org

[^2]: Schreibe Dein Programm!

[^3]: Graham Hutton: 

[^4]: Hearts auf Wikipedia

[^5]: Vaughn, Vernon: Domain-Driven Design Distilled, Pearson, 2016.
