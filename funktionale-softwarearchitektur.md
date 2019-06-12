# Funktionale Softwarearchitektur

#### Michael Sperber, Active Group GmbH; Peter Thiemann, Universität Freiburg

## Abstract

Der Entwurf von nachhaltigen Softwarearchitekturen ist eine Herausvorderung: Mit der
Größe steigt in vielen klassisch objektorientierten Softwareprojekten
die Komplexität überproportional an.  Mit viel Disziplin und regelmäßigen
Refaktorisierungen lässt sich das Problem eine Weile in Schach halten,
aber die wechselseitigen Abhängigkeiten und komplexen Abläufe von
Zustandsveränderungen nehmen mit der Zeit trotzdem zu.
Die funktionale Softwarearchitektur geht an die Strukturierung großer
Systeme anders heran als objektorientierte Ansätze und vermeidet so
viele Quellen von Komplexität und Wechselwirkungen im System.

## Aspekte funktionaler Softwarearchitektur

"Funktionale Softwarearchitektur" steht für das Ergebnis eines
Softwareentwurfs mit den Mitteln der funktionalen Programmierung.
Sie zeichnet sich unter anderem durch folgende Aspekte aus:

* An die Stelle des Objekts mit gekapseltem Zustand tritt die *Funktion*, die auf
  *unveränderlichen Daten* arbeitet.

<!-- Auch sonst werden die *Effekte* eingeschränkt, die Funktionen haben können. -->

* Funktionale Sprachen (ob statisch oder
  dynamisch) erlauben ein von *Typen*  getriebenes,
  systematisches Design von Datenmodellen und Funktionen.

* Statt starrer hierarchischen
  Strukturen entstehen flexible <!-- sogenannte *Kombinatormodelle* und --> in die
  funktionale Programmiersprache *eingebettete domänenspezifische
  Sprachen*.

Wir konzentrieren uns in diesem Artikel auf den ersten Punkt, also
den Umgang mit Funktionen und unveränderlichen Daten.  Dabei werden wir
auch die Rolle von Typen beleuchten.

Der Code zu diesem Artikel ist in einem Github-Repositorium abrufbar:

[`https://github.com/funktionale-programmierung/hearts/`](https://github.com/funktionale-programmierung/hearts/)

## Funktionale Programmiersprachen

Funktionale Softwarearchitektur ist in (fast) jeder Programmiersprache
möglich, aber in einer funktionalen
Sprache wie Haskell, OCaml, Clojure, Scala, Elixir, Erlang, F# oder
Swift ist diese Herangehensweise besonders natürlich.
Funktionale Softwarearchitektur wird in der Regel als
Code ausgedrückt, also nicht in Form von Diagrammen.  Entsprechend
benutzen wir für die Beispiele in diesem Artikel die funktionale
Sprache Haskell[^1], die besonders kurze und elegante Programme
ermöglicht.  Keine Sorge: Wir erläutern den Code, so dass er auch ohne
Vorkenntnisse in Haskell lesbar ist.  Wer dadurch auf Haskell
neugierig geworden ist, kann sich eine Einführung in 
funktionale Programmierung[^2] und ein Buch zu Haskell[^3] zu Gemüte
führen.

## Überblick

Wir erklären den Entwurf einer funktionalen Softwarearchitektur anhand des
Kartenspiel *Hearts*[^4], von dem wir nur die wichtigsten Teile
umsetzen.

Hearts wird zu Viert gespielt.  In jeder Runde eröffnet
eine Spielerin, indem sie eine Karte ausspielt. (Zu Beginn des Spiels
muss das die Kreuz Zwei sein.)  Die nächste Spielerin muss, wenn
möglich, eine Karte mit der gleichen Farbe wie die Eröffnungskarte
ausspielen. Anderenfalls darf sie eine beliebige Karte abwerfen.  Haben alle Spielerinnen eine Karte
ausgespielt, muss die Spielerin den Stich einziehen, deren Karte die
gleiche Farbe wie die Eröffnungskarte hat sowie den höchsten Wert.
Ziel ist, mit den eingezogenen Karten einen möglichst geringen
Punktestand zu erreichen. Dabei zählt die Pik Dame 13 Punkte und jede
Herzkarte einen Punkt; alle weiteren Karten 0 Punkte.

![Modellierung des Spielablaufs](gameplay.pdf)

Als Basis des Entwurfs verwenden wir ein klassisches taktisches Muster aus dem
*Domain-Driven Design*[^5] und modellieren das Kartenspiel auf der
Basis von *domain events*. 
Abbildung 1 zeigt den Ablauf: Jede Spielerin nimmt Events entgegen,
die den bisherigen Spielverlauf repräsentieren, und generiert dafür
Commands, die Spielzüge repräsentieren.  Die "Gameplay"-Komponente
nimmt die Commands entgegen, überprüft sie auf Korrektheit (War die
Spielerin überhaupt dran? War der Spielzug regelkonform?) und
generiert ihrerseits daraus wieder Events.
Das Muster ist das gleiche wie in "objektorientiertem DDD", aber die
Umsetzung unterscheidet sich durch die Verwendung von unveränderlichen Daten und Funktionen. 

Im Beispiel stellen wir die Kommunikation zwischen den einzelnen Komponenten der
Architektur direkt mit Funktionsaufrufen her, aber andere
Mechanismen - nebenläufige Prozesse oder Mikroservices - sind
auch möglich.

## Programmieren mit unveränderlichen Daten

Eine Vorbemerkung: "Unveränderliche Daten" bedeutet, dass es keine Zuweisungen gibt, die Attribute von
Objekten verändern könnten.  Wenn Veränderung modelliert werden soll,
so generiert ein funktionales Programm neue Objekte.  Diese
Einschränkung bietet enorme Vorteile:

* Es gibt niemals Probleme mit verdeckten Zustandsänderungen durch
  Methodenaufrufe oder nebenläufige Prozesse. 
  
* Es gibt keine inkonsistenten Zwischenzustände, wenn ein
  Programm erst das eine Feld, dann das nächste etc. setzt.

* Das Programm kann problemlos durch ein Gedächtnis erweitert werden,
  das zum Beispiel zum früheren Spielständen zurückkehrt, wenn eine
  Spielerin ihren Zug zurücknimmt.

* Es gibt keine verstecken Abhängigkeiten durch die Kommunikation von
  Zustand hinter den Kulissen.

Aus den gleichen Gründen ist in Java das Programmieren mit
Value-Objekten oft nützlich.
  
## Kartenspiel modellieren

Die konkrete Modellierung beginnt mit den Spielkarten.  Die folgende
Deklaration des Datentyps `Card` definiert einen Recordtyp (struct)
und legt damit fest, dass eine Karte eine Farbe ("suit") und einen Wert ("rank") hat.

```haskell
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq, Ord)
```

Die Zeile `deriving (Show, Eq, Ord)` sorgt dafür, dass Haskell
automatisch Funktionen generiert, um einen Wert auszudrucken (`Show` -
analog zum Beispiel zu `toString()` in Java), auf Gleichheit zu testen
(`Eq` - analog zu `equals()`) und mit "größer als" und "kleiner als" zu
vergleichen (`Ord` - analog zu `compareTo()`).

Weiter werden noch Definitionen von `Suit` und `Rank`
benötigt:

```haskell
data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Show, Eq, Ord)
  
data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)
```

Hier handelt es sich um Aufzählungen (vergleichbar mit `enum` in Java) - das `|` steht für "Oder",
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

Die Deklaration von `Cards` definiert auch die "Getter-Funktionen" `suit` und
`rank`, die wie Funktionen verwendet werden. 

Für Hearts wird ein kompletter Satz Karten benötigt.  Der wird durch
folgende Definitionen generiert, die jeweils eine Liste aller Farben,
eine Liste aller Werte und schließlich daraus eine Liste aller Karten
(also aller Kombinationen aus Farben und Werten) konstruiert.

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
Klammern) von Farben ist.  Die Definitionen für `allRanks` und `deck` benutzen
sogenannte Comprehension-Syntax[^6], um die Werte und
schließlich alle Karten aufzuzählen.

Für die Umsetzung eines Kartenspiels müssen die Karten repräsentiert werden,
die eine Spielerin auf der Hand hat: als Menge ("set") von Karten.

```haskell
type Hand = Set Card
```

Der Typ `Set` ist ein generischer Type, der von der Standardbibliothek
importiert wird.  Für `Hand` wird eine `type`-Deklaration verwendet,
die ein Typsynonym definiert.

Einige Hilfsdefinitionen erleichtern den Umgang mit dem Typ `Hand`.
Die Funktion `isHandEmpty` hat den Typ `Hand -> Bool`, was eine 
"Funktion, die eine `Hand` als Eingabe nimmt und ein `Bool` als Ausgabe
liefert" beschreibt:

```haskell
isHandEmpty :: Hand -> Bool
isHandEmpty hand = Set.null hand
```

Die Funktion `containsCard` ist nimmt zwei Argumente und prüft mit
Hilfe der Library-Funktion `Set.member`, ob eine
gegebene Karte zu einer Hand gehört:

```haskell
containsCard :: Card -> Hand -> Bool
containsCard card hand = Set.member card hand
```

Der Typ `Card -> Hand -> Bool` wird erst verständlich,
wenn er von rechts geklammert wird: `Card -> (Hand -> Bool)`.
Das bedeutet, dass die Funktion zunächst eine Karte
akzeptiert und dann *eine Funktion liefert*, die ihrerseits eine Hand
akzeptiert und dann einen booleschen Wert zurückliefert.  Streng
genommen kennt Haskell nur einstellige Funktionen und "simuliert"
mehrstellige Funktionen durch diese Technik[^7].

Die nächste Funktion zeigt beispielhaft, wie der Umgang
mit unveränderlichen Daten funktioniert - `removeCard` entfernt eine
Karte aus einer Hand:

```haskell
removeCard :: Card -> Hand -> Hand
removeCard card hand = Set.delete card hand
```

In idiomatischem Java hätte das `Hand`-Objekt eine Methode `void
removeCard(Card card)`, die den Zustand des `Hand`-Objekts
 entsprechend verändert.  Nicht so in der funktionalen Programmierung, wo
`removeCard` eine neue Hand liefert und die alte Hand unverändert
lässt.  Nach:

```haskell
hand2 = removeCard card hand1
```

ist `hand1` immer noch die "alte" Hand und `hand2` die neue.

Das ist in Haskell nicht nur eine Konvention: Eine Funktion *kann* 
nicht einfach so Objekte "verändern", es handelt es sich im Sprech der
funktionalen Programmierung immer um eine "reine" oder "pure"
Funktion.  Diese *rein funktionale Programmierung* macht die Typsignatur
enorm nützlich, weil sie alles aufführt, was in die Funktion
hineingeht und wieder hinausgeht: Es gibt keine versteckten
Abhängigkeiten zu globalem Zustand und alle Ausgaben stehen hinter dem
rechten Pfeil der Signatur. Daher sind die rechten und linken Seiten
einer Definitionsgleichung überall im Programm jederzeit austauschbar.

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
map :: (a -> b) -> [a] ->[b]
```

Das `a` und das `b` sind *Typvariablen*, in Java-Sprech Generics.
Ausgesprochen steht dort: `map` akzeptiert 
eine Funktion, die aus einem `a` ein `b` macht, sowie eine Liste von `a`s und liefert eine Liste
von `b`s.  Bei `cardsOfTrick` ist `a` das Zwei-Tupel `(PlayerName,
Card)` und `b` ist `Card`.  Die Funktion `snd` extrahiert aus dem
Zwei-Tupel die zweite Komponente und hat deshalb folgenden Typ:

```haskell
snd :: (a, b) -> b
```

Solche generischen Funktionen gibt es (inzwischen) auch in Java, aber
in funktionalen Sprachen kommen sie im Zusammenhang mit
Higher-Order-Funktionen viel häufiger zur Anwendung. 
Sie sind ein wichtiger Aspekt funktionaler
Architektur: Diese macht nicht an der konkreten Modellierung von
fachlichem Wissen halt, sondern erlaubt den Entwicklerinnen,
Abstraktionen zu bilden, die das fachliche Wissen verallgemeinern.

Das Zusammenspiel von generischen Higher-Order-Funktionen und anderen
reinen Funktionen liefert die Basis für ein extrem mächtiges Konstruktionsprinzip:
Weil Funktionen nie etwas
"verstecktes" machen, können sie bedenkenlos zu immer größeren
Gebilden zusammengestöpselt werden.  In OO-Sprachen wächst mit der
Größe das Risiko, dass versteckte Effekte unerwünschte
Wechselwirkungen haben.  Deshalb sind Disziplin und eigene
architektonische Patterns notwendig, um die resultierende Komplexität
in den Griff zu bekommen.  In der funktionalen Programmierung ist das
nicht so. Dementsprechend ist dort das "Programmieren im Großen" dem
"Programmieren im Kleinen" ziemlich ähnlich.

## Spiel-Logik

Die Spiellogik bildet den Mittelbau der Architektur.  Ein Event-Storming liefert folgende
Event-Klassen:

* Die Karten wurden ausgeteilt.
* Eine neue Spielerin ist an der Reihe.
* Eine Spielerin hat eine bestimmte Karte ausgespielt.
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

Das `HandsDealt`-Event trägt eine "Map" zwischen Spielernamen und ihren
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
implementiert.  Die Regeln beziehen sich auf den *Zustand* des
Spiels: welche Karten ausgespielt werden dürfen, welche
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

Die Liste im Feld `gameStatePlayers` wird dabei immer so rotiert,
dass die nächste Spielerin vorn steht.  Für die beiden Felder
`gameStateHands` und `gameStateStacks` müssen jeweils Karten *pro
Spieler* vorgehalten werden, darum sind die dazugehörigen Typen
Syonyme für Maps:

```haskell
type PlayerStacks = Map PlayerName (Set Card)
type PlayerHands  = Map PlayerName Hand
```

Auch bei der Umsetzung der Spielregeln macht sich die funktionale
Architektur bemerkbar:  Während des Spiels wird der Zustand nicht
verändert, sondern jede Änderung erzeugt einen neuen Zustand.  Die
zentrale Funktion für die Verarbeitung eines Commands 
hat deswegen folgende Signatur:

```haskell
processGameCommand :: GameCommand -> GameState -> (GameState, [GameEvent])
```

Mit anderen Worten (Repräsentation des) Zustand vorher rein, Command
rein, Tupel aus (Repräsentation des) neuem Zustand und Liste
resultierender Events raus.  Hier ist die Implementierung der
Gleichung für das `DealHands`-Command:

```haskell
processGameCommand (DealHands hands) state =
  let event = HandsDealt hands
  in (processGameEvent state event, [event])
```

Da dieser Befehl von der "Spielleitung" kommt, führt er immer zu einem
`HandsDealt`-Event.  Der Effekt des Events auf den Zustand wird durch
die Funktion `processGameEvent` berechnet, deren Definition aus
Platzgründen fehlt, aber deren Arbeitsweise sich wieder gut an der
Typsignatur ablesen lässt:

```haskell
processGameEvent :: GameState -> GameEvent -> GameState
```

Die Kernlogik ist in der Gleichung für das `PlayCard`-Command. Diese
verlässt sich auf Hilfsfunktionen `playValid` (die einen Spielzug auf
Korrektheit überprüft), `whoTakesTrick` (die berechnet, wer den Stich
einziehen muss) und `gameOver` (die feststellt, ob das Spiel vorbei
ist).  Der Code führt eine Reihe von Fallunterscheidungen in Form von
`if ... then ... else` durch und bindet lokale Variablen (insbesondere
Events `event1`, `event2` etc. und Zwischenzustände `state1`, `state2`
mit `let`:

```haskell
processGameCommand (PlayCard player card) state =
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

Es ist deutlich zu sehen, dass der Zustand niemals verändert wird und
dass alle Zwischenzustände separate, voneinander unabhängige Objekte sind.

## Zustand verwalten

Die Funktion `processCommand` bildet die Abfolge von Zuständen durch
unterschiedliche Variablen und die Abhängigkeiten dazwischen ab.  Das
ist in vielen Situationen gerade richtig, hier aber unnötig
fehleranfällig - wenn zum Beispiel irgendwo statt `state3` mal
`state2` steht.  Unnötig ist es deshalb, weil die Funktion einen
sequenziellen Prozess abbildet, und dem wäre besser durch eine
sequenzielle Notation gedient.

Sequenzieller Prozesse lassen sich gut durch *Monaden* beschreiben,
ein typisch funktionales Architekturpattern.  Mit Monaden entstehen
immer noch Funktionen, aber die Notation wechselt in eine sequenzielle
Form, die z.B. den Zustand automatisch weiterreicht.

### Peter's code, minimal set

```haskell
type GameConstraints m = (MonadIO m, MonadState GameState m, MonadWriter [GameEvent] m)
```

```haskell
processGameCommandM' :: GameConstraints m => GameCommand -> m ()
processGameCommandM' (DealHands playerHands) =
   processAndPublishEvent (HandsDealt playerHands)
processGameCommandM' (PlayCard playerName card) =
   do playIsValid <- playValidM playerName card
      if playIsValid then
        do processAndPublishEvent (CardPlayed playerName card)
           turnIsOver <- turnOverM
           if turnIsOver then
             do trick <- currentTrickM
                let trickTaker = whoTakesTrick trick
                processAndPublishEvent (TrickTaken trickTaker trick)
                gameIsOver <- gameOverM
                if gameIsOver 
                then processAndPublishEvent (GameOver)
                else processAndPublishEvent (PlayerTurn trickTaker)
           else
             do nextPlayer <- nextPlayerM
                processAndPublishEvent (PlayerTurn nextPlayer)
      else
        do nextPlayer <- nextPlayerM
           processAndPublishEvent (IllegalMove nextPlayer)
           processAndPublishEvent (PlayerTurn nextPlayer)
```

```haskell
processAndPublishEvent :: GameConstraints m => GameEvent -> m ()
processAndPublishEvent gameEvent = do
  processGameEventM gameEvent
  Writer.tell [gameEvent]
```

```haskell
processGameEvent :: GameConstraints m => GameEvent -> m ()
```

```haskell
type PlayerConstraints m = (MonadIO m, MonadWriter [GameCommand] m)
```

```haskell
data PlayerCommandProcessor =
  PlayerCommandProcessor (forall m . PlayerConstraints m =>
                         GameEvent -> m PlayerCommandProcessor)

data PlayerPackage = 
  PlayerPackage
  { playerName :: PlayerName
  , commandProcessor :: PlayerCommandProcessor
  }

runPlayer :: PlayerConstraints m => PlayerCommandProcessor -> GameEvent -> m PlayerCommandProcessor
runPlayer (PlayerCommandProcessor f) gameEvent =
  f gameEvent
```

### Mike's code, minimal set

```haskell
processGameCommandM :: MonadGameEventSourcing monad => GameCommand -> monad ()
processGameCommandM (DealHands playerHands) =
   processGameEventM (HandsDealt playerHands)
processGameCommandM (PlayCard playerName card) =
   do playIsValid <- playValidM playerName card
      if playIsValid then
        do processGameEventM (CardPlayed playerName card)
           turnIsOver <- turnOverM
           if turnIsOver then
             do trick <- currentTrickM
                let trickTaker = whoTakesTrick trick
                processGameEventM (TrickTaken trickTaker trick)
                gameIsOver <- gameOverM
                if gameIsOver 
                then processGameEventM (GameOver)
                else processGameEventM (PlayerTurn trickTaker)
           else
             do nextPlayer <- nextPlayerM
                processGameEventM (PlayerTurn nextPlayer)
      else
        do nextPlayer <- nextPlayerM
           processGameEventM (IllegalMove nextPlayer)
           processGameEventM (PlayerTurn nextPlayer)
```

```haskell
class MonadTrans monadT => Player player monadT event command | player -> monadT, player -> event, player -> command where
  play :: Monad monad => player -> EventProcessor (monadT monad) event command

type EventProcessor monad event command = event -> monad [command]
```

```haskell
type EventSourcingT state event monad = StateT state (WriterT [event] monad)
type MonadEventSourcing state event monad =
  (MonadState state monad, MonadWriter [event] monad)
```

```haskell
type Players monad event command = Map PlayerName (EventProcessor monad event command)

playEvent :: Monad monad => Players monad GameEvent GameCommand -> GameEvent -> monad (Seq GameCommand)

playCommand :: Monad monad => Players monad GameEvent GameCommand -> GameCommand -> GameEventSourcingT monad ()

playGame :: Monad monad => Players monad GameEvent GameCommand -> [Card] -> GameEventSourcingT monad ()
```


## Fazit

Funktionale Softwarearchitektur bedeutet erst einmal, dass ein
Entwickler nicht so leicht Architekturprinzipien verletzen kann: Nicht mal so eben
schnell ein Attribut verändern oder - speziell in Haskell - externe
Effekte auslösen, weil es gerade so passt.  Stattdessen führen neue
Zustände immer gleich zu neuen Objekten und alle Effekte müssen
explizit deklariert werden.  Das ist für Entwicklerinnen mit
OO-Hintergrund gewöhnungsbedürftig.  Diese Einschränkungen
bringen aber Verbesserungen der Architekturqualität mit sich: geringere
Kopplung, weniger Abhängigkeiten, deklarierte und kontrollierte
Effekte - das alles steigert die Robustheit, Flexibilität und
Wartbarkeit des Codes.

<!-- ## Quellen (Fußnoten) -->

[^1]: [`https://www.haskell.org/`](https://www.haskell.org)

[^2]: Michael Sperber, Herbert Klaeren: *Schreibe Dein Programm!*, [`https://www.deinprogramm.de`](https://www.deinprogramm.de/)

[^3]: Hutton, Graham: *Programming in Haskell*, 2nd edition, 2016.

[^4]: Hearts auf Wikipedia, [`https://en.wikipedia.org/wiki/Hearts_(card_game)`](https://en.wikipedia.org/wiki/Hearts_(card_game))

[^5]: Vaughn, Vernon: *Domain-Driven Design Distilled*, Pearson, 2016.

[^6]: Die auch in Python Eingang gefunden hat.

[^7]: Funktionale Programmierer sprechen von *curried functions* und *currying*, [`https://de.wikipedia.org/wiki/Currying`](https://de.wikipedia.org/wiki/Currying). 

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
dynamischen Analysemethoden für JavaScript sowie Typsystemen für
Protokolle. 

