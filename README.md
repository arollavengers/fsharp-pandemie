# fsharp-pandemie
Les virus se répandent


# Links

* [Overview of types in F#](http://fsharpforfunandprofit.com/posts/overview-of-types-in-fsharp/)
* 

# Règles
## Principe général

Réussir à contrer la progression de la maladie dans le monde entier en trouvant des remèdes.

## But du jeu
Les joueurs doivent user de leur force (spécifique) pour lutter contre la progression de 4 maladies à travers le monde.

## Les villes

Le plateau est constitué de villes de différentes couleurs, celles-ci indiquant par quelles maladie elles sont susceptibles d'êtres contaminées. Chaque ville est reliée à certaines de ces voisines (villes dites adjacentes). 

## Contamination

Une ville peut être contaminée, on augmente alors son niveau d'infection (pour cette maladie: couleur en fonction de la maladie).
Si une ville atteint le niveau 3 d'infection pour une maladie, chaque nouvelle contamination n'augmentera pas le niveau, mais déclenchera une éclosion.

### éclosion
Une éclosion contamine les villes adjacentes pour sa maladie. Chaque éclosion est comptabilisé dans la jauge des éclosions.

### éclosion en chaine
Une contamination par éclosion peut entrainer une nouvelle éclosion, sauf dans les villes qui ont déjà subis l'éclosion pour cette couleur pendant ce tour


