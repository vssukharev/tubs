
# 'tubs' cutting edge lazily evaluated table editor

## Disclaimer

AI ASSISTED:
1. Partially file [src/main/scala/org/tubs/executor/CellParser.scala]
2. Helped with setting up Json (de)serializer
3. Scala metaprogramming assistance
4. Scalafx frontend (lots of slop ought to be then manually unslopped)

## All possible expressions with their precedence (from lowest to highest)
1. for ... do ...  /  if ... then ... else ...
2. Range operator ( .. )
3. Logical ( and, or )
3. Comparisons ( == ,  < , etc.)
4. Additive ( + ,  - )
5. Multiplicative ( * ,  / ,  mod )
6. Power ( ^ ) - right associative
7. Unary operators ( - ,  not )
8. Primary expressions:
  1. literals - integer, floating, boolean
  2. variables - identifiers starting with lower case letters
  3. cell references - B2, B\[i], \[j]2, \[i]\[j]

## TODO
- Google docs appdata clear
- Help page for user interface

