private
decrementNormalVersionAt: index
  | int col |
  col := self normalVersion.
  int := col at: index.
  int > 0
    ifTrue: [ col at: index put: int - 1 ]