private
decrementVersionAt: index
  | int |
  int := self at: index.
  (int := self at: index) isString
    ifFalse: [ 
      int > 0
        ifTrue: [ self at: index put: int - 1 ] ]