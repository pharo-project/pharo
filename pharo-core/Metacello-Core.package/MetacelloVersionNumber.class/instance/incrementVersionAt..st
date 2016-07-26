private
incrementVersionAt: index
  | int |
  int := self at: index.
  (int := self at: index) isString
    ifFalse: [ self at: index put: int + 1 ]