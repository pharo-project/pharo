private
incrementNormalVersionAt: index
  | int col |
  col := self normalVersion.
  int := col at: index.
  col at: index put: int + 1