operations
decrementMinorVersion
  self size < 2
    ifTrue: [ self at: 2 put: 0 ].
  self decrementVersionAt: 2