operations
incrementPatchVersion
  self size < 2
    ifTrue: [ self at: 2 put: 0 ].
  self size < 3
    ifTrue: [ self at: 3 put: 0 ].
  self incrementVersionAt: 3