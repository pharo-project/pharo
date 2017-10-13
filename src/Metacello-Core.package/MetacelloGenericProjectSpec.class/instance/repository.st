querying
repository
    | specs |
    self deprecated: 'Use repositories or repositorySpecs'.
    (specs := self repositorySpecs) isEmpty
        ifTrue: [ ^ nil ].
    ^ specs first