querying
repository
    self deprecated: 'Use repositories or repositorySpecs'.
    self repositorySpecs isEmpty
        ifTrue: [ ^ nil ].
    ^ self repositorySpecs first