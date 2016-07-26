querying
projectClass
    self className == nil
        ifTrue: [ ^ nil ].
    ^ Smalltalk at: self className asSymbol ifAbsent: [  ]