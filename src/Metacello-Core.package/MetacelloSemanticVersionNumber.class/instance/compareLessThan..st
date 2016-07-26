private
compareLessThan: aMetacelloVersionNumber
    | myComponents otherComponents defaultResult |
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    myComponents := self normalVersion.
    otherComponents := aMetacelloVersionNumber normalVersion.
    defaultResult := true.
    (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ defaultResult := false ]
        ifFalse: [ 
            (self compareLessThan: myComponents other: otherComponents version: #'normal')
                ifFalse: [ ^ false ] ].
    myComponents := self preReleaseVersion.
    otherComponents := aMetacelloVersionNumber preReleaseVersion.
    (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ 
            myComponents size > 0
                ifTrue: [ defaultResult := false ] ]
        ifFalse: [ ^ self compareLessThan: myComponents other: otherComponents version: #'preRelease' ].
    myComponents := self buildVersion.
    otherComponents := aMetacelloVersionNumber buildVersion.
    ^ (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ defaultResult ]
        ifFalse: [ self compareLessThan: myComponents other: otherComponents version: #'build' ]