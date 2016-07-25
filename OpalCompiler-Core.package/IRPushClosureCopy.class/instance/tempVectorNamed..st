accessing
tempVectorNamed: aName
    (self blockHasTempVector: aName)
        ifTrue: [ ^ blockSequence tempVectorNamed: aName ].
    (self sequenceHasTempVector: aName)
                ifTrue: [ ^ sequence tempVectorNamed: aName ].
    ^ nil