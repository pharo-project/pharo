querying
packageNames
    packageList == nil
        ifTrue: [ ^ #() ].
    ^ self packages map keys asSet