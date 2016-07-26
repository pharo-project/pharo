querying
packageNames
    "leave reference to packages for upgrade purposes"

    packages == nil
        ifTrue: [ ^ super packageNames ].
    ^ self packages map keys asSet