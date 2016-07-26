comparing
= aMetacelloVersionNumber
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    ^ self compareEqualTo: aMetacelloVersionNumber