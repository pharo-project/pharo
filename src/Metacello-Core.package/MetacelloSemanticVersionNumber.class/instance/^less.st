comparing
< aMetacelloVersionNumber
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    ^ self compareLessThan: aMetacelloVersionNumber