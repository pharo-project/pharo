testing
currentVersionMismatch
           | wc loadedPkgName |
           (wc := self workingCopy) isNil
                   ifTrue: [ ^ false ].
           wc ancestry ancestors isEmpty
                   ifTrue: [ ^ false ].
           loadedPkgName := wc ancestry ancestors first name.
           loadedPkgName = self name
                   ifTrue: [ ^false ].
            ^ true
