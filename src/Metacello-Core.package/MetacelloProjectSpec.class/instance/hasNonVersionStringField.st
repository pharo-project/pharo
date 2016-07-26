testing
hasNonVersionStringField
    | hasVersionString hasOperator hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
    hasClassName := self hasClassName.
    hasVersionString := self versionString ~~ nil.
    hasOperator := operator ~~ nil.
    hasProjectPackage := (self file ~~ nil and: [ hasClassName and: [ self className ~= self name ] ])
        or: [ self hasRepository ].
    hasLoads := self loads ~~ nil.
    hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
    hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
    ^ hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt