project lookup
getConfigurationUnconditionalLoad: unconditionalLoad
    | spec |
    spec := self projectSpec.
    Smalltalk
        at: spec className asSymbol
        ifPresent: [ :cl | 
            unconditionalLoad
                ifFalse: [ ^ cl ] ].
    (spec := self lookupProjectSpecFor: spec) projectPackage load.
    ^ Smalltalk at: spec className asSymbol