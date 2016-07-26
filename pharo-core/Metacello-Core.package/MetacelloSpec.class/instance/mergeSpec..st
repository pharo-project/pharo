merging
mergeSpec: aSpec
    | newSpec nonOverridable |
    self validateMergeForSpec: aSpec.
    newSpec := self copy.
    nonOverridable := self nonOverridable.
    aSpec mergeMap
        keysAndValuesDo: [ :key :value | 
            (nonOverridable includes: key)
                ifFalse: [ 
                    value ~~ nil
                        ifTrue: [ newSpec instVarNamed: key asString put: value ] ] ].
    ^ newSpec