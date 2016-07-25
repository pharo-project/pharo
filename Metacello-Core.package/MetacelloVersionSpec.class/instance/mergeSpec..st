merging
mergeSpec: anotherSpec
    | newSpec map anotherPackages |
    newSpec := super mergeSpec: anotherSpec.
    map := anotherSpec mergeMap.
    (anotherPackages := map at: #'packageList') isEmpty not
        ifTrue: [ 
            newSpec
                packages:
                    (self packages isEmpty
                        ifTrue: [ anotherPackages ]
                        ifFalse: [ self packages mergeSpec: anotherPackages ]) ].
    ^ newSpec