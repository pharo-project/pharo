merging
mergeSpec: anotherSpec
    | newSpec map anotherRepositories |
    newSpec := super mergeSpec: anotherSpec.
    map := anotherSpec mergeMap.
    (anotherRepositories := map at: #'repositories') isEmpty not
        ifTrue: [ 
            newSpec
                repositories:
                    (self repositories isEmpty
                        ifTrue: [ anotherRepositories ]
                        ifFalse: [ self repositories mergeSpec: anotherRepositories ]) ].
    ^ newSpec