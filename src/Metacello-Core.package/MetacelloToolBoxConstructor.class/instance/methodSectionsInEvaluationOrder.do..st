enumeration
methodSectionsInEvaluationOrder: attributes do: aBlock
    "breadth first traversal ... to collect selected sections, then evaluate individual sections in attribute order"

    | selected processed |
    selected := IdentitySet new.
    self methodSection: self inEvaluationOrder: attributes do: [ :methodSection | selected add: methodSection ].
    processed := IdentitySet new.
    attributes
        do: [ :attribute | 
            | list |
            list := OrderedCollection new.
            selected
                do: [ :methodSection | 
                    (processed includes: methodSection)
                        ifFalse: [ 
                            (methodSection attributes includes: attribute)
                                ifTrue: [ 
                                    list add: methodSection.
                                    processed add: methodSection ] ] ].
            list do: aBlock ]