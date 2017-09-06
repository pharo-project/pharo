running
check: aMethod forCritiquesDo: aCritiqueBlock
    (self ignoredSelectors includes: aMethod selector)
        ifTrue: [ ^ self ].
    aMethod methodClass superclass
        ifNotNil: [ :superclass |
            (superclass lookupSelector: aMethod selector)
            ifNotNil: [ :overridenMethod |
                aMethod ast = overridenMethod ast ifTrue: [
                     aCritiqueBlock cull: (self critiqueFor: aMethod) ] ] ]