enumerating
check: aNode forCritiquesDo: aCriticBlock
    (aNode isMessage and: [ 
    self badSelectors includes: aNode selector ])
        ifTrue: [ aCriticBlock cull: (self critiqueFor: aNode) ]