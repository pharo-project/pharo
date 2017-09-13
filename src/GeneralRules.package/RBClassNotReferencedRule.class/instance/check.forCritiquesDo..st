enumerating
check: aClass forCritiquesDo: aCritiqueBlock

    aClass isUsed ifFalse: [
        aCritiqueBlock cull: (self critiqueFor: aClass) ] 