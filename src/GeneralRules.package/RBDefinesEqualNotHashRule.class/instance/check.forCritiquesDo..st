enumerating
check: aClass forCritiquesDo: aCritiqueBlock 
	((aClass includesSelector: #=) and: [ (aClass includesSelector: #hash) not ]) ifTrue: [
        aCritiqueBlock cull: (self critiqueFor: aClass) ]