running
check: aClass forCritiquesDo: aCritiqueBlock 
	(aClass selectors size >= self methodsCount) ifTrue: [
        aCritiqueBlock cull: (self critiqueFor: aClass) ]
		