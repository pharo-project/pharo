running
check: aMethod forCritiquesDo: aCritiqueBlock 
	(aMethod ast isPrimitive not and: 
		[ matcher 
			executeMethod: aMethod ast
			initialAnswer: false ]) ifTrue: [
        aCritiqueBlock cull: (self critiqueFor: aMethod) ]