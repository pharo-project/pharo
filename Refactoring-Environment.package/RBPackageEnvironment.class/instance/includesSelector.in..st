testing
includesSelector: aSelector in: aClass
	^ (environment includesSelector: aSelector in: aClass) 
		and: [ self packages anySatisfy: [ :package | 
				package includesSelector: aSelector ofClass: aClass ] ]