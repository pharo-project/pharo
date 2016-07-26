testing
includesClass: aClass
	^ (environment includesClass: aClass) and: [ aClass selectors anySatisfy: [ :each | self includesSelector: each in: aClass ] ]