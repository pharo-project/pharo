testing
includesSelector: aSelector in: aClass
	^ (super includesSelector: aSelector in: aClass) and: [ environmentDictionaries anySatisfy: [ :env | env includesSelector: aSelector in: aClass ] ]