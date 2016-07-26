testing
includesClass: aClass
	^ (super includesClass: aClass) and: [ environmentDictionaries anySatisfy: [ :env | env includesClass: aClass ] ]