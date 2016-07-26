testing
includesCategory: aCategory
	^ (super includesCategory: aCategory) and: [ environmentDictionaries anySatisfy: [ :env | env includesCategory: aCategory ] ]