accessing
formatedDisplayValueOf: anObject
	^ self format glamourValue: (anObject asGlamorousMultiValue, self entity asGlamorousMultiValue)