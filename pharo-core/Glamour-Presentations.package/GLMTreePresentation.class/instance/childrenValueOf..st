accessing
childrenValueOf: anObject
	^ self children glamourValue: (anObject asGlamorousMultiValue, self entity asGlamorousMultiValue)