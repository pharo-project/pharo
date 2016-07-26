accessing
childrenValueOf: anObject atLevel: anInteger
	^ self children glamourValue: (
		anObject asGlamorousMultiValue, 
		self entity asGlamorousMultiValue,
		anInteger asGlamorousMultiValue,
		self asGlamorousMultiValue)