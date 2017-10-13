setUps
modifiedMethodDictionaryCopy
	| copy |
	
	copy := self class methodDict copy.
	copy at: #methodAddedToIncreaseTheDict put: Object >> #=.
	^copy.