reading
categoryFromDoIt: aString
	| tokens  |
	tokens := aString parseLiterals.
	tokens size = 3 ifFalse: [self error: 'Unrecognized category definition'].
	^ tokens at: 3