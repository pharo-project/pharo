accessing
category
	| tokens  |
	tokens := source parseLiterals.
	tokens size = 3 ifFalse: [self error: 'Unrecognized category definition'].
	^ tokens at: 3