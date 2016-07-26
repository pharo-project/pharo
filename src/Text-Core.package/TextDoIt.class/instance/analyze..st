testing
analyze: aString

	| list |
	list := super analyze: aString.
	evalString := list at: 1.
	^ list at: 2