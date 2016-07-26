testing
analyze: aString

	| list |
	list := super analyze: aString.
	url := list at: 1.
	^ list at: 2