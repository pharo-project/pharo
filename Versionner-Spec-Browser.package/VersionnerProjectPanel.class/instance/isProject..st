testing
isProject: selection
	| node |
	
	(selection size = 1) ifFalse: [  ^false ].
	node := selection at: 1.
	^ node parentNode isNil