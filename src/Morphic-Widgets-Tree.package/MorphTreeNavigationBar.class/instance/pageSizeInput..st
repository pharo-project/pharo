accessing
pageSizeInput: aString
	| input |
	input := aString trimBoth.
	input ifEmpty: [input := '1'].
	(input beginsWith: '/' )
		ifTrue: [ | numberOfPages |
			input := (input copyFrom: 2 to: input size) trimBoth.
			numberOfPages := Integer readFromString: input.
			numberOfPages > 0 
				ifTrue: [self changePageSize: (self nodeList size // numberOfPages)]
				ifFalse: [treeMorph flash.
					^ false]]
		ifFalse: [ | newPageSize |
			newPageSize := Integer readFromString: input.
			newPageSize > 0 
				ifTrue: [self changePageSize: newPageSize]
				ifFalse: [treeMorph flash.
					^ false]].
	^ true
