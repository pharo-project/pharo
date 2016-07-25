navigation
chunkSizeInput: aString
	| input newPageSize |
	input := aString trimBoth.
	input ifEmpty: [treeMorph flash.
		^ false].
	newPageSize := Integer readFromString: input.
	newPageSize > 0 
		ifTrue: [self changePageSize: newPageSize.
			self nextPage.
			pageSizeEditor 
				ifNotNil: [pageSizeEditor takeKeyboardFocus]]
		ifFalse: [treeMorph flash.
			^ false].
	^ true