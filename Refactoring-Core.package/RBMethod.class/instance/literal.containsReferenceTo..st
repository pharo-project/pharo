private
literal: anObject containsReferenceTo: aSymbol
	anObject = aSymbol
		ifTrue: [ ^ true ].
	anObject class = Array
		ifFalse: [ ^ false ].
	^ anObject anySatisfy: [ :each | self literal: each containsReferenceTo: aSymbol ]