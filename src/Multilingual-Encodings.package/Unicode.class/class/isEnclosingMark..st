character classification
isEnclosingMark: char
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [ ^ false ].
	^ (GeneralCategory at: index) = Me