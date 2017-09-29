character classification
isOtherSymbol: char
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [ ^ false ].
	^ (GeneralCategory at: index) = So