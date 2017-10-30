character classification
isUppercase: char 
	| index |
	index := char charCode + 1.
	index > GeneralCategory size
		ifTrue: [^ false].
	^ (GeneralCategory at: index) = Lu