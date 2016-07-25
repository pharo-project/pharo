character classification
isLowercase: char 
	| value |
	value := char charCode.
	value > (GeneralCategory size - 1)
		ifTrue: [^ false].
	^ (GeneralCategory at: value + 1)
		= Ll