character classification
isLetter: char 
	| value codeCat |
	value := char charCode.
	value > (GeneralCategory size - 1)
		ifTrue: [^ false].
	^ (codeCat := GeneralCategory at: value + 1) >= Ll
		and: [codeCat <= Lu]