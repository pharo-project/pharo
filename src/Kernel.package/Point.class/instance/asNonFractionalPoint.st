converting
asNonFractionalPoint
	(x isFraction or: [ y isFraction ])
		ifTrue: [ ^ x asFloat @ y asFloat ]