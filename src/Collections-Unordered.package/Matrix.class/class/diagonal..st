special instance creation
diagonal: aCollection
	|r i|
	r := self zeros: aCollection size.
	i := 0.
	aCollection do: [:each | i := i+1. r at: i at: i put: each].
	^r