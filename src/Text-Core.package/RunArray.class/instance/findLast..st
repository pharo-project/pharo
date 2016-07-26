enumerating
findLast: aBlock
	| index |
	index := values size + 1.
	[(index := index - 1) >= 1] whileTrue:
		[(aBlock value: (values at: index)) ifTrue: [^(1 to: index) detectSum: [:i | runs at: i]]].
	^0