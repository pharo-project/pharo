testing
shortOptionAt: aString ifAbsent: absentBlock
	| index |
	index := (arguments indexOf: '-', aString) + 1.
	^ (index <= 1 or: [ index > arguments size ])
		ifFalse: [ arguments at: index ]
		ifTrue: absentBlock