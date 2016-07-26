accessing
optionAt: aString ifAbsent: absentBlock
	^ (aString size = 1)
		ifTrue: [ self shortOptionAt: aString ifAbsent: absentBlock ]
		ifFalse: [ self longOptionAt: aString ifAbsent: absentBlock ]