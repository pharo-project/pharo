expression types
expression

	(hereType == #word and: [tokenType == #leftArrow])
		ifTrue: [^ self assignment: self variable].
	hereType == #leftBrace
		ifTrue: [self braceExpression]
		ifFalse: [self primaryExpression ifFalse: [^ false]].
	(self messagePart: 3 repeat: true)
		ifTrue: [hereType == #semicolon ifTrue: [self cascade]].
	^ true