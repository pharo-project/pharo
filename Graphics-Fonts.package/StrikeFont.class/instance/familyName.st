accessing
familyName
	| nameString firstDigit |
	nameString := self name.
	firstDigit := nameString findFirst: [ :m | m isDigit ].
	^ firstDigit > 0
		ifTrue: [ (nameString copyFrom: 1 to: firstDigit - 1) trimRight ]
		ifFalse: [ nameString ]