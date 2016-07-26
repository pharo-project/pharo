parsing month - private
parseMonth

	self isTwoDigitMonthPattern ifTrue: [ ^self parseTwoDigitMonth ].
	self parseVariableDigitMonth 