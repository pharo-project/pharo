parsing year - private
parseYear

	self isFourDigitYearPattern ifTrue: [ ^ self parseFourDigitYear ].
	self isTwoDigitYearPattern ifTrue: [ ^ self parseTwoDigitYear ].
	self parseVariableDigitYear