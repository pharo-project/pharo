parsing - private
parseNextPattern

	self readNextChar.

	self isEscape ifTrue: [ ^ self parseEscapePattern ].
	self isYearPattern ifTrue: [ ^ self parseYear ].
	self isMonthPattern ifTrue: [ ^ self parseMonth ].
	self isDayPattern ifTrue: [ ^ self parseDay ].
	
	self parseSameChar