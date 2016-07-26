printing
printOn: aStream delimiter: aCharacter
	self isWorkingDirectory ifTrue: [aStream nextPut: $.. ^ self].
	super printOn: aStream delimiter: aCharacter