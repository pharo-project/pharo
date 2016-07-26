tokenizing
nextChar
	self atEndOfChars ifTrue: [ ^nil ].
	pos := pos + 1.
	^text at: (pos-1)