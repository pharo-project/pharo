querying
lastNonBlankCharacterBlockInLine: aLine
	| idx str blanks |
	idx := aLine last.
	str := self text string.
	blanks := { Character space. Character tab. Character cr. Character lf }.
	[ idx >= aLine first ]
		whileTrue: [ 
			(blanks includes: (str at: idx))
				ifFalse: [ ^ self characterBlockForIndex: idx ].
			idx := idx - 1 ].
	^ self characterBlockForIndex: aLine first 