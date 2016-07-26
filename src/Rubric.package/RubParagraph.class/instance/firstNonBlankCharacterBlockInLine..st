querying
firstNonBlankCharacterBlockInLine: aLine
	| idx str blanks |
	idx := aLine first.
	str := self text string.
	blanks := { Character space. Character tab }.
	[ idx <= aLine last ]
		whileTrue: [ | c |
			c := str at: idx.
			(blanks includes: c)
				ifFalse: [ ^ self characterBlockForIndex: idx ].
			idx := idx + 1 ].
	^ self characterBlockForIndex: aLine last 