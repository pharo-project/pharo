multi-character scans
typeTableAt: aCharacter
	^typeTable
		at: aCharacter charCode
		ifAbsent:
			[aCharacter == DoItCharacter
				ifTrue: [#doIt]
				ifFalse: [#xLetter]]