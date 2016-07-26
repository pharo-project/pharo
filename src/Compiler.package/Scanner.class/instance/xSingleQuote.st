multi-character scans
xSingleQuote
	"String."

	self step.
	buffer reset.
	[hereChar == $' 
		and: [aheadChar == $' 
				ifTrue: [self step. false]
				ifFalse: [true]]]
		whileFalse: 
			[hereChar == DoItCharacter
				ifTrue: [^self offEnd: 'Unmatched string quote'].
			buffer nextPut: self step].
	self step.
	token := buffer contents.
	tokenType := #string