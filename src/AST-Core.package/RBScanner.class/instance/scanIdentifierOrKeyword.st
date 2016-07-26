private-scanning
scanIdentifierOrKeyword
	| name |
	self scanName.
	(currentCharacter = $: and: [stream peek ~= $=]) 
		ifTrue: [^self scanKeyword].
	name := buffer contents.
	name = 'true' 
		ifTrue: 
			[^RBLiteralToken 
				value: true
				start: tokenStart
				stop: self previousStepPosition].
	name = 'false' 
		ifTrue: 
			[^RBLiteralToken 
				value: false
				start: tokenStart
				stop: self previousStepPosition].
	name = 'nil' 
		ifTrue: 
			[^RBLiteralToken 
				value: nil
				start: tokenStart
				stop: self previousStepPosition].
	^RBIdentifierToken value: name start: tokenStart