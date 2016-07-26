parse
parseExternalCall
	self scanNext.
	self scanPast: #externalCallType.
	currentToken = '*'
		ifTrue: [self scanPast: #externalCallTypePointerIndicator].
	currentTokenFirst isDigit
		ifTrue: [self scanPast: #integer]
		ifFalse: [self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentTokenFirst == $(.
	self scanPast: #leftParenthesis.
	[currentTokenFirst ~~ $)]
		whileTrue: [self scanPast: #externalCallType.
			currentToken = '*'
				ifTrue: [self scanPast: #externalCallTypePointerIndicator]].
	self scanPast: #rightParenthesis.
	currentToken = 'module:'
		ifTrue: [self scanPast: #module.
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd