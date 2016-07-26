parse
parseBlock
	self enterBlock.
	self scanPast: #blockStart level: bracketDepth.
	currentTokenFirst == $:
		ifTrue: [self parseBlockArguments].
	currentTokenFirst == $|
		ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $].
	self scanPast: #blockEnd level: bracketDepth.
	self leaveBlock