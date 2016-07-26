parse
parseBraceArray
	self parseStatementListForBraceArray.
	self failUnless: currentTokenFirst == $}.
	self scanPast: #rightBrace