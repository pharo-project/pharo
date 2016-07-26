parse
parseArray
	[currentTokenFirst == $)]
		whileFalse: [self parseLiteralArrayElement].
	self scanPast: #arrayEnd