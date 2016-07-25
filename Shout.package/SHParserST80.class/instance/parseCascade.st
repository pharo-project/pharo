parse
parseCascade
	self parseKeyword.
	[currentTokenFirst == $;]
		whileTrue: [self scanPast: #cascadeSeparator.
			self parseKeyword]