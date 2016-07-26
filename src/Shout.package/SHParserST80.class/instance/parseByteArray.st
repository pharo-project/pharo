parse
parseByteArray
	[currentTokenFirst == $]]
		whileFalse: [self scanPast: #integer].
	self scanPast: #byteArrayEnd