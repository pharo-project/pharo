accessing
localName
	^ name ifNotNil: [
		(name findTokens: FileSystem disk delimiter asString) last]