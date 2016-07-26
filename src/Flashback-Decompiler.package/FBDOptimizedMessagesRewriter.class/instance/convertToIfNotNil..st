converting
convertToIfNotNil: msgNode
	msgNode selector: #ifNotNil:.
	msgNode arguments: {msgNode arguments last}