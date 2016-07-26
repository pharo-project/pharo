converting
convertToIfNil: msgNode
	msgNode selector: #ifNil:.
	msgNode arguments: {msgNode arguments first}