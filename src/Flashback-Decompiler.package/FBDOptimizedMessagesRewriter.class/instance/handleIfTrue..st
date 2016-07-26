handling
handleIfTrue: msgNode
	(self isIfNilIfNotNil: msgNode)
		ifTrue: [ 
			msgNode receiver: msgNode receiver receiver.
			msgNode selector: #ifNil: ]