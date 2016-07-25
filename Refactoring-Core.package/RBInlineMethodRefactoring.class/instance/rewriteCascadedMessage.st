transforming
rewriteCascadedMessage
	| index messages |
	messages := sourceMessage parent messages.
	index := (1 to: messages size) 
				detect: [:i | sourceMessage == (messages at: i)]
				ifNone: [0].
	inlineParseTree body addNodesFirst: (messages copyFrom: 1 to: index - 1).
	self removeReturns.
	inlineParseTree body 
		addNodes: (messages copyFrom: index + 1 to: messages size).
	inlineParseTree addReturn