private
styleCloseBracket: aBlockNode
	bracketLevel := bracketLevel - 1.
	self addStyle: self currentClosedBracketStyleName from: aBlockNode right to: aBlockNode right