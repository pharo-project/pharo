private
styleOpenBracket: aBlockNode
	self addStyle: self currentOpenedBracketStyleName  from: aBlockNode left to: aBlockNode left.
	bracketLevel := bracketLevel + 1