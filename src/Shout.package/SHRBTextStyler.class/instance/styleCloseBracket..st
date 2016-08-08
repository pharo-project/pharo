private
styleCloseBracket: aBlockNode
	bracketLevel := bracketLevel - 1.
	(aBlockNode right isZero or: [  aBlockNode value isNil ]) ifTrue:[^ self].
	self addStyle: self currentClosedBracketStyleName from: aBlockNode right to: aBlockNode right