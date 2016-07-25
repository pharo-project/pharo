visiting rb nodes
visitBlockNode: aBlockNode
	aBlockNode comments do: [ :comment | self addStyle: #comment from: comment start to: comment stop ].
	aBlockNode arguments do: [ :argument | self addStyle: #blockPatternArg forNode: argument ].
	self styleOpenBracket: aBlockNode.
	self visitNode: aBlockNode body.
	self styleCloseBracket: aBlockNode