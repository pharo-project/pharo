visiting
visitBlockNode: aBlockNode 

	aBlockNode comments do: [ :comment | 
			self 
			addStyle: #comment
			forNode: comment ].

	aBlockNode arguments do: [ :argument|
		self 
			addStyle: #blockPatternArg
			forNode: argument ].
		
	"self styleOpenBracket: aBlockNode."	
	self visitNode: aBlockNode body.
	"self styleCloseBracket: aBlockNode"