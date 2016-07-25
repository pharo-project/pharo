visiting
visitBlockNode: aBlockNode 
	aBlockNode arguments do: [ :argument|
		self 
			addStyle: #blockPatternArg
			forNode: argument ].
	
	self visitNode: aBlockNode body