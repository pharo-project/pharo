visiting
visitMethodNode: aMethodNode 

	aMethodNode comments do: [ :comment |
		self 
			addStyle: #comment
			forNode: comment ].
		
	aMethodNode arguments do: [ :argument |
		self 
			addStyle: #patternArg
			forNode: argument ].
		
		aMethodNode selectorParts
		with: aMethodNode keywordsPositions
		do: [ :keyword :position | 
			self
				addStyle: #patternKeyword
				startPosition: position
				stopPosition: position + keyword size - 1 
		].
	
	aMethodNode pragmas do: [ :each | 
		self visitNode: each 
	].
		
	self visitNode: aMethodNode body