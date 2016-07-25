visiting rb nodes
visitMethodNode: aMethodNode 
aMethodNode comments do: [ :comment|
		self 
			addStyle: #comment
			from: comment start
			to: comment stop ].
	aMethodNode arguments do: [ :argument|
		self 
			addStyle: #patternArg
			forNode: argument ].
		
	aMethodNode selectorParts with: aMethodNode keywordsPositions do:  [ :keyword :position |
		self 
			addStyle: #patternKeyword
			from: position
			to: position + keyword size - 1 ].
	
	aMethodNode pragmas
		do: [ :each | self visitNode: each ].
		
	self visitNode: aMethodNode body