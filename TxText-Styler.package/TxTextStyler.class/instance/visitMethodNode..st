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
		
	aMethodNode selectorParts do: [ :selectorPart |
		self 
			addStyle: #patternKeyword
			forNode: selectorPart ].
	
	aMethodNode pragmas
		do: [ :each | self visitNode: each ].
		
	self visitNode: aMethodNode body