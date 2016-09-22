visiting
visitPragmaNode: aPragmaNode

	aPragmaNode selectorParts 
		with: aPragmaNode keywordsPositions
		do: [ :keyword :position |
			self 
				addStyle: #pragmaKeyword
				startPosition: position
				stopPosition: position + keyword size 
		].
	
	aPragmaNode arguments do: [ :each | self visitNode: each ]