visiting rb nodes
visitPragmaNode: aPragmaNode
	aPragmaNode selectorParts with: aPragmaNode keywordsPositions do: [ :keyword : position|
	self 
		addStyle: #pragmaKeyword
		from: position
		to: position + keyword size ].
	
	aPragmaNode arguments do: [ :each | self visitNode: each ]