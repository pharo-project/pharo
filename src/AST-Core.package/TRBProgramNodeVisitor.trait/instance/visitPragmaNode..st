visiting
visitPragmaNode: aPragmaNode
	aPragmaNode arguments do: [ :each | self visitNode: each ]