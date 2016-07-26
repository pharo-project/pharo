visitor-double dispatching
visitPragmaNode: aPragmaNode
	aPragmaNode arguments: (aPragmaNode arguments collect: [ :each | self visitNode: each ])