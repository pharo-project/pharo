visitor
visitPragmaNode: aPragmaNode
	super visitPragmaNode: aPragmaNode.
	(aPragmaNode selector = #compilerOptions:)
		ifTrue: [ aPragmaNode asPragma sendTo: aPragmaNode methodNode compilationContext ].