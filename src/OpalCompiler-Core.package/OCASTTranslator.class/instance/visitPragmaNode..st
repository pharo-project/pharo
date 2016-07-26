visitor-double dispatching
visitPragmaNode: aPragmaNode
	methodBuilder addPragma: aPragmaNode asPragma.
	aPragmaNode isPrimitiveError ifTrue: [
		methodBuilder storeTemp: (aPragmaNode argumentAt: #error:) value].
