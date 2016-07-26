matching
foundMatchFor: aProgramNode 
	| newNode |
	newNode := replaceTree copyInContext: self context.
	aProgramNode replaceMethodSource: newNode.
	newNode copyCommentsFrom: aProgramNode.
	^newNode