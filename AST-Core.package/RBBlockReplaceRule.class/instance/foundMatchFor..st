matching
foundMatchFor: aProgramNode 
	| newNode |
	newNode := replaceBlock value: aProgramNode.
	aProgramNode replaceMethodSource: newNode.
	^newNode