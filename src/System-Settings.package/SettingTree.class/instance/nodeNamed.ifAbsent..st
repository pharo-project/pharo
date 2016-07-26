private tree building
nodeNamed: aSymbol ifAbsent: aBlock
	^ self nodeList detect: [:d | d settingDeclaration name = aSymbol] ifNone: aBlock