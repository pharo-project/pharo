pool definition
initializeInstVarNames: aClass prefixedBy: aString

	| token |
	aClass instVarNames doWithIndex:[:instVarName :index| | value aToken |
		aToken := (aString, instVarName first asUppercase asString, (instVarName copyFrom: 2 to: instVarName size),'Index') asSymbol.
		value := index - 1.
		(self bindingOf: aToken) ifNil:[self addClassVarNamed: aToken].
		(self bindingOf: aToken) value: value.
	].
	token := (aString, aClass name,'Size') asSymbol.
	(self bindingOf: token) ifNil:[self addClassVarNamed: token].
	(self bindingOf: token) value: aClass instSize.