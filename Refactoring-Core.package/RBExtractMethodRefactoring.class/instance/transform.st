transforming
transform
	| existingSelector |
	existingSelector := self existingSelector.
	self nameNewMethod: (existingSelector isNil 
				ifTrue: [self getNewMethodName]
				ifFalse: [existingSelector]).
	existingSelector isNil 
		ifTrue: 
			[class compile: extractedParseTree newSource
				withAttributesFrom: (class methodFor: selector)].
	class compileTree: modifiedParseTree