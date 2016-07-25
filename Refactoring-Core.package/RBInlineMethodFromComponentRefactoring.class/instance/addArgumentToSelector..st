transforming
addArgumentToSelector: aSymbol 
	^aSymbol isInfix 
		ifTrue: [#value:value:]
		ifFalse: [(aSymbol , 'value:') asSymbol]