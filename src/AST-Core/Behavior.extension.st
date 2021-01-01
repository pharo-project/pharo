Extension { #name : #Behavior }

{ #category : #'*AST-Core' }
Behavior >> parseTreeFor: aSymbol [	
	^ (self compiledMethodAt: aSymbol) parseTree
]
