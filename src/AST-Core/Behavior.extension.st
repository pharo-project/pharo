Extension { #name : #Behavior }

{ #category : #'*ast-core' }
Behavior >> parseTreeFor: aSymbol [	
	^ (self compiledMethodAt: aSymbol) parseTree
]
