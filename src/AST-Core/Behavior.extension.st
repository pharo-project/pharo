Extension { #name : 'Behavior' }

{ #category : '*AST-Core' }
Behavior >> parseTreeForSelector: aSymbol [
	^ (self compiledMethodAt: aSymbol) parseTree
]
