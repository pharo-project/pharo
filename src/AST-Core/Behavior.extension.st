Extension { #name : #Behavior }

{ #category : #'*AST-Core' }
Behavior >> parseTreeFor: aSymbol [

	self
		deprecated: 'Use parseTreeForSelector: instead'
		transformWith: '`@receiver parseTreeFor: `@argument'
			-> '`@receiver parseTreeForSelector: `@argument'.
	^ self parseTreeForSelector: aSymbol
]

{ #category : #'*AST-Core' }
Behavior >> parseTreeForSelector: aSymbol [
	^ (self compiledMethodAt: aSymbol) parseTree
]
