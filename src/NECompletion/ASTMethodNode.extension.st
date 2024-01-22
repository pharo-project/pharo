Extension { #name : 'ASTMethodNode' }

{ #category : '*NECompletion' }
ASTMethodNode >> completionEntries: offset [
	^Symbol selectorTable
		select: [ :each | each beginsWith: self selector]
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ]
]
