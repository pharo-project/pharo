Extension { #name : 'OCMethodNode' }

{ #category : '*NECompletion' }
OCMethodNode >> completionEntries: offset [
	^Symbol selectorTable
		select: [ :each | each beginsWith: self selector]
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ]
]
