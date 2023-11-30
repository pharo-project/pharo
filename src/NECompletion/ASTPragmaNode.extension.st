Extension { #name : 'ASTPragmaNode' }

{ #category : '*NECompletion' }
ASTPragmaNode >> completionEntries: offset [
	^Symbol allSymbols
		select: [ :each | each beginsWith: self selector]
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ]
]
