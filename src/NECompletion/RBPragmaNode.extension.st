Extension { #name : #RBPragmaNode }

{ #category : #'*NECompletion' }
RBPragmaNode >> completionEntries [
	^Symbol allSymbols 
		select: [ :each | each beginsWith: self selector] 
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ]
]
