Extension { #name : #RBPragmaNode }

{ #category : #'*NECompletion' }
RBPragmaNode >> completionEntries [
	^Symbol allSymbols 
		select: [ :each | each beginsWith: self selector] 
		thenCollect: [ :each | NECEntry contents: each node: self ]
]
