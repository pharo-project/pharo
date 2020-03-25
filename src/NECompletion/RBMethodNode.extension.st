Extension { #name : #RBMethodNode }

{ #category : #'*NECompletion' }
RBMethodNode >> completionEntries: offset [
	^Symbol selectorTable 
		select: [ :each | each beginsWith: self selector] 
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ]

]
