Extension { #name : #RBMethodNode }

{ #category : #'*NECompletion' }
RBMethodNode >> completionEntries [
	^Symbol selectorTable 
		select: [ :each | each beginsWith: self selector] 
		thenCollect: [ :each | NECEntry contents: each node: self ]

]
