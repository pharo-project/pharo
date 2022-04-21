Extension { #name : #ASTMessageNode }

{ #category : #'*NECompletion' }
ASTMessageNode >> completionEntries: offset [
	| selectors |
	selectors := (receiver hasProperty: #type)
		ifTrue: [ (receiver propertyAt: #type) allSelectors ] 	
		ifFalse: [ Symbol selectorTable ].
		
	^selectors 
		select: [ :each | each beginsWith: self selector ] 
		thenCollect: [ :each | NECSelectorEntry contents: each node: self ]
]

{ #category : #'*NECompletion' }
ASTMessageNode >> completionToken: offset [

	^ self selector
]
