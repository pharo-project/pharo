Extension { #name : #RBMessageNode }

{ #category : #'*NECompletion' }
RBMessageNode >> completionEntries [
	| selectors |
	selectors := (receiver hasProperty: #type)
		ifTrue: [ (receiver propertyAt: #type) allSelectors ] 	
		ifFalse: [ Symbol selectorTable ].
		
	^selectors 
		select: [ :each | each beginsWith: self selector ] 
		thenCollect: [ :each | NECSelectorEntry contents: each node: self ]
]

{ #category : #'*NECompletion' }
RBMessageNode >> completionToken [

	^ self selector
]
