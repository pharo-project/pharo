Extension { #name : #RBMessageNode }

{ #category : #'*NECompletion' }
RBMessageNode >> completionEntries [
	^(receiver hasProperty: #type)
		ifTrue: [ self select: (receiver propertyAt: #type) allSelectors beginningWith: self selector ] 	
		ifFalse: [ self select: Symbol selectorTable beginningWith: self selector ]
]

{ #category : #'*NECompletion' }
RBMessageNode >> completionToken [

	^ self selector
]
