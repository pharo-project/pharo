Extension { #name : #RBMessageNode }

{ #category : #'*NECompletion' }
RBMessageNode >> completionEntries [
	^ self 
		propertyAt: #type 
		ifPresent:  [ :class | self select: class allSelectors beginningWith: self selector ] 
		ifAbsent:  [ self select: Symbol selectorTable beginningWith: self selector ]
]

{ #category : #'*NECompletion' }
RBMessageNode >> completionToken [

	^ self selector
]
