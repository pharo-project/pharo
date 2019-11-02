Extension { #name : #RBMethodNode }

{ #category : #'*NECompletion' }
RBMethodNode >> completionEntries [
	^ self select: Symbol selectorTable beginningWith: self selector
]
