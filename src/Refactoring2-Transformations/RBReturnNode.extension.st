Extension { #name : #RBReturnNode }

{ #category : #'*Refactoring2-Transformations' }
RBReturnNode >> removeNode: aNode [

	self parent removeNode: self
]
