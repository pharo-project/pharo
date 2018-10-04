Extension { #name : #RBMessageNode }

{ #category : #'*Refactoring2-Transformations' }
RBMessageNode >> removeFromTree [

	self parent removeNode: self
]

{ #category : #'*Refactoring2-Transformations' }
RBMessageNode >> removeNode: aNode [

	self replaceNode: aNode withNode: aNode receiver
]
