Extension { #name : #RBAssignmentNode }

{ #category : #'*Refactoring2-Transformations' }
RBAssignmentNode >> removeNode: aNode [

	self replaceNode: aNode withNode: aNode receiver
]
