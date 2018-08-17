Extension { #name : #RBCascadeNode }

{ #category : #'*Refactoring2-Transformations' }
RBCascadeNode >> removeNode: aNode [

	self messages remove: aNode ifAbsent: [ ]
]
