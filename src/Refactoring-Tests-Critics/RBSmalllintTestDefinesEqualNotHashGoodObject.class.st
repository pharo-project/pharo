"
This class shouldn't trigger the RBDefinesEqualNotHashRule critic.
"
Class {
	#name : #RBSmalllintTestDefinesEqualNotHashGoodObject,
	#superclass : #RBSmalllintTestSuperclassObject,
	#category : 'Refactoring-Tests-Critics'
}

{ #category : #comparing }
RBSmalllintTestDefinesEqualNotHashGoodObject >> = anObject [
	^ self == anObject
		or: [self species == anObject species]
]

{ #category : #comparing }
RBSmalllintTestDefinesEqualNotHashGoodObject >> hash [
	^ self species identityHash
]
