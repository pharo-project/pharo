"
This class shouldn't trigger the RBDefinesEqualNotHashRule critic.
"
Class {
	#name : 'RBSmalllintTestDefinesEqualNotHashGoodObject',
	#superclass : 'RBSmalllintTestSuperclassObject',
	#category : 'Refactoring-Critics-Tests-Base',
	#package : 'Refactoring-Critics-Tests',
	#tag : 'Base'
}

{ #category : 'comparing' }
RBSmalllintTestDefinesEqualNotHashGoodObject >> = anObject [
	^ self == anObject
		or: [self species == anObject species]
]

{ #category : 'comparing' }
RBSmalllintTestDefinesEqualNotHashGoodObject >> hash [
	^ self species identityHash
]
