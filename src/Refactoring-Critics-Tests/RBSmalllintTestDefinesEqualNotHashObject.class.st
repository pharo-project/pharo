"
This class should trigger the RBDefinesEqualNotHashRule critic.
"
Class {
	#name : 'RBSmalllintTestDefinesEqualNotHashObject',
	#superclass : 'RBSmalllintTestSuperclassObject',
	#category : 'Refactoring-Critics-Tests-Base',
	#package : 'Refactoring-Critics-Tests',
	#tag : 'Base'
}

{ #category : 'comparing' }
RBSmalllintTestDefinesEqualNotHashObject >> = anObject [
	^ self == anObject
		or: [self species == anObject species]
]
