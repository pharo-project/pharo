"
This class should trigger the RBDefinesEqualNotHashRule critic.
"
Class {
	#name : #RBSmalllintTestDefinesEqualNotHashObject,
	#superclass : #RBSmalllintTestSuperclassObject,
	#category : #'Refactoring-Tests-Critics-Base'
}

{ #category : #comparing }
RBSmalllintTestDefinesEqualNotHashObject >> = anObject [
	^ self == anObject
		or: [self species == anObject species]
]
