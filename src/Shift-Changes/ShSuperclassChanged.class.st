"
I model the change in a superclass that should be propagated to the subclasses.
"
Class {
	#name : #ShSuperclassChanged,
	#superclass : #ShAbstractChange,
	#category : #'Shift-Changes'
}

{ #category : #testing }
ShSuperclassChanged >> hasToMigrateInstances [
	^ true.
]

{ #category : #testing }
ShSuperclassChanged >> propagateToSubclasses: anotherBuilder [
	anotherBuilder changes add: self
]
