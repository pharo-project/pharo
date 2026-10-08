"
I model the change in a superclass that should be propagated to the subclasses.
"
Class {
	#name : 'ShSuperclassChanged',
	#superclass : 'ShAbstractChange',
	#category : 'Shift-ClassBuilder-Changes',
	#package : 'Shift-ClassBuilder',
	#tag : 'Changes'
}

{ #category : 'testing' }
ShSuperclassChanged >> hasToMigrateInstances [
	^ true
]

{ #category : 'testing' }
ShSuperclassChanged >> instancesHasShrinked [
	
	^ builder oldClass classLayout size > builder newClass classLayout size
]

{ #category : 'testing' }
ShSuperclassChanged >> propagateToSubclasses: anotherBuilder [
	anotherBuilder changes add: self
]
