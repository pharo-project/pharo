"
I model Class Variables and therefore Pool Variables, too. I am stored as a binding in the classPool of the class defining me.

The compiler forwards bytecode generation to me for accessing the variable.

You can subclass me and implement #read and #write: for creating special kinds of globals that can be used as special class Variables (similar to special Slots).

"
Class {
	#name : #ClassVariable,
	#superclass : #LiteralVariable,
	#category : #'Slot-Core-Variables'
}

{ #category : #printing }
ClassVariable >> definitionString [
	"non special globals are defined by the symbol"
	^ self needsFullDefinition
		ifTrue: [ super definitionString ]
		ifFalse: [ self name printString ]

]

{ #category : #'code generation' }
ClassVariable >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self.


]

{ #category : #'code generation' }
ClassVariable >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self.
]

{ #category : #testing }
ClassVariable >> isClassVariable [
	^ true
]

{ #category : #testing }
ClassVariable >> isReferenced [
 	"A class variable can only be accessed in the defintionClass and its subclasses (both class and instance side)"
 	
	^ self definingClass withAllSubclasses
 		anySatisfy: [ :behavior | behavior hasSelectorReferringTo: self . behavior class hasSelectorReferringTo: self ]
]

{ #category : #queries }
ClassVariable >> usingMethods [
	self definingClass isPool ifTrue: [ ^ super usingMethods ].
	"if we are a class variable, we only need to search the sublasses and metaclasses"
	^ self definingClass withAllSubclasses flatCollect: [ :class | 
			(class whichMethodsReferTo: self), (class class whichMethodsReferTo: self) ]
]
